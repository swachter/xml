package eu.swdev.xml.pushparser

import java.io.{StringReader, StringWriter}
import javax.xml.stream.events.Comment
import javax.xml.stream.{XMLInputFactory, XMLEventReader}
import javax.xml.transform.Source

import eu.swdev.pushparser.PushParserMod
import eu.swdev.xml.base.{Location, PublicId, ResId, SystemId}
import eu.swdev.xml.log.Messages
import eu.swdev.xml.name._
import shapeless.HNil

import scala.util.{Failure, Success, Try}

/**
  */
trait XmlPushParserMod extends PushParserMod {

  type Input = XmlEvent

  type State = XmlParserState

  type Payload

  //

  sealed trait XmlEvent {
    def location: Location

    def rawXml: String
  }

  sealed trait StartDocumentEvent extends XmlEvent

  sealed trait StartElementEvent extends XmlEvent {
    def name: QName

    def newNamespaces: TraversableOnce[(Prefix, Namespace)]

    def attrs: Map[QName, String]
  }

  sealed trait EndElementEvent extends XmlEvent {
    def name: QName
  }

  sealed trait EndDocumentEvent extends XmlEvent

  sealed trait CharacterEvent extends XmlEvent {
    def text: String
  }

  //

  case class XmlParserState(
    namespacesStack: List[Namespaces],
    startedElements: List[QName],
    log: Messages,
    dataStack: List[StateData],
    payload: Payload
  ) {
    def updateData(stateData: StateData): XmlParserState = copy(dataStack = stateData :: dataStack.tail)
  }

  sealed trait StateData

  case class StartedElementData(attrs: Map[QName, String], processed: Set[QName]) extends StateData

  case object NoStateData extends StateData

  def initialState(initialPayload: Payload) = XmlParserState(List(Namespaces.initial), Nil, Nil, List(NoStateData), initialPayload)

  //

  def abort[O](state: State, msg: String): Abort[O] = Abort(state.copy(log = msg :: state.log))

  def fail[O](string: String) = Parser {
    abort(_, string)
  }

  val getPayload: Parser[Payload] = Parser { state => Done(state.payload, state) }

  def setPayload(payload: Payload): Parser[Unit] = Parser { state => Done((), state.copy(payload = payload))}

  val startDocument: Parser[Location] = Parser(state => {
    Await {
      case Some(e: StartDocumentEvent) =>
        Done(e.location, state.copy(dataStack = NoStateData :: state.dataStack))
      case i => abort(state, s"can not start document - unexpected input: $i")

    }
  })

  def startElement(name: QName): Parser[Location] = Parser(state => {
    Await {
      case Some(e: StartElementEvent) =>
        if (name == e.name) {
          val ns = state.namespacesStack.head.nestedScope(e.newNamespaces) :: state.namespacesStack
          val es = e.name :: state.startedElements
          Done(e.location, state.copy(namespacesStack = ns, startedElements = es, dataStack = StartedElementData(e.attrs, Set[QName]()) :: state.dataStack))
        } else {
          abort(state, s"can not start element; names differ - name: $name; event.name: ${e.name}")
        }
      case i => abort(state, s"can not start element - name: $name; unexpected input: $i")
    }
  })

  val endDocument: Parser[Location] = Parser(state => {
    Await {
      case Some(e: EndDocumentEvent) =>
        Done(e.location, state.copy(dataStack = state.dataStack.tail))
      case i => abort(state, s"can not end document - unexpected input: $i")

    }
  })

  val endElement: Parser[Unit] = Parser { state =>
    state.dataStack.headOption match {
      case Some(StartedElementData(attrs, processed)) => {
        Await {
          case Some(e: EndElementEvent) =>
            if (e.name == state.startedElements.head) {
              val unprocessedAttrs = attrs.filter(t => !processed.contains(t._1))
              val resLog = if (unprocessedAttrs.isEmpty) state.log else s"unprocessed attributes: $unprocessedAttrs" :: state.log
              Done((), state.copy(namespacesStack = state.namespacesStack.tail, startedElements = state.startedElements.tail, log = resLog, dataStack = state.dataStack.tail))
            } else {
              abort(state, s"can not end element; names differ - event.name: ${e.name}; head: ${state.startedElements.head}")
            }
          case Some(i) => abort(state, s"can not end element '${state.startedElements.head}' - unexpected input: $i at position: ${i.location}")
          case i => abort(state, s"can not end element - missing input: $i")
        }
      }
      case _ => abort(state, s"can not end element; no element has started - unexpected parser state: $state")
    }
  }

  def selectAttrs(filter: QName => Boolean): Parser[Map[QName, String]] = Parser { state =>
    state.dataStack.headOption match {
      case Some(StartedElementData(attrs, processed)) => {
        val selectedAttrs = attrs.filterKeys(qn => filter(qn) && !processed.contains(qn))
        Done(selectedAttrs, state.updateData(StartedElementData(attrs, processed ++ selectedAttrs.keys)))
      }
      case _ => abort(state, s"can not extract open attributes")
    }
  }

  def optionalAttr(an: QName): Parser[Option[String]] = Parser { state =>
    state.dataStack.headOption match {
      case Some(StartedElementData(attrs, processed)) => {
        if (attrs.contains(an)) {
          Done(Some(attrs(an)), state.updateData(StartedElementData(attrs, processed + an)))
        } else {
          Done(None, state)
        }
      }
      case _ => abort(state, s"can not extract optional attribute - name: $an")
    }
  }

  def requiredAttr(an: QName): Parser[String] = Parser { state =>
    state.dataStack.headOption match {
      case Some(StartedElementData(attrs, processed)) => {
        if (attrs.contains(an)) {
          Done(attrs(an), state.updateData(StartedElementData(attrs, processed + an)))
        } else {
          abort(state, s"missing attribute - name: $an")
        }
      }
      case _ => abort(state, s"can not access attributes; invalid parser state - attribute name: $an; parser state: $state")
    }
  }

  def resolveQn(lexicalRep: String): Parser[QName] = Parser(state => {
    val (pr, ln) = QName.parse(lexicalRep)
    val p = pr.getOrElse(NoPrefix)
    // the default namespace is considered for QNames
    state.namespacesStack.head.namespaceForPrefix(p) match {
        case Some(n) => Done(QNameFactory.caching(n, ln), state)
        case None => abort(state, s"unbounded namespace prefix: $p")
      }
    }
  )

  def parseInt(lexicalRep: String): Parser[Int] = Parser(state => {
    Try(lexicalRep.toInt) match {
      case Success(i) => Done(i, state)
      case Failure(ex) => abort(state, s"invalid integer: $lexicalRep; exception: ${ex.getMessage}")
    }
  })

  def document[O](p: Parser[O]) = for {
    _ <- startDocument
    o <- p
    _ <- endDocument
  } yield o

  val rawXml: Parser[String] = Parser(state => {

    def concat(strings: List[String]): String = {
      val size = strings.map(_.length).sum
      val sb = strings.foldRight(new StringBuilder(size))((s, accu) => {
        accu.append(s);
        accu
      })
      sb.toString()
    }

    def go(depth: Int, strings: List[String]): Step[String] = {
      Await(_ match {
        case Some(event) => {
          event match {
            case e: StartElementEvent => go(depth + 1, event.rawXml :: strings)
            case e: EndElementEvent => {
              if (depth == 0) {
                Replay(List(e), Done(concat(strings), state))
              } else {
                go(depth - 1, event.rawXml :: strings)
              }
            }
            case e => go(depth, event.rawXml :: strings)
          }
        }
        case None => Done(concat(strings), state)
      })
    }
    go(0, Nil)
  })

  def parseDocument[O](p: Parser[O], state: State, inputs: DriveInputs): DriveResult[O] = document(p).drive(state, inputs)
}

trait XmlEventReaderInputs {
  self: XmlPushParserMod =>

  def inputs(reader: XMLEventReader): self.DriveInputs = {

    import javax.xml.namespace.{QName => Jqn}
    import javax.xml.stream.{events => je}

    implicit def toQName(jqn: Jqn): QName = {
      val ns = jqn.getNamespaceURI
      val ln = jqn.getLocalPart
      QNameFactory.caching(Namespace(ns), LocalName(ln))
    }

    trait XmlEventImpl extends XmlEvent {

      def underlying: je.XMLEvent

      def location = {
        val ul = underlying.getLocation
        Location(ResId(if (ul.getPublicId != null) Some(new PublicId(ul.getPublicId)) else None, if (ul.getSystemId != null) Some(new SystemId(ul.getSystemId)) else None), ul.getLineNumber, ul.getColumnNumber)
      }

      def rawXml: String = {
        val writer = new StringWriter()
        underlying.writeAsEncodedUnicode(writer)
        writer.toString
      }
    }

    case class StartDocumentEventImpl(underlying: je.StartDocument) extends StartDocumentEvent with XmlEventImpl

    case class StartElementEventImpl(
      name: QName,
      attrs: Map[QName, String],
      newNamespaces: Map[Prefix, Namespace],
      underlying: je.StartElement) extends StartElementEvent with XmlEventImpl

    case class EndElementEventImpl(name: QName, underlying: je.EndElement) extends EndElementEvent with XmlEventImpl

    case class EndDocumentEventImpl(underlying: je.EndDocument) extends EndDocumentEvent with XmlEventImpl

    case class CharacterEventImpl(text: String, underlying: je.Characters) extends CharacterEvent with XmlEventImpl

    def convertEvent(event: je.XMLEvent): XmlEvent = {
      event match {
        case e: je.Characters => CharacterEventImpl(e.getData, e)
        case e: je.StartElement => {
          import scala.collection.JavaConverters._
          val attrs = e.getAttributes.asScala.asInstanceOf[Iterator[je.Attribute]].map(a => toQName(a.getName) -> a.getValue).toMap
          val newNamespaces = e.getNamespaces.asScala.asInstanceOf[Iterator[je.Namespace]].map(n => Prefix(n.getPrefix) -> Namespace(n.getNamespaceURI)).toMap
          StartElementEventImpl(e.getName, attrs, newNamespaces, e)
        }
        case e: je.EndElement => EndElementEventImpl(e.getName, e)
        case e: je.StartDocument => StartDocumentEventImpl(e)
        case e: je.EndDocument => EndDocumentEventImpl(e)
      }
    }

    def go: Stream[je.XMLEvent] = if (reader.hasNext) {
      Stream.cons(reader.nextEvent, go)
    } else {
      reader.close()
      Stream.empty
    }

    go.filter(e => if (e.isCharacters) !e.asCharacters().isWhiteSpace else true).filter(!_.isInstanceOf[Comment]).map(convertEvent(_))

  }

  private val inputFactory = XMLInputFactory.newInstance()

  def inputs(source: Source): DriveInputs = {
    val reader = inputFactory.createXMLEventReader(source)
    inputs(reader)
  }



}

