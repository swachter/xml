package eu.swdev.xml.pushparser

import java.io.StringWriter
import javax.xml.stream.XMLEventReader

import eu.swdev.pushparser.PushParserMod
import eu.swdev.xml.base.{Location, PublicId, ResId, SystemId}
import eu.swdev.xml.name._
import shapeless.HNil

/**
  */
trait XmlPushParserMod extends PushParserMod {

  type Input = XmlEvent

  type State = XmlParserState

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
                             namespaces: List[Namespaces],
                             startedElements: List[QName],
                             log: List[String],
                             data: List[StateData])

  sealed trait StateData

  case class StartedElementData(attrs: Map[QName, String], processed: Set[QName]) extends StateData

  case object NoStateData extends StateData

  val initialState = XmlParserState(List(Namespaces.initial), Nil, Nil, List(NoStateData))

  //

  def abort[O](state: State, msg: String): Abort[O] = Abort(state.copy(log = msg :: state.log))

  def fail[O](string: String) = Parser{abort(_, string)}

  val startDocument: Parser[Location] = Parser(state => {
    Await {
      case Some(e: StartDocumentEvent) =>
          Done(e.location, XmlParserState(state.namespaces, state.startedElements, state.log, NoStateData :: state.data))
      case i => abort(state, s"can not start document - unexpected input: $i")

    }
  })

  def startElement(name: QName): Parser[Location] = Parser(state => {
    Await {
      case Some(e: StartElementEvent) =>
        if (name == e.name) {
          val ns = state.namespaces.head.nestedScope(e.newNamespaces) :: state.namespaces
          val es = e.name :: state.startedElements
          Done(e.location, XmlParserState(ns, es, state.log, StartedElementData(e.attrs, Set[QName]()) :: state.data))
        } else {
          abort(state, s"can not start element; names differ - name: $name; event.name: ${e.name}")
        }
      case i => abort(state, s"can not start element - name: $name; unexpected input: $i")
    }
  })

  val endDocument: Parser[Location] = Parser(state => {
    Await {
      case Some(e: EndDocumentEvent) =>
        Done(e.location, XmlParserState(state.namespaces, state.startedElements, state.log, state.data.tail))
      case i => abort(state, s"can not end document - unexpected input: $i")

    }
  })

  val endElement: Parser[Unit] = Parser(state => {
    Await {
      case Some(e: EndElementEvent) =>
        if (e.name == state.startedElements.head) {
          Done((), XmlParserState(state.namespaces.tail, state.startedElements.tail, state.log, state.data.tail))
        } else {
          abort(state, s"can not end element; names differ - event.name: ${e.name}; head: ${state.startedElements.head}")
        }
      case i => abort(state, s"can not end element - unexpected input: $i")
    }
  })

  def selectAttrs(filter: QName => Boolean): Parser[Map[QName, String]] = Parser {
    case state@XmlParserState(_, _, _, StartedElementData(attrs, processed) :: tail) => {
      val selectedAttrs = attrs.filterKeys(qn => filter(qn) && !processed.contains(qn))
      Done(selectedAttrs, state.copy(data = StartedElementData(attrs, processed ++ selectedAttrs.keys) :: tail))
    }
    case state => abort(state, s"can not extract open attributes")
  }

  def optionalAttr(an: QName): Parser[Option[String]] = Parser {
    case state@XmlParserState(_, _, _, StartedElementData(attrs, processed) :: tail) => {
      if (attrs.contains(an)) {
        Done(Some(attrs(an)), state.copy(data = StartedElementData(attrs, processed + an) :: tail))
      } else {
        Done(None, state)
      }
    }
    case state => abort(state, s"can not extract optional attribute - name: $an")
  }

  def requiredAttr(an: QName): Parser[String] = Parser {
    case state@XmlParserState(_, _, _, StartedElementData(attrs, processed) :: tail) => {
      if (attrs.contains(an)) {
        Done(attrs(an), state.copy(data = StartedElementData(attrs, processed + an) :: tail))
      } else {
        abort(state, s"missing attribute - name: $an")
      }
    }
    case state => abort(state, s"can not extract required attribute - name: $an")
  }

  def document[O](p: Parser[O]) = for {
    _ <- startDocument
    o <- p
    _ <- endDocument
  } yield o

  val rawXml: Parser[String] = Parser(state => {

    def concat(strings: List[String]): String = {
      val size = strings.map(_.length).sum
      val sb = strings.foldRight(new StringBuilder(size))((s, accu) => { accu.append(s); accu })
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

}

trait XmlEventReaderInputs { self: XmlPushParserMod =>

  def inputs(reader: XMLEventReader): self.DriveInputs = {

    import javax.xml.namespace.{QName => Jqn}
    import javax.xml.stream.{events => je}

    implicit def toQName(jqn: Jqn): QName = {
      val ns = jqn.getNamespaceURI
      val ln = jqn.getLocalPart
      QNameFactory.caching(new Namespace(ns), new LocalName(ln))
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
          val newNamespaces = e.getNamespaces.asScala.asInstanceOf[Iterator[je.Namespace]].map(n => new Prefix(n.getPrefix) -> new Namespace(n.getNamespaceURI)).toMap
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
      Stream.empty
    }

    go.filter(e => if (e.isCharacters) !e.asCharacters().isWhiteSpace else true).map(convertEvent(_))

  }

}

