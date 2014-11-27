package eu.swdev.xml.xsd.instantiation

import eu.swdev.xml.name.{Namespace, QNameFactory, QName}
import eu.swdev.xml.schema.ComplexType
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._

import scala.reflect.ClassTag

/**
  */

trait JobMod {

  case class Job[A](run: State => Step[A]) { self =>

    def map[B](f: A => B): Job[B] = Job { state => run(state) map f }

    def flatMap[B](f: A => Job[B]): Job[B] = Job { state =>

      def go(step: Step[A]): Step[B] = step match {
        case Await(name, symbolSpace, rec) => Await(name, symbolSpace, rec andThen go)
        case Done(a, s1) => f(a).run(s1)
        case Abort(state) => Abort(state)
        case Created(o, next) => Created(o, go(next))
      }

      go(run(state))
    }

  }

  object Job {

    def unit[A](a: A) = Job { state => Done(a, state) }

    def map2[A, B, C](ja: Job[A], jb: Job[B])(f: (A, B) => C): Job[C] = for {
      a <- ja
      b <- jb
    } yield f(a, b)

    def sequence[A](seq: Seq[Job[A]]): Job[Seq[A]] = seq.foldRight(unit(Seq[A]()))((job, acc) => map2(job, acc)(_ +: _))

  }

  trait Step[+A] {

    def map[B](f: A => B): Step[B] = this match {
      case Await(name, symbolSpace, rec) => Await(name, symbolSpace, rec andThen Step.lift(f))
      case Done(a, state) => Done(f(a), state)
      case Abort(state) => Abort(state)
      case Created(a, next) => Created(a, next map f)
    }

  }

  object Step {
    def lift[A, B](f: A => B): Step[A] => Step[B] = sa => sa map f
  }

  case class Await[A, I](name: QName, symbolSpace: SymbolSpace[I], rec: Option[I] => Step[A]) extends Step[A]

  case class Done[A](value: A, state: State) extends Step[A]

  case class Abort(state: State) extends Step[Nothing]
  
  case class Created[A, O](value: O, next: Step[A]) extends Step[A]
  
  sealed trait SymbolSpace[X] {
    type SymbolType = X
    def name: String
  }

  object SymbolSpace {
    implicit case object Type extends SymbolSpace[Type] {
      def name = "type"
    }
    implicit case object ElemDecl extends SymbolSpace[ElemDecl] {
      def name = "element declaration"
    }
    implicit case object AttrDecl extends SymbolSpace[AttrDecl] {
      def name = "attribute declaration"
    }
    implicit case object Group extends SymbolSpace[Group] {
      def name = "group"
    }
    implicit case object AttrGroup extends SymbolSpace[AttrGroup] {
      def name = "attribute group"
    }
    //implicit case object IdentityConstraint extends SymbolSpace[IdentityConstraint]
    //implicit case object Notation extends SymbolSpace[Notation]
  }

  trait Awaitable[X] {
    type SymbolType
    val symbolSpace: SymbolSpace[SymbolType]
    def checkSymbolType(s: SymbolType): Option[Class[_]]
  }

  implicit def symbolsAreAwaitable[S](implicit ss: SymbolSpace[S]) = new Awaitable[S] {
    type SymbolType = S
    val symbolSpace = ss
    override def checkSymbolType(s: S) = None
  }

  implicit val simpleTypeIsAwaitable = new Awaitable[SimpleType] {
    type SymbolType = Type
    val symbolSpace = SymbolSpace.Type
    override def checkSymbolType(s: Type) = if (s.isInstanceOf[SimpleType]) None else Some(classOf[SimpleType])
  }


  case class JobConf(schemaElem: SchemaElem) {
    def schemaTargetNamespace: Namespace = Namespace(schemaElem.targetNamespace)
    def attributeFormDefault: Form = schemaElem.attributeFormDefault.value
  }
  
  type JobMsg = String
  type JobLog = List[JobMsg]

  case class State(config: JobConf, log: JobLog)

//  def jobError(msg: String): JobMsg= msg
//  def concatLog(l1: JobLog, l2: JobLog): JobLog = l1 ++ l2
//
//  implicit def jobMsgToState(jobMsg: JobMsg): Sta = jobMsg :: Nil

  def addError(state: State, msg: String): State = state.copy(log = msg +: state.log)

  val emptyJobLog = Nil

  def abort[C](msg: String): Job[C] = Job[C] { state => Abort(addError(state, msg)) }

//  def await[A: SymbolSpace](name: QName): Job[A] = Job { state => Await[A, A](name, implicitly[SymbolSpace[A]], {
//    case Some(a) => Done(a, state)
//    case None => Abort(addError(state, s"unresolved reference - symbol space: ${implicitly[SymbolSpace[A]].name}; name: $name"))
//  })}

  def await[A](name: QName)(implicit ev: Awaitable[A]): Job[A] = Job { state => Await[A, ev.symbolSpace.SymbolType](name, ev.symbolSpace, {
    case Some(s) => {
      ev.checkSymbolType(s) match {
        case None => Done(s.asInstanceOf[A], state)
        case Some(requiredType) => Abort(addError(state, s"reference has not the required type - symbol space: ${ev.symbolSpace.name}; name: $name; required type: ${requiredType.getName}; actual type: ${s.getClass.getName}"))
      }
    }
    case None => Abort(addError(state, s"unresolved reference - symbol space: ${ev.symbolSpace.name}; name: $name"))
  })}



  def created[A](a: A): Job[Unit] = Job { state => Created(a, Done((), state)) }

//  def getState: Job[State] = Job { state => Done(state, state) }
//  
//  def setState(state: State): Job[Unit] = Job { _ => Done((), state) }
  
  def getConf: Job[JobConf] = Job { state => Done(state.config, state) }
  
  //
  //
  //

  def complexTypeJob(cmp: ComplexTypeElem): Job[ComplexType] = for {
    baseType <- await[Type](cmp.content.base)
    attrs <- attrsModelJob(cmp.content)
    ct = ComplexType(???, baseType, ???, attrs, null, cmp.abstrct.value, ???, ???, ???)
    _ <- created(ct)
  } yield ct

  def attrsModelJob(cmp: ComplexTypeContent): Job[AttrsModel] = {
    val (attrs, anyAttribute) = cmp match {
      case _: SimpleContentModel => (Nil, None)
      case m: ComplexContentAbbrev => (m.attrs, m.anyAttribute)
      case m: ComplexDerivation => (m.attrs, m.anyAttribute)
    }
    attrsModelJob(attrs, anyAttribute) 
  }

  def attrsModelJob(attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem]): Job[AttrsModel] = {
    val attrJobs: Seq[Job[AttrsModel]] = attrs map {
      case Left(ae) => ??? // attrsModelJob(ae)
      case Right(ag) => attrsModelJob(ag)
    }
    val listJob: Job[Seq[AttrsModel]] = Job.sequence(attrsModelJob(anyAttribute) +: attrJobs)
    listJob.map(_.foldLeft(AttrsModel.empty)((am, acc) => acc.add(am)))
  }

  def attrsModelJob(ref: AttributeGroupRefElem): Job[AttrsModel] = await[AttrGroup](ref.ref) map (_.attrsModel)

//  def attrsModelJob(ae: AttributeElemL): Job[AttrsModel] = {
//    val attrUseJob: Job[AttrUse] = (ae.name, ae.ref) match {
//      case (Some(an), _) =>
//      case (_, Some(an)) => for {
//
//      } yield AttrUse()
//      case _ => abort(s"invalid attribute declaration at: ${ae.loc}")
//    }
//    attrUseJob.map(u => AttrsModel(Map(u.decl.name -> u), None))
//  }

  // 3.10.2.2
  def attrsModelJob(anyAttribute: Option[AnyAttributeElem]): Job[AttrsModel] = for {
    conf <- getConf
  } yield {
    anyAttribute match {
      case Some(aae) => {
        val disallowedNames = aae.notQName.map(_.foldRight(DisallowedNames.empty)((item, acc) => item match {
          case QNameItem.Qn(qn) => acc.copy(qNames = acc.qNames + qn)
          case QNameItem.Defined => acc.copy(defined = true)
        })).getOrElse(DisallowedNames.empty)
        val namespaceConstraint = (aae.namespace, aae.notNamespace) match {
          case (Some(ns), _) => ns match {
            case NamespaceDefToken.Any => NamespaceConstraint.Any
            case NamespaceDefToken.Other => NamespaceConstraint.Not(Set(Namespace.NoNamespace, conf.schemaTargetNamespace))
            case NamespaceDefToken.Items(list) => NamespaceConstraint.Enum(convertNamespaceTokens(list, conf))
          }
          case (_, Some(nn)) => NamespaceConstraint.Not(convertNamespaceTokens(nn, conf))
          case _ => NamespaceConstraint.Any
        }
        AttrsModel(Map(), Some(Wildcard(namespaceConstraint, disallowedNames, aae.processContents.value)))
      }
      case None => AttrsModel.empty
    }
  }

  // 3.2.2.2
  def attrDeclJob(ae: AttributeElemL): Job[AttrDecl] = (ae.name, ae.ref) match {
    case (Some(an), _) => for {
      conf <- getConf
      simpleType <- simpleTypeJob(ae)
    } yield {
      val targetNamespace: Namespace = ae.targetNamespace.map(Namespace(_)).getOrElse {
        if (ae.form.map(_ == Form.Qualified).getOrElse(false ) || (ae.form == None && conf.attributeFormDefault == Form.Qualified)) {
          conf.schemaTargetNamespace
        } else {
          Namespace.NoNamespace
        }
      }

      AttrDecl(an, targetNamespace, simpleType, None, ae.inheritable.getOrElse(false))
    }
    case (_, Some(ar)) => await[AttrDecl](ar)
  }

  // 3.2.2.1 / 3.2.2.2
  def simpleTypeJob(ae: AttributeElem): Job[SimpleType] = {
    ae.refType.fold(ae.simpleType.fold(Job.unit[SimpleType](anySimpleType))(simpleTypeJob(_)))(qn => await[SimpleType](qn))
  }

  def simpleTypeJob(st: SimpleTypeElem): Job[SimpleType] = ???

  def convertNamespaceTokens(list: List[NamespaceItemToken], conf: JobConf): Set[Namespace] = {
    list.map {
      case NamespaceItemToken.Local => Namespace.NoNamespace
      case NamespaceItemToken.TargetNamespace => conf.schemaTargetNamespace
      case NamespaceItemToken.Uri(uri) => Namespace(uri)
    } toSet
  }

}

