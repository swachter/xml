package eu.swdev.xml.xsd.instantiation

import eu.swdev.xml.name.{Namespace, QNameFactory, QName}
import eu.swdev.xml.schema.ComplexType
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._

/**
  */

trait JobMod {

  case class Job[A](run: (JobConfig, JobLog) => (A, JobLog)) { self =>

    def map[B](f: A => B): Job[B] = Job { (conf, log) =>
      val (a, l) = self.run(conf, log)
      (f(a), l)
    }

  }
  
  trait Step[C] {
    
    def map[D](f: C => D): Step[D] = this match {
      case Await(name, symbolSpace, rec) => Await(name, symbolSpace, mapReceive(rec, f))
      case Done(value, log) => Done(f(value), log)
      case Abort(log) => Abort(log)
      case Created(value, next) => Created(value, next map f)
    }
    
    def flatMap[D](f: C => Step[D]): Step[D] = this match {
      case Await(name, symbolSpace, rec) => Await(name, symbolSpace, flatMapReceive(rec, f))
      case Done(value, log) => f(value) prependLog log
      case Abort(log) => Abort(log)
      case Created(value, next) => Created(value, next flatMap f)
      
    }
    
    private def prependLog(log: JobLog): Step[C] = this match {
      case Await(name, symbolSpace, rec) => Await(name, symbolSpace, prependReceive(rec, log))
      case Done(value, l) => Done(value, concatLog(log, l))
      case Abort(l) => Abort(concatLog(log, l))
      case Created(value, next) => Created(value, next prependLog log)
    }

    private def mapReceive[X, D](rec: Option[X] => Step[C], f: C => D): Option[X] => Step[D] = ox => rec(ox) map f
    private def flatMapReceive[X, D](rec: Option[X] => Step[C], f: C => Step[D]): Option[X] => Step[D] = ox => rec(ox) flatMap f
    private def prependReceive[X](rec: Option[X] => Step[C], log: JobLog): Option[X] => Step[C] = ox => rec(ox) prependLog log

  }


  case class Await[C, X](name: QName, symbolSpace: SymbolSpace[X], rec: Option[X] => Step[C]) extends Step[C]

  case class Done[C](value: C, log: JobLog) extends Step[C]

  case class Abort[C](log: JobLog) extends Step[C]
  
  case class Created[C](value: ComplexType, next: Step[C]) extends Step[C]
  
  object Step {

    def unit[A](a: A): Step[A] = Done(a, emptyJobLog)

    def map2[A, B, C](ja: Step[A], jb: Step[B])(f: (A, B) => C): Step[C] = for {
      a <- ja
      b <- jb
    } yield f(a, b)

    def sequence[A](seq: Seq[Step[A]]): Step[Seq[A]] = seq.foldRight(unit(Seq[A]()))((job, acc) => map2(job, acc)(_ +: _))

  }

  sealed trait SymbolSpace[X] {
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

  type JobConfig = SchemaElem
  type JobMsg = String
  type JobLog = List[JobMsg]

  def jobError(msg: String): JobMsg= msg
  def concatLog(l1: JobLog, l2: JobLog): JobLog = l1 ++ l2

  implicit def jobMsgToJobLog(jobMsg: JobMsg): JobLog = jobMsg :: Nil
  val emptyJobLog = Nil

  def abort[C](msg: String): Step[C] = Abort(jobError(msg))

  def await[C: SymbolSpace](name: QName): Step[C] = Await[C, C](name, implicitly[SymbolSpace[C]], {
    case Some(c) =>  Done(c, emptyJobLog)
    case None => Abort(jobError(s"unresolved reference - symbol space: ${implicitly[SymbolSpace[C]].name}; name: $name"))
  })

  def created(ct: ComplexType): Step[Unit] = Created(ct, Done((), emptyJobLog))

  //
  //
  //

  def complexTypeJob(cmp: ComplexTypeElem): Step[ComplexType] = for {
    baseType <- await[Type](cmp.content.base)
    attrs <- attrsModelJob(cmp.content)
    ct = ComplexType(???, baseType, ???, attrs, null, cmp.abstrct.value, ???, ???, ???)
    _ <- created(ct)
  } yield ct

  def attrsModelJob(cmp: ComplexTypeContent): Step[AttrsModel] = {
    val (attrs, anyAttribute) = cmp match {
      case _: SimpleContentModel => (Nil, None)
      case m: ComplexContentAbbrev => (m.attrs, m.anyAttribute)
      case m: ComplexDerivation => (m.attrs, m.anyAttribute)
    }
    attrsModelJob(attrs, anyAttribute) 
  }

  def attrsModelJob(attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem]): Step[AttrsModel] = {
    val attrJobs: Seq[Step[AttrsModel]] = attrs map {
      case Left(ae) => ??? // attrsModelJob(ae)
      case Right(ag) => attrsModelJob(ag)
    }
    val listJob: Step[Seq[AttrsModel]] = Step.sequence(attrsModelJob(anyAttribute) +: attrJobs)
    listJob.map(_.foldLeft(AttrsModel.empty)((am, acc) => acc.add(am)))
  }

  def attrsModelJob(ref: AttributeGroupRefElem): Step[AttrsModel] = await[AttrGroup](ref.ref) map (_.attrsModel)

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

  def attrsModelJob(anyAttribute: Option[AnyAttributeElem]): Step[AttrsModel] = Step.unit(anyAttribute match {
    case Some(aae) => {
      val disallowedNames = aae.notQName.map(_.foldRight(DisallowedNames.empty)((item, acc) => item match {
        case QNameItem.Qn(qn) => acc.copy(qNames = acc.qNames + qn)
        case QNameItem.Defined => acc.copy(defined = true)
      })).getOrElse(DisallowedNames.empty)
      val namespaceConstraint = (aae.namespace, aae.notNamespace) match {
        case (Some(ns), _) => ns match {
          case NamespaceDefToken.Any => NamespaceConstraint.Any
          case NamespaceDefToken.Other => ???
          case NamespaceDefToken.Items(list) => NamespaceConstraint.Enum(convertNamespaceTokens(list))
        }
        case (_, Some(nn)) => NamespaceConstraint.Not(convertNamespaceTokens(nn))
        case _ => NamespaceConstraint.Any
      }
      AttrsModel(Map(), Some(Wildcard(namespaceConstraint, disallowedNames, aae.processContents.value)))
    }
    case None => AttrsModel.empty
  })

  def attrDeclJob(ae: AttributeElemL): Step[AttrDecl] = (ae.name, ae.ref) match {
    case (Some(an), _) => for {

    } yield AttrDecl(an, ae.targetNamespace.)
    case (_, Some(ar)) => await[AttrDecl](ar)
  }

  def convertNamespaceTokens(list: List[NamespaceItemToken]): Set[Namespace] = {
    list.map {
      case NamespaceItemToken.Local => Namespace.NoNamespace
      case NamespaceItemToken.TargetNamespace => ???
      case NamespaceItemToken.Uri(uri) => new Namespace(uri.toString)
    } toSet
  }

}

