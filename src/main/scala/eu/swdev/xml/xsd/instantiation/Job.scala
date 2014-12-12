package eu.swdev.xml.xsd.instantiation

import eu.swdev.xml.base.SomeValue
import eu.swdev.xml.name._
import eu.swdev.xml.schema.ComplexType
import eu.swdev.xml.schema.Facets.{HasLength, FacetOp}
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  */

trait JobMod {

  case class Job[+A](run: State => Step[A]) { self =>

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

    def &[O2](p2: => Job[O2]): Job[(A, O2)] = for {
      v1 <- this
      v2 <- p2
    } yield (v1, v2)


  }

  object Job {

    def unit[A](a: A) = Job { state => Done(a, state) }

    def map2[A, B, C](ja: Job[A], jb: Job[B])(f: (A, B) => C): Job[C] = for {
      a <- ja
      b <- jb
    } yield f(a, b)

    def sequence[A](seq: Seq[Job[A]]): Job[Seq[A]] = seq.foldRight(unit(Seq[A]()))((job, acc) => map2(job, acc)(_ +: _))

    def traverse[A, B](as: Seq[A])(f: A => Job[B]): Job[Seq[B]] =
      as.foldRight(unit(Seq[B]()))((a, p) => map2(f(a), p)(_ +: _))
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

  def addError(state: State, msg: String): State = state.copy(log = msg +: state.log)

  val emptyJobLog = Nil

  def abort[C](msg: String): Job[C] = Job[C] { state => Abort(addError(state, msg)) }

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

  // part 2; 4.1.2
  def simpleTypeJob(st: SimpleTypeElem): Job[SimpleType] = st.derivation match {
    case d: SimpleTypeRestrictionElem => for {
      typeName <- typeName(st.name)
      baseType <- d.base.fold(simpleTypeJob(d.tpe.get))(await[SimpleType](_))
      restrictedType <- simpleTypeRestriction(typeName, baseType, d.facetSpecs)
    } yield restrictedType
    case d: ListElem => for {
      typeName <- typeName(st.name)
      itemType <- d.itemType.fold(simpleTypeJob(d.simpleType.get))(await[SimpleType](_))
    } yield ListType(typeName, anySimpleType, Facets.withWspCollapse[ListVal], itemType)
    case d: UnionElem => for {
      typeName <- typeName(st.name)
      memberTypes <- d.memberTypes.fold(Job.unit(Seq[SimpleType]()))(qns => Job.traverse(qns)(await[SimpleType](_))) & Job.traverse(d.simpleTypes)(simpleTypeJob(_))
    } yield UnionType(typeName, anySimpleType, Facets.empty[SimpleVal], memberTypes._1 ++ memberTypes._2)
  }

  def simpleTypeRestriction(typeName: QName, simpleType: SimpleType, facetSpecs: Seq[FacetSpec]): Job[SimpleType] = {

    def go[VAL <: SimpleVal](
      facets: Facets[VAL],
      facetsHandler: FacetSpec => Either[String, Facets[VAL] => Either[String, Facets[VAL]]],
      fss: Seq[FacetSpec])
    : Job[Facets[VAL]] = fss match {
      case head :: tail => facetsHandler(head) match {
        case Left(left) => abort(left)
        case Right(right) => right(facets) match {
          case Left(left) => abort(left)
          case Right(right) => go(right, facetsHandler, tail)
        }
      }
      case _ => Job.unit(facets)
    }

    def generalFacetsHandler[T <: SimpleType](baseType: T): PartialFunction[FacetSpec, Facets[T#VAL] => Either[String, Facets[T#VAL]]] = {
      case f: PatternsFacetSpec => facets => {
        eu.swdev.util.traverse(f.value.toList)(pe => Try { pe.value.r } match {
          case Success(s) => Right(s)
          case Failure(f) => Left(f.getMessage)
        }).fold[Either[String, Facets[T#VAL]]](
            Left(_),
            facets.pattern.checkAndSet(_)
          )
      }
      case f: EnumerationsFacetSpec => facets => {
        eu.swdev.util.traverse(f.value.toList)(ee => baseType.createVal(ee.value, ee.namespaces)).fold[Either[String, Facets[T#VAL]]](
          Left(_),
          facets.enum.checkAndSet(_)
        )
      }
      case f: AssertElem => ???
    }

    def whitespaceFacetHandler[X <: SimpleVal]: PartialFunction[FacetSpec, Facets[X] => Either[String, Facets[X]]] = {
      case f: WhitespaceElem => facets => facets.whitespace.checkAndSet(f.value)
    }

    def lengthFacetsHandler[X <: SimpleVal: HasLength]: PartialFunction[FacetSpec, Facets[X] => Either[String, Facets[X]]] = {
      case f: LengthElem => facets => facets.length.checkAndSet(f.value)
      case f: MinLengthElem => facets => facets.minLength.checkAndSet(f.value)
      case f: MaxLengthElem => facets => facets.maxLength.checkAndSet(f.value)
    }

    def orderFacetsHandler[T <: SimpleType](baseType: T)(implicit ev: Ordering[T#VAL]): PartialFunction[FacetSpec, Facets[T#VAL] => Either[String, Facets[T#VAL]]] = {
      case f: MinInclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.minInc.checkAndSet(v))
      case f: MinExclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.minExc.checkAndSet(v))
      case f: MaxInclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.maxInc.checkAndSet(v))
      case f: MaxExclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.maxExc.checkAndSet(v))
    }

    def joinedFacetsHandler[X <: SimpleVal](pfs: PartialFunction[FacetSpec, Facets[X] => Either[String, Facets[X]]]*): FacetSpec => Either[String, Facets[X] => Either[String, Facets[X]]] = {
      fs => pfs.find(_.isDefinedAt(fs)).fold[Either[String, Facets[X] => Either[String, Facets[X]]]](Left("unsupported facet"))(pf => Right((facets: Facets[X]) => pf.apply(fs).apply(facets)))
    }

    simpleType match {

      case baseType: ListType => {

        val facetsHandler = joinedFacetsHandler(generalFacetsHandler(baseType), whitespaceFacetHandler, lengthFacetsHandler[ListVal])

        for {
          fs <- go(baseType.facets, facetsHandler, facetSpecs)
        } yield ListType(typeName, baseType, fs, baseType.itemType)

      }

      case baseType: UnionType => {

        val facetsHandler = joinedFacetsHandler(generalFacetsHandler(baseType))

        for {
          fs <- go(baseType.facets, facetsHandler, facetSpecs)
        } yield UnionType(typeName, baseType, fs, baseType.memberTypes)

      }

      case baseType: AtomicType => {

        def atj[T <: AtomicType](bt: T)(create: (QName, T, Facets[bt.VAL]) => T)(handlers: PartialFunction[FacetSpec, Facets[bt.VAL] => Either[String, Facets[bt.VAL]]]*): Job[T] = for {
          fs <- go(bt.facets, joinedFacetsHandler(handlers: _*), facetSpecs)
        } yield create(typeName, bt, fs)

        baseType.accept(new AtomicTypeVisitor[Job[AtomicType], Unit] {

          override def visit(tpe: anyAtomicType.type, p: Unit): Job[AtomicType] = abort("anyAtomicType can not be used as a base type")

          override def visit(tpe: BooleanType, p: Unit): Job[AtomicType] = atj(tpe)(BooleanType.apply)(generalFacetsHandler(tpe), whitespaceFacetHandler)

          override def visit(tpe: DecimalType, p: Unit): Job[AtomicType] = atj(tpe)(DecimalType.apply)(generalFacetsHandler(tpe), orderFacetsHandler(tpe), whitespaceFacetHandler)

          override def visit(tpe: DoubleType, p: Unit): Job[AtomicType] = atj(tpe)(DoubleType.apply)(generalFacetsHandler(tpe), orderFacetsHandler(tpe), whitespaceFacetHandler)

          override def visit(tpe: untypedAtomicType.type, p: Unit): Job[AtomicType] = abort("untypedAtomic type can not be used as a base type")

          override def visit(tpe: IntegerType, p: Unit): Job[AtomicType] = atj(tpe)(IntegerType.apply)(generalFacetsHandler(tpe), orderFacetsHandler(tpe), whitespaceFacetHandler)

          override def visit(tpe: LongType, p: Unit): Job[AtomicType] = atj(tpe)(LongType.apply)(generalFacetsHandler(tpe), orderFacetsHandler(tpe), whitespaceFacetHandler)

          override def visit(tpe: IntType, p: Unit): Job[AtomicType] = atj(tpe)(IntType.apply)(generalFacetsHandler(tpe), orderFacetsHandler(tpe), whitespaceFacetHandler)

          override def visit(tpe: StringType, p: Unit): Job[AtomicType] = atj(tpe)(StringType.apply)(generalFacetsHandler(tpe), orderFacetsHandler(tpe), lengthFacetsHandler[AtomicVal[String]], whitespaceFacetHandler)

          override def visit(tpe: QNameType, p: Unit): Job[AtomicType] = atj(tpe)(QNameType.apply)(generalFacetsHandler(tpe), whitespaceFacetHandler)

        }, ())

      }

    }

  }

  // part 2; 4.1.2
  def typeName(someName: SomeValue[String]): Job[QName] = for {
    conf <- getConf
  } yield QNameFactory.caching(conf.schemaTargetNamespace, LocalName(someName.value))

  //def baseTypeJob(str: SimpleTypeRestrictionElem): Job[SimpleType] = str.base.fold(simpleTypeJob(str.tpe.get))(await[SimpleType](_))

  def convertNamespaceTokens(list: List[NamespaceItemToken], conf: JobConf): Set[Namespace] = {
    list.map {
      case NamespaceItemToken.Local => Namespace.NoNamespace
      case NamespaceItemToken.TargetNamespace => conf.schemaTargetNamespace
      case NamespaceItemToken.Uri(uri) => Namespace(uri)
    } toSet
  }

}

