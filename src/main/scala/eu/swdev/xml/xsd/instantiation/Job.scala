package eu.swdev.xml.xsd.instantiation

import eu.swdev.xml.base.{DefaultValue, SomeValue}
import eu.swdev.xml.{name, log}
import eu.swdev.xml.log.{Message, Messages}
import eu.swdev.xml.name._
import eu.swdev.xml.schema.ComplexType
import eu.swdev.xml.schema.Facets.{HasTimeZone, HasDigits, HasLength, FacetOp}
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  */

object JobMod {

  case class Job[+A](run: State => Step[A]) { self =>

    def map[B](f: A => B): Job[B] = Job { state => run(state) map f }

    def flatMap[B](f: A => Job[B]): Job[B] = Job { state =>

      def go(step: Step[A]): Step[B] = step match {
        case Await(ref, rec) => Await(ref, rec andThen go)
        case Done(a, s1) => f(a).run(s1)
        case Abort(state) => Abort(state)
        case Created(o, next) => Created(o, go(next))
      }

      go(run(state))
    }

    def >>=[B](f: A => Job[B]) = this.flatMap(f)

    def &[O2](p2: => Job[O2]): Job[(A, O2)] = for {
      v1 <- this
      v2 <- p2
    } yield (v1, v2)

    def filter(f: A => Boolean): Job[A] = withFilter(f)

    def withFilter(f: A => Boolean): Job[A] = Job(state => {
      run(state).filter(f)
    })


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

  /**
   *
   * @tparam A The type of the final result.
   */
  trait Step[+A] {

    def map[B](f: A => B): Step[B] = this match {
      case Await(ref, rec) => Await(ref, rec andThen Step.lift(f))
      case Done(a, state) => Done(f(a), state)
      case Abort(state) => Abort(state)
      case Created(o, next) => Created(o, next map f)
    }

    def filter(f: A => Boolean): Step[A] = this match {
      case Await(ref, rec) => Await(ref, rec andThen (step => step.filter(f)))
      case Done(a, state) => if (f(a)) Done(a, state) else Abort(state)
      case Abort(state) => Abort(state)
      case Created(o, next) => Created(o, next filter f)
    }

  }

  object Step {
    def lift[A, B](f: A => B): Step[A] => Step[B] = sa => sa map f
  }

  case class Await[A, I](ref: Ref[I], rec: Option[I] => Step[A]) extends Step[A]

  case class Done[A](value: A, state: State) extends Step[A]

  case class Abort(state: State) extends Step[Nothing]
  
  case class Created[A, O <: SchemaTopComponent](value: O, next: Step[A]) extends Step[A]
  
  sealed trait Ref[X] {
    def symbolSpace: SymbolSpace[X]
  }

  sealed case class LocalRef[X](symbolSpace: SymbolSpace[X], ncName: LocalName, stage: Stage) extends Ref[X]

  sealed case class GlobalRef[X](symbolSpace: SymbolSpace[X], qName: QName, schemaLocation: Option[String]) extends Ref[X]

  sealed trait Stage
  case object Created extends Stage
  case object Completed extends Stage

  case class JobConf(schemaElem: SchemaElem, schemaTargetNamespace: Namespace) {
    def attributeFormDefault: Form = schemaElem.attributeFormDefault.value
    val schemaLocations: Map[Namespace, Option[String]] = schemaElem.schemaTop.collect { case Left(ie: ImportElem) => ie } map ( ie => (ie.namespace.map(new Namespace(_)).getOrElse(NoNamespace) -> ie.schemaLocation )) toMap
  }
  
  case class State(config: JobConf, log: Messages)

  def addError(state: State, msg: Message): State = state.copy(log = log.prepend(msg, state.log))

  def abort[C](msg: String): Job[C] = Job[C] { state => Abort(addError(state, msg)) }

  def await[A](name: QName, stage: Stage = Completed)(implicit ev: SymbolSpace[A], ct: ClassTag[A]): Job[A] = {
    Job { state => {

      def doAwait(ref: Ref[A]) = Await[A, A](ref, {
        case Some(s) => {
          if (ct.runtimeClass.isInstance(s)) {
            Done(s.asInstanceOf[A], state)
          } else {
            Abort(addError(state, s"reference has not the required type - symbol space: ${ev.name}; name: $name; required type: ${ct.runtimeClass.getName}; actual type: ${s.getClass.getName}"))
          }
        }
        case None => Abort(addError(state, s"unresolved reference - symbol space: ${ev.name}; name: $name"))
      })

      if (name.namespace == state.config.schemaTargetNamespace) {
        doAwait(LocalRef(ev, name.localName, stage))
      } else if (name.namespace == XsdNamespace) {
        doAwait(GlobalRef(ev, name, None))
      } else {
        state.config.schemaLocations.get(name.namespace).fold[Step[A]] {
          Abort(addError(state, s"namespace ${name.namespace} is not imported"))
        } {
          schemaLocation => doAwait(GlobalRef(ev, name, schemaLocation))
        }
      }

    }}
  }

  def created[O <: SchemaTopComponent](a: O): Job[Unit] = Job { state => Created(a, Done((), state)) }

  def getConf: Job[JobConf] = Job { state => Done(state.config, state) }
  
  //
  //
  //


  // 3.4.1
  def complexTypeJob(cmp: ComplexTypeElem): Job[ComplexType] = for {
    conf <- getConf
    typeName <- typeName(cmp.name)
    baseType <- await[Type](cmp.content.base, Created)
    attrs <- attrsModelJob(cmp.content)
    finl = cmp.finl.getOrElse(conf.schemaElem.finalDefault.value).toSet[CtDerivationCtrl]
    block = cmp.block.getOrElse(conf.schemaElem.blockDefault.value).toSet[CtBlockCtrl]
    assertions <- assertionsJob(cmp.content)
    (contentModel, completeContentModelJob) <- contentModelJob(cmp.content, baseType, cmp.mixed)
    ct = ComplexType(typeName, baseType, cmp.content.derivationMethod, attrs, contentModel, cmp.abstrct.value, finl, block, assertions)
    _ <- created(ct)
    _ <- completeContentModelJob
  } yield ct

  def assertionsJob(content: ComplexTypeContentCmp): Job[Seq[Assertion]] = {
    val assertElems: Seq[AssertElem] = content match {
      case c: ComplexContentAbbrev => c.asserts
      case c: ComplexContentElem => c.derivation.asserts
      case c: SimpleContentElem => c.derivation.asserts
    }
    Job.traverse(assertElems)(assertionJob(_))
  }

  def assertionJob(ae: AssertElem): Job[Assertion] = for {
    conf <- getConf
  } yield {
    val xPathDefaultNamespace = ae.xPathDefaultNamespace.getOrElse(conf.schemaElem.xPathDefaultNamespace.value) match {
      case XPathDefaultNamespace.Default => ae.namespaces.defaultNamespace
      case XPathDefaultNamespace.Target => conf.schemaTargetNamespace
      case XPathDefaultNamespace.Local => NoNamespace
      case XPathDefaultNamespace.Uri(uri) => Namespace(uri)
    }
    Assertion(ae.test, xPathDefaultNamespace)
  }

  def contentModelJob(content: ComplexTypeContentCmp, baseType: Type, ctMixed: Option[Boolean]): Job[(ContentType, Job[Unit])] = {
    content match {
      case c: ComplexContentCmp => complexContentJob(c, baseType, ctMixed)
      case c: SimpleContentElem => for {
        simpleTypeDefinition <- simpleTypeDefinitionJob(c.derivation)
      } yield (SimpleContentType(simpleTypeDefinition), Job.unit(()))
    }
  }

  // 3.4.2.2 determine the simple type definition of the content type property
  def simpleTypeDefinitionJob(derivation: SimpleDerivationCmp): Job[SimpleType] = {
    await[Type](derivation.base) >>= { baseType =>
      (baseType, derivation) match {
        // 1
        case (bt: ComplexType, d: SimpleContentRestrictionElem) if (bt.content.simpleTypeDefinition.isDefined) => {
          for {
            b <- d.simpleType.fold {
              Job.unit(bt.content.simpleTypeDefinition.get)
            } {
              simpleTypeElem => simpleTypeJob(simpleTypeElem)
            }
            typeName <- typeName(DefaultValue(d.syntheticTypeName))
            restricted <- simpleTypeRestriction(typeName, b, d.facetSpecs)
          } yield restricted
        }
        // 2
        case (bt: ComplexType, d: SimpleContentRestrictionElem) if (bt.content.isMixedAndEmptiable) => {
          for {
            sb <- d.simpleType.fold {
              Job.unit[SimpleType](anySimpleType)
            } {
              simpleTypeElem => simpleTypeJob(simpleTypeElem)
            }
            typeName <- typeName(DefaultValue(d.syntheticTypeName))
            restricted <- simpleTypeRestriction(typeName, sb, d.facetSpecs)
          } yield restricted
        }
        // 3
        case (bt: ComplexType, d: SimpleContentExtensionElem) if (bt.content.simpleTypeDefinition.isDefined) => {
          Job.unit(bt.content.simpleTypeDefinition.get)
        }
        // 4
        case (bt: SimpleType, d: SimpleContentExtensionElem) => {
          Job.unit(bt)
        }
        // 5
        case _ => {
          Job.unit(anySimpleType)
        }
      }
    }
  }

  // 3.4.2.3.3
  def complexContentJob(complexContent: ComplexContentCmp, baseType: Type, ctMixed: Option[Boolean]): Job[(ComplexContentType, Job[Unit])] = {
    // 1
    val effectiveMixed: Boolean = complexContent.mixed.orElse(ctMixed).getOrElse(false)
    // 2
    val explicitContent: Option[TypeDefParticleCmp] = complexContent.typeDefParticle.filter {
      // 2.1.2
      case p: AllElem if (p.particles.isEmpty) => false
      case p: SequenceElem if (p.particles.isEmpty) => false
      // 2.1.3
      case p: ChoiceElem if (p.minOccurs.getOrElse(1) == 0 && p.particles.isEmpty) => false
      // 2.1.4
      case t: TypeDefParticleCmp if (t.maxOccurs.getOrElse(1) == 0) => false
      case _ => true
    }
    // 3
    val effectiveContent: Option[TypeDefParticleCmp] = explicitContent.orElse(if (effectiveMixed) {
      // 3.1.1
      Some(SequenceElem(complexContent.loc, None, None, Some(1), Some(MaxOccurs.Bounded(1)), Seq(), Map()))
    } else {
      // 3.1.2
      None
    })

    //
    def contentTypeBasedOnDerivedTypeOnly: (Job[ComplexContentType], Job[Unit]) = {
      effectiveContent.fold {
        // 4.1.1
        (Job.unit[ComplexContentType](EmptyContentType), Job.unit(()))
      } {
        // 4.1.2
        tdpc => {
          val (gj, completionJob) = groupJob(tdpc)
          (gj.map(ElementsContentType(_, effectiveMixed, None)), completionJob)
        }
      }
    }

    // 4
    val (explicitContentTypeJob, completionJob) = complexContent.derivationMethod match {
      // 4.1
      case Relation.Restriction => contentTypeBasedOnDerivedTypeOnly

      // 4.2
      case Relation.Extension => baseType match {

        // 4.2.1
        case baseType: SimpleType => contentTypeBasedOnDerivedTypeOnly

        case baseType: ComplexType => baseType.content match {

          // 4.2.1
          case EmptyContentType => contentTypeBasedOnDerivedTypeOnly
          // 4.2.1
          case baseContentType: SimpleContentType => contentTypeBasedOnDerivedTypeOnly
          // 4.2.2
          case baseContentType: ElementsContentType if (effectiveContent.isEmpty) => (Job.unit(baseContentType), Job.unit(()))
            // 4.2.3
          case baseContentType: ElementsContentType => {

            val (particleJob, completionJob) = baseContentType.group match {
              // 4.2.3.1
              case baseParticle: AllGroupParticle if (explicitContent.isEmpty) => (Job.unit(baseParticle), Job.unit(()))
              // 4.2.3.2 & 4.2.3.3
              case baseParticle: AllGroupParticle if (explicitContent.isDefined) => {
                val (gj, completionJob) = groupJob(effectiveContent.get)
                (gj.map(_ match {
                  // 4.2.3.2
                  case groupParticle: AllGroupParticle => AllGroupParticle(Occurs(effectiveContent.get.minOccurs.getOrElse(1), MaxOccurs.one), baseParticle.nested ++ groupParticle.nested)
                  // 4.2.3.3
                  case groupParticle => SeqGroupParticle(Occurs(1, MaxOccurs.one), Seq(baseParticle, groupParticle))
                }), completionJob)
              }

              // 4.2.3.3
              case baseParticle => {
                val (gj, completionJob) = groupJob(effectiveContent.get)
                (gj.map(groupParticle => SeqGroupParticle(Occurs(1, MaxOccurs.one), Seq(baseParticle, groupParticle))), completionJob)
              }

            }

            (particleJob.map(ElementsContentType(_, effectiveMixed, baseContentType.open)), completionJob)

          }

        }

      }
    }

    val contentTypeJob: Job[ComplexContentType] = explicitContentTypeJob >>= {
      explicitContentType => for {
        conf <- getConf
      } yield {
        // 5
        val optWildcardElement: Option[OpenContentCmp] = complexContent.openContent.orElse(conf.schemaElem.defaultOpenContent.filter(doc => {
          !explicitContentType.isEmpty || doc.appliesToEmpty.value
        }))
        optWildcardElement.fold {
          // 6.1
          explicitContentType
        } {
          wildcardElement => {
            if (wildcardElement.mode.value == OpenContentMode.None) {
              // 6.1
              explicitContentType
            } else {
              // 6.2
              val w: Wildcard = wildcardElement.optAny.fold {
                // there is no <any> element inside openContent
                // -> use a wildcard that corresponds to an empty <any> element
                Wildcard(NamespaceConstraint.Any(DisallowedNames.empty), ProcessContents.Strict)
              } {
                any => Wildcard(namespaceConstraint(any, conf.schemaTargetNamespace), any.processContents.value)
              }
              explicitContentType match {
                case EmptyContentType => {
                  val openContent = OpenContent(wildcardElement.mode.value, w)
                  ElementsContentType(SeqGroupParticle(Occurs(1, MaxOccurs.one), Seq()), false, Some(openContent))
                }
                case explicitContentType: ElementsContentType => {
                  val openContent = explicitContentType.open.fold {
                    OpenContent(wildcardElement.mode.value, w)
                  } {
                    openContent => OpenContent(wildcardElement.mode.value, Wildcard(openContent.wildcard.namespaceConstraint.union(w.namespaceConstraint), w.processContents))
                  }
                  explicitContentType.copy(open = Some(openContent))
                }
              }
            }
          }
        }
      }
    }

    contentTypeJob.map((_, completionJob))
  }
  
  // 3.10.2.2
  def wildcard(cmp: WildcardCmp, targetNamespace: Namespace): Wildcard = {
    Wildcard(namespaceConstraint(cmp, targetNamespace), cmp.processContents.value)
  }

  // 3.10.2.2
  def namespaceConstraint(cmp: WildcardCmp, targetNamespace: Namespace): NamespaceConstraint = {
    def namespaceSet(list: List[NamespaceItemToken]): Set[Namespace] = list.foldLeft(Set[Namespace]())((acc, tok) => acc + (tok match {
      case NamespaceItemToken.TargetNamespace => targetNamespace
      case NamespaceItemToken.Local => name.NoNamespace
      case NamespaceItemToken.Uri(uri) => Namespace(uri)
    }))
    val disallowedNames: DisallowedNames = cmp.notQName.fold {
      DisallowedNames.empty
    } {
      qNameItems => qNameItems.foldLeft(DisallowedNames.empty)((acc, qNameItem) => {
        qNameItem match {
          case QNameItem.Defined => acc.copy(defined = true)
          case QNameItem.DefinedSibling => acc.copy(sibling = true)
          case QNameItem.Qn(qn) => acc.copy(names = acc.names + qn)
        }
      })
    }
    (cmp.namespace, cmp.notNamespace) match {
      case (Some(NamespaceDefToken.Any), _) => NamespaceConstraint.Any(disallowedNames)
      case (Some(NamespaceDefToken.Other), _) => NamespaceConstraint.Not(disallowedNames, Set(NoNamespace, targetNamespace))
      case (Some(NamespaceDefToken.Items(list)), _) => NamespaceConstraint.Enum(disallowedNames, namespaceSet(list))
      case (_, Some(list)) => NamespaceConstraint.Not(disallowedNames, namespaceSet(list))
      case _ => NamespaceConstraint.Any(disallowedNames)
    }

  }

  def groupJob(particleCmp: TypeDefParticleCmp): (Job[GroupParticle], Job[Unit]) = {
    ???
  }

  def attrsModelJob(cmp: ComplexTypeContentCmp): Job[AttrsModel] = {
    cmp match {
      case _: SimpleContentType => attrsModelJob(Nil, None)
      case m: ComplexContentAbbrev => attrsModelJob(m.attrs, m.anyAttribute)
      case m: ComplexDerivationCmp => attrsModelJob(m.attrs, m.anyAttribute)
    }
  }

  def attrsModelJob(attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem]): Job[AttrsModel] = {
    val attrModelJobsForElemsAndGroups: Seq[Job[AttrsModel]] = attrs map {
      case Left(ae) => attrsModelJob(ae)
      case Right(ag) => attrsModelJob(ag)
    }
    val attrModelJobs = anyAttribute.fold {
      attrModelJobsForElemsAndGroups
    } {
      any => attrsModelJob(any) +: attrModelJobsForElemsAndGroups
    }
    val listJob: Job[Seq[AttrsModel]] = Job.sequence(attrModelJobs)
    listJob.map(_.foldLeft(AttrsModel.empty)((am, acc) => acc.add(am)))
  }

  def attrsModelJob(ref: AttributeGroupRefElem): Job[AttrsModel] = await[AttrGroup](ref.ref) map (_.attrsModel)

  def attrsModelJob(ae: AttributeElemL): Job[AttrsModel] = {
    val attrDeclJob: Job[AttrDecl] = (ae.name, ae.ref) match {
      case (Some(an), _) => for {
        conf <- getConf
        simpleType <- ((ae.simpleType, ae.refType) match {
          case (Some(simpleTypeElem), _) => simpleTypeJob(simpleTypeElem)
          case (_, Some(ref)) => await[SimpleType](ref)
          case _ => Job.unit(anySimpleType)
        })
      } yield {
        val namespace = (ae.form, conf.schemaElem.attributeFormDefault.value) match {
          case (Some(form), _) => if (form == Form.Qualified) conf.schemaTargetNamespace else NoNamespace
          case (_, form) => if (form == Form.Qualified) conf.schemaTargetNamespace else NoNamespace
        }
        AttrDecl(QNameFactory.caching.apply(namespace, LocalName(an)), simpleType, None, ae.inheritable.getOrElse(false))
      }
      case (_, Some(ref)) => await[AttrDecl](ref)
      case _ => abort(s"invalid attribute declaration at: ${ae.loc}")
    }
    val constraint = (ae.default, ae.fixed) match {
      case (Some(s), _) => Some(ValueConstraint(s, true))
      case (_, Some(s)) => Some(ValueConstraint(s, false))
      case _ => None
    }
    attrDeclJob.map(attrDecl => {
      AttrsModel(Map(attrDecl.name -> AttrUse(attrDecl, ae.use.value, constraint)), None)
    })
  }

  // 3.10.2.2
  def attrsModelJob(anyAttribute: AnyAttributeElem): Job[AttrsModel] = for {
    conf <- getConf
  } yield {
    AttrsModel(Map(), Some(wildcard(anyAttribute, conf.schemaTargetNamespace)))
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

      AttrDecl(QNameFactory.caching(targetNamespace, LocalName(an)), simpleType, None, ae.inheritable.getOrElse(false))
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

  def simpleTypeRestriction(typeName: QName, simpleType: SimpleType, facetSpecs: Seq[FacetCmp]): Job[SimpleType] = {

    // a function that either restricts a set of facets or returns an error
    type FacetsRestriction[X <: SimpleVal] = Facets[X] => Either[String, Facets[X]]

    // a partial function that transforms a FacetSpec into a FacetsRestriction
    type FacetHandler[X <: SimpleVal] = PartialFunction[FacetCmp, FacetsRestriction[X]]

    // a function that transforms a FacetSpec into a FacetsRestriction or an error
    type FacetCompiler[X <: SimpleVal] = FacetCmp => Either[String, FacetsRestriction[X]]

    // joins a sequence of FacetHandlers into FacetCompiler
    def facetCompiler[X <: SimpleVal](pfs: FacetHandler[X]*): FacetCompiler[X] = {
      fs => pfs.find(_.isDefinedAt(fs)).fold[Either[String, FacetsRestriction[X]]](Left("unsupported facet"))(pf => Right((facets: Facets[X]) => pf.apply(fs).apply(facets)))
    }

    // recursively apply FacetSpecs on Facets
    def go[VAL <: SimpleVal](
      compiler: FacetCompiler[VAL],
      facets: Facets[VAL],
      specs: Seq[FacetCmp])
    : Job[Facets[VAL]] = specs match {
      case head :: tail => compiler(head) match {
        case Left(left) => abort(left)
        case Right(right) => right(facets) match {
          case Left(left) => abort(left)
          case Right(right) => go(compiler, right, tail)
        }
      }
      case _ => Job.unit(facets)
    }

    def restrict[T <: SimpleType](bt: T)(create: (QName, T, Facets[bt.VAL]) => T)(handlers: PartialFunction[FacetCmp, Facets[bt.VAL] => Either[String, Facets[bt.VAL]]]*): Job[T] = for {
      fs <- go(facetCompiler(handlers: _*), bt.facets, facetSpecs)
    } yield create(typeName, bt, fs)

    def generalFacetsHandler(baseType: SimpleType): FacetHandler[baseType.VAL] = {
      case f: PatternsFacetSpec => facets => {
        eu.swdev.util.traverse(f.value.toList)(pe => Try { pe.value.r } match {
          case Success(s) => Right(s)
          case Failure(f) => Left(f.getMessage)
        }).fold[Either[String, Facets[baseType.VAL]]](
            Left(_),
            facets.pattern.checkAndSet(_)
          )
      }
      case f: EnumerationsFacetSpec => facets => {
        eu.swdev.util.traverse(f.value.toList)(ee => baseType.createVal(ee.value, ee.namespaces)).fold[Either[String, Facets[baseType.VAL]]](
          Left(_),
          facets.enum.checkAndSet(_)
        )
      }
      case f: AssertElem => ???
    }

    def whitespaceHandler[X <: SimpleVal]: FacetHandler[X] = {
      case f: WhitespaceElem => facets => facets.whitespace.checkAndSet(f.value)
    }

    def explicitTimeZoneHandler[X <: SimpleVal: HasTimeZone]: FacetHandler[X] = {
      case f: ExplicitTimeZoneElem => facets => facets.explicitTimeZone.checkAndSet(f.value)
    }

    def lengthHandler[X <: SimpleVal: HasLength]: FacetHandler[X] = {
      case f: LengthElem => facets => facets.length.checkAndSet(f.value)
      case f: MinLengthElem => facets => facets.minLength.checkAndSet(f.value)
      case f: MaxLengthElem => facets => facets.maxLength.checkAndSet(f.value)
    }

    def orderHandler(baseType: SimpleType)(implicit ev: Ordering[baseType.VAL]): PartialFunction[FacetCmp, Facets[baseType.VAL] => Either[String, Facets[baseType.VAL]]] = {
      case f: MinInclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.minInc.checkAndSet(v))
      case f: MinExclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.minExc.checkAndSet(v))
      case f: MaxInclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.maxInc.checkAndSet(v))
      case f: MaxExclusiveElem => facets => baseType.createVal(f.value, f.namespaces).right.flatMap(v => facets.maxExc.checkAndSet(v))
    }

    def digitsHandler[X <: SimpleVal: HasDigits]: FacetHandler[X] = {
      case f: TotalDigitsElem => facets => facets.totalDigits.checkAndSet(f.value)
      case f: FractionDigitsElem => facets => facets.fractionDigits.checkAndSet(f.value)
    }

    simpleType match {

      case baseType: ListType => restrict(baseType)(ListType(_, _, _, baseType.itemType))(generalFacetsHandler(baseType), whitespaceHandler, lengthHandler[ListVal])

      case baseType: UnionType => restrict(baseType)(UnionType(_, _, _, baseType.memberTypes))(generalFacetsHandler(baseType))

      case baseType: AtomicType => {

        baseType.accept(new AtomicTypeVisitor[Job[AtomicType], Unit] {

          override def visit(tpe: anyAtomicType.type, p: Unit): Job[AtomicType] = abort("anyAtomicType can not be used as a base type")

          override def visit(tpe: BooleanType, p: Unit): Job[AtomicType] = restrict(tpe)(BooleanType.apply)(generalFacetsHandler(tpe), whitespaceHandler)

          override def visit(tpe: DecimalType, p: Unit): Job[AtomicType] = restrict(tpe)(DecimalType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[BigDecimal]], whitespaceHandler)

          override def visit(tpe: DoubleType, p: Unit): Job[AtomicType] = restrict(tpe)(DoubleType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), whitespaceHandler)

          override def visit(tpe: untypedAtomicType.type, p: Unit): Job[AtomicType] = abort("untypedAtomic type can not be used as a base type")

          override def visit(tpe: IntegerType, p: Unit): Job[AtomicType] = restrict(tpe)(IntegerType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[BigInt]], whitespaceHandler)

          override def visit(tpe: LongType, p: Unit): Job[AtomicType] = restrict(tpe)(LongType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[Long]], whitespaceHandler)

          override def visit(tpe: IntType, p: Unit): Job[AtomicType] = restrict(tpe)(IntType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[Int]], whitespaceHandler)

          override def visit(tpe: StringType, p: Unit): Job[AtomicType] = restrict(tpe)(StringType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), lengthHandler[AtomicVal[String]], whitespaceHandler)

          override def visit(tpe: QNameType, p: Unit): Job[AtomicType] = restrict(tpe)(QNameType.apply)(generalFacetsHandler(tpe), whitespaceHandler)

        }, ())

      }

    }

  }

  // part 2; 4.1.2
  def typeName(someName: SomeValue[String]): Job[QName] = for {
    conf <- getConf
  } yield QNameFactory.caching(conf.schemaTargetNamespace, LocalName(someName.value))

  def convertNamespaceTokens(list: List[NamespaceItemToken], conf: JobConf): Set[Namespace] = {
    list.map {
      case NamespaceItemToken.Local => Namespace.NoNamespace
      case NamespaceItemToken.TargetNamespace => conf.schemaTargetNamespace
      case NamespaceItemToken.Uri(uri) => Namespace(uri)
    } toSet
  }

}

