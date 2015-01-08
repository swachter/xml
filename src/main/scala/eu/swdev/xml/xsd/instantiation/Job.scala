package eu.swdev.xml.xsd.instantiation

import java.net.URI

import eu.swdev.xml.base.{Location, DefaultValue, SomeValue}
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

case class SchemaImportHint(schemaLocation: Option[String], baseUri: Option[URI])

case class JobConf(schemaElem: SchemaElem, optTargetNamespace: Option[Namespace]) {
  def attributeFormDefault: Form = schemaElem.attributeFormDefault.value
  val schemaTargetNamespace: Namespace = optTargetNamespace.orElse(schemaElem.targetNamespace.map(Namespace(_))).getOrElse(NoNamespace)
  val importHints: Map[Namespace, SchemaImportHint] =
    schemaElem.compositions.collect { case Left(ie: ImportElem) => ie } map ( ie => (ie.namespace.map(Namespace(_)).getOrElse(NoNamespace) -> SchemaImportHint(ie.schemaLocation, ie.baseUri))) toMap
  def isChameleon: Boolean = optTargetNamespace.isDefined
}

object SchemaImportHint {
  val None = SchemaImportHint(Option.empty, Option.empty)
}

case class Job[+A](run: State => Step[A]) { self =>

  def map[B](f: A => B): Job[B] = Job { state => run(state) map f }

  def flatMap[B](f: A => Job[B]): Job[B] = Job { state =>

    def go(step: Step[A]): Step[B] = step match {
      case Await(ref, rec) => Await(ref, rec andThen go)
      case Done(a, s1) => f(a).run(s1)
      case Abort(state) => Abort(state)
      case Define(o, stage, next) => Define(o, stage, go(next))
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
    case Define(o, stage, next) => Define(o, stage, next map f)
  }

  def filter(f: A => Boolean): Step[A] = this match {
    case Await(ref, rec) => Await(ref, rec andThen (step => step.filter(f)))
    case Done(a, state) => if (f(a)) Done(a, state) else Abort(state)
    case Abort(state) => Abort(state)
    case Define(o, stage, next) => Define(o, stage, next filter f)
  }

}

object Step {
  def lift[A, B](f: A => B): Step[A] => Step[B] = sa => sa map f
}

case class Await[A, I](ref: Ref[I], rec: Option[I] => Step[A]) extends Step[A]

case class Done[A](value: A, state: State) extends Step[A]

case class Abort(state: State) extends Step[Nothing]

case class Define[A, O <: SchemaTopComponent](value: O, stage: Stage, next: Step[A]) extends Step[A]

sealed trait Ref[X] {
  def symbolSpace: SymbolSpace[X]
}

sealed case class LocalRef[X](symbolSpace: SymbolSpace[X], ncName: LocalName, stage: Stage) extends Ref[X]

sealed case class GlobalRef[X](symbolSpace: SymbolSpace[X], qName: QName, importHint: SchemaImportHint) extends Ref[X]

sealed trait Stage

case object Created extends Stage
case object Completed extends Stage

case class State(log: Messages)

case class JobMod(conf: JobConf) {

  def addError(state: State, msg: Message): State = state.copy(log = log.prepend(msg, state.log))

  def abort[C](msg: String): Job[C] = Job[C] { state => Abort(addError(state, msg)) }

  def await[A](name: QName, loc: Location, stage: Stage = Completed)(implicit ev: SymbolSpace[A], ct: ClassTag[A]): Job[A] = {
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

      if (name.namespace == conf.schemaTargetNamespace) {
        doAwait(LocalRef(ev, name.localName, stage))
      } else if (name.namespace == NoNamespace && conf.isChameleon) {
        doAwait(LocalRef(ev, name.localName, stage))
      } else if (name.namespace == XsdNamespace) {
        doAwait(GlobalRef(ev, name, SchemaImportHint.None))
      } else {
        conf.importHints.get(name.namespace).fold[Step[A]] {
          Abort(addError(state, s"can not resolve symbol; namespace not imported - namespace: ${name.namespace}; local name: ${name.localName}; symbol space: ${ev.name}; location: $loc"))
        } {
          importHint => doAwait(GlobalRef(ev, name, importHint))
        }
      }

    }}
  }

  def define[O <: SchemaTopComponent](a: O, stage: Stage): Job[Unit] = Job { state => Define(a, stage, Done((), state)) }

  //
  //
  //


  // 3.4.1
  def complexTypeJob(cmp: ComplexTypeElem): Job[ComplexType] = for {
    (complexType, continuation1) <- createComplexTypeJob(cmp)
    _ <- define(complexType, Created)
    continuation2 <- continuation1
    _ <- define(complexType, Completed)
    _ <- continuation2
  } yield complexType

  def createComplexTypeJob(cmp: ComplexTypeElem): Job[(ComplexType, Job[Job[Unit]])] = for {
    baseType <- await[Type](cmp.content.base, cmp.loc, Created)
    attrs <- attrsModelJob(cmp.content, if (cmp.defaultAttributesApply.value) conf.schemaElem.defaultAttributes else None)
    mergedAttrs <- mergeWithInheritedAttrs(attrs, baseType, cmp.content.derivationMethod)
    finl = cmp.finl.getOrElse(conf.schemaElem.finalDefault.value).toSet[CtDerivationCtrl]
    block = cmp.block.getOrElse(conf.schemaElem.blockDefault.value).toSet[CtBlockCtrl]
    assertions <- assertionsJob(cmp.content)
    (contentModel, completeContentModelJob) <- contentModelJob(cmp.content, baseType, cmp.mixed)
  } yield (ComplexType(componentQName(cmp.name), baseType, cmp.content.derivationMethod, attrs, contentModel, cmp.abstrct.value, finl, block, assertions), completeContentModelJob)

  // 3.4.2.4 & 3.4.2.5
  def mergeWithInheritedAttrs(attrs: AttrsModel, baseType: Type, derivationMethod: CtDerivationCtrl): Job[AttrsModel] = {
    val usesJob: Job[Map[QName, AttrUse]] = baseType match {
      case baseType: ComplexType => derivationMethod match {
        case Relation.Extension => {
          // TODO check valid extension (3.4.6.2)
          Job.unit(baseType.attrs.attrUses ++ attrs.attrUses)
        }
        case Relation.Restriction => {
          // TODO check valid restriction (3.4.6.3)
          // inherited attributes are overridden
          Job.unit(baseType.attrs.attrUses ++ attrs.attrUses)
        }
      }
      case _ => Job.unit(attrs.attrUses)
    }
    // 3.4.2.5
    val wildcard: Option[Wildcard] = derivationMethod match {
      // 2.1
      case Relation.Restriction => {
        // TODO check restriction
        attrs.wildcard
      }
      // 2.2
      case Relation.Extension => {
        // 2.2.1
        val baseWildcard = baseType match {
          case baseType: ComplexType => baseType.attrs.wildcard
          case _ => None
        }
        // 2.2.2
        (attrs.wildcard, baseWildcard) match {
          // 2.2.2.1
          case (w, None) => w
          // 2.2.2.2
          case (None, w) => w
          // 2.2.2.3
          case (Some(w), Some(bw)) => Some(Wildcard(w.namespaceConstraint.union(bw.namespaceConstraint), w.processContents))
        }
      }
    }
    usesJob.map(AttrsModel(_, wildcard))
  }

  def assertionsJob(content: ComplexTypeContentCmp): Job[Seq[Assertion]] = {
    val assertElems: Seq[AssertElem] = content match {
      case c: ComplexContentAbbrev => c.asserts
      case c: ComplexContentElem => c.derivation.asserts
      case c: SimpleContentElem => c.derivation.asserts
    }
    Job.traverse(assertElems)(assertionJob(_))
  }

  def assertionJob(ae: AssertElem): Job[Assertion] = {
    val xPathDefaultNamespace = ae.xPathDefaultNamespace.getOrElse(conf.schemaElem.xPathDefaultNamespace.value) match {
      case XPathDefaultNamespace.Default => ae.namespaces.defaultNamespace
      case XPathDefaultNamespace.Target => conf.schemaTargetNamespace
      case XPathDefaultNamespace.Local => NoNamespace
      case XPathDefaultNamespace.Uri(uri) => Namespace(uri)
    }
    Job.unit(Assertion(ae.test, xPathDefaultNamespace))
  }

  def contentModelJob(content: ComplexTypeContentCmp, baseType: Type, ctMixed: Option[Boolean]): Job[(ContentType, Job[Job[Unit]])] = {
    content match {
      case c: ComplexContentCmp => complexContentJob(c, baseType, ctMixed)
      case c: SimpleContentElem => for {
        simpleTypeDefinition <- simpleTypeDefinitionJob(c.derivation)
      } yield (SimpleContentType(simpleTypeDefinition), Job.unit(Job.unit(())))
    }
  }

  // 3.4.2.2 determine the simple type definition of the content type property
  def simpleTypeDefinitionJob(derivation: SimpleDerivationCmp): Job[SimpleType] = {
    await[Type](derivation.base, derivation.loc) >>= { baseType =>
      (baseType, derivation) match {
        // 1
        case (bt: ComplexType, d: SimpleContentRestrictionElem) if (bt.content.simpleTypeDefinition.isDefined) => {
          for {
            b <- d.simpleType.fold {
              Job.unit(bt.content.simpleTypeDefinition.get)
            } {
              simpleTypeElem => simpleTypeJob(simpleTypeElem)
            }
            restricted <- simpleTypeRestriction(componentQName(d.syntheticTypeName), b, d.facetSpecs)
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
            restricted <- simpleTypeRestriction(componentQName(d.syntheticTypeName), sb, d.facetSpecs)
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
  def complexContentJob(complexContent: ComplexContentCmp, baseType: Type, ctMixed: Option[Boolean]): Job[(ComplexContentType, Job[Job[Unit]])] = {
    // 1
    val effectiveMixed: Boolean = complexContent.mixed.orElse(ctMixed).getOrElse(false)
    // 2
    val explicitContent: Option[TypeDefParticleCmp] = complexContent.typeDefParticle.filter {
      // 2.1.2
      case p: AllElem if (p.nested.isEmpty) => false
      case p: SequenceElem if (p.nested.isEmpty) => false
      // 2.1.3
      case p: ChoiceElem if (p.occurs.min == 0 && p.nested.isEmpty) => false
      // 2.1.4
      case t: TypeDefParticleCmp if (t.occurs.max == MaxOccurs.zero) => false
      case _ => true
    }
    // 3
    val effectiveContent: Option[TypeDefParticleCmp] = explicitContent.orElse(if (effectiveMixed) {
      // 3.1.1
      Some(SequenceElem(complexContent.loc, None, None, Occurs.once, Seq(), Map()))
    } else {
      // 3.1.2
      None
    })

    //
    def contentTypeBasedOnDerivedTypeOnly: Job[(ComplexContentType, Job[Job[Unit]])] = {
      effectiveContent.fold[Job[(ComplexContentType, Job[Job[Unit]])]] {
        // 4.1.1
        Job.unit((EmptyContentType, Job.unit(Job.unit(()))))
      } {
        // 4.1.2
        typeDefParticleCmp => {
          val gj: Job[(GroupParticle, Job[Job[Unit]])] = groupJob(typeDefParticleCmp)
          gj.map(t => (ElementsContentType(t._1, effectiveMixed, None), t._2))
        }
      }
    }

    // 4
    val explicitContentTypeJob: Job[(ComplexContentType, Job[Job[Unit]])] = complexContent.derivationMethod match {
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
          case baseContentType: ElementsContentType if (effectiveContent.isEmpty) => Job.unit((baseContentType), Job.unit(Job.unit(())))
          // 4.2.3
          case baseContentType: ElementsContentType => {

            val particleJob: Job[(GroupParticle, Job[Job[Unit]])] = baseContentType.group match {
              // 4.2.3.1
              case baseParticle: AllParticle if (explicitContent.isEmpty) => Job.unit(baseParticle, Job.unit(Job.unit(())))
              // 4.2.3.2 & 4.2.3.3
              case baseParticle: AllParticle if (explicitContent.isDefined) => {
                groupJob(effectiveContent.get).map {
                  // 4.2.3.2
                  case (groupParticle: AllParticle, completionJob) => (AllParticle(Occurs(effectiveContent.get.occurs.min, MaxOccurs.one), baseParticle.nested ++ groupParticle.nested), completionJob)
                  // 4.2.3.3
                  case (groupParticle, completionJob) => (SeqParticle(Occurs.once, Seq(baseParticle, groupParticle)), completionJob)
                }
              }
              // 4.2.3.3
              case baseParticle => {
                groupJob(effectiveContent.get).map {
                  case (groupParticle, completionJob) => (SeqParticle(Occurs.once, Seq(baseParticle, groupParticle)), completionJob)
                }
              }

            }

            particleJob.map {
              case (groupParticle, completionJob) => (ElementsContentType(groupParticle, effectiveMixed, baseContentType.open), completionJob)
            }

          }
        }

      }
    }

    explicitContentTypeJob.map {
      case (explicitContentType, completionJob) => {
        // 5
        val optWildcardElement: Option[OpenContentCmp] = complexContent.openContent.orElse(conf.schemaElem.defaultOpenContent.filter(doc => {
          !explicitContentType.isEmpty || doc.appliesToEmpty.value
        }))
        optWildcardElement.fold {
          // 6.1
          (explicitContentType, completionJob)
        } {
          wildcardElement => {
            if (wildcardElement.mode.value == OpenContentMode.None) {
              // 6.1
              (explicitContentType, completionJob)
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
                  (ElementsContentType(SeqParticle(Occurs(1, MaxOccurs.one), Seq()), false, Some(openContent)), completionJob)
                }
                case explicitContentType: ElementsContentType => {
                  val openContent = explicitContentType.open.fold {
                    OpenContent(wildcardElement.mode.value, w)
                  } {
                    openContent => OpenContent(wildcardElement.mode.value, Wildcard(openContent.wildcard.namespaceConstraint.union(w.namespaceConstraint), w.processContents))
                  }
                  (explicitContentType.copy(open = Some(openContent)), completionJob)
                }
              }
            }
          }
        }
      }
    }
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

  def notationJob(cmp: NotationElem): Job[Notation] = (cmp.publ, cmp.system) match {
    case (p@Some(_), s@Some(_)) => Job.unit(Notation(componentQName(cmp.name), SysPubId.BothIds(p, s)))
    case (p@Some(_), _) => Job.unit(Notation(componentQName(cmp.name), SysPubId.PubId(p)))
    case (_, s@Some(_)) => Job.unit(Notation(componentQName(cmp.name), SysPubId.SysId(s)))
    case (None, None) => abort(s"notation needs systemId or publicId")
  }

  def groupDefJob(cmp: GroupDefElem): Job[GroupDef] = for {
    (groupParticle, continuation1) <- groupJob(cmp.particle)
    groupDef = GroupDef(componentQName(cmp.name), groupParticle)
    _ <- define(groupDef, Created)
    continuation2 <- continuation1
    _ <- define(groupDef, Completed)
    _ <- continuation2
  } yield groupDef

  def genericGroupJob[X](cmp: GroupParticleCmp)(f: (Occurs, Seq[Particle]) => X): Job[(X, Job[Job[Unit]])] = {
    nestedParticlesJob(cmp.nested).map {
      case (nestedParticles, completionJob) => (f(cmp.occurs, nestedParticles), completionJob)
    }
  }

//  def groupJob(cmp: ChoiceOrSeqCmp): Job[(ChoiceOrSeqParticle, Job[Job[Unit]])] = cmp match {
//    case cmp: ChoiceElem => genericGroupJob(cmp)(ChoiceParticle(_, _))
//    case cmp: SequenceElem => genericGroupJob(cmp)(SeqParticle(_, _))
//  }

  def groupJob(particleCmp: TypeDefParticleCmp): Job[(GroupParticle, Job[Job[Unit]])] = {
    particleCmp match {
      case cmp: AllElem => genericGroupJob(cmp)(AllParticle(_, _))
      case cmp: ChoiceElem => genericGroupJob(cmp)(ChoiceParticle(_, _))
      case cmp: SequenceElem => genericGroupJob(cmp)(SeqParticle(_, _))
      case cmp: GroupRefElem => {
        await[GroupDef](cmp.ref, cmp.loc).map(gd => (gd.particle.withOccurs(cmp.occurs), Job.unit(Job.unit(()))))
      }
    }
  }

  def nestedParticlesJob(nested: Seq[NestedParticleCmp]): Job[(Seq[NestedParticle], Job[Job[Unit]])] = {
    val seq: Seq[Job[(NestedParticle, Job[Job[Unit]])]] = nested.map(nestedParticleJob(_))

    val sequenced: Job[Seq[(NestedParticle, Job[Job[Unit]])]] = Job.sequence(seq)

    sequenced.map(tupelSeq => {
      (tupelSeq.map(_._1), Job.sequence(tupelSeq.map(_._2)).map(_ => Job.unit(())))
    })
  }

  def nestedParticleJob(cmp: NestedParticleCmp): Job[(NestedParticle, Job[Job[Unit]])] = {
    cmp match {
      case cmp: AnyElem => Job.unit(ElemWildcard(cmp.occurs, wildcard(cmp, conf.schemaTargetNamespace)), Job.unit(Job.unit(())))
      case cmp: ChoiceElem => genericGroupJob(cmp)(ChoiceParticle(_, _))
      case cmp: ElementElem => createElemDeclJob(cmp)
      case cmp: GroupRefElem => {
        await[GroupDef](cmp.ref, cmp.loc) >>= {
          gd => gd.particle match {
            case groupParticle: NestedParticle => Job.unit((groupParticle, Job.unit(Job.unit(()))))
            case groupParticle => abort(s"referenced group $groupParticle can not be nested inside another group")
          }
        }
      }
      case cmp: SequenceElem => genericGroupJob(cmp)(SeqParticle(_, _))
    }
  }

  def elemDeclJob(cmp: ElementElem): Job[ElemDecl] = for {
    (elemDecl, continuation1) <- createElemDeclJob(cmp)
    _ <- define(elemDecl, Created)
    continuation2 <- continuation1
    _ <- define(elemDecl, Completed)
    _ <- continuation2
  } yield elemDecl

  // 3.3.2.1
  def createElemDeclJob(cmp: ElementElem): Job[(ElemDecl, Job[Job[Unit]])] = {
    (cmp.name, cmp.ref) match {
      case (Some(name), _) => {
        for {
          identityConstraints <- Job.sequence(cmp.constraints.map(identityConstraint(_)))
          (elementType: Type, typeCompletionJob: Job[Job[Unit]]) <- cmp.inlinedType.map {
            // 1
            case Left(simpleTypeElem) => simpleTypeJob(simpleTypeElem).map((_, Job.unit(Job.unit(()))))
            case Right(complexTypeElem) => createComplexTypeJob(complexTypeElem)
          }.orElse {
            // 2
            cmp.refType.map { qn => await[Type](qn, cmp.loc, Created).map((_, Job.unit(Job.unit(())))) }
          }.orElse {
            // 3
            cmp.subsitutionGroup.map { qns =>
              // the referenced element declaration must be completed because its element type is accessed
              await[ElemDecl](qns.head, cmp.loc).map(ed => (ed.elemType, Job.unit(Job.unit(()))))
            }
          }.getOrElse(Job.unit((anyType, Job.unit(Job.unit(())))))
        } yield {
          val elemDecl = ElemDecl(cmp.occurs, QNameFactory.caching.apply(conf.schemaTargetNamespace, LocalName(name)), elementType, cmp.nillable.getOrElse(false), cmp.abstr.value, valueConstraint(cmp), identityConstraints)
          (elemDecl, typeCompletionJob)
        }
      }
      case (_, Some(ref)) => await[ElemDecl](ref, cmp.loc).map((_, Job.unit(Job.unit(()))))
      case _ => abort(s"missing name or ref attribute on element declaration")
    }
  }

  def identityConstraint(cmp: IdentityConstraintCmp[_]): Job[IdentityConstraint] = {
    cmp match {
      case cmp: KeyElem => cmp.refOrDef match {
        case Left(ref) => await[KeyConstraint](ref, cmp.loc)
        case Right(keyDef) => {
          val c = KeyConstraint(componentQName(keyDef.name), selector(keyDef.selector), keyDef.fields.map(field(_)))
          for {
            _ <- define(c, Completed)
          } yield c
        }
      }
      case cmp: KeyRefElem => cmp.refOrDef match {
        case Left(ref) => await[KeyRefConstraint](ref, cmp.loc)
        case Right(keyDef) => for {
          referencedKey <- await[KeyOrUniqueConstraint](keyDef.refer, cmp.loc)
          c = KeyRefConstraint(componentQName(keyDef.name), selector(keyDef.selector), keyDef.fields.map(field(_)), referencedKey)
          _ <- define(c, Completed)
        } yield c
      }
      case cmp: UniqueElem => cmp.refOrDef match {
        case Left(ref) => await[UniqueConstraint](ref, cmp.loc)
        case Right(keyDef) => {
          val c = UniqueConstraint(componentQName(keyDef.name), selector(keyDef.selector), keyDef.fields.map(field(_)))
          for {
            _ <- define(c, Completed)
          } yield c
        }
      }
    }
  }

  def selector(cmp: SelectorElem): String = cmp.xPath
  def field(cmp: FieldElem): String = cmp.xPath

  def attrGroupJob(cmp: AttributeGroupDefElem): Job[AttrGroup] = for {
    attrsModel <- attrsModelJob2(cmp.attrs, cmp.anyAttribute)
  } yield AttrGroup(componentQName(cmp.name), attrsModel)

  def attrsModelJob(cmp: ComplexTypeContentCmp, defaultAttributes: Option[QName]): Job[AttrsModel] = {
    val job: Job[AttrsModel] = cmp match {
      case m: SimpleContentElem => attrsModelJob2(m.derivation.attrs, m.derivation.anyAttribute)
      case m: ComplexContentAbbrev => attrsModelJob2(m.attrs, m.anyAttribute)
      case m: ComplexContentElem => attrsModelJob2(m.derivation.attrs, m.derivation.anyAttribute)
    }
    defaultAttributes.fold {
      job
    } {
      // when a default attribute group must be considered then add it at the end
      qn => job >>= { attrsModel => await[AttrGroup](qn, cmp.loc).map(defaultAttributeGroup => attrsModel.add(defaultAttributeGroup.attrsModel)) }
    }
  }

  def attrsModelJob2(attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem]): Job[AttrsModel] = {
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

  def attrsModelJob(cmp: AttributeGroupRefElem): Job[AttrsModel] = await[AttrGroup](cmp.ref, cmp.loc) map (_.attrsModel)

  def attrsModelJob(ae: AttributeElemL): Job[AttrsModel] = {
    attrDeclJob(ae).map(attrDecl => {
      AttrsModel(Map(attrDecl.name -> AttrUse(attrDecl, ae.use.value, valueConstraint(ae))), None)
    })
  }

  def valueConstraint(cmp: ValueConstraintCmp): Option[ValueConstraint] = (cmp.default, cmp.fixed) match {
    case (Some(s), _) => Some(ValueConstraint(s, true))
    case (_, Some(s)) => Some(ValueConstraint(s, false))
    case _ => None
  }

  // 3.10.2.2
  def attrsModelJob(anyAttribute: AnyAttributeElem): Job[AttrsModel] = Job.unit(AttrsModel(Map(), Some(wildcard(anyAttribute, conf.schemaTargetNamespace))))

  // 3.2.2.2
  def attrDeclJob(cmp: AttributeCmp): Job[AttrDecl] = (cmp.name, cmp.ref) match {
    case (Some(an), _) => for {
      simpleType <- simpleTypeJob(cmp)
    } yield {
      val namespace = (cmp.form, conf.schemaElem.attributeFormDefault.value) match {
        case (Some(form), _) => if (form == Form.Qualified) conf.schemaTargetNamespace else NoNamespace
        case (_, form) => if (form == Form.Qualified) conf.schemaTargetNamespace else NoNamespace
      }
      AttrDecl(QNameFactory.caching.apply(namespace, LocalName(an)), simpleType, None, cmp.inheritable.getOrElse(false))
    }
    case (_, Some(ref)) => await[AttrDecl](ref, cmp.loc)
    case _ => abort(s"attribute must have a name or a ref attribute at: ${cmp.loc}")
  }

  // 3.2.2.1 / 3.2.2.2
  def simpleTypeJob(cmp: AttributeCmp): Job[SimpleType] = (cmp.simpleType, cmp.refType) match {
    case (Some(simpleTypeElem), _) => simpleTypeJob(simpleTypeElem)
    case (_, Some(ref)) => await[SimpleType](ref, cmp.loc)
    case _ => Job.unit(anySimpleType)
  }

  // part 2; 4.1.2
  def simpleTypeJob(st: SimpleTypeElem): Job[SimpleType] = st.derivation match {
    case d: SimpleTypeRestrictionElem => for {
      baseType <- d.base.fold(simpleTypeJob(d.tpe.get))(await[SimpleType](_, d.loc))
      restrictedType <- simpleTypeRestriction(componentQName(st.name), baseType, d.facetSpecs)
    } yield restrictedType
    case d: ListElem => for {
      itemType <- d.itemType.fold(simpleTypeJob(d.simpleType.get))(await[SimpleType](_, d.loc))
    } yield ListType(componentQName(st.name), anySimpleType, Facets.withWspCollapse[ListVal], itemType)
    case d: UnionElem => for {
      memberTypes <- d.memberTypes.fold(Job.unit(Seq[SimpleType]()))(qns => Job.traverse(qns)(await[SimpleType](_, d.loc))) & Job.traverse(d.simpleTypes)(simpleTypeJob(_))
    } yield UnionType(componentQName(st.name), anySimpleType, Facets.empty[SimpleVal], memberTypes._1 ++ memberTypes._2)
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

          override def visit(tpe: FloatType, p: Unit): Job[AtomicType] = restrict(tpe)(FloatType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), whitespaceHandler)

          override def visit(tpe: untypedAtomicType.type, p: Unit): Job[AtomicType] = abort("untypedAtomic type can not be used as a base type")

          override def visit(tpe: IntegerType, p: Unit): Job[AtomicType] = restrict(tpe)(IntegerType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[BigInt]], whitespaceHandler)

          override def visit(tpe: LongType, p: Unit): Job[AtomicType] = restrict(tpe)(LongType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[Long]], whitespaceHandler)

          override def visit(tpe: IntType, p: Unit): Job[AtomicType] = restrict(tpe)(IntType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[Int]], whitespaceHandler)

          override def visit(tpe: ShortType, p: Unit): Job[AtomicType] = restrict(tpe)(ShortType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[Short]], whitespaceHandler)

          override def visit(tpe: ByteType, p: Unit): Job[AtomicType] = restrict(tpe)(ByteType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), digitsHandler[AtomicVal[Byte]], whitespaceHandler)

          override def visit(tpe: StringType, p: Unit): Job[AtomicType] = restrict(tpe)(StringType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), lengthHandler[AtomicVal[String]], whitespaceHandler)

          override def visit(tpe: QNameType, p: Unit): Job[AtomicType] = restrict(tpe)(QNameType.apply)(generalFacetsHandler(tpe), whitespaceHandler)

          override def visit(tpe: DateType, p: Unit): Job[AtomicType] = restrict(tpe)(DateType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), explicitTimeZoneHandler, whitespaceHandler)
          override def visit(tpe: DateTimeType, p: Unit): Job[AtomicType] = restrict(tpe)(DateTimeType.apply)(generalFacetsHandler(tpe), orderHandler(tpe), explicitTimeZoneHandler, whitespaceHandler)
        }, ())

      }

    }

  }

  // part 2; 4.1.2
  def componentQName(someName: SomeValue[String]): QName = componentQName(someName.value)

  def componentQName(name: String): QName = QNameFactory.caching(conf.schemaTargetNamespace, LocalName(name))

  def convertNamespaceTokens(list: List[NamespaceItemToken], conf: JobConf): Set[Namespace] = {
    list.map {
      case NamespaceItemToken.Local => Namespace.NoNamespace
      case NamespaceItemToken.TargetNamespace => conf.schemaTargetNamespace
      case NamespaceItemToken.Uri(uri) => Namespace(uri)
    } toSet
  }

}

