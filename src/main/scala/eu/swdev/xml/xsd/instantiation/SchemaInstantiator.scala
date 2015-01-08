package eu.swdev.xml.xsd.instantiation

import java.util

import eu.swdev.xml.fsm.RestrictionCheck
import eu.swdev.xml.log._
import eu.swdev.xml.name.{LocalName, QName, Namespace}
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._

import scala.annotation.tailrec

/**
  */
trait SchemaInstantiator { self: SchemaStore with SchemaResolver with SchemaParser =>

  def jobs(schema: SchemaElem, jobConf: JobConf): (Messages, Seq[Job[SchemaTopComponent]]) = {

    val compositionMsgsAndJobs: Seq[(Messages, Seq[Job[SchemaTopComponent]])] = for {
      cmp <- schema.compositions.collect { case Left(cmp: NonImportCompositionCmp) => cmp }
    } yield {
      cmp match {
        case cmp: IncludeElem => {
          val (resolveMsgs, optResolved) = resolveInclude(cmp.schemaLocation, cmp.baseUri)
          optResolved.fold[(Messages, Seq[Job[SchemaTopComponent]])] {
            (resolveMsgs, Seq())
          } {
            case (inputs, baseUri) => {
              val (parseMsgs, optSchemaElem) = parseSchemaDocument(inputs, baseUri)
              optSchemaElem.fold[(Messages, Seq[Job[SchemaTopComponent]])] {
                (concat(resolveMsgs, parseMsgs), Seq())
              } {
                schemaElem => {
                  val (jobsMsgs, jbs) = jobs(schemaElem, JobConf(schema, Some(jobConf.schemaTargetNamespace)))
                  (concat(resolveMsgs, parseMsgs, jobsMsgs), jbs)
                }
              }
            }
          }
        }
        case cmp: RedefineElem => ???
        case cmp: OverrideElem => ???
      }
    }
    
    val jobMod = JobMod(jobConf)

    import jobMod._

    val schemaTopJobs = for {
      top <- schema.schemaTop.collect { case Left(stge: SchemaTopGroupElem) => stge }
    } yield {
      top match {
        case e: AttributeElemG => attrDeclJob(e)
        case e: AttributeGroupDefElem => attrGroupJob(e)
        case e: ComplexTypeElem => complexTypeJob(e)
        case e: ElementElem => elemDeclJob(e)
        case e: GroupDefElem => groupDefJob(e)
        case e: NotationElem => notationJob(e)
        case e: SimpleTypeElem => simpleTypeJob(e)
      }
    }

    (join(compositionMsgsAndJobs.map(_._1)), compositionMsgsAndJobs.map(_._2).flatten ++ schemaTopJobs)

  }

  def instantiate(schemaElem: SchemaElem): (Messages, Option[Schema]) = {

    type LKEY[X] = (LocalName, Stage, SymbolSpace[X])
    type VAL[X <: SchemaTopComponent] = Seq[Option[X] => Step[SchemaTopComponent]]

    case class LocalWaiting(underlying: Map[Any, Any]) {

      def +[X <: SchemaTopComponent](kv: (LocalName, Stage, Option[X] => Step[SchemaTopComponent]))(implicit ev: SymbolSpace[X]) = {
        val key: LKEY[X] = (kv._1, kv._2, ev)
        val oldVal: VAL[X] = underlying.getOrElse(key, Seq()).asInstanceOf[VAL[X]]
        val newVal: VAL[X] = kv._3 +: oldVal
        val newMap = underlying + (key -> newVal)
        new LocalWaiting(newMap)
      }

      def get[X <: SchemaTopComponent](name: LocalName, stage: Stage)(implicit ev: SymbolSpace[X]): Option[VAL[X]] = {
        val key: LKEY[X] = (name, stage, ev)
        underlying.get(key).asInstanceOf[Option[VAL[X]]]
      }

      def -[X <: SchemaTopComponent](name: LocalName, stage: Stage)(implicit ev: SymbolSpace[X]) = {
        val key: LKEY[X] = (name, stage, ev)
        new LocalWaiting(underlying - key)
      }

    }

    type GKEY[X] = (QName, SymbolSpace[X])

    case class GlobalWaiting(underlying: Map[Any, Any]) {

      def +[X <: SchemaTopComponent](kv: (QName, Option[X] => Step[SchemaTopComponent]))(implicit ev: SymbolSpace[X]) = {
        val key: GKEY[X] = (kv._1, ev)
        val oldVal: VAL[X] = get(kv._1).getOrElse(Seq())
        val newVal: VAL[X] = kv._2 +: oldVal
        val newMap = underlying + (key -> newVal)
        new GlobalWaiting(newMap)
      }

      def get[X <: SchemaTopComponent](name: QName)(implicit ev: SymbolSpace[X]): Option[VAL[X]] = {
        val key: GKEY[X] = (name, ev)
        underlying.get(key).asInstanceOf[Option[VAL[X]]]
      }

    }

    type RESENTRY[X] = (LKEY[X], X)

    case class Results(underlying: Map[Any, Any]) {

      def +[X](kv: (LocalName, Stage, X))(implicit ev: SymbolSpace[X]) = {
        val key: LKEY[X] = (kv._1, kv._2, ev)
        val entry: RESENTRY[X] = key -> kv._3
        new Results(underlying + entry)
      }

      def get[X](name: LocalName, stage: Stage)(implicit ev: SymbolSpace[X]): Option[X] = {
        val key: LKEY[X] = (name, stage, ev)
        underlying.get(key).asInstanceOf[Option[X]]
      }

      def entries: Iterable[RESENTRY[_]] = underlying.toIterable.asInstanceOf[Iterable[RESENTRY[_]]]
    }

    case class InstantiationState(
      steps: Seq[Step[SchemaTopComponent]],
      localWaiting: LocalWaiting,
      globalWaiting: GlobalWaiting,
      imported: Map[Namespace, Option[Schema]],
      log: Messages,
      results: Results)

    @tailrec
    def drive[A <: SchemaTopComponent](step: Step[A], state: InstantiationState): InstantiationState = {
      step match {
        case Await(ref, rec) => {
          ref match {
            case LocalRef(symbolSpace, ncName, stage) => state.results.get(ncName, stage)(symbolSpace) match {
              case s@Some(i) => drive(rec(s), state)
                // local symbol not (yet) available -> register local waiting continuation
              case None => state.copy(localWaiting = state.localWaiting.+(ncName, stage, rec)(symbolSpace))
            }
            case GlobalRef(symbolSpace, qName, importHint) => state.imported.get(qName.namespace) match {
              case Some(optSchema) => {
                optSchema match {
                  case Some(schema) => {
                    val st = schema.symbolTable.get(qName.localName)(symbolSpace)
                    drive(rec(st), state)
                  }
                  // the schema could not be imported
                  case None => {
                    state
                  }
                }
              }
              // the schema has not yet been imported
              case None => {
                val t = importSchema(qName.namespace, importHint)
                val newState = state.copy(log = concat(t._1, state.log), imported = state.imported + (qName.namespace -> t._2))
                drive(step, newState)
              }
            }
          }
        }
        case Done(a, jobState) => {
          val newState = stateAfterDefinition(a, Completed, state)
          newState.copy(
            log = concat(jobState.log, newState.log)
          )
        }
        case Abort(jobState) => {
          state.copy(log = concat(jobState.log, state.log))
        }
        case Define(o, stage, next) => {
          val newState = stateAfterDefinition(o, stage, state)
          drive(next, newState)
        }
      }
    }

    def stateAfterDefinition[C <: SchemaTopComponent](c: C, stageX: Stage, stateX: InstantiationState): InstantiationState = {
      def go(stage: Stage, state: InstantiationState): InstantiationState = {
        val t = localNameAndSymbolSpace(c)
        val ova: Option[VAL[C]] = state.localWaiting.get(t._1, stage)(t._2)
        val newSteps: Seq[Step[SchemaTopComponent]] = ova.map(_.map(_(Some(c)))).getOrElse(Seq())
        val newLocalWaiting = state.localWaiting.-(t._1, stage)(t._2)
        state.copy(
          results = state.results.+(t._1, stage, c)(t._2),
          localWaiting = newLocalWaiting,
          steps = newSteps ++ state.steps)
      }
      stageX match {
        case Created => go(Created, stateX)
        case Completed => go(Completed, go(Created, stateX))
      }
    }

    def localNameAndSymbolSpace[A <: SchemaTopComponent](a: A): (LocalName, SymbolSpace[A]) = {
      a match {
        case c: ComplexType => (c.name.localName, SymbolSpace.Type.asInstanceOf[SymbolSpace[A]])
        case c: SimpleType => (c.name.localName, SymbolSpace.Type.asInstanceOf[SymbolSpace[A]])
        case c: ElemDecl => (c.name.localName, SymbolSpace.ElemDecl.asInstanceOf[SymbolSpace[A]])
        case c: GroupDef => (c.name.localName, SymbolSpace.Group.asInstanceOf[SymbolSpace[A]])
        case c: AttrDecl => (c.name.localName, SymbolSpace.AttrDecl.asInstanceOf[SymbolSpace[A]])
        case c: AttrGroup => (c.name.localName, SymbolSpace.AttrGroup.asInstanceOf[SymbolSpace[A]])
        case c: IdentityConstraint => (c.name.localName, SymbolSpace.IdentityConstraint.asInstanceOf[SymbolSpace[A]])
        case c: Notation => (c.name.localName, SymbolSpace.Notation.asInstanceOf[SymbolSpace[A]])
      }
    }

    @tailrec
    def driveSteps(state: InstantiationState): InstantiationState = {
      if (state.steps.isEmpty) {
        state
      } else {
        val newState = state.steps.foldLeft(state.copy(steps = Seq()))((acc, step) => {
          drive(step, acc)
        })
        driveSteps(newState)
      }
    }

    val jobConf = JobConf(schemaElem, None)
    val (msgs, initialJobs) = jobs(schemaElem, jobConf)
    val jobState = new State(emptyMessages)
    val initialSteps: Seq[Step[SchemaTopComponent]] = initialJobs.map(_.run(jobState))

    val initialState = InstantiationState(initialSteps, LocalWaiting(Map.empty), GlobalWaiting(Map.empty), Map.empty, emptyMessages, Results(Map.empty))

    val finalState = driveSteps(initialState)

    val symbolTable = finalState.results.entries.foldLeft(SymbolTable.empty)((acc, entry) => {
      def add[X](e: RESENTRY[X]): SymbolTable = {
        acc.+(e._1._1, e._2)(e._1._3)
      }
      add(entry)
    })

    // TODO validations
    val validationMsgs = symbolTable.underlying.keys.map(key => key._2 match {
      case SymbolSpace.Type => validate(symbolTable.get(key._1)(SymbolSpace.Type).get)
      case _ => Iterator.empty
    }).flatten

    (concat(finalState.log, concat(validationMsgs)), Some(Schema(jobConf.schemaTargetNamespace, symbolTable)))

  }

  val schemaMap = new util.HashMap[Namespace, (Messages, Option[Schema])]()

}

object validate {
  def apply(tpe: Type): Iterable[Message] = {
    tpe match {
      case tpe: ComplexType => tpe.content match {
        case cnt: ElementsContentType => tpe.baseType match {
          case bt: ComplexType => bt.content match {
            case bcnt: ElementsContentType => {
              RestrictionCheck.isValid(bcnt.group, cnt.group)
            }
            case _ => Nil
          }
          case _ => Nil
        }
        case _ => Nil
      }
      case _ => Nil
    }
  }
}
