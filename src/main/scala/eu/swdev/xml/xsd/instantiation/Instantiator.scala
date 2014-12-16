package eu.swdev.xml.xsd.instantiation

import javax.print.attribute.standard.JobState

import eu.swdev.xml.name.{LocalName, QName, Namespace}
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._

import scala.annotation.tailrec

/**
  */
object Instantiator {

  val jobMod = new JobMod {}

  import jobMod._

  def jobs(schema: SchemaElem): Seq[Job[SchemaTopComponent]] = {

    for {
      top <- schema.schemaTop.collect { case Left(stge: SchemaTopGroupElem) => stge }
    } yield {
      top match {
        case e: AttributeElemG => ???
        case e: AttributeGroupDefElem => ???
        case e: ComplexTypeElem => complexTypeJob(e)
        case e: ElementElem => ???
        case e: GroupDefElem => ???
        case e: NotationElem => ???
        case e: SimpleTypeElem => simpleTypeJob(e)
      }
    }

  }

  def instantiate(
    schemaElem: SchemaElem,
    targetNamespace: Namespace,
    schemaImport: (Namespace, Option[String]) => (JobLog, Option[Schema])
  ): Either[JobLog, (JobLog, Schema)] = {

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
      log: JobLog,
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
            case GlobalRef(symbolSpace, qName, schemaLocation) => state.imported.get(qName.namespace) match {
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
                val t = schemaImport(qName.namespace, schemaLocation)
                val newState = state.copy(log = concatLogs(t._1, state.log), imported = state.imported + (qName.namespace -> t._2))
                drive(step, newState)
              }
            }
          }
        }
        case Done(a, jobState) => {
          val t = localNameAndSymbolSpace(a)
          val ova: Option[VAL[A]] = state.localWaiting.get(t._1, Completed)(t._2)
          val newSteps: Seq[Step[SchemaTopComponent]] = ova.map(_.map(_(Some(a)))).getOrElse(Seq())
          val newLocalWaiting = state.localWaiting.-(t._1, Completed)(t._2)
          state.copy(
            results = state.results.+(t._1, Completed, a)(t._2),
            localWaiting = newLocalWaiting,
            steps = newSteps ++ state.steps,
            log = concatLogs(jobState.log, state.log))
        }
        case Abort(jobState) => {
          state.copy(log = concatLogs(jobState.log, state.log))
        }
        case Created(a, next) => {
          val t = localNameAndSymbolSpace(a)
          val ova: Option[VAL[A]] = state.localWaiting.get(t._1, Created)(t._2)
          val newSteps: Seq[Step[SchemaTopComponent]] = ova.map(_.map(_(Some(a)))).getOrElse(Seq())
          val newLocalWaiting = state.localWaiting.-(t._1, Created)(t._2)
          val newState = state.copy(
            results = state.results.+(t._1, Created, a)(t._2),
            localWaiting = newLocalWaiting,
            steps = newSteps ++ state.steps)
          drive(next, newState)
        }
      }
    }

    def localNameAndSymbolSpace[A <: SchemaTopComponent](a: A): (LocalName, SymbolSpace[A]) = {
      a match {
        case c: ComplexType => (c.name.localName, SymbolSpace.Type.asInstanceOf[SymbolSpace[A]])
        case c: SimpleType => (c.name.localName, SymbolSpace.Type.asInstanceOf[SymbolSpace[A]])

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
    val initialJobs = jobs(schemaElem)
    val jobConfig = new JobConf(schemaElem, targetNamespace)
    val jobState = new State(jobConfig, emptyJobLog)
    val initialSteps: Seq[Step[SchemaTopComponent]] = initialJobs.map(_.run(jobState))

    val initialState = InstantiationState(initialSteps, LocalWaiting(Map.empty), GlobalWaiting(Map.empty), Map.empty, emptyJobLog, Results(Map.empty))

    val finalState = driveSteps(initialState)

    if (finalState.log.isEmpty) {
      val symbolTable = finalState.results.entries.foldLeft(SymbolTable.empty)((acc, entry) => {
        def add[X](e: RESENTRY[X]): SymbolTable = {
          acc.+(e._1._1, e._2)(e._1._3)
        }
        add(entry)
      })
      Right((finalState.log, Schema(targetNamespace, symbolTable)))
    } else {
      Left(finalState.log)
    }

  }
}

