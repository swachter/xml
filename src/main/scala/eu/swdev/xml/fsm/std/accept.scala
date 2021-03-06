package eu.swdev.xml.fsm.std

import eu.swdev.xml.fsm.StateMachine
import eu.swdev.xml.schema._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer



/**
 * Represents a non-deterministic finite state machine that corresponds to the content model
 * represented by a particle.
 * 
 * The state machine is built by linking its start state and its end state using transitions and intermediate states.
 * The linkage is done recursively by traversing the particle.
 *
 * Occurrence constraints are represented by possibly many intermediate states forming an extensional representation.
 *
 * Therefore this representation is feasible only for moderate minimum and maximum occurrence constraint numbers. The
 * special case of an unbounded maximum number of occurrence is handled differently and does not have a negative impact.
 */
case class Acceptor(particle: Particle) extends StateMachine[Transition] {

  val startState = addState
  val endState = addState

  linkStatesByParticle(startState, endState, particle)

  /**
   * Links two states by transitions that account for the occurrence constraint of a particle.
   *
   * Intermediate states are introduced as necessary.
   * 
   * Let p be a particle. The following examples show what transitions and states are introduced for certain min and max
   * constraints. Epsilon transitions are represented by --e--> and particle transitions by --p-->
   *
   * p*: from --e--> u   // allows zero occurrences
   *     u    --p--> u   // allows an unbounded number of occurrences
   *     u    --e--> to  // end
   *
   * p+: from --p--> u   // requires at least one occurrence
   *     u    --p--> u   // allows an unbounded number of occurrences
   *     u    --e--> to  // end
   *
   * p?: from --p--> to  // allows one occurrence
   *     from --e--> to  // allows zero occurrences
   *
   * min=1; max=3 (two intermediate states u and v are introduced)
   *     from --p--> u   // requires the first occurrence
   *     u    --p--> v   // requires the second occurrence
   *     v    --p--> to  // requires the third occurrence
   *     u    --e--> to  // allows to skip to the end after the first occurrence
   *     v    --e--> to  // allows to skip to the end after the second occurrence
   *
   *
   * @param from
   * @param pto
   * @param particle
   */
  def linkStatesByParticle(from: Int, pto: Int, particle: Particle): Unit = {

    val (nbParticleSteps, nbEpsilonSteps, to) = particle.occurs match {
      case Occurs(min, MaxOccurs.Unbounded) => {
        val to = addState
        linkStatesByTerm(to, to, particle)
        addTransition(to, EpsilonTransition(pto))
        (min, 0, to)
      }
      case Occurs(min, MaxOccurs.Bounded(max)) => (max, max - min, pto)
    }

    nbParticleSteps match {
      case 0 => {
        // a*
        addTransition(from, EpsilonTransition(to))
      }
      case 1 => {
        linkStatesByTerm(from, to, particle)
        if (nbEpsilonSteps >= nbParticleSteps) {
          addTransition(from, EpsilonTransition(to))
        }
      }
      case _ => {
        def go(f: Int, nbps: Int): Unit = {
          if (nbEpsilonSteps >= nbps) {
            addTransition(f, EpsilonTransition(to))
          }
          if (nbps > 1) {
            val next = addState
            linkStatesByTerm(f, next, particle)
            go(next, nbps - 1)
          } else {
            linkStatesByTerm(f, to, particle)
          }
        }
        go(from, nbParticleSteps)
      }
    }

  }

  /**
   * Links states by transitions that account for the particle kind.
   * 
   * Intermediate states are introduced as necessary.
   * 
   * @param from
   * @param to
   * @param particle
   */
  def linkStatesByTerm(from: Int, to: Int, particle: Particle): Unit = {
    particle match {
      case particle: ElemDecl => {
        // TODO consider substitution groups
        addTransition(from, DeclTransition(to, particle))
      }
      case particle: ElemWildcard => addTransition(from, WildcardTransition(to, particle.wildcard))
      case particle: AllParticle => {
        // an all group is represented by a choice of sequences
        def go(f: Int, ps: Seq[Particle]): Unit = {
          ps match {
            case head :: _ :: _ => ps.permutations.foreach {
              case h :: t => {
                val next = addState
                linkStatesByParticle(f, next, h)
                go(next, t)
              }
            }
            case head :: _ => linkStatesByParticle(f, to, head)
            case _ => addTransition(f, EpsilonTransition(to))
          }
        }
        go(from, particle.nested)
      }
      case particle: ChoiceParticle => particle.nested.foreach(linkStatesByParticle(from, to, _))
      case particle: SeqParticle => {
        def go(f: Int, ps: Seq[Particle]): Unit = {
          ps match {
            case head :: _ :: _ => {
              val next = addState
              linkStatesByParticle(f, next, head)
              go(next, ps.tail)
            }
            case head :: _ => linkStatesByParticle(f, to, head)
            case _ => addTransition(f, EpsilonTransition(to))
          }
        }
        go(from, particle.nested)
      }
    }

  }
  
}

/**
 * Represents a deterministic finite state machine for a non-deterministic acceptor.
 *
 * @param acceptor
 */
case class Determinized(acceptor: Acceptor) extends StateMachine[NonEpsilonTransition] {

  val isAccepting = ArrayBuffer[Boolean]()
  
  val dStates = ArrayBuffer[Set[Int]]()
  val dStateNumber= scala.collection.mutable.Map[Set[Int], Int]()

  subsetConstruction

  def epsilonClosure(state: Int): Set[Int] = {
    @tailrec
    def go(stack: List[Int], ec: Set[Int]): Set[Int] = {
      stack match {
        case head :: tail => {
          val newTos = acceptor.transitions(head).collect { case EpsilonTransition(to) => to }.filter(!ec.contains(_))
          val newStack = newTos.foldRight(tail)(_ :: _)
          go(newStack, ec ++ newTos)
        }
        case _ => ec
      }
    }
    go(state :: Nil, Set(state))
  }

  def addDState(dState: Set[Int]): Int = {
    val stateNumber = addState
    dStates += dState
    isAccepting += dState.contains(acceptor.endState)
    dStateNumber.put(dState, stateNumber)
    stateNumber
  }

  def subsetConstruction: Unit = {
    addDState(epsilonClosure(acceptor.startState))
    var marked = 0
    while (marked < dStates.size) {
      for {
        state <- dStates(marked)
        transition <- acceptor.transitions(state).collect { case t: NonEpsilonTransition => t }
      } {
        val u = epsilonClosure(transition.toState)
        val to = dStateNumber.getOrElse(u, addDState(u))
        transition match {
          case DeclTransition(_, decl) => addTransition(marked, DeclTransition(to, decl))
          case WildcardTransition(_, wildcard) => addTransition(marked, WildcardTransition(to, wildcard))
        }
      }
      marked = marked + 1
    }
  }

  def isUniqueParticleAttributionSatisfied: Option[String] = {
    def check(ab: ArrayBuffer[NonEpsilonTransition]): Option[String] = {
      def goo(seq: Seq[NonEpsilonTransition]): Option[String] = {
        seq match {
          case head :: tail => tail.find(Transition.overlap(head, _)).map(t => s"transitions overlap - t1: $head; t2: $t").orElse(goo(tail))
          case _ => None
        }
      }
      goo(ab.toList)
    }
    def go(seq: Seq[ArrayBuffer[NonEpsilonTransition]]): Option[String] = {
      seq match {
        case h :: t => check(h).orElse(go(t))
        case _ => None
      }
    }
    go(transitions.toList)
  }

}


sealed trait Transition {
  def toState: Int
}

object Transition {

  // Appendix J (analysis of the unique particle attribution constraint)
  def overlap(t1: NonEpsilonTransition, t2: NonEpsilonTransition): Boolean = (t1, t2) match {
    case (DeclTransition(_, d1), DeclTransition(_, d2)) => {
      // TODO: consider subsitution groups
      d1.name == d2.name
    }
    case (WildcardTransition(_, w1), WildcardTransition(_, w2)) => {
      w1.namespaceConstraint.intersect(w2.namespaceConstraint) match {
        case c: NamespaceConstraint.Any => true
        case c: NamespaceConstraint.Not => true
        case c: NamespaceConstraint.Enum => !c.namespaces.isEmpty
      }
    }
    case _ => false
  }

  def isRestriction(baseTrans: NonEpsilonTransition, restTrans: NonEpsilonTransition): Boolean = (baseTrans, restTrans) match {
    case (DeclTransition(_, d1), DeclTransition(_, d2)) => d1.name == d2.name
    case (WildcardTransition(_, w1), WildcardTransition(_, w2)) => {
      w2.namespaceConstraint.isSubsetOf(w1.namespaceConstraint) && w2.processContents.isRestrictionOf(w1.processContents)
    }
    case _ => false
  }

}

sealed trait NonEpsilonTransition extends Transition

sealed case class EpsilonTransition(toState: Int) extends Transition
sealed case class WildcardTransition(toState: Int, wildcard: Wildcard) extends NonEpsilonTransition
sealed case class DeclTransition(toState: Int, decl: ElemDecl) extends NonEpsilonTransition

object RestrictionCheck {
  
  def isValid(baseParticle: Particle, restParticle: Particle): Option[String] = {
    
    val baseAcceptor = Acceptor(baseParticle)
    val restAcceptor = Acceptor(restParticle)
    
    val baseDeterminized = Determinized(baseAcceptor)
    val restDeterminized = Determinized(restAcceptor)

    val visited = scala.collection.mutable.Set[(Int, Int)]()

    def search(baseState: Int, restState: Int): Option[String] = {
      if (visited.contains((baseState, restState))) {
        None
      } else {
        visited += ((baseState, restState))
        def go(seq: Seq[NonEpsilonTransition]): Option[String] = {
          seq match {
            case h :: t => {
              val optBaseTrans = baseDeterminized.transitions(baseState).find(Transition.isRestriction(_, h))
              optBaseTrans.fold[Option[String]] {
                Some(s"invalid restriction - particle does not match base content model: $h")
              } {
                // a corresponding transition exists in the base state machine
                // -> check the remaining restricted transitions and then search if following the transitions in the
                // base / restricted machine detects an error
                baseTrans => go(t).orElse(search(baseTrans.toState, h.toState))
              }
            }
            case _ => {
              if (restDeterminized.isAccepting(restState) && !baseDeterminized.isAccepting(baseState)) {
                Some(s"invalid restriction - restricted content model ends in an accepting state whereas the base content model doesn't")
              } else {
                None
              }
            }
          }
        }
        go(restDeterminized.transitions(restState).toList)
      }

    }

    search(0, 0)

  }

}