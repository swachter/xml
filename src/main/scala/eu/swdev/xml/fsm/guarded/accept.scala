package eu.swdev.xml.fsm.guarded

import eu.swdev.xml.fsm.StateMachine
import eu.swdev.xml.schema._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/* Translate a content model into a deterministic finite state machine with ranges
 *
 * The implementation is based on work by Henry S. Thompson
 * (cf. "Efficient implementation of content models with numerical occurrence constraints", XTech 2006,
 * Amsterdam, Netherlands; download: http://www.ltg.ed.ac.uk/~ht/XTech_2006_paper.pdf)
 */


// Guard must not be a case class because equals & hashCode must reflect object identity
class Guard(val occurs: Occurs)

sealed trait Transition {
  def minGuards: Set[Guard]
  def maxGuards: Set[Guard]
}

sealed trait NonExitTransition extends Transition {
  def toState: Int
}

sealed trait NonEpsilonTransition extends Transition

sealed case class EpsilonTransition(minGuards: Set[Guard], maxGuards: Set[Guard], toState: Int) extends NonExitTransition
sealed case class ExitTransition(minGuards: Set[Guard], maxGuards: Set[Guard]) extends NonEpsilonTransition

sealed case class WildcardTransition(minGuards: Set[Guard], maxGuards: Set[Guard], toState: Int, wildcard: Wildcard) extends NonEpsilonTransition with NonExitTransition
sealed case class DeclTransition(minGuards: Set[Guard], maxGuards: Set[Guard], toState: Int, decl: ElemDecl) extends NonEpsilonTransition with NonExitTransition
sealed case class IncrementTransition(minGuards: Set[Guard], maxGuards: Set[Guard], toState: Int, guard: Guard) extends NonEpsilonTransition with NonExitTransition

object Transition {

  // Appendix J (analysis of the unique particle attribution constraint)
  def overlap(t1: NonEpsilonTransition, t2: NonEpsilonTransition): Boolean = (t1, t2) match {
    case (DeclTransition(_, _, _, d1), DeclTransition(_, _, _, d2)) => {
      // TODO: consider subsitution groups
      d1.name == d2.name
    }
    case (WildcardTransition(_, _, _, w1), WildcardTransition(_, _, _, w2)) => {
      w1.namespaceConstraint.intersect(w2.namespaceConstraint) match {
        case c: NamespaceConstraint.Any => true
        case c: NamespaceConstraint.Not => true
        case c: NamespaceConstraint.Enum => !c.namespaces.isEmpty
      }
    }
    case _ => false
  }

  def isRestriction(baseTrans: NonEpsilonTransition, restTrans: NonEpsilonTransition): Boolean = (baseTrans, restTrans) match {
    case (DeclTransition(_, _, _, d1), DeclTransition(_, _, _, d2)) => d1.name == d2.name
    case (WildcardTransition(_, _, _, w1), WildcardTransition(_, _, _, w2)) => {
      w2.namespaceConstraint.isSubsetOf(w1.namespaceConstraint) && w2.processContents.isRestrictionOf(w1.processContents)
    }
    case _ => false
  }

}



/**
 * Represents a non-deterministic finite state machine that corresponds to the content model
 * represented by a particle.
 *
 * The state machine is built by linking its start state and its end state using transitions and intermediate states.
 * The linkage is done recursively by traversing the particle.
 */
case class Acceptor(particle: Particle) extends StateMachine[Transition] {

  val startState = addState
  val endState = addState

  linkStatesByParticle(startState, endState, particle)
  addTransition(endState, ExitTransition(Set.empty, Set.empty))

  /**
   * Links two states by transitions that account for the occurrence constraint of a particle.
   *
   * Intermediate states are introduced as necessary.
   *
   * @param from
   * @param to
   * @param particle
   */
  def linkStatesByParticle(from: Int, to: Int, particle: Particle): Unit = {

    particle.occurs match {

      case Occurs(0, MaxOccurs.Bounded(1)) => {

        // from --t--> to
        // from --e--> to

        linkStatesByTerm(from, to, particle)
        addTransition(from, EpsilonTransition(Set.empty, Set.empty, to))

      }

      case Occurs(0, MaxOccurs.Unbounded) => {

        // one intermediate state u
        //
        // from --e--> to
        // from --t--> u
        // u    --e--> from

        val u = addState
        linkStatesByTerm(from, u, particle)
        addTransition(u, EpsilonTransition(Set.empty, Set.empty, from))
        addTransition(from, EpsilonTransition(Set.empty, Set.empty, to))

      }

      case Occurs(1, MaxOccurs.Bounded(1)) => {

        // from --t--> to

        linkStatesByTerm(from, to, particle)

      }

      case Occurs(1, MaxOccurs.Unbounded) => {

        // one intermediate state u
        //
        // from --t--> u
        // u    --e--> from
        // u    --e--> to

        val u = addState
        linkStatesByTerm(from, u, particle)
        addTransition(u, EpsilonTransition(Set.empty, Set.empty, from))
        addTransition(u, EpsilonTransition(Set.empty, Set.empty, to))
      }
      case Occurs(min, max) => {

        // three intermediate states u, v, w
        //
        // from --e|--> u
        // u    --t-->  v
        // v    --+-->  w
        // w    --e-->  from
        //
        // from --e!--> to     ; if minOccurs == 0
        // w    --e!--> to     ; if minOccurs != 0

        val u = addState
        val v = addState
        val w = addState
        val guard = new Guard(particle.occurs)
        linkStatesByTerm(u, v, particle)
        addTransition(from, EpsilonTransition(Set.empty, Set(guard), u))
        addTransition(v, IncrementTransition(Set.empty, Set.empty, w, guard))
        addTransition(w, EpsilonTransition(Set.empty, Set.empty, from))
        if (min == 0) {
          addTransition(from, EpsilonTransition(Set(guard), Set.empty, to))
        } else {
          addTransition(w, EpsilonTransition(Set(guard), Set.empty, to))
        }
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
        addTransition(from, DeclTransition(Set.empty, Set.empty, to, particle))
      }
      case particle: ElemWildcard => addTransition(from, WildcardTransition(Set.empty, Set.empty, to, particle.wildcard))
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
            case _ => addTransition(f, EpsilonTransition(Set.empty, Set.empty, to))
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
            case _ => addTransition(f, EpsilonTransition(Set.empty, Set.empty, to))
          }
        }
        go(from, particle.nested)
      }
    }

  }

}

// deterministic state member
case class DStateMember(origState: Int, minGuards: Set[Guard], maxGuards: Set[Guard])

/**
 * Represents a deterministic finite state machine for a non-deterministic acceptor.
 *
 * @param acceptor
 */
case class Determinized(acceptor: Acceptor) extends StateMachine[NonEpsilonTransition] {

  // deterministic state
  type DState = Set[DStateMember]

  val dStates = ArrayBuffer[DState]()
  val dStateNumber=  scala.collection.mutable.Map[DState, Int]()

  subsetConstruction

  def epsilonClosure(origState: Int): DState = {
    @tailrec
    def go(stack: List[DStateMember], accu: DState): DState = {
      stack match {
        case head :: tail => {
          val newMembers: Seq[DStateMember] = acceptor.transitions(head.origState).collect {
            case EpsilonTransition(minGuards, maxGuards, toState) => DStateMember(toState, head.minGuards ++ minGuards, head.maxGuards ++ maxGuards)
          }.filter(!accu.contains(_))
          val newStack = newMembers.foldRight(tail)(_ :: _)
          go(newStack, accu ++ newMembers)
        }
        case _ => accu
      }
    }
    val dsm = DStateMember(origState, Set.empty, Set.empty)
    go(dsm :: Nil, Set(dsm))
  }

  def addDState(dState: DState): Int = {
    val stateNumber = addState
    dStates += dState
    dStateNumber.put(dState, stateNumber)
    stateNumber
  }

  def calcClosureAndGetDStateNumber(origState: Int): Int = {
    val y = epsilonClosure(origState)
    dStateNumber.getOrElse(y, addDState(y))
  }

  def subsetConstruction: Unit = {
    addDState(epsilonClosure(acceptor.startState))
    var marked = 0
    while (marked < dStates.size) {
      for {
        dsm <- dStates(marked)
        transition <- acceptor.transitions(dsm.origState).collect { case t: NonEpsilonTransition => t }
      } {
        val minGuards = dsm.minGuards ++ transition.minGuards
        val maxGuards = dsm.maxGuards ++ transition.maxGuards
        transition match {
          case WildcardTransition(_, _, toState, wc) => {
            addTransition(marked, WildcardTransition(minGuards, maxGuards, calcClosureAndGetDStateNumber(toState), wc))
          }
          case DeclTransition(_, _, toState, decl) => {
            addTransition(marked, DeclTransition(minGuards, maxGuards, calcClosureAndGetDStateNumber(toState), decl))
          }
          case IncrementTransition(_, _, toState, guard) => {
            addTransition(marked, IncrementTransition(minGuards, maxGuards, calcClosureAndGetDStateNumber(toState), guard))
          }
          case ExitTransition(_, _) => {
            addTransition(marked, ExitTransition(minGuards, maxGuards))
          }
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

