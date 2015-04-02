package eu.swdev.xml.fsm

import scala.collection.mutable.ArrayBuffer

/**
 * Represents a state machine.
 *
 * States are identified by successive integers starting at 0. Each state can have a number
 * of transitions that start at that state.
 */
trait StateMachine[T] {

   val transitions = ArrayBuffer[ArrayBuffer[T]]()

   def addState: Int = {
     transitions += ArrayBuffer[T]()
     transitions.size - 1
   }

   def addTransition(from: Int, t: T): Unit = {
     transitions(from) += t
   }

 }
