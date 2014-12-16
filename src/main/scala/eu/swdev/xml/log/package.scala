package eu.swdev.xml

/**
  */
package object log {

  type Message = String
  type Messages = List[Message]

  def prepend(msg: Message, msgs: Messages): Messages = msg :: msgs
  def concat(msgs1: Messages, msgs2: Messages): Messages = msgs1 ++ msgs2

  val emptyMessages = Nil

}
