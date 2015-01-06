package eu.swdev.xml

/**
  */
package object log {

  type Message = String
  type Messages = List[Message]

  def prepend(msg: Message, msgs: Messages): Messages = msg :: msgs
  def concat(msgs: Messages*): Messages = msgs.foldRight(emptyMessages)(_ ++ _)
  def join(msgs: Seq[Messages]): Messages = msgs.foldRight(emptyMessages)(_ ++ _)
  def isEmpty(msgs: Messages): Boolean = msgs.isEmpty

  val emptyMessages = List[Message]()

}
