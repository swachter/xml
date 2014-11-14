package eu.swdev.pushparser

import shapeless.{HNil, HList}
import shapeless.ops.hlist.Prepend

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq

/**
  */
trait PushParserMod { self =>

  type State
  type Input

  type DriveReplay = List[Input]
  type DriveInputs = LinearSeq[Input]

  type DriveResult[O] = (Option[O], State, DriveReplay, DriveInputs)

  case class Parser[+O](run: State => Step[O]) {

    def map[O1](f: O => O1): Parser[O1] = Parser(state => {
      run(state) map f
    })

    def flatMap[O1](f: O => Parser[O1]): Parser[O1] = Parser(state => {

      def go(step: Step[O]): Step[O1] = step match {
        case Await(rec) => Await(o => go(rec(o)))
        case Done(out, state1) => f(out).run(state1)
        case Replay(input, next) => Replay(input, go(next))
        case Abort(state1) => Abort(state1)
      }

      go(run(state))
    })

    def filter(f: O => Boolean): Parser[O] = withFilter(f)

    def withFilter(f: O => Boolean): Parser[O] = Parser(state => {
      run(state).filter(f)
    })

    def &[O2](p2: => Parser[O2]): Parser[(O, O2)] = for {
      v1 <- this
      v2 <- p2
    } yield (v1, v2)

    def |[O2 >: O](p2: => Parser[O2]): Parser[O2] = Parser(state => {

      def go(step: Step[O], replay: List[Input]): Step[O2] = step match {
        case Await(rec) => Await(o => go(rec(o), o.map(e => e :: replay).getOrElse(replay)))
        case Done(out, state1) => Done(out, state1)
        case Replay(input, next) => Replay(input, go(next, replay.drop(input.length)))
        case Abort(_) => Replay(replay, p2.run(state))
      }

      go(run(state), List[Input]())
    })

    def opt: Parser[Option[O]] = this.map(x => Some(x)) | success(None)

    def rep: Parser[List[O]] = rep1 | success(List[O]())

    def rep1: Parser[List[O]] = (this & rep).map(t => t._1 :: t._2)

    //

    def drive(initialState: State, inputs: DriveInputs): DriveResult[O] = self.drive(run(initialState), inputs, Nil)

  }

//  trait ParserOpsNN {
//
//    implicit class ConsOpNN[O1](val p1: Parser[O1]) {
//      import shapeless.::
//      //      def asHList: Parser[O :: HNil] = Parser(state => p1.run(state).map(o => o :: HNil))
//
//      def ::[O2](p2: Parser[O2])(implicit prep: Prepend[O2 :: HNil, O1 :: HNil]): Parser[prep.Out] = for {
//        v1 <- p1
//        v2 <- p2
//      } yield prep(v2 :: HNil, v1 :: HNil)
//    }
//
//  }
//
//  trait ParserOpsNL extends ParserOpsNN {
//
//    implicit class ConsOpNL[O1](val p1: Parser[O1]) {
//      import shapeless.::
//      //      def asHList: Parser[O :: HNil] = Parser(state => p1.run(state).map(o => o :: HNil))
//
//      def ::[O2 <: HList](p2: Parser[O2])(implicit prep: Prepend[O2, O1 :: HNil]): Parser[prep.Out] = for {
//        v1 <- p1
//        v2 <- p2
//      } yield prep(v2, v1 :: HNil)
//    }
//
//  }
//
//  trait ParserOpsLN extends ParserOpsNL {
//
//    implicit class ConsOpLN[O1 <: HList](val p1: Parser[O1]) {
//      import shapeless.::
//
//      def ::[O2](p2: Parser[O2])(implicit prep: Prepend[O2 :: HNil, O1]): Parser[prep.Out] = for {
//        v1 <- p1
//        v2 <- p2
//      } yield prep(v2 :: HNil, v1)
//
//    }
//
//  }
  
  trait HListParser[IN, OUT <: HList] {
    def apply(p: Parser[IN]): Parser[OUT]
  }

  trait LowPriorityHListParser {

    import shapeless.::

    implicit def mapHListParser[O] = new HListParser[O, O :: HNil] {
      override def apply(p: Parser[O]): Parser[O :: HNil] = p map (_ :: HNil)
    }

  }
  object HListParser extends LowPriorityHListParser {

    implicit def noopHListParser[O <: HList] = new HListParser[O, O] {
      override def apply(p: Parser[O]): Parser[O] = p
    }

  }

  object Parser {

    implicit class HListParserOps[O1, O1HL <: HList](val p1: Parser[O1])(implicit val ev1: HListParser[O1, O1HL]) {

      def ::[O2, O2HL <: HList](p2: Parser[O2])(implicit ev2: HListParser[O2, O2HL]) /*: PrependHListParsers[ev1.Out, ev2.Out] */ = PrependHListParsers(ev1(p1), ev2(p2))

      def append[O2, O2HL <: HList](p2: Parser[O2])(implicit ev2: HListParser[O2, O2HL]) /*: PrependHListParsers[ev1.Out, ev2.Out] */ = AppendHListParsers(ev1(p1), ev2(p2))

    }

    case class PrependHListParsers[O1 <: HList, O2 <: HList](p1: Parser[O1], p2: Parser[O2]) {
      def ::[O3, O3HL <: HList](p3: Parser[O3])(implicit ev3: HListParser[O3, O3HL], prep2: Prepend[O2, O1]): PrependHListParsers[prep2.Out, O3HL] = {
        val tailParser: Parser[prep2.Out] = for {
          v2 <- p2
          v1 <- p1
        } yield prep2(v2, v1)

        PrependHListParsers(tailParser, ev3(p3))
      }
    }

    object PrependHListParsers {
      implicit def toHListParser[O1 <: HList, O2 <: HList](prepend: PrependHListParsers[O1, O2])(implicit prep: Prepend[O2, O1]): Parser[prep.Out] = for {
        v2 <- prepend.p2
        v1 <- prepend.p1
      } yield prep(v2, v1)
    }

    case class AppendHListParsers[O1 <: HList, O2 <: HList](p1: Parser[O1], p2: Parser[O2]) {
      def append[O3, O3HL <: HList](p3: Parser[O3])(implicit ev3: HListParser[O3, O3HL], prep2: Prepend[O1, O2]): PrependHListParsers[prep2.Out, O3HL] = {
        val tailParser: Parser[prep2.Out] = for {
          v1 <- p1
          v2 <- p2
        } yield prep2(v1, v2)

        PrependHListParsers(tailParser, ev3(p3))
      }
    }

    object AppendHListParsers {
      implicit def toHListParser[O1 <: HList, O2 <: HList](append: AppendHListParsers[O1, O2])(implicit prep: Prepend[O1, O2]): Parser[prep.Out] = for {
        v1 <- append.p1
        v2 <- append.p2
      } yield prep(v1, v2)
    }

  }

  def success[X](x: X): Parser[X] = Parser(s => Done(x, s))

  val pNil: Parser[HNil] = Parser(state => Done(HNil, state))

  //implicit def liftToHList[O](p: Parser[O]) = p.asHList

  @tailrec
  private def drive[X](step: Step[X], inputs: DriveInputs, replay: DriveReplay): DriveResult[X] = step match {
    case Await(rec) => {
      if (replay.isEmpty) {
        if (inputs.isEmpty) {
          drive(rec(None), inputs, replay)
        } else {
          drive(rec(Some(inputs.head)), inputs.tail, replay)
        }
      } else {
        drive(rec(Some(replay.head)), inputs, replay.tail)
      }
    }
    case Replay(input, next) => drive(next, inputs, input.reverse ++ replay)
    case Done(out, state) => (Some(out), state, replay, inputs)
    case Abort(state) => (None, state, replay, inputs)
  }

  /**
    */
  sealed trait Step[+O] {

    def map[O1](f: O => O1): Step[O1] = this match {
      case Await(receive) => Await(oe => receive(oe) map f)
      case Done(out, state) => Done(f(out), state)
      case Replay(input, next) => Replay(input, next map f)
      case Abort(state) => Abort(state)
    }

    def filter(f: O => Boolean): Step[O] = this match {
      case Await(receive) => Await(oe => receive(oe) filter f)
      case Done(out, state) => if (f(out)) Done(out, state) else Abort(state)
      case Replay(input, next) => Replay(input, next filter f)
      case Abort(state) => Abort(state)
    }

  }

  case class Await[O](receive: Option[Input] => Step[O]) extends Step[O]

  case class Done[O](out: O, state: State) extends Step[O]

  case class Replay[O](input: List[Input], next: Step[O]) extends Step[O]

  case class Abort[O](state: State) extends Step[O]

}

