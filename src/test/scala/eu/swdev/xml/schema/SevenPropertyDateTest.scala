package eu.swdev.xml.schema

import eu.swdev.xml.data._
import org.scalatest.{Inside, FunSuite}

/**
  */
class SevenPropertyDateTest extends FunSuite with Inside {

  import SevenPropertyDate._

  test("gYear") {
    inside("1967") {
      case GYearRegEx("1967", null) =>
    }
    inside("1967+14:00") {
      case GYearRegEx("1967", "+14:00") =>
    }
    inside("1967Z") {
      case GYearRegEx("1967", "Z") =>
    }
    inside("1967-10:00") {
      case GYearRegEx("1967", "-10:00") =>
    }
  }

  test("date") {
    inside("1967-07-15") {
      case DateRegEx("1967","07", "15", null) =>
    }
  }

  test("dateTime") {
    inside(DateTime("1974-04-26T23:23:51")) {
      case Right(_) =>
    }
  }

  test("timeZone") {
    assert(OffsetTimeZone(810).toString == "+13:30")
    TimeZone("+13:30") match {
      case Right(OffsetTimeZone(offset)) => assert(offset == 810)
      case _ =>
    }
    inside(TimeZone("Z")) { case Right(Zulu) => }
    inside(TimeZone("+13:30")) { case Right(OffsetTimeZone(810)) => }
  }

}
