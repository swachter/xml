package eu.swdev.xml.data

import eu.swdev.xml.schema.Facets.HasTimeZone

/**
  */
// Part 2; D.2.1
object SevenPropertyDate {

  val yearFrag = """(-?(?:[1-9][0-9]{3,}|0[0-9]{3,3}))"""

  val monthFrag = """(0[1-9]|1[0-2])"""

  val dayFrag = """(0[1-9]|[12][0-9]|3[01])"""

  val hourFrag = """([01][0-9]|2[0-3])"""

  val minuteFrag = """([0-5][0-9])"""

  val secondFrag = """([0-5][0-9](?:\.[0-9]+)?)"""

  val timeZoneFrag = """(Z|(?:\+|-)(?:(?:0[0-9]|1[0-3]):[0-5][0-9]|14:00))"""

  val endOfDayFrag = """24:00:00(:?\.0+)?"""

  val GYearRegEx = s"$yearFrag$timeZoneFrag?".r

  val DateRegEx = s"$yearFrag-$monthFrag-$dayFrag$timeZoneFrag?".r

  val DateTimeRegEx = s"$yearFrag-$monthFrag-${dayFrag}T$hourFrag:$minuteFrag:$secondFrag$timeZoneFrag?".r

  val DateTimeEndOfDayRegEx = s"$yearFrag-$monthFrag-${dayFrag}T$endOfDayFrag$timeZoneFrag?".r

}

object DateTimeCalculation {

  import scalaz.syntax.state._
  import scalaz.State, State._

  type CState[A] = State[Int, A]

  def addAndNormalize(value: Int, increment: Int, minInc: Int, maxInc: Int): CState[Int] = for {
    carry <- get[Int]
    tmp = value + increment + carry
    newCarry = if (tmp < minInc) -1 else if (tmp > maxInc) 1 else 0
  } yield tmp - newCarry * (maxInc - minInc + 1)

  def joinCompare(compares: Int*): Int = {
    compares.iterator.find(_ != 0).getOrElse(0)
  }
}

sealed trait TimeZone {
  def signum: Int
  def hours: Int
  def minutes: Int
}

case object Zulu extends TimeZone {
  override def signum = 0
  override def hours = 0
  override def minutes = 0
  override def toString: String = "Z"
}

case class OffsetTimeZone(offset: Int) extends TimeZone {
  def signum: Int = scala.math.signum(offset)
  def hours: Int = offset.abs / 60
  def minutes: Int = offset.abs % 60
  override def toString: String = f"${if (signum < 0) '-' else '+'}$hours%02d:$minutes%02d"
}

object TimeZone {

  val TimeZoneRegEx = """(\+|-)(0[0-9]|1[0-3]):([0-5][0-9])""".r

  def apply(string: String): Either[String, TimeZone] = string match {
    case "Z" => Right(Zulu)
    case "-14:00" => Right(OffsetTimeZone(-14 * 60))
    case "+14:00" => Right(OffsetTimeZone(14 * 60))
    case TimeZoneRegEx(sign, hours, minutes) => Right(OffsetTimeZone((if (sign == "+") 1 else -1) * (hours.toInt * 60 + minutes.toInt)))
    case _ => Left(s"invalid time zone: $string")
  }

  def apply(string: Option[String]): Option[Either[String, TimeZone]] = string.map(apply(_))

}

/**
  */
case class Date(year: Int, month: Int, day: Int, timeZone: Option[TimeZone]) {

  override def toString: String = f"""$year%4d-$month%2d-$day%2d${timeZone.getOrElse("")}"""

}

object Date {

  def apply(string: String): Either[String, Date] = string match {
    case SevenPropertyDate.DateRegEx(year, month, day, timeZone) => {
      def res(otz: Option[TimeZone]): Either[String, Date] = {
        val y = year.toInt
        val m = month.toInt
        val d = day.toInt
        val days = daysOfMonth(y, m)
        if (d <= days) {
          Right(Date(y, m, d, otz))
        } else {
          Left(s"invalid date - month has only $days days")
        }
      }
      TimeZone(Option(timeZone)).fold {
        res(None)
      } {
        etz => etz.right.flatMap(tz => res(Some(tz)))
      }
    }
    case _ => Left(s"date can not be parsed: $string")
  }

  def daysOfMonth(y: Int, month: Int) = month match {
    case m if (m == 4 || m == 6 || m == 9 || m == 11) => 30
    case 2 => {
      if (y % 4 != 0 || y % 100 == 0 && y % 400 != 0) {
        28
      } else {
        29
      }
    }
    case _ => 31
  }

  implicit val ordering = new Ordering[Date] {

    override def compare(x: Date, y: Date): Int =
      DateTimeCalculation.joinCompare(x.year.compareTo(y.year), x.month.compareTo(y.month), x.day.compareTo(y.day))

  }

  implicit val hasTimeZone = new HasTimeZone[Date] {
    override def hasTimeZone(x: Date): Boolean = x.timeZone.isDefined
  }
}

case class DateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: BigDecimal, timeZone: Option[TimeZone]) {

  override def toString: String = {
    val secInt = second.intValue
    val stripped = second.bigDecimal.stripTrailingZeros
    val secFrag = if (stripped.scale > 0) {
      val s = stripped.toPlainString
      s.substring(s.indexOf('.') + 1)
    } else {
      ""
    }
    f"""$year%4d-$month%2d-$day%2dT$hour%2d:$minute%2d:$secInt%2d$secFrag${timeZone.getOrElse("")}"""
  }

  def normalize: DateTime = {
    val tz = timeZone.getOrElse(Zulu)

    import DateTimeCalculation.addAndNormalize
    import scalaz.Scalaz.get

    val normalizeAction = for {
      nMinute <- addAndNormalize(minute, tz.signum * tz.minutes, 0, 59)
      nHour <- addAndNormalize(hour, tz.signum * tz.hours, 0, 23)
      nDay <- addAndNormalize(day, 0, 1, Date.daysOfMonth(year, month))
      nMonth <- addAndNormalize(month, 0, 1, 12)
      yCarry <- get
    } yield DateTime(year + yCarry, nMonth, nDay, nHour, nMinute, second, None)

    normalizeAction.run(0)._2
  }

}

object DateTime {
  def apply(string: String): Either[String, DateTime] = {
    def res(year: String, month: String, day: String, hour: Int, minute: Int, second: BigDecimal, timeZone: Option[TimeZone]): Either[String, DateTime] = {
      val y = year.toInt
      val m = month.toInt
      val d = day.toInt
      val days = Date.daysOfMonth(y, m)
      if (d <= days) {
        Right(DateTime(y, m, d, hour, minute, second, timeZone))
      } else {
        Left(s"invalid dateTime - value: $string; month has only $days days")
      }
    }
    string match {
      case SevenPropertyDate.DateTimeRegEx(year, month, day, hour, minute, second, timeZone) => {
        TimeZone(Option(timeZone)).fold {
          res(year, month, day, hour.toInt, minute.toInt, BigDecimal(second), None)
        } {
          etz => etz.right.flatMap(tz => res(year, month, day, hour.toInt, minute.toInt, BigDecimal(second), Some(tz)))
        }
      }
      case SevenPropertyDate.DateTimeEndOfDayRegEx(year, month, day, timeZone) => {
        TimeZone(Option(timeZone)).fold {
          res(year, month, day, 24, 0, BigDecimal(0), None)
        } {
          etz => etz.right.flatMap(tz => res(year, month, day, 24, 0, 0, Some(tz)))
        }
      }
      case _ => Left(s"dateTime can not be parsed: $string")
    }
  }

  implicit val ordering = new Ordering[DateTime] {

    override def compare(l: DateTime, r: DateTime): Int = {
      val x = l.normalize
      val y = r.normalize
      DateTimeCalculation.joinCompare(x.year.compareTo(y.year), x.month.compareTo(y.month), x.day.compareTo(y.day), x.hour.compareTo(y.hour), x.minute.compareTo(y.minute), x.second.compare(y.second))
    }

  }

  implicit val hasTimeZone = new HasTimeZone[DateTime] {
    override def hasTimeZone(x: DateTime): Boolean = x.timeZone.isDefined
  }

}

