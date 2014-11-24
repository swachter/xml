package eu.swdev.xml.xsd.testCollection

import java.net.URL

import org.scalatest.{FunSuite, Inside}

/**
  */
class XsdTestCollectionParserTest extends FunSuite with Inside {

  test("read suite") {

    val suiteUrl = this.getClass.getResource("/xmlschema2006-11-06/suite.xml")

    println(s"suite-url: $suiteUrl")

    val suiteParseResult = XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSuite)(suiteUrl)

    inside (suiteParseResult) {
      case (Some(TestSuiteElem(_, _, name, releaseDate, schemaVersion, testSetRefs, _)), _, _, _) =>
    }

    for {

      suite <- suiteParseResult._1
      testSetRef <- suite.testSetRefs

    } {

      val setUrl = new URL(suiteUrl, testSetRef.href)

      println(s"set-url: $setUrl")

      val setParseResult = XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSet)(setUrl)

      inside (setParseResult) {
        case (Some(TestSetElem(_, _, contributor, name, testSetRefs, _)), _, _, _) => println(s"name: $name")
      }

    }
  }

}
