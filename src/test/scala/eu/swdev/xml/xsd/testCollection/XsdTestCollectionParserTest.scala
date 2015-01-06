package eu.swdev.xml.xsd.testCollection

import java.net.URL

import org.scalatest.{FunSuite, Inside}

/**
  */
class XsdTestCollectionParserTest extends FunSuite with Inside {

  test("read suite") {

    val suiteUri = this.getClass.getResource("/xmlschema2006-11-06/suite.xml").toURI

    println(s"suite-url: $suiteUri")

    val suiteParseResult = XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSuite)(suiteUri)

    inside (suiteParseResult) {
      case (Some(TestSuiteElem(_, _, name, releaseDate, schemaVersion, testSetRefs, _)), _, _, _) =>
    }

    for {

      suite <- suiteParseResult._1
      testSetRef <- suite.testSetRefs

    } {

      val setUri = suiteUri.resolve(testSetRef.href)

      println(s"set-url: $setUri")

      val setParseResult = XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSet)(setUri)

      inside (setParseResult) {
        case (Some(TestSetElem(_, _, contributor, name, testSetRefs, _)), _, _, _) => println(s"name: $name")
      }

    }
  }

}
