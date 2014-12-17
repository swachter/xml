package eu.swdev.xml.xsd.cmp

import eu.swdev.xml.schema.{TypeDerivationCtrl, StDerivationCtrl, CtDerivationCtrl, Relation}
import org.scalatest.FunSuite

/**
  */
class RelationTest extends FunSuite {

   test("RelationSet toSet conversion") {

     import Relation.{Extension, Restriction, Union}

     val ext = RelationSet.Items[TypeDerivationCtrl](List(Restriction))

     val complexCtrl = ext.toSet[CtDerivationCtrl]

     val simpleCtrl = ext.toSet[StDerivationCtrl]

     assert(complexCtrl.size == 1)
     assert(simpleCtrl.size == 1)

     assert(RelationSet.all[CtDerivationCtrl].toSet[CtDerivationCtrl].size == 2)
     assert(RelationSet.all[StDerivationCtrl].toSet[StDerivationCtrl].size == 3)
   }

}
