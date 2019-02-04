package goggles

import goggles.testdsl._
import org.specs2._


class ErrorOffsetSpec extends Specification with ScalaCheck {

  def is = 
    s2"""
      A broken last segment should have the correct offset: 
        get"$$obj.^name" $getName

        get"$$obj.^$$optic" $getOptic

        get"$$obj.^$${optic}" $getOpticCurlies

        get"$$obj^*" $getStar

        get"$$obj^?" $getQ

        get"$$obj^[0]" $getIndex

        get"$${obj}.^name" $getCurliesName

        get"$$obj[0].^name" $getIndexLiteralName

        get"$$obj[$$i].^name" $getInterpIndexName

        get"$$obj[$${i}].^name" $getInterpIndexCurliesName

        get"$$obj.$$optic.^name" $getOpticName

        get"$$obj.$${optic}.^name" $getOpticCurliesName

        get"$$obj.name1.^name2" $getNameName

        get"$$obj.name1^*" $getNameStar

        lens"$$optic.^name" $lensOpticName

        lens"$$optic^*" $lensOpticStar

        set"$$obj.name" $setName
        set"$$obj.name1.name2" $setNameName
    """

    import Fixture._
    
    def getName = testGet"$myBasket.BOGUS".lastSegmentOffset === 9
    def getOptic = true === false
    def getOpticCurlies = true === false
    def getStar = true === false
    def getQ = true === false
    def getIndex = true === false
    def getCurliesName = true === false
    def getIndexLiteralName = true === false
    def getInterpIndexName = true === false
    def getInterpIndexCurliesName = true === false
    def getOpticName = true === false
    def getOpticCurliesName = true === false
    def getNameName = true === false
    def getNameStar = true === false
    def lensOpticName = true === false
    def lensOpticStar = true === false
    def setName = true === false
    def setNameName = true === false


}
