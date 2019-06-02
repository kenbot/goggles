package goggles

import goggles.testdsl._
import org.specs2._


class ErrorOffsetSpec extends Specification with ScalaCheck {

  def is = 
    s2"""
      A broken last segment should have the correct offset: 
        get"$$obj.^name" $getName

        get"$$obj.$$^optic" $getOptic

        get"$$obj.$${^bogusOptic}" $getOpticCurlies

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
    
    val notAnOptic = 473
    val zero = 0

    def getName = testGet"$myBasket.BOGUS".lastSegmentOffset === 14
    def getOptic = testGet"$myBasket.$notAnOptic".lastSegmentOffset === 15
    def getOpticCurlies = testGet"$myBasket.${notAnOptic}".lastSegmentOffset === 16
    def getStar = testGet"$myBasket*".lastSegmentOffset === 13
    def getQ = testGet"$myBasket?".lastSegmentOffset === 13
    def getIndex = testGet"$myBasket[0]".lastSegmentOffset === 14
    def getCurliesName = testGet"${myBasket}.BOGUS".lastSegmentOffset === 16
    def getIndexLiteralName = testGet"$myItemList[0].BOGUS".lastSegmentOffset === 19
    def getInterpIndexName = testGet"$myItemList[$zero].BOGUS".lastSegmentOffset === 23
    def getInterpIndexCurliesName = testGet"$myItemList[${zero}].BOGUS".lastSegmentOffset === 25
    def getOpticName = testGet"$myBasket.$basketItems.BOGUS".lastSegmentOffset === 27
    def getOpticCurliesName = testGet"$myBasket.${basketItems}.BOGUS".lastSegmentOffset === 29
    def getNameName = testGet"$myBasket.items.BOGUS".lastSegmentOffset === 20
    def getNameStar = testGet"$myBasket.user*".lastSegmentOffset === 18
    def lensOpticName = testLens"$basketUser.BOGUS".lastSegmentOffset === 18
    def lensOpticStar = testLens"$basketUser*".lastSegmentOffset === 17
    def setName = testSet"$myBasket.BOGUS".lastSegmentOffset === 14
    def setNameName = testSet"$myBasket.items.BOGUS".lastSegmentOffset === 20


}
