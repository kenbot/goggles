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
    val BASKET = myBasket

    def getName = testGet"$myBasket.BOGUS".errorOffset === Some(10)
    def getOptic = testGet"$myBasket.$notAnOptic".errorOffset === Some(11)
    def getOpticCurlies = testGet"$myBasket.${notAnOptic}".errorOffset === Some(12)
    def getStar = testGet"$myBasket*".errorOffset === Some(9)
    def getQ = testGet"$myBasket?".errorOffset === Some(9)
    def getIndex = testGet"$myBasket[0]".errorOffset === Some(10)
    
    def getIndexLiteralName = testGet"$myItemList[0].BOGUS".errorOffset === Some(15)
    def getInterpIndexName = testGet"$myItemList[$zero].BOGUS".errorOffset === Some(19)
    def getInterpIndexCurliesName = testGet"$myItemList[${zero}].BOGUS".errorOffset === Some(21)
    def getOpticName = testGet"$myBasket.$basketItems.BOGUS".errorOffset === Some(23)
    def getOpticCurliesName = testGet"$myBasket.${basketItems}.BOGUS".errorOffset === Some(25)
    def getNameName = testGet"$myBasket.items.BOGUS".errorOffset === Some(16)
    def getNameStar = testGet"$myBasket.user*".errorOffset === Some(14)
    def lensOpticName = testLens"$basketUser.BOGUS".errorOffset === Some(12)
    def lensOpticStar = testLens"$basketUser*".errorOffset === Some(11)
    def setName = testSet"$myBasket.BOGUS".errorOffset === Some(10)
    def setNameName = testSet"$myBasket.items.BOGUS".errorOffset === Some(16)
}
