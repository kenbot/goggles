package goggles

import goggles.macros.errors._
import goggles.macros.interpret.OpticType
import goggles.macros.lex._
import goggles.testdsl._
import monocle.{Fold, Getter, Setter}
import org.specs2._
import OpticType._

import Fixture._

import scalaz.Monoid


class ErrorOffsetSpec extends Specification with ScalaCheck {

  def is = ???
    s2"""
      A broken last segment should have the correct offset: 
        get"$$obj.name" $getName
                  ^
        get"$$obj.$$optic" $getOptic
                  ^
        get"$$obj.$${optic}" $getOpticCurlies
                  ^
        get"$$obj*" $getStar
                 ^
        get"$$obj?" $getQ
                 ^
        get"$$obj[0]" $getIndex
                 ^
        get"$${obj}.name" $getCurliesName
                    ^
        get"$$obj[0].name" $getIndexLiteralName
                     ^
        get"$$obj[$$i].name" $getInterpIndexName
                       ^
        get"$$obj[$${i}].name" $getInterpIndexCurliesName
                         ^
        get"$$obj.$$optic.name" $getOpticName
                          ^
        get"$$obj.$${optic}.name" $getOpticCurliesName
                            ^
        get"$$obj.name1.name2" $getNameName
                        ^
        get"$$obj.name1*" $getNameStar
                       ^
        lens"$$optic.name" $lensOpticName
                     ^

        lens"$$optic*" $lensOpticStar
                    ^
        set"$$obj.name" $setName
        set"$$obj.name1.name2" $setNameName
    """

    import Fixture._
    
    def getName = {
      testGet"$myBasket.BOGUS"

    }
    def getOptic = ???
    def getOpticCurlies = ???
    def getStar = ???
    def getQ = ???
    def getIndex = ???
    def getCurliesName = ???
    def getIndexLiteralName = ???
    def getInterpIndexName = ???
    def getInterpIndexCurliesName = ???
    def getOpticName = ???
    def getOpticCurliesName = ???
    def getNameName = ???
    def getNameStar = ???
    def lensOpticName = ???
    def lensOpticStar = ???
    def setName = ???
    def setNameName = ???


}
