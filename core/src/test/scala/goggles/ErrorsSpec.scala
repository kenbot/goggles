package goggles

import goggles.macros._
import goggles.testdsl._
import monocle.{Getter, Setter}
import org.specs2._
import OpticType._

import Fixture._

class A
class B
class C
class D
class HasArgsMethod { def bogus(a: Int): Int = 0 }
class HasMultiParamMethod { def bogus()(): Int = 0 }
class NoCopy(val i: Int)
class HasCopyNoDefaults(val i: Int) { def copy(i: Int) = 1 }
class HasCopyMissingArg(val i: Int) { def copy(somethingElse: Int = 3) = 1 }
class HasCopyNoArgs(val i: Int) { def copy = 1 }
class HasCopyMultiParamLists(val i: Int) { def copy()() = 1 }

class ErrorsSpec extends Specification with ScalaCheck {
  def is =
    s2"""
      Check that a correct one passes: $correct

      Error scenarios:
        get"" fails $empty
        get"^" fails $wrongChar
        get"$${obj}field" fails $nameNoDot
        get"$${obj}$$interp" fails $interpNoDot
        get"$$obj.*" fails $invalidAfterDot1
        get"$$obj.?" fails $invalidAfterDot2
        get"$$obj.[0]" fails $invalidAfterDot3
        get"*" fails $nonInterpStart
        get"$$obj.]" fails $unexpectedCloseBracket
        get"$$obj.field." fails $endingDot
        get"$$obj[]" fails $emptyIndex
        get"$$obj[*]" fails $invalidIndex
        get"$$obj[" fails $unclosedOpenBracket
        get"$$noFoo.foo" fails $nameNotFound
        get"$$hasFooWithArgs.foo" fails $nameHasArgs
        get"$$hasFooWithMultiParamLists.foo" fails $nameHasMultiParamLists
        get"$$obj.$$notAnOptic" fails $notAnOptic
        get"$$obj.$$setter.$$getter" fails $wrongKindOfOptic
        get"$$a.$$aToB.$$xToZ" fails $typesDontMatch
        get"$$noEach*" fails $noEach
        get"$$noPossible?" fails $noPossible
        get"$$noIndex[n]" fails $noIndex
        get"$$indexedWithA[$$b]" fails $wrongIndexType
        set"$$hasNoCopyMethod.field" fails $copyMethodNotFound
        set"$$hasCopyButHasMultiParamLists.field" fails $copyMethodMultiParamLists
        set"$$hasCopyMethodNoArgs.field" fails $copyMethodNoArgs
        set"$$copyMethodNoFooArg.foo" fails $copyMethodLacksNamedArgument
        set"$$copyMethodNoDefaults.field" fails $copyMethodLacksParameterDefaults
    """

  def correct =
    testGet"$myBasket.items*.qty" === Right(List(11,22,33))

  def empty =
    testGet"" === Left(EmptyError)

  def wrongChar =
    testGet"^" === Left(UnrecognisedChar('^'))

  def nameNoDot =
    testGet"${myBasket}items" === Left(NameWithNoDot("items"))

  def interpNoDot =
    testGet"$myBasket$basketItems" === Left(InterpOpticWithNoDot)

  def invalidAfterDot1 =
    testGet"$myBasket.*" === Left(InvalidAfterDot(Token.Star))

  def invalidAfterDot2 =
    testGet"$myBasket.?" === Left(InvalidAfterDot(Token.Question))

  def invalidAfterDot3 =
    testGet"$myBasket.[0]" === Left(InvalidAfterDot(Token.OpenBracket))

  def nonInterpStart =
    testGet"*" === Left(NonInterpolatedStart(Token.Star))

  def unexpectedCloseBracket =
    testGet"$myBasket.]" === Left(UnexpectedCloseBracket)

  def endingDot =
    testGet"$myBasket.items." === Left(EndingDot)

  def emptyIndex =
    testGet"$myBasket.items[]" === Left(NoIndexSupplied)

  def invalidIndex =
    testGet"$myBasket.items[*?!]" === Left(InvalidIndexSupplied(Token.Star))

  def unclosedOpenBracket =
    testGet"$myBasket.items[" === Left(UnclosedOpenBracket)

  def nameNotFound =
    testGet"$myBasket.bogus" === Left(NameNotFound("bogus", "goggles.Fixture.ShoppingBasket"))

  def nameHasArgs =
    testGet"${new HasArgsMethod}.bogus" === Left(NameHasArguments("bogus", "goggles.HasArgsMethod"))

  def nameHasMultiParamLists =
    testGet"${new HasMultiParamMethod}.bogus" === Left(NameHasMultiParamLists("bogus", "goggles.HasMultiParamMethod"))

  def notAnOptic = {
    testGet"$myBasket.${new A}" === Left(InterpNotAnOptic("goggles.A"))
  }

  def wrongKindOfOptic = {
    val setter = Setter[B,C](_ => _ => new B)
    val getter = Getter[A,B](_ => new B)

    testGet"${new A}.$getter.$setter" === Left(WrongKindOfOptic(GetterType, SetterType))
  }

  def typesDontMatch = {
    val aToB = Getter[A,B](_ => new B)
    val cToD = Getter[C,D](_ => new D)

    testGet"${new A}.$aToB.$cToD" === Left(TypesDontMatch("goggles.B", "goggles.C"))
  }

  def noEach = {
    testGet"${new A}*" === Left(ImplicitEachNotFound("goggles.A"))
  }

  def noPossible = {
    testGet"${new A}?" === Left(ImplicitPossibleNotFound("goggles.A"))
  }

  def noIndex = {
    testGet"${new A}[0]" === Left(ImplicitIndexNotFound("goggles.A", "Int"))
  }

  def wrongIndexType = {
    val i = "a"
    testGet"${List(1,2,3)}[$i]" === Left(ImplicitIndexNotFound("List[Int]", "String"))
  }

  def copyMethodNotFound =
    testSet"${new NoCopy(2)}.i" === Left(CopyMethodNotFound("i", "goggles.NoCopy"))

  def copyMethodMultiParamLists =
    testSet"${new HasCopyMultiParamLists(2)}.i" === Left(CopyMethodHasMultiParamLists("i", "goggles.HasCopyMultiParamLists"))

  def copyMethodNoArgs =
    testSet"${new HasCopyNoArgs(2)}.i" === Left(CopyMethodHasNoArguments("i", "goggles.HasCopyNoArgs"))

  def copyMethodLacksNamedArgument =
    testSet"${new HasCopyMissingArg(2)}.i" === Left(CopyMethodLacksNamedArgument("i", "goggles.HasCopyMissingArg"))

  def copyMethodLacksParameterDefaults =
    testSet"${new HasCopyNoDefaults(2)}.i" === Left(CopyMethodLacksParameterDefaults("i", "goggles.HasCopyNoDefaults", List("i")))

}
