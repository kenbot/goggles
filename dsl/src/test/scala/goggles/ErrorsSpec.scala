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
        get"$$obj.$$setter" fails $getSetter
        set"$$obj.$$getter" fails $setGetter
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
        get"$$obj.$$setter.$$getter" fails $wrongKindOfOpticSG
        get"$$obj.$$setter.$$fold" fails $wrongKindOfOpticSF
        get"$$obj.$$getter.$$setter" fails $wrongKindOfOpticGS
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
    testGet"$myBasket.items*.qty".errorOrTree === Right(List(11,22,33))

  def empty =
    testGet"".errorOrTree === Left(EmptyError)

  def wrongChar =
    testGet"^".errorOrTree === Left(UnrecognisedChar('^'))

  def getSetter = {
    val setUser = Setter[ShoppingBasket, User](f => b => b.copy(user = f(b.user)))
    testGet"$myBasket.$setUser".errorOrTree === Left(GetterOpticRequired(SetterType))
  }

  def setGetter = {
    val getUser = Getter[ShoppingBasket, User](_.user)
    testSet"$myBasket.$getUser".errorOrTree === Left(SetterOpticRequired(GetterType))
  }

  def nameNoDot =
    testGet"${myBasket}items".errorOrTree === Left(NameWithNoDot("items"))

  def interpNoDot =
    testGet"$myBasket$basketItems".errorOrTree === Left(InterpOpticWithNoDot)

  def invalidAfterDot1 =
    testGet"$myBasket.*".errorOrTree === Left(InvalidAfterDot(Token.Star))

  def invalidAfterDot2 =
    testGet"$myBasket.?".errorOrTree === Left(InvalidAfterDot(Token.Question))

  def invalidAfterDot3 =
    testGet"$myBasket.[0]".errorOrTree === Left(InvalidAfterDot(Token.OpenBracket))

  def nonInterpStart =
    testGet"*".errorOrTree === Left(NonInterpolatedStart(Token.Star))

  def unexpectedCloseBracket =
    testGet"$myBasket.]".errorOrTree === Left(UnexpectedCloseBracket)

  def endingDot =
    testGet"$myBasket.items.".errorOrTree === Left(EndingDot)

  def emptyIndex =
    testGet"$myBasket.items[]".errorOrTree === Left(NoIndexSupplied)

  def invalidIndex =
    testGet"$myBasket.items[*?!]".errorOrTree === Left(InvalidIndexSupplied(Token.Star))

  def unclosedOpenBracket =
    testGet"$myBasket.items[".errorOrTree === Left(UnclosedOpenBracket)

  def nameNotFound =
    testGet"$myBasket.bogus".errorOrTree === Left(NameNotFound("bogus", "goggles.Fixture.ShoppingBasket"))

  def nameHasArgs =
    testGet"${new HasArgsMethod}.bogus".errorOrTree === Left(NameHasArguments("bogus", "goggles.HasArgsMethod"))

  def nameHasMultiParamLists =
    testGet"${new HasMultiParamMethod}.bogus".errorOrTree === Left(NameHasMultiParamLists("bogus", "goggles.HasMultiParamMethod"))

  def notAnOptic = {
    testGet"$myBasket.$Apple".errorOrTree === Left(InterpNotAnOptic("$Apple", "goggles.Fixture.Apple"))
  }

  def wrongKindOfOpticGS = {
    val getter = Getter[Apple,Banana](_ => Banana)
    val setter = Setter[Banana,Carrot](_ => _ => Banana)

    testGet"$Apple.$getter.$setter".errorOrTree === Left(WrongKindOfOptic(".$setter", "goggles.Fixture.Banana", "goggles.Fixture.Carrot", GetterType, SetterType))
  }

  def wrongKindOfOpticSG = {
    val setter = Setter[Apple,Banana](_ => _ => Apple)
    val getter = Getter[Banana,Carrot](_ => Carrot)

    testGet"$Apple.$setter.$getter".errorOrTree === Left(WrongKindOfOptic(".$getter", "goggles.Fixture.Banana", "goggles.Fixture.Carrot", SetterType, GetterType))
  }

  def wrongKindOfOpticSF = {
    val setter = Setter[Apple,Banana](_ => _ => Apple)

    val fold = new Fold[Banana, Carrot] {
      def foldMap[M: Monoid](f: Carrot => M)(b: Banana): M = Monoid[M].zero
    }

    testGet"$Apple.$setter.$fold".errorOrTree === Left(WrongKindOfOptic(".$fold", "goggles.Fixture.Banana", "goggles.Fixture.Carrot", SetterType, FoldType))
  }

  def typesDontMatch = {
    val aToB = Getter[Apple,Banana](_ => Banana)
    val cToD = Getter[Carrot,Dolmades](_ => Dolmades)

    testGet"$Apple.$aToB.$cToD".errorOrTree === Left(TypesDontMatch(".$cToD", "goggles.Fixture.Carrot", "goggles.Fixture.Dolmades", "goggles.Fixture.Banana", "goggles.Fixture.Carrot"))
  }

  def noEach = {
    testGet"$Apple*".errorOrTree === Left(ImplicitEachNotFound("*", "goggles.Fixture.Apple"))
  }

  def noPossible = {
    testGet"$Apple?".errorOrTree === Left(ImplicitPossibleNotFound("?", "goggles.Fixture.Apple"))
  }

  def noIndex = {
    testGet"$Apple[0]".errorOrTree === Left(ImplicitIndexNotFound("[0]", "goggles.Fixture.Apple", "Int"))
  }

  def wrongIndexType = {
    val i = "a"
    testGet"${List(1,2,3)}[$i]".errorOrTree === Left(ImplicitIndexNotFound("[$i]", "List[Int]", "String"))
  }

  def copyMethodNotFound =
    testSet"${new NoCopy(2)}.i".errorOrTree === Left(CopyMethodNotFound("i", "goggles.NoCopy"))

  def copyMethodMultiParamLists =
    testSet"${new HasCopyMultiParamLists(2)}.i".errorOrTree === Left(CopyMethodHasMultiParamLists("i", "goggles.HasCopyMultiParamLists"))

  def copyMethodNoArgs =
    testSet"${new HasCopyNoArgs(2)}.i".errorOrTree === Left(CopyMethodHasNoArguments("i", "goggles.HasCopyNoArgs"))

  def copyMethodLacksNamedArgument =
    testSet"${new HasCopyMissingArg(2)}.i".errorOrTree === Left(CopyMethodLacksNamedArgument("i", "goggles.HasCopyMissingArg"))

  def copyMethodLacksParameterDefaults =
    testSet"${new HasCopyNoDefaults(2)}.i".errorOrTree === Left(CopyMethodLacksParameterDefaults("i", "goggles.HasCopyNoDefaults", List("i")))

}
