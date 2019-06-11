package goggles

import goggles.macros.errors.{SyntaxError, UserError}
import goggles.macros.interpret.OpticType
import goggles.macros.lex.Token
import goggles.testdsl._
import monocle.{Fold, Getter, Setter}
import org.specs2._
import org.specs2.execute.Failure
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
    testGet"$myBasket.items*.qty".errorOrResult === Right(List(11,22,33))

  def empty =
    testGet"".errorOrResult === Left(SyntaxError.EmptyError)

  def wrongChar =
    testGet"^".errorOrResult === Left(SyntaxError.UnrecognisedChar('^'))

  def getSetter = {
    val setUser = Setter[ShoppingBasket, User](f => b => b.copy(user = f(b.user)))
    testGet"$myBasket.$setUser".errorOrResult === Left(UserError.GetterOpticRequired(SetterType))
  }

  def setGetter = {
    val getUser = Getter[ShoppingBasket, User](_.user)
    testSet"$myBasket.$getUser".errorOrResult === Left(UserError.SetterOpticRequired(GetterType))
  }

  def nameNoDot =
    testGet"${myBasket}items".errorOrResult === Left(SyntaxError.NameWithNoDot("items"))

  def interpNoDot =
    testGet"$myBasket$basketItems".errorOrResult === Left(SyntaxError.InterpOpticWithNoDot)

  def invalidAfterDot1 = {
    testGet"$myBasket.*".errorOrResult match {
      case Left(SyntaxError.InvalidAfterDot(token: Token.Star)) => success
      case oops => Failure(s"Expecting Left(SyntaxError.InvalidAfterDot(token: Token.Star)), found $oops")
    }
  }

  def invalidAfterDot2 = {
    testGet"$myBasket.?".errorOrResult match {
      case Left(SyntaxError.InvalidAfterDot(token: Token.Question)) => success
      case oops => Failure(s"Expecting Left(SyntaxError.InvalidAfterDot(token: Token.Question)), found $oops")
    }
  }
    

  def invalidAfterDot3 = {
    testGet"$myBasket.[0]".errorOrResult match {
      case Left(SyntaxError.InvalidAfterDot(token: Token.OpenBracket)) => success
      case oops => Failure(s"Expecting Left(SyntaxError.InvalidAfterDot(token: Token.OpenBracket)), found $oops")
    }
  }

  def nonInterpStart = {
    testGet"*".errorOrResult match {
      case Left(SyntaxError.NonInterpolatedStart(token: Token.Star)) => success
      case oops => Failure(s"Expecting Left(SyntaxError.NonInterpolatedStart(token: Token.Star)), found $oops")
    }
  }

  def unexpectedCloseBracket =
    testGet"$myBasket.]".errorOrResult === Left(SyntaxError.UnexpectedCloseBracket)

  def endingDot =
    testGet"$myBasket.items.".errorOrResult === Left(SyntaxError.EndingDot)

  def emptyIndex =
    testGet"$myBasket.items[]".errorOrResult === Left(SyntaxError.NoIndexSupplied)

  def invalidIndex = {
    testGet"$myBasket.items[*?!]".errorOrResult match {
      case Left(SyntaxError.InvalidIndexSupplied(token: Token.Star)) => success
      case oops => Failure(s"Expecting Left(SyntaxError.InvalidIndexSupplied(token: Token.Star)), found $oops")
    }
  }

  def unclosedOpenBracket =
    testGet"$myBasket.items[".errorOrResult === Left(SyntaxError.UnclosedOpenBracket)

  def nameNotFound =
    testGet"$myBasket.bogus".errorOrResult === Left(UserError.NameNotFound("bogus", "goggles.Fixture.ShoppingBasket"))

  def nameHasArgs =
    testGet"${new HasArgsMethod}.bogus".errorOrResult === Left(UserError.NameHasArguments("bogus", "goggles.HasArgsMethod"))

  def nameHasMultiParamLists =
    testGet"${new HasMultiParamMethod}.bogus".errorOrResult === Left(UserError.NameHasMultiParamLists("bogus", "goggles.HasMultiParamMethod"))

  def notAnOptic = {
    testGet"$myBasket.$Apple".errorOrResult === Left(UserError.InterpNotAnOptic("$Apple", "goggles.Fixture.Apple"))
  }

  def wrongKindOfOpticGS = {
    val getter = Getter[Apple,Banana](_ => Banana)
    val setter = Setter[Banana,Carrot](_ => _ => Banana)

    testGet"$Apple.$getter.$setter".errorOrResult === Left(UserError.WrongKindOfOptic(".$setter", "goggles.Fixture.Banana", "goggles.Fixture.Carrot", GetterType, SetterType))
  }

  def wrongKindOfOpticSG = {
    val setter = Setter[Apple,Banana](_ => _ => Apple)
    val getter = Getter[Banana,Carrot](_ => Carrot)

    testGet"$Apple.$setter.$getter".errorOrResult === Left(UserError.WrongKindOfOptic(".$getter", "goggles.Fixture.Banana", "goggles.Fixture.Carrot", SetterType, GetterType))
  }

  def wrongKindOfOpticSF = {
    val setter = Setter[Apple,Banana](_ => _ => Apple)

    val fold = new Fold[Banana, Carrot] {
      def foldMap[M: Monoid](f: Carrot => M)(b: Banana): M = Monoid[M].zero
    }

    testGet"$Apple.$setter.$fold".errorOrResult === Left(UserError.WrongKindOfOptic(".$fold", "goggles.Fixture.Banana", "goggles.Fixture.Carrot", SetterType, FoldType))
  }

  def typesDontMatch = {
    val aToB = Getter[Apple,Banana](_ => Banana)
    val cToD = Getter[Carrot,Dolmades](_ => Dolmades)

    testGet"$Apple.$aToB.$cToD".errorOrResult === Left(UserError.TypesDontMatch(".$cToD", "goggles.Fixture.Carrot", "goggles.Fixture.Dolmades", "goggles.Fixture.Banana", "goggles.Fixture.Carrot"))
  }

  def noEach = {
    testGet"$Apple*".errorOrResult === Left(UserError.ImplicitEachNotFound("*", "goggles.Fixture.Apple"))
  }

  def noPossible = {
    testGet"$Apple?".errorOrResult === Left(UserError.ImplicitPossibleNotFound("?", "goggles.Fixture.Apple"))
  }

  def noIndex = {
    testGet"$Apple[0]".errorOrResult === Left(UserError.ImplicitIndexNotFound("[0]", "goggles.Fixture.Apple", "Int"))
  }

  def wrongIndexType = {
    val i = "a"
    testGet"${List(1,2,3)}[$i]".errorOrResult === Left(UserError.ImplicitIndexNotFound("[$i]", "List[Int]", "String"))
  }

  def copyMethodNotFound =
    testSet"${new NoCopy(2)}.i".errorOrResult === Left(UserError.CopyMethodNotFound("i", "goggles.NoCopy"))

  def copyMethodMultiParamLists =
    testSet"${new HasCopyMultiParamLists(2)}.i".errorOrResult === Left(UserError.CopyMethodHasMultiParamLists("i", "goggles.HasCopyMultiParamLists"))

  def copyMethodNoArgs =
    testSet"${new HasCopyNoArgs(2)}.i".errorOrResult === Left(UserError.CopyMethodHasNoArguments("i", "goggles.HasCopyNoArgs"))

  def copyMethodLacksNamedArgument =
    testSet"${new HasCopyMissingArg(2)}.i".errorOrResult === Left(UserError.CopyMethodLacksNamedArgument("i", "goggles.HasCopyMissingArg"))

  def copyMethodLacksParameterDefaults =
    testSet"${new HasCopyNoDefaults(2)}.i".errorOrResult === Left(UserError.CopyMethodLacksParameterDefaults("i", "goggles.HasCopyNoDefaults", List("i")))

}
