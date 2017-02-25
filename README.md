# Goggles
[![Build Status](https://secure.travis-ci.org/kenbot/goggles.png?branch=master)](http://travis-ci.org/kenbot/goggles)

## Principled, typesafe optics DSL
Optics libraries are either too limited, or too hard to use.
Goggles builds on Scala's powerful Monocle library, making
immutability easy, fun, and boring, like it should be.

You already know how to use it. 

```scala

import goggles._ 

case class Topping(cherries: Int)
case class Cake(toppings: List[Topping])
case class Bakery(cakes: List[Cake])

val myBakery = Bakery(List(Cake(List(Topping(0), Topping(3))), 
                           Cake(List(Topping(4))), 
                           Cake(Nil)))

get"$myBakery.cakes*.toppings[0].cherries"
// List(0, 4)

set"$myBakery.cakes*.toppings[0].cherries" := 7
// Bakery(List(Cake(List(Topping(7), Topping(3))), 
//             Cake(List(Topping(7))), 
//             Cake(Nil)))

```
The DSL runs in the compiler, and is completely typesafe.
It generates plain Monocle code.

## Motivation
### 1. Functional programming needs optics 
In imperative programming, a game world might be updated like this:
```
game.currentLevel.player.health += 20
```
Even just holding a reference to the player, we can be confident
that the change will be seen by everyone observing, without
knowing anything about the external environment. However,
mutability conveys a severe penalty in complexity of
behaviour, and our human ability to reason about the code.

On the other hand, naively using immutable structures leads
to unfortunate problems.
```
  game.copy(currentLevel = 
    game.currentLevel.copy(player = 
      game.currentLevel.player.copy(health =
        game.currentLevel.player.health + 20
      )
    )
  )
```
Ugly, yes, but it gets worse: recreating the object graph is
a dire failure of modularity. We must now know exactly where
the object sits in the world-structure, and how to recreate
every detail up to the root. Modularity is supposed to be
a flagship benefit of FP - how embarrassing!

_Optics_ are a family of pure-functional techniques that
model access and mutation with composable abstractions.
They are the best answer that has emerged to this
dilemma; without them FP is dismally unsuited to a
range of mundane problems.


### 2. Power vs ease-of-use. Why choose?
The modifying-immutable-structures problem has been addressed
in a variety of ways.

Dynamic environments such as Clojure, jq, and Javascript have
features that allow easy manipulation of structures without
mutation, but in a very constrained, domain-specific context.

Haskell's `Control.Lens` is wonderfully powerful and abstract,
but has a reputation for being difficult to learn and use.
Why can't we have our cake and eat it too?

### 3. Monocle + Goggles
Monocle is the leading optics library in Scala; it has a small,
well-designed core, but its day-to-day user experience leaves
a little to be desired. It has much of the power of
`Control.Lens`, and also has much of the conceptual weight
and learning curve.

This makes it an ideal core for building an optics DSL.
Goggles aims to provide an intuitive, discoverable interface
over Monocle for beginners, while helping experienced users
get the job done with a minimum of fuss.

## Features
### Navigate case class-like fields by name
```scala
import goggles._ 

case class City(name: String, population: Int)
case class State(name: String, capital: City)
val state = State("Victoria", City("Melbourne", 4087000))

get"$state.capital.population"
// 4087000

set"$state.capital.population" += 1
// State("Victoria", City("Melbourne", 4087001))
```
The `+=` is syntax sugar; it requires an implicit `scala.Numeric`
 in scope for the result type.

### Interpolate any Monocle optic
```scala

import goggles._ 
import monocle.std.string.stringToInt
import monocle.std.int.intToChar

get"${"113"}.$stringToInt.$intToChar"
// Some('q')

set"${"113"}.$stringToInt.$intToChar" ~= (_.toUpper)
// "81" 
```

### Compose Monocle optics
```scala
import goggles._
import monocle.std.string.stringToInt
import monocle.std.int.intToChar

val myLens = lens"$stringToInt.$intToChar"
// monocle.PPrism[String,String,Char,Char] = monocle.PPrism$$anon$1@2b6b0069

myLens.getOption("113")
// Some('q')
```

### Traverse over collections
```scala
import goggles._

case class Point(x: Double, y: Double)
val polygon = List(Point(0.0, 0.0), Point(0.0, 1.0), Point(1.0, 1.0), Point(1.0, 0.0))
get"$polygon*.x"
// List(0.0, 0.0, 1.0, 1.0)

set"$polygon*.x" += 1.5
List(Point(1.5, 0.0), Point(1.5, 1.0), Point(2.5, 1.0), Point(2.5, 0.0))

```
Any type for which an implicit `monocle.function.Each` is
in scope can use `*`


### Select optional values
```scala
import goggles._

case class Estate(farm: Option[Farm])
case class Farm(prizeChicken: Option[Chicken])
case class Chicken(egg: Option[Egg])
case class Egg(weight: Double)
val estate = Estate(Some(Farm(Some(Chicken(Some(Egg(2.3)))))))

get"$estate.farm?.prizeChicken?.egg?.weight"
// Some(2.3)

set"$estate.farm?.prizeChicken?.egg?.weight" *= 2
// Estate(Some(Farm(Some(Chicken(Some(Egg(4.6)))))))

```
Any type for which an implicit `monocle.function.Possible` is
in scope can use `?`

### Select indexed values
```scala
import goggles._

sealed trait Square
case object - extends Square
case object X extends Square
case object O extends Square


val ticTac: Vector[Vector[Square]] = 
  Vector(
    Vector(X, -, -),
    Vector(O, X, -),
    Vector(-, O, O))

val i = 0
get"$ticTac[$i][0]"
// Some(X) 

set"$ticTac[2][0]" := O
//  Vector(
//    Vector(X, -, -),
//    Vector(O, X, -),
//    Vector(O, O, O))
```
Any type for which an implicit `monocle.function.Index` is
in scope can use `[i]` with an index.

### Great compilation error messages
Helpful compiler errors are a first class part of Goggles'
design, hopefully encouraging exploration, clarifying optics
concepts and allowing the functionality to be discoverable.
```
scala> get"$myBasket.items*.qty.foo"
<console>:18: error: Int doesn't have a 'foo' method

 Sections  │ Types                         │ Optics
───────────┼───────────────────────────────┼───────────────────────────
 $myBasket │ ShoppingBasket                │
 .items    │ ShoppingBasket  ⇒  List[Item] │ Lens
 *         │ List[Item]      ⇒  Item       │ Traversal
 .qty      │ Item            ⇒  Int        │ Lens, returning Traversal
 foo       │ Int             ⇒  ???        │

       get"$myBasket.items*.qty.foo"
       ^
```


## Comparison to other approaches
### Goggles itself
```scala
set"$myBakery.cakes*.toppings[0].cherries" := 7
```

Goggles is not an optics library itself; it is only a new user
interface built on a subset of Monocle, and interoperates
seamlessly with the rest. It uses whitebox macros, meaning
that the contents of the macro decide the static return type.

Goggles takes the view that macros that base their behaviour
on the structure of code rather than its value are not
referentially-transparent, and not consistent with the
best traditions of FP. Repurposing Scala's syntax to do
things that aren't Scala is surprising to users and
imposes an unnecessary cognitive burden.

Extensions to `StringContext` form the main mechanism, because:
* It isn't Scala, and the String clearly demarcates regular
Scala from the designated DSL area.
* This gives us enormous flexibility to choose the syntax we want.

There are some disadvantages:
* There is no IDE support out of the box: it just looks like
a string to IDEs. (Could this be remedied with plugins?)
* Because interpolated optics get evaluated before the rest
of the macros, the type inference is poor for arguments.

### [QuickLens](https://github.com/adamw/quicklens)
```scala
modify(myBakery)(_.cakes.each.toppings.at(0).cherries).setTo(7)
```

QuickLens is designed to be a lightweight alternative to
Monocle; it is solely focused on manipulating case classes.
It uses a fluent API with blackbox macros, which deconstruct
the given code tree to discover path information. It supports
several features like "each" traversal and indexing, but lacks
an overarching, cohesive optics model outside of the DSL.
In addition, the fluent API supports manipulating several
points in the path at once, and Prism-style navigation of
sum types.

### [Monocle's internal DSL](https://github.com/julien-truffaut/Monocle/blob/master/macro/shared/src/main/scala/monocle/macros/syntax/GenApplyLensSyntax.scala)
```
myBakery.lens(_.cakes)
```
Monocle itself contains some internal syntactic helpers,
including a simple DSL for convenient case class manipulation.
Currently it uses a blackbox macro to deconstruct a code
tree, which generates a `monocle.Lens`.


### [Shapeless Lenses](https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/lenses.scala)
```
lens[Bakery].cakes.get(myBakery)
```
Shapeless offers Lens and Prisms, which allow automatic
navigation of case classes and sum types. It uses Dynamic
to allow Scala-like syntax, and uses a thicket of typeclasses
to prove validity. It supports indexing and products, but
not traversals. As with many of Shapeless' concepts,
understanding the mechanism used requires a high level
of proficiency, and the compile errors are quite unhelpful.
