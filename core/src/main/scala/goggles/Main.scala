package goggles

import goggles.lens._

import monocle._
import monocle.macros._


@Lenses case class Banana(n: Int, os: Option[String])
@Lenses case class Bunch(bs: List[Banana])
@Lenses case class FruitBowl(b: Banana)

object Main {


  def main(args: Array[String]): Unit = {

    val banana1 = new Banana(11, Some("hello"))
    val banana2 = new Banana(22, Some("ni hao"))
    val banana3 = new Banana(33, Some("bonjour"))
    val bunch = new Bunch(List(banana1, banana2, banana3))
    val bowl = FruitBowl(banana2)


    val bananaN = Banana.n
    val fruitBowlB = FruitBowl.b
    println(lens".fruitBowlB.${Banana.n}".get(bowl))

    //println(lens"two.optThree".get(obj))


  }
}
