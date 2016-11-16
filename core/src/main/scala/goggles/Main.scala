package goggles

import goggles.lens._

import monocle._
import monocle.macros._


@Lenses case class One(two: Two)
@Lenses case class Two(optThree: Option[Three])
@Lenses case class Three(list: List[One])

object Main {
  import One._
  import Two._
  import Three._


  def main(args: Array[String]): Unit = {


    val obj = One(Two(Some(Three(Nil))))


    println(get"obj.two.optThree")


  }
}
