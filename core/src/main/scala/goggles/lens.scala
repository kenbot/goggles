package goggles

package object lens {
  implicit class MonocleDsl(sc: StringContext) {
    import goggles.macros._
    import scala.language.experimental.macros

    def get(): Any = macro Macros.getImpl
    def set(): ModifyOps[_, _, _, _] = macro Macros.setImpl
    def lens(args: Any*): Any = macro Macros.lensImpl
  }
}
