import goggles.macros._

package object goggles {

  implicit class GogglesDsl(sc: StringContext) {
    def get(args: Any*): Any = macro MonocleMacros.getImpl
    //def set(args: Any*): ModifyOps[_, _, _, _] = macro MonocleMacros.setImpl
    def lens(args: Any*): Any = macro MonocleMacros.lensImpl
  }

}
