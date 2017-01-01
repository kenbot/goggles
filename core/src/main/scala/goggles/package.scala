import goggles.macros._

package object goggles {

  implicit class GogglesDsl(sc: StringContext) {
    def get(args: Any*): Any = macro GogglesMacros.getImpl
    def set(args: Any*): ModifyOps[_, _, _, _] = macro GogglesMacros.setImpl
    def lens(args: Any*): Any = macro GogglesMacros.lensImpl
  }

}
