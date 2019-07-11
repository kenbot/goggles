package goggles

import goggles.macros.interpret.MacroResult
import goggles.macros.{ModifyOps, RuntimeMacros}

package object testdsl {

  implicit class TestDsl(sc: StringContext) {
    def testGet(args: Any*): MacroResult[String, Any] = macro RuntimeMacros.getImpl

    def testSet(args: Any*): MacroResult[String, ModifyOps[_, _, _, _]] = macro RuntimeMacros.setImpl

    def testLens(args: Any*): MacroResult[String, Any] = macro RuntimeMacros.lensImpl
  }
}
