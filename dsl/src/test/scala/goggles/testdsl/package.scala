package goggles

import goggles.macros.interpret.MacroResult
import goggles.macros.{ModifyOps, TestMacros}

package object testdsl {

  implicit class TestDsl(sc: StringContext) {
    def testGet(args: Any*): MacroResult[String, Any] = macro TestMacros.getImpl

    def testSet(args: Any*): MacroResult[String, ModifyOps[_, _, _, _]] = macro TestMacros.setImpl

    def testLens(args: Any*): MacroResult[String, Any] = macro TestMacros.lensImpl
  }
}
