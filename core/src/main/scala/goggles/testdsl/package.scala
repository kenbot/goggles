package goggles

import goggles.macros.{GogglesError, ModifyOps, TestMacros}

package object testdsl {

  implicit class TestDsl(sc: StringContext) {
    def testGet(args: Any*): Either[GogglesError[String], Any] = macro TestMacros.getImpl

    def testSet(args: Any*): Either[GogglesError[String], ModifyOps[_, _, _, _]] = macro TestMacros.setImpl

    def testLens(args: Any*): Either[GogglesError[String], Any] = macro TestMacros.lensImpl
  }
}
