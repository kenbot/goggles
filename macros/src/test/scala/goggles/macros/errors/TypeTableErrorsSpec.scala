package goggles.macros.errors

import goggles.macros.interpret.{OpticInfo, OpticType, DslMode}
import org.specs2._

class TypeTableErrorsSpec extends Specification with ScalaCheck { def is =
  s2"""
       Error table
         Should be empty if < 2 optic infos $expectingEmpty
    """

  val u = scala.reflect.runtime.universe

  def expectingEmpty = {
    val mode = DslMode.Get
    val opticInfo = OpticInfo("bob", u.typeOf[Int], u.typeOf[Int], OpticType.TraversalType, OpticType.TraversalType)

    val table = TypeTableErrors.table(mode,
      GetterOpticRequired(OpticType.SetterType),
      List(opticInfo))

    table === Table.empty
  }

}
