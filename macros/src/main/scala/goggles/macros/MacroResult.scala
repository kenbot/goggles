package goggles.macros

case class MacroResult[Type, Tree](
  errorOrTree: Either[GogglesError[Type], Tree],
  infos: List[OpticInfo[Type]])

object MacroResult {
  def tupled[Type,Tree](tuple: (Either[GogglesError[Type], Tree],
                                List[OpticInfo[Type]])) = {
    MacroResult(tuple._1, tuple._2)
  }
}

