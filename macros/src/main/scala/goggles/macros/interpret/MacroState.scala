package goggles.macros.interpret

import goggles.macros.parse.LensExpr

private[goggles] case class MacroState[+Type,+Arg](
  args: List[Arg], 
  currentLensExpr: Option[LensExpr],
  remainingLensExprs: List[LensExpr],
  reversedInfos: List[OpticInfo[Type]], 
  offset: Int) {

  def infos: List[OpticInfo[Type]] = 
    reversedInfos.reverse

  def addOpticInfo[T >: Type](info: OpticInfo[T]): MacroState[T, Arg] = 
    copy(reversedInfos = info :: reversedInfos)

  def lastOpticInfo: Option[OpticInfo[Type]] = 
    reversedInfos.headOption

  def popArg: Option[(Arg, MacroState[Type, Arg])] = args match {
    case head :: tail => Some((head, copy(args = tail)))
    case Nil => None
  }

  def popLensExpr: Option[(LensExpr, MacroState[Type, Arg])] = remainingLensExprs match {
    case head :: tail => Some((head, copy(currentLensExpr = Some(head), remainingLensExprs = tail)))
    case Nil => None
  }

  def addToOffset(n: Int): MacroState[Type, Arg] = 
    copy(offset = offset + n)
}