package goggles.macros.interpret

private[goggles] case class OpticInfo[+Type](
  label: String,
  sourceType: Type,
  targetType: Type,
  opticType: OpticType,
  compositeOpticType: OpticType) {

  def map[U](f: Type => U): OpticInfo[U] = {
    copy(sourceType = f(sourceType),
      targetType = f(targetType))
  }
}
