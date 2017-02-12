package goggles.macros

case class OpticInfo[+T](
  label: String,
  sourceType: T,
  targetType: T,
  opticType: OpticType,
  compositeOpticType: OpticType) {

  def map[U](f: T => U): OpticInfo[U] = {
    copy(sourceType = f(sourceType),
      targetType = f(targetType))
  }
}
