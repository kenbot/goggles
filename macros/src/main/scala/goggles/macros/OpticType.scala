package goggles.macros

private[goggles] sealed abstract class OpticType(
  val typeName: String,
  val polymorphic: Boolean,
  val composeVerb: String,
  val getVerb: Option[String]) {

  import OpticType._

  def monoTypeName: String =
    if (polymorphic) typeName.tail
    else typeName

  /* The result type of row.composeXXX(column) for Monocle optics.
   *
   *    FGSTOPLI
   *   +--------
   *  F|ffffffff
   *  G|fg-fffgg
   *  S|--ssssss
   *  T|ffsttttt
   *  O|ffstoooo
   *  P|ffstopop
   *  L|fgstooll
   *  I|fgstopli
   */
  def compose(o: OpticType): Option[OpticType] = (this, o) match {
    case (FoldType, _) => Some(FoldType)
    case (GetterType, SetterType) => None
    case (GetterType, LensType | IsoType | GetterType) => Some(GetterType)
    case (GetterType, _) => Some(FoldType)
    case (SetterType, FoldType | GetterType) => None
    case (SetterType, _) => Some(SetterType)
    case (TraversalType, FoldType | GetterType) => Some(FoldType)
    case (TraversalType, SetterType) => Some(SetterType)
    case (TraversalType, _) => Some(TraversalType)
    case (OptionalType, FoldType | GetterType) => Some(FoldType)
    case (OptionalType, SetterType) => Some(SetterType)
    case (OptionalType, TraversalType) => Some(TraversalType)
    case (OptionalType, _) => Some(OptionalType)
    case (PrismType, FoldType | GetterType) => Some(FoldType)
    case (PrismType, SetterType) => Some(SetterType)
    case (PrismType, TraversalType) => Some(TraversalType)
    case (PrismType, OptionalType | LensType) => Some(OptionalType)
    case (PrismType, _) => Some(PrismType)
    case (LensType, PrismType) => Some(OptionalType)
    case (LensType, IsoType) => Some(LensType)
    case (LensType, other) => Some(other)
    case (IsoType, other) => Some(other)
  }
}

private[goggles] object OpticType {
  val all = List(FoldType, GetterType, SetterType,
    TraversalType, OptionalType, PrismType,
    LensType, IsoType)

  case object FoldType extends OpticType("Fold", false, "composeFold", Some("getAll"))
  case object GetterType extends OpticType("Getter", false, "composeGetter", Some("get"))
  case object SetterType extends OpticType("PSetter", true, "composeSetter", None)
  case object TraversalType extends OpticType("PTraversal", true, "composeTraversal", Some("getAll"))
  case object OptionalType extends OpticType("POptional", true, "composeOptional", Some("getOption"))
  case object PrismType extends OpticType("PPrism", true, "composePrism", Some("getOption"))
  case object LensType extends OpticType("PLens", true, "composeLens", Some("get"))
  case object IsoType extends OpticType("PIso", true, "composeIso", Some("get"))
}

