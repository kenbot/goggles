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

  def article: String = this match {
    case FoldType | Fold1Type | GetterType | SetterType | TraversalType | PrismType | LensType => "a"
    case OptionalType | IsoType => "an"
  }

  /*
   * Required so that Fold1 can properly return an Option instead of a List.
   */
  def getVerbPostfix: Option[String] = this match {
    case Fold1Type => Some("headOption")
    case _ => None
  }

  /* The result type of row.composeXXX(column) for Monocle optics.
   *
   * We introduce the concept of "Fold1", where we know that
   * a Fold views no more than 1 element. It can also be thought of as
   * a "read-only Optional". This allows us to statically return
   * optional results rather than lists in some cases, which means
   * that Getters can be swapped in for read-only Lenses without
   * altering behaviour.
   *
   *    F1GSTOPLI
   *   +---------
   *  F|fffffffff
   *  1|f11-f1111
   *  G|f1g-f11gg
   *  S|---ssssss
   *  T|fffsttttt
   *  O|f11stoooo
   *  P|f11stopop
   *  L|f1gstooll
   *  I|f1gstopli
   */
  def compose(o: OpticType): Option[OpticType] = (this, o) match {
    case (FoldType, _) => Some(FoldType)
    case (Fold1Type, SetterType) => None
    case (Fold1Type, FoldType | TraversalType) => Some(FoldType)
    case (Fold1Type, _) => Some(Fold1Type)
    case (GetterType, SetterType) => None
    case (GetterType, Fold1Type | OptionalType | PrismType) => Some(Fold1Type)
    case (GetterType, LensType | IsoType | GetterType) => Some(GetterType)
    case (GetterType, _) => Some(FoldType)
    case (SetterType, FoldType | GetterType) => None
    case (SetterType, _) => Some(SetterType)
    case (TraversalType, FoldType | GetterType) => Some(FoldType)
    case (TraversalType, SetterType) => Some(SetterType)
    case (TraversalType, _) => Some(TraversalType)
    case (OptionalType, FoldType) => Some(FoldType)
    case (OptionalType, Fold1Type | GetterType) => Some(Fold1Type)
    case (OptionalType, SetterType) => Some(SetterType)
    case (OptionalType, TraversalType) => Some(TraversalType)
    case (OptionalType, _) => Some(OptionalType)
    case (PrismType, FoldType) => Some(FoldType)
    case (PrismType, Fold1Type | GetterType) => Some(Fold1Type)
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
  val all = List(FoldType, Fold1Type, GetterType, SetterType,
    TraversalType, OptionalType, PrismType,
    LensType, IsoType)

  case object FoldType extends OpticType("Fold", false, "composeFold", Some("getAll"))
  case object Fold1Type extends OpticType("Fold", false, "composeFold", Some("getAll"))
  case object GetterType extends OpticType("Getter", false, "composeGetter", Some("get"))
  case object SetterType extends OpticType("PSetter", true, "composeSetter", None)
  case object TraversalType extends OpticType("PTraversal", true, "composeTraversal", Some("getAll"))
  case object OptionalType extends OpticType("POptional", true, "composeOptional", Some("getOption"))
  case object PrismType extends OpticType("PPrism", true, "composePrism", Some("getOption"))
  case object LensType extends OpticType("PLens", true, "composeLens", Some("get"))
  case object IsoType extends OpticType("PIso", true, "composeIso", Some("get"))
}

