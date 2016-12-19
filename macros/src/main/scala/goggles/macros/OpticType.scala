package goggles.macros

private[goggles] sealed abstract class OpticType(
  val typeName: String,
  val polymorphic: Boolean,
  val composeVerb: String,
  val getVerb: Option[String])

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

