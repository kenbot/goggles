package goggles.macros.interpret.infrastructure

trait StringContextInterpreter {
  self: Contextual =>
  
  def contextStringParts: List[String] = {
    import c.universe._
  
    c.prefix.tree match {
      case Apply(f, List(Apply(g, rawParts))) => rawParts.map {
        case Literal(Constant(str: String)) => str
      }
    }
  }
}