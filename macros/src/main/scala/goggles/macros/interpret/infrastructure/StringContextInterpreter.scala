package goggles.macros.interpret.infrastructure

trait StringContextInterpreter {
  self: Contextual =>
  
  def contextStringParts: List[String] = {
    import c.universe._
    
    contextStringTrees.map {
      case Literal(Constant(str: String)) => str
    }
  }

  def contextStringPartsWithOffsets: List[(String, Int)] = {
    import c.universe._
    
    contextStringTrees.map {
      case tree @ Literal(Constant(str: String)) => (str, tree.pos.start)
    }
  }

  def contextStringTrees: List[c.Tree] = {
    import c.universe._
  
    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => rawParts
    }
  }
}