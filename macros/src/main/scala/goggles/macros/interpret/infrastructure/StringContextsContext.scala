package goggles.macros.interpret.infrastructure
import goggles.macros.errors.{InternalError, ErrorAt}
import goggles.macros.lex.Fragment

trait StringContextsContext {
  self: Contextual =>

  def contextStringFragments(args: List[c.Expr[Any]]): Either[ErrorAt[c.Type], List[Fragment]] = {
    def loop(remainingArgOffsets: List[Int], remainingFragments: List[Fragment]): Either[ErrorAt[c.Type], List[Fragment]] = {
      (remainingArgOffsets, remainingFragments) match {
        case (arg :: args, frag :: frags) => loop(args, frags).map(recurse => frag :: Fragment.Argument(arg) :: recurse)
        case (Nil, frags) => Right(frags)
        case _ => Left(InternalError.NotEnoughArguments.at(0))
      }
    }

    val argOffsets = args.map(arg => getRelativeOffset(arg.tree.pos.start))
    loop(argOffsets, verbatimContextStringFragments)
  }

  private def verbatimContextStringFragments: List[Fragment] = {
    import c.universe._

    verbatimContextStringTrees.map {
      case tree @ Literal(Constant(str: String)) => Fragment.Verbatim(str, getRelativeOffset(tree.pos.start))
    }
  }

  private def verbatimContextStringTrees: List[c.Tree] = {
    import c.universe._
  
    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => rawParts
    }
    
  }

  private def macroStartOffset: Int = verbatimContextStringTrees.headOption match {
    case Some(firstTree) => firstTree.pos.start
    case None => ???
  }

  def getRelativeOffset(absoluteOffset: Int): Int = 
    absoluteOffset - macroStartOffset

  def getAbsoluteOffset(relativeOffset: Int): Int = 
    relativeOffset + macroStartOffset
}