package goggles.macros.interpret.infrastructure

import goggles.macros.interpret._
import goggles.macros.errors._

trait InterpreterActions {
    this: Contextual => 
  
    def typeCheckOrElse(tree: c.Tree, orElse: => GogglesError[c.Type]): Interpret[c.Tree] = {
      val typed = c.typecheck(tree, silent = true)
      if (typed.isEmpty) Parse.raiseError(orElse)
      else Parse.pure(typed)
    }
  
    def getArgLabel(tree: c.Tree): String = {
      val pos = tree.pos
      val src = new String(pos.source.content)
      val label = src.substring(pos.start, pos.end)
      if (label.forall(_.isUnicodeIdentifierPart)) s"$$$label"
      else s"$${$label}"
    }
  
    def getLastOpticInfo(name: String): Interpret[OpticInfo[c.Type]] = {
      Parse.getLastOpticInfo[c.Type, c.Expr[Any]].flatMap {
        case Some(info) => Parse.pure(info)
        case None => Parse.raiseError(OpticInfoNotFound(name))
      }
    }
    
    def storeOpticInfo(label: String, sourceType: c.Type, targetType: c.Type, opticType: OpticType): Interpret[Unit] = {
      for {
        lastInfo <- Parse.getLastOpticInfo[c.Type, c.Expr[Any]]
        nextOpticType = lastInfo match {
          case Some(info) => info.compositeOpticType.compose(opticType)
          case None => Some(opticType)
        }
        fromOptic = lastInfo.fold(opticType)(_.compositeOpticType)
        composed <- Parse.fromOption(nextOpticType,
                                     WrongKindOfOptic(label, sourceType, targetType, fromOptic, opticType))
        _ <- Parse.storeOpticInfo(OpticInfo(label, sourceType.resultType, targetType.resultType, opticType, composed))
      } yield ()
    }

    def patternMatchOrElse[A, R](a: A, orElse: => GogglesError[c.Type])(pf: PartialFunction[A,R]): Interpret[R] =
      if (pf.isDefinedAt(a)) Parse.pure(pf(a))
      else Parse.raiseError(orElse)

    def getLastTargetType(name: String): Interpret[c.Type] =
      getLastOpticInfo(name).map(_.targetType)
  }