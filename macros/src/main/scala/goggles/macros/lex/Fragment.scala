package goggles.macros.lex

sealed trait Fragment {
  def offset: Int 
}

object Fragment {
  case class Verbatim(text: String, offset: Int) extends Fragment
  case class Argument(offset: Int) extends Fragment
}