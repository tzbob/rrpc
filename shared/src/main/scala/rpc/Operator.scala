package rpc

import rpc.error.TypeError

sealed trait Operator
object Operator {
  private def binary(
      f: (Literal, Literal) => Literal): List[Literal] => Literal = {
    case List(l1, l2) => f(l1, l2)
  }

  private def binaryB(
      f: (Boolean, Boolean) => Boolean): List[Literal] => Literal = binary {
    case (Literal.Bool(b1), Literal.Bool(b2)) => Literal.Bool(f(b1, b2))
    case e                                    => throw TypeError(e, "(Bool, Bool)")
  }

  private def binaryIB(f: (Int, Int) => Boolean): List[Literal] => Literal =
    binary {
      case (Literal.Int(i1), Literal.Int(i2)) => Literal.Bool(f(i1, i2))
      case e                                  => throw TypeError(e, "(Int, Int)")
    }

  private def binaryI(f: (Int, Int) => Int): List[Literal] => Literal = binary {
    case (Literal.Int(i1), Literal.Int(i2)) => Literal.Int(f(i1, i2))
    case e                                  => throw TypeError(e, "(Int, Int)")
  }

  val operators = Map[Operator, List[Literal] => Literal](
    Not -> { case List(Literal.Bool(b)) => Literal.Bool(!b) },
    Or  -> binaryB(_ || _),
    And -> binaryB(_ && _),
    Eq  -> binary((x, y) => Literal.Bool(x == y)),
    Neq -> binary((x, y) => Literal.Bool(x != y)),
    Lt  -> binaryIB(_ < _),
    Le  -> binaryIB(_ <= _),
    Gt  -> binaryIB(_ > _),
    Gt  -> binaryIB(_ > _),
    Ge  -> binaryIB(_ >= _),
    Add -> binaryI(_ + _),
    Sub -> binaryI(_ - _),
    Mul -> binaryI(_ * _),
    Div -> binaryI(_ / _),
    Neg -> { case List(Literal.Int(i)) => Literal.Int(-i) }
  )

  case object Not extends Operator //{l}. Bool -l-> Bool
  case object Or  extends Operator //{l}. (Bool, Bool) -l-> Bool
  case object And extends Operator //{l}. (Bool, Bool) -l-> Bool
  case object Eq  extends Operator //{l}. (Bool, Bool) -l-> Bool
  case object Neq extends Operator //{l}. (Bool, Bool) -l-> Bool
  case object Lt  extends Operator //{l}. (Int, Int) -l-> Bool
  case object Le  extends Operator //{l}. (Int, Int) -l-> Bool
  case object Gt  extends Operator //{l}. (Int, Int) -l-> Bool
  case object Ge  extends Operator //{l}. (Int, Int) -l-> Bool
  case object Add extends Operator //{l}. (Int, Int) -l-> Int
  case object Sub extends Operator //{l}. (Int, Int) -l-> Int
  case object Mul extends Operator //{l}. (Int, Int) -l-> Int
  case object Div extends Operator //{l}. (Int, Int) -l-> Int
  case object Neg extends Operator //{l}. Int -l-> Int
}
