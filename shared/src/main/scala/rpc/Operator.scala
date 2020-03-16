package rpc

import cats.syntax.functor._
import io.circe.{Decoder, DecodingFailure, HCursor}

sealed trait Operator
object Operator {
  private def opD(op: Operator): Decoder[Operator] =
    (c: HCursor) => {
      val opName = op.getClass.getSimpleName.dropRight(1)
      if (c.downField(opName).succeeded)
        Left(DecodingFailure(s"Cannot decode $op", c.history))
      else Right(op)
    }

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

  implicit val opDImpl: Decoder[Operator] =
    List(Not, Or, And, Eq, Neq, Lt, Le, Gt, Ge, Add, Sub, Mul, Div, Neg)
      .map(opD)
      .reduceLeft(_ or _)
}
