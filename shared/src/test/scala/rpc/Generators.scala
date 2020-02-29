package rpc

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck._

object Generators {
  val TT = TypedTerm
  val TL = TypedLocation
  val L  = Location

  val genLocation    = oneOf(const(L.Client), const(L.Server))
  val genVarLocation = arbitrary[Int].map(TL.Var)
  val genTypedLocation: Gen[TypedLocation] =
    oneOf(const(TL.client), const(TL.server), genVarLocation)

  val genTpeVar = arbitrary[Int].map(Tpe.Var)
  val genFun = for {
    a   <- genTpe
    loc <- genTypedLocation
    b   <- genTpe
  } yield Tpe.Fun(a, loc, b)

  def genTpe: Gen[Tpe] = oneOf(const(Tpe.Int), genTpeVar, lzy(genFun))

  val genConst = arbitrary[Int].map(TT.Const)
  val genVar   = arbitrary[String].map(TT.Var)
  val genLam = for {
    loc  <- genLocation
    name <- arbitrary[String]
    tpe  <- genTpe
    body <- genTypedTerm
  } yield TT.Lam(loc, name, tpe, body)

  val genApp = for {
    f     <- genLam
    tl    <- genTypedLocation
    param <- genTypedTerm
  } yield TT.App(tl, f, param)

  def genTypedTerm: Gen[TypedTerm] =
    oneOf(genConst, genVar, lzy(genLam), lzy(genApp)) // FIXME no closures yet
}
