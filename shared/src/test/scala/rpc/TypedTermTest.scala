package rpc

class TypedTermTest extends org.scalatest.FunSuite {

  val TT = TypedTerm

  val v = TT.Var("test")
  val c = TT.Const(1)
  val lam = TT.Lam(Location.Server,
                   "lammy",
                   Tpe.Fun(Tpe.Int, TypedLocation.server, Tpe.Var(0)),
                   v)
  val app = TT.App(TypedLocation.client, lam, c)

  test("traverse lists all terms in a term tree as a stream") {
    assert(TT.traverse(app).toSet === Set(lam, app, v, c))
  }

  test("substitute can sub out app param") {
    assert(TT.substitute(app, c, TT.Const(5)) === app.copy(param = TT.Const(5)))
  }

  test("substitute can sub out deep terms") {
    assert(
      TT.substitute(app, v, TT.Const(5)) ===
        app.copy(fun = lam.copy(body = TT.Const(5))))
  }

  test("substitute can sub out multiple terms") {
    val var0          = TT.Var("0")
    val multipleTerms = TT.App(TypedLocation.client, var0, var0)
    val c2            = TT.Const(2)

    assert(
      TT.substitute(multipleTerms, var0, c2) === TT
        .App(TypedLocation.client, c2, c2))
  }

}
