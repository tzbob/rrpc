package rpc

// p.20 "A Theory of RPC calculi for client-server model"
object Compiler {
  private val ST = StateTerm

  private def makeVar(id: Int, variable: String) = {
    val yid = s"$variable$id"
    yid -> ST.Var(yid)
  }

  private def helper(anfId: Int,
                     typedTerm: TypedTerm,
                     tierSpecifics: (Int,
                                     TypedLocation,
                                     ST.Var,
                                     ST.Var) => StateTerm): (Int, StateTerm) = {
    typedTerm match {
      case TypedTerm.Const(i)  => anfId -> ST.Const(i)
      case TypedTerm.Var(name) => anfId -> ST.Var(name)
      // switch compilation context to client
      case TypedTerm.Lam(TypedLocation.client, name, _, body) =>
        val (postCompId, compBody) = helper(anfId, body, clientSpecifics)
        postCompId -> ST.Lam(TypedLocation.client, List(name), compBody)
      // switch compilation context to server
      case TypedTerm.Lam(TypedLocation.server, name, _, body) =>
        val (postCompId, compBody) = helper(anfId, body, serverSpecifics)
        postCompId -> ST.Lam(TypedLocation.client, List(name), compBody)
      case TypedTerm.App(loc, fun, param) =>
        val (postFunId, compFun) = helper(anfId + 1, fun, tierSpecifics)
        val (postParamId, compParam) =
          helper(postFunId + 1, param, tierSpecifics)

        val (fid, fVar) = makeVar(anfId, "f")
        val (xid, xVar) = makeVar(anfId, "x")

        val body = tierSpecifics(anfId, loc, fVar, xVar)
        postParamId -> ST.Let(fid, compFun, ST.Let(xid, compParam, body))
    }
  }

  private def clientSpecifics(id: Int,
                              loc: TypedLocation,
                              fVar: StateTerm.Var,
                              xVar: StateTerm.Var) = {
    loc match {
      case TypedLocation.client => ST.App(TypedLocation.client, fVar, xVar)
      case TypedLocation.server => ST.Request(fVar, List(xVar))
    }
  }

  def client(typedTerm: TypedTerm): StateTerm =
    helper(0, typedTerm, clientSpecifics)._2

  private def serverSpecifics(id: Int,
                              loc: TypedLocation,
                              fVar: StateTerm.Var,
                              xVar: StateTerm.Var) = {
    loc match {
      case TypedLocation.client =>
        val (yid, yVar) = makeVar(id, "y")
        val (zid, zVar) = makeVar(id, "z")

        ST.Call(
          ST.Lam(TypedLocation.client,
                 List(zid),
                 ST.Let(yid,
                        ST.App(TypedLocation.client, fVar, zVar),
                        ST.Return(yVar))),
          List(xVar)
        )
      case TypedLocation.server => ST.App(TypedLocation.server, fVar, xVar)
    }
  }

  def server(typedTerm: TypedTerm): StateTerm =
    helper(0, typedTerm, serverSpecifics)._2
}
