package rpc

object MonoInterpreter {
  def interpret(term: Term): TypedTerm = interpret(Infer.infer(term))
  def interpret(term: TypedTerm): TypedTerm = trace(term).last

  def trace(term: TypedTerm): List[TypedTerm] = {
    def traceInEnv(env: Map[String, TypedTerm],
                   term: TypedTerm): List[TypedTerm] = {
      term match {
        case TypedTerm.Var(name) =>
          if (env.contains(name)) List(env(name))
          else throw new RuntimeException(s"$name not in $env")
        case TypedTerm.Const(_) => List(term)

        case l @ TypedTerm.Lam(_, _, _, _) => List(TypedTerm.Closure(l, env))

        case TypedTerm.App(loc, fun, param) =>
          traceInEnv(env, fun) match {
            case funTrace @ (TypedTerm
                  .Closure(TypedTerm.Lam(loc, name, _, body), enclosedEnv) :: _) =>
              val paramTrace @ (result :: _) = traceInEnv(env, param)
              val newEnv                     = env ++ enclosedEnv + (name -> result)
              traceInEnv(newEnv, body) ::: paramTrace ::: funTrace
            case _ => throw new RuntimeException(s"Cannot apply $fun")
          }
      }

    }

    traceInEnv(Map.empty, term).reverse
  }
}
