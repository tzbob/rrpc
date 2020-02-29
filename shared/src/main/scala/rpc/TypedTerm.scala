package rpc

import cats.Show

sealed trait TypedTerm

object TypedTerm {
  case class Const(i: Int)     extends TypedTerm
  case class Var(name: String) extends TypedTerm
  case class Lam(location: Location,
                 name: String,
                 argumentType: Tpe,
                 body: TypedTerm)
      extends TypedTerm
  case class App(loc: TypedLocation, fun: TypedTerm, param: TypedTerm)
      extends TypedTerm

  // Only used at runtime, how to model?
  case class Closure(lam: TypedTerm, env: Map[String, TypedTerm])
      extends TypedTerm

  def traverse(typedTerm: TypedTerm): Stream[TypedTerm] =
    typedTerm #:: (typedTerm match {
      case Lam(_, _, _, body)   => traverse(body)
      case Closure(lam, _)      => traverse(lam)
      case App(loc, fun, param) => traverse(fun) #::: traverse(param)
      case x                    => Stream.empty
    })

  def freeVariables(interTerm: TypedTerm): List[TypedTerm.Var] = {
    def helper(interTerm: TypedTerm,
               bounded: Set[TypedTerm.Var]): List[TypedTerm.Var] = {
      interTerm match {
        case v @ Var(name) => if (bounded contains v) Nil else List(v)
        case Const(i)      => Nil
        case Lam(location, name, argumentType, body) =>
          helper(body, bounded + Var(name))
        case App(loc, fun, param) =>
          helper(fun, bounded) ::: helper(param, bounded)
      }
    }
    helper(interTerm, Set.empty)
  }

  def substitute(t: TypedTerm,
                 target: TypedTerm,
                 replacement: TypedTerm): TypedTerm = {
    val sub = substitute(_, target, replacement)
    if (t == target) replacement
    else
      t match {
        case App(loc, fun, param) => App(loc, sub(fun), sub(param))
        case Lam(location, name, argumentType, body) =>
          Lam(location, name, argumentType, sub(body))
        case Closure(lam, env) => Closure(sub(lam), env)
        case x                 => x
      }
  }

  def applyTypeSubstitution(
      typedTerm: TypedTerm,
      tpeSub: Map[Tpe, Tpe],
      locSub: Map[TypedLocation, TypedLocation]): TypedTerm = {
    def subOrKeep[A](a: A, map: Map[A, A]): A = map.get(a).getOrElse(a)
    typedTerm match {
      case Const(_) => typedTerm
      case Var(_)   => typedTerm
      case Lam(l, name, argTpe, bTpe) =>
        Lam(l,
            name,
            subOrKeep(argTpe, tpeSub),
            applyTypeSubstitution(bTpe, tpeSub, locSub))
      case App(tl, fun, param) =>
        App(subOrKeep(tl, locSub),
            applyTypeSubstitution(fun, tpeSub, locSub),
            applyTypeSubstitution(param, tpeSub, locSub))
    }
  }

  // Pretty print implementations

  /**
    * Show instance for pretty printing
    */
  object PrettyShow extends Show[TypedTerm] {
    override def show(t: TypedTerm): String = t match {
      case Const(i)  => s"C$i"
      case Var(name) => name
      case App(loc, fun, param) =>
        s"(${show(fun)}) ${loc.superscript}(${show(param)})"
      case Lam(location, name, _, body) =>
        s"λ${location.superscript}$name. ${show(body)}"
      case Closure(lam, env) =>
        s"[$env]${show(lam)}"
    }
  }

  /**
    * Show instance for pretty printing with type ascriptions on lambda arguments
    */
  object PrettyTypedShow extends Show[TypedTerm] {
    override def show(t: TypedTerm): String = t match {
      case App(loc, fun, param) =>
        s"(${show(fun)}) ${loc.superscript}(${show(param)})"
      case Lam(location, name, argumentType, body) =>
        s"λ${location.superscript}$name: ${Show[Tpe].show(argumentType)}. ${show(body)}"
      case t => PrettyShow.show(t)
    }
  }
}