package rpc

import cats.Show

trait TypedTerm

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

  def substitute(typedTerm: TypedTerm,
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
            substitute(bTpe, tpeSub, locSub))
      case App(tl, fun, param) =>
        App(subOrKeep(tl, locSub),
            substitute(fun, tpeSub, locSub),
            substitute(param, tpeSub, locSub))
    }
  }

  object PrettyShow extends Show[TypedTerm] {
    override def show(t: TypedTerm): String = t match {
      case Const(i)  => s"C$i"
      case Var(name) => name
      case App(loc, fun, param) =>
        s"(${show(fun)}) ${loc.superscript}(${show(param)})"
      case Lam(location, name, _, body) =>
        s"λ${location.superscript}$name. ${show(body)}"
    }
  }

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
