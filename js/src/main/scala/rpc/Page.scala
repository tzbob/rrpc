package rpc

import izumi.logstage.api.IzLogger
import org.scalajs.dom
import rpc.Value.{Constant, Constructed}
import snabbdom.VNode

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

case class Page(init: Value, view: Value, update: Value, mountPoint: String)

trait Binding
object Binding {
  case class TargetValue(closure: Value.Closure) extends Binding
  case class EmptyValue(msg: Value)              extends Binding
  case class KeyPress(key: Int, msg: Value)      extends Binding
}

object Page {
  def pageFromValue(v: Value): Option[Page] = v match {
    case Constructed("Page",
                     List(init, update, view, Constant(Literal.String(mnt)))) =>
      Some(Page(init, update, view, mnt))
    case _ => None
  }

  def htmlFromValue(v: Value)(onEvent: (dom.Event, Binding) => Unit): VNode = {
    v match {
      case Constructed(
          "Element",
          List(Constant(Literal.String(tag)), rawAttrList, rawElList)) =>
        val attributes = attributeFromValue(rawAttrList)
        val properties = propertyFromValue(rawAttrList)
        val bindings   = valueBindFromValue(rawAttrList, onEvent)

        val children =
          listFromNilCons(rawElList).map(htmlFromValue(_)(onEvent))

        val data = js.Dynamic.literal(
          attrs = attributes,
          props = properties,
          on = bindings
        )

        snabbdom.h(tag, data, children.toJSArray)
      case Constructed("Txt", List(Constant(Literal.String(txt)))) =>
        txt.asInstanceOf[VNode] // TODO: what's the proper way of doing this?
      case _ =>
        throw new RuntimeException(s"Cannot create HTML elements from ${v}")
    }
  }

  private def listFromNilCons(attrList: Value): List[Value] = attrList match {
    case Constructed("Cons", List(v, vs)) => v :: listFromNilCons(vs)
    case Constructed("Nil", _)            => Nil
  }

  private def attributeFromValue(vs: Value): js.Dictionary[String] =
    listFromNilCons(vs)
      .collect {
        case Value.Constructed("Attribute",
                               List(Value.Constant(Literal.String(name)),
                                    Value.Constant(Literal.String(value)))) =>
          name -> value
      }
      .toMap
      .toJSDictionary

  private def propertyFromValue(
      propList: Value): js.Dictionary[String | Boolean | Int] =
    listFromNilCons(propList)
      .collect {
        case Value.Constructed("Property",
                               List(Value.Constant(Literal.String(name)),
                                    Value.Constant(valLit))) =>
          val value: String | Boolean | Int = valLit match {
            case Literal.Bool(b)     => b
            case Literal.String("true") => true
            case Literal.String("false") => false
            case Literal.String(str) => str
            case Literal.Int(i)      => i
            case _ =>
              throw new RuntimeException(
                s"Cannot create Property from ${valLit}")
          }
          name -> value
      }
      .toMap
      .toJSDictionary

  private def valueBindFromValue(
      vs: Value,
      applyEvtToClosure: (dom.Event, Binding) => Unit)
    : js.Dictionary[js.Function1[dom.Event, Unit]] =
    listFromNilCons(vs)
      .collect {
        case Value.Constructed("ValueBind",
                               List(Value.Constant(Literal.String(name)),
                                    cl @ Value.Closure(_, _))) =>
          val callBack: js.Function1[dom.Event, Unit] =
            applyEvtToClosure(_, Binding.TargetValue(cl))
          name -> callBack
        case Value.Constructed("EventBind",
                               List(Value.Constant(Literal.String(name)), v)) =>
          val callBack: js.Function1[dom.Event, Unit] =
            applyEvtToClosure(_, Binding.EmptyValue(v))
          name -> callBack
        case Value.Constructed(
            "KeyBind",
            List(Value.Constant(Literal.Int(keyCode)), msg)) =>
          val callBack: js.Function1[dom.Event, Unit] =
            applyEvtToClosure(_, Binding.KeyPress(keyCode, msg))
          "keyup" -> callBack
      }
      .toMap
      .toJSDictionary
}
