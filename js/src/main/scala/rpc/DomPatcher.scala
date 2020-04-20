package rpc

import org.scalajs.dom
import org.scalajs.dom.Element
import snabbdom.{Snabbdom, VNode}

import scala.scalajs.js
import scala.scalajs.js.|

class DomPatcher(initialVDom: VNode, targetElement: Element) {
  private[this] val target = {
    targetElement.innerHTML = ""
    targetElement
  }

  val parent: Element = {
    val dynamic = target.asInstanceOf[js.Dynamic]
    dynamic.parentElement.asInstanceOf[dom.Element]
  }

  private[this] var oldVDom: VNode = {
    DomPatcher.patch(target, initialVDom)
  }

  def applyNewState(vdom: VNode): Unit = {
    oldVDom = DomPatcher.patch(oldVDom, vdom)
  }
}

object DomPatcher {
  val patch: js.Function2[|[VNode, Element], VNode, VNode] =
    Snabbdom.init(
      js.Array(snabbdom.`class`,
               snabbdom.props,
               snabbdom.attributes,
               snabbdom.style,
               snabbdom.eventlisteners))
}
