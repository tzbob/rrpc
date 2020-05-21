package examples

import rpc._

object Simple extends RpcApp with RpcAppInt {
  val hostname = "localhost"
  val port     = 8080

  val apps = Map(
    "todomvc" -> Right(
      """
      <meta charset="utf-8">
      <title>PolyRPC • TodoMVC</title>
      <link rel="stylesheet" href="r/index.css">""",
      """
      <section class="todoapp">
        <div id='body'></div>
      </section>
      <footer class="info">
			<p>Double-click to edit a todo</p>
			<p>Written by <a href="http://twitter.com/tzbob">Bob</a></p>
			<p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
      </footer>
      <link rel="stylesheet" href="r/base.css">"""
    )
  )
}
