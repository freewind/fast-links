package libs

import scala.scalajs.js

object NodeWebkit {
  val gui: Gui = js.Dynamic.global.require("nw.gui").asInstanceOf[Gui]
}

trait Gui extends js.Object {
  val Shell: Shell = js.native
}

trait Shell extends js.Object {
  def openExternal(url: String): Unit = js.native
}
