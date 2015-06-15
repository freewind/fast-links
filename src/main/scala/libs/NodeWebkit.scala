package libs

import scala.scalajs.js

object NodeWebkit {
  val gui: Gui = js.Dynamic.global.require("nw.gui").asInstanceOf[Gui]
}

trait Gui extends js.Object {
  val Shell: Shell = js.native
  val Window: WindowFactory = js.native
}

trait WindowFactory extends js.Object {
  def get(): Window = js.native
}

trait Window extends js.Object {
  def on(event: String, callback: js.Function0[Unit] = ???): Unit = js.native
}

trait Shell extends js.Object {
  def openExternal(url: String): Unit = js.native
}
