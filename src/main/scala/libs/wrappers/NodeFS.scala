package libs.wrappers

import scala.scalajs.js

trait NodeFS extends js.Object {
  def readFile(path: String, options: js.Any, callback: js.Function2[js.Any, String, Unit] = ???): Unit = js.native
}
