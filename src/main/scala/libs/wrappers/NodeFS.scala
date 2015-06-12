package libs.wrappers

import scala.scalajs.js

trait NodeFS extends js.Object {
  def readFileSync(path: String, options: js.Any): String = js.native
  def writeFileSync(path: String, content: String, options: js.Any): Unit = js.native
}
