package libs

import scala.scalajs.js

object SanitizeFilename extends js.GlobalScope {

  val sanitizeFilename: SanitizeFilename = js.native

}

trait SanitizeFilename extends js.Object {

  def apply(str: String): String = js.native

}
