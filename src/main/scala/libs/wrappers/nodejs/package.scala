package libs.wrappers

import scala.scalajs.js

package object nodejs {

  val fs: NodeFS = js.Dynamic.global.require("fs").asInstanceOf[NodeFS]

}
