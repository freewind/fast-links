package libs

import libs.wrappers.nodejs

import scala.scalajs.js

class JsException extends RuntimeException

object NodeJs {

  def readFile(file: String): String = {
    nodejs.fs.readFileSync(file, js.Dynamic.literal("encoding" -> "UTF-8"))
  }

  def writeFile(file: String, content: String): Unit = {
    nodejs.fs.writeFileSync(file, content, js.Dynamic.literal("encoding" -> "UTF-8"))
  }

}
