package libs

import libs.wrappers.nodejs

import scala.scalajs.js

class JsException(message: String) extends RuntimeException(message)

object NodeJs {

  def readFile(file: String): Option[String] = {
    if (nodejs.fs.existsSync(file)) {
      Some(nodejs.fs.readFileSync(file, js.Dynamic.literal("encoding" -> "UTF-8")))
    } else {
      None
    }
  }

  def writeFile(file: String, content: String): Unit = {
    nodejs.mkdirp.sync(nodejs.path.dirname(file))
    nodejs.fs.writeFileSync(file, content, js.Dynamic.literal("encoding" -> "UTF-8"))
  }

  def userHome: String = {
    if (nodejs.process.platform == "win32") {
      nodejs.process.env.USERPROFILE
    } else {
      nodejs.process.env.HOME
    }
  }

}
