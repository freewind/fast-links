package libs

import libs.wrappers.nodejs

import scala.concurrent.{Promise, Future}
import scala.scalajs.js

import scala.concurrent.ExecutionContext.Implicits.global

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
    mkdirp(nodejs.path.dirname(file)).foreach { _ =>
      nodejs.fs.writeFileSync(file, content, js.Dynamic.literal("encoding" -> "UTF-8"))
    }
  }

  def mkdirp(dir: String): Future[String] = {
    val promise = Promise[String]()
    nodejs.mkdirp.apply(dir, (error: js.Any) => {
      if (error != null) {
        promise.failure(new JsException("Can't make dir for: " + dir))
      } else {
        promise.success(dir)
      }
      ()
    })
    promise.future
  }

  def userHome: String = {
    if (nodejs.process.platform == "win32") {
      nodejs.process.env.USERPROFILE
    } else {
      nodejs.process.env.HOME
    }
  }

}
