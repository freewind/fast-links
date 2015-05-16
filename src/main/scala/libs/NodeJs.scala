package libs

import libs.wrappers.nodejs

import scala.concurrent.{Future, Promise}
import scala.scalajs.js

class JsException extends RuntimeException

object NodeJs {

  def readFile(file: String): Future[String] = {
    val promise = Promise[String]()
    nodejs.fs.readFile(file, js.Dynamic.literal("encoding" -> "UTF-8"), (error: js.Any, data: String) => {
      if (error != null) {
        promise.failure(new JsException())
      } else {
        promise.success(data)
      }
      ()
    })
    promise.future
  }

}
