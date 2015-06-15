package libs.wrappers

import scala.scalajs.js

package object nodejs {

  val process: Process = js.Dynamic.global.process.asInstanceOf[Process]
  val fs: NodeFS = js.Dynamic.global.require("fs").asInstanceOf[NodeFS]
  val mkdirp: Mkdirp = js.Dynamic.global.require("mkdirp").asInstanceOf[Mkdirp]
  val path: Path = js.Dynamic.global.require("path").asInstanceOf[Path]

}


trait NodeFS extends js.Object {
  def readFileSync(path: String, options: js.Any): String = js.native
  def writeFileSync(path: String, content: String, options: js.Any): Unit = js.native
  def existsSync(path: String): Boolean = js.native
}

trait Mkdirp extends js.Object {
  def sync(path: String): Unit = js.native
}

trait Path extends js.Object {
  def dirname(path: String): String = js.native
}

trait Process extends js.Object {
  def platform: String = js.native
  def env: Env = js.native
}

trait Env extends js.Object {
  val USERPROFILE: String = js.native
  val HOME: String = js.native
}
