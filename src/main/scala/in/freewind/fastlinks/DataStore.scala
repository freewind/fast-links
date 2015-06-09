package in.freewind.fastlinks

import libs.NodeJs
import org.widok.Var
import upickle._
import scala.concurrent.ExecutionContext.Implicits.global

object DataStore {

  val meta = Var[Option[Meta]](None)
  val allCategories = meta.map(_.toSeq.flatMap(_.categories))

  def loadData(): Unit = {
    NodeJs.readFile("/Users/twer/workspace/fast-links/data.json").map(upickle.read[Meta]).map { meta =>
      meta.copy(categories = meta.categories.map(category => category.copy(projects = category.projects.sortBy(_.name))))
    }.foreach(meta := Some(_))
  }

}
