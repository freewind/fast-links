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

  def addLink(selectedLinkGroup: LinkGroup, link: Link): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map { linkGroup =>
            val links = if (linkGroup == selectedLinkGroup) {
              linkGroup.links :+ link
            } else {
              linkGroup.links
            }
            linkGroup.copy(links = links)
          })
        })
      })
    }
  }

  def findProject(id: String): Option[Project] = {
    meta.get.flatMap(_.categories.flatMap(_.projects).find(_.id == id))
  }

}
