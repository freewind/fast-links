package in.freewind.fastlinks

import libs.NodeJs
import org.widok.Var
import upickle._
import scala.concurrent.ExecutionContext.Implicits.global

object DataStore {

  val dataFilePath = "/Users/twer/workspace/fast-links/data.json"

  val meta = Var[Option[Meta]](None)

  val allCategories = meta.map(_.toSeq.flatMap(_.categories))

  def loadData(): Unit = {
    meta := Some(sortProjects(upickle.read[Meta](NodeJs.readFile(dataFilePath))))
  }

  private def sortProjects(meta: Meta) = {
    meta.copy(categories = meta.categories.map(category => category.copy(projects = category.projects.sortBy(_.name))))
  }

  def saveData(): Unit = {
    meta.get.foreach(m => NodeJs.writeFile(dataFilePath, upickle.write[Meta](m)))
  }

  def addOrUpdateLink(selectedLinkGroup: LinkGroup, link: Link): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map { linkGroup =>
            val links = if (linkGroup == selectedLinkGroup) {
              if (linkGroup.links.exists(_.id == link.id)) {
                linkGroup.links.map {
                  case l if l.id == link.id => link
                  case l => l
                }
              } else {
                linkGroup.links :+ link
              }
            } else {
              linkGroup.links
            }
            linkGroup.copy(links = links)
          })
        })
      })
    }
  }

  def deleteLink(link: Link): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map { linkGroup =>
            val links = linkGroup.links.filter(_.id != link.id)
            linkGroup.copy(links = links)
          })
        })
      })
    }
  }

  def moveLink(link: Link, targetLinkGroup: LinkGroup): Unit = {
    deleteLink(link)
    addOrUpdateLink(targetLinkGroup, link)
  }

  def createNewLinkGroup(selectedProject: Project, linkGroup: LinkGroup): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map {
          case project if project.id == selectedProject.id => project.copy(linkGroups = project.linkGroups :+ linkGroup)
          case p => p
        })
      })
    }
  }

  def updateLinkGroup(linkGroup: LinkGroup): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map {
            case g if g.id == linkGroup.id => linkGroup
            case g => g
          })
        })
      })
    }
  }

  def moveLinkGroup(linkGroup: LinkGroup, targetProject: Project): Unit = {
    deleteLinkGroup(linkGroup)
    createNewLinkGroup(targetProject, linkGroup)
  }

  def deleteLinkGroup(linkGroup: LinkGroup): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.filter(_.id != linkGroup.id))
        })
      })
    }
  }

  def findProject(id: String): Option[Project] = {
    meta.get.flatMap(_.categories.flatMap(_.projects).find(_.id == id))
  }

  def firstProject: Option[Project] = {
    meta.get.flatMap(_.categories.flatMap(_.projects).headOption)
  }

  def createNewProject(selectedCategory: Category, project: Project): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map {
        case category if selectedCategory.id == category.id => category.copy(projects = category.projects :+ project)
        case c => c
      })
    }
  }

  def deleteProject(deleting: Project): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.filter(_.id != deleting.id))
      })
    }
  }

  def updateProject(project: Project): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map {
          case p if p.id == project.id => project
          case p => p
        })
      })
    }
  }

  def createCategory(category: Category): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories :+ category)
    }
  }

  def updateCategory(category: Category): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map {
        case c if c.id == category.id => category
        case c => c
      })
    }
  }

  def deleteCategory(deleting: Category): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.filter(_.id != deleting.id))
    }
  }

}
