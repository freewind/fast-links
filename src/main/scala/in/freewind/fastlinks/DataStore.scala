package in.freewind.fastlinks

import libs.wrappers.nodejs
import libs.NodeJs
import org.widok.Var
import upickle._
import scala.scalajs.js

case class Config(dataFilePath: String)

object DataStore {

  val configFilePath = s"${NodeJs.userHome}/.fast-links/config.json"

  val meta = Var[Option[Meta]](None)
  val config = Var[Option[Config]](None)

  val allCategories = meta.map(_.toSeq.flatMap(_.categories))

  def changeDataFilePath(dataFilePath: String): Unit = {
    config.update {
      case Some(cfg) => Some(cfg.copy(dataFilePath = dataFilePath))
      case _ => Some(Config(dataFilePath))
    }
    saveData()
    loadData()
  }

  def loadData(): Unit = {
    NodeJs.readFile(configFilePath) match {
      case Some(configContent) =>
        config := Some(upickle.read[Config](configContent))
        config.attach {
          case Some(cfg) =>
            NodeJs.readFile(cfg.dataFilePath) match {
              case Some(dataContent) => meta := Some(sortProjects(upickle.read[Meta](dataContent)))
              case _ => meta := None
            }
          case _ => meta := None
        }
      case _ => meta := None
    }
  }

  private def sortProjects(meta: Meta) = {
    meta.copy(categories = meta.categories.map(category => category.copy(projects = category.projects.sortBy(_.name))))
  }

  def saveData(): Unit = {
    //    config.get.foreach { cfg =>
    //      NodeJs.writeFile(configFilePath, JsBeautifier.js_beautify(upickle.write[Config](cfg)))
    //      meta.get.foreach(m => NodeJs.writeFile(cfg.dataFilePath, JsBeautifier.js_beautify(upickle.write[Meta](m))))
    //    }
    saveData2()
  }

  def saveData2(): Unit = {
    config.get.foreach { cfg =>
      NodeJs.writeFile(configFilePath, prettyJson(upickle.write[Config](cfg)))
      meta.get.foreach { m =>
        val m2 = new Meta2(m.categories.map(c => new Category2(c.id, c.name, c.projects.map(getProjectFileName), c.description)))
        val dirname = nodejs.path.dirname(cfg.dataFilePath)

        NodeJs.writeFile(dirname + "/meta.json", prettyJson(upickle.write[Meta2](m2)))
        m.categories.flatMap(_.projects).foreach { project =>
          val targetProjectFile = dirname + s"/projects/${getProjectFileName(project)}"
          println(targetProjectFile)
          NodeJs.writeFile(targetProjectFile, prettyJson(upickle.write[Project](project)))
        }

        val invalidProjectFiles = nodejs.fs.readdirSync(dirname + "/projects").filterNot(m2.categories.flatMap(_.projects).contains)
        invalidProjectFiles.map(filename => dirname + s"/projects/$filename").foreach(nodejs.fs.unlinkSync)
      }
    }
  }

  private def prettyJson(jsonStr: String): String = {
    val obj = js.JSON.parse(jsonStr)
    js.JSON.stringify(obj.asInstanceOf[js.Any], null.asInstanceOf[js.Array[js.Any]], 4)
  }

  private def getProjectFileName(project: Project): String = {
    val projectName = nodejs.sanitizeFilename(project.name).toLowerCase.replace(' ', '-')
    s"$projectName.${project.id}.json"
  }

  def addOrUpdateLink(selectedLinkGroup: LinkGroup, link: Link): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map { linkGroup =>
            val links = if (linkGroup.id == selectedLinkGroup.id) {
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

  def changeLinkParent(link: Link, targetLinkGroup: LinkGroup): Unit = {
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

  def changeLinkGroupParent(linkGroup: LinkGroup, targetProject: Project): Unit = {
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

  def moveLinkBefore(source: Link, target: Link): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map { linkGroup =>
            linkGroup.copy(links = linkGroup.links.flatMap {
              case link if link.id == source.id => None
              case link if link.id == target.id => Seq(source, target)
              case link => Some(link)
            })
          })
        })
      })
    }

  }

}
