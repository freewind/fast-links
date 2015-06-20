package in.freewind.fastlinks

import libs.wrappers.nodejs
import libs.NodeJs
import org.widok.Var
import upickle._
import scala.scalajs.js

case class Config(dataDirPath: String) {
  val metaFilePath = dataDirPath + "/meta.json"
  val projectsDirPath = dataDirPath + "/projects"
}

object DataStore {

  val configFilePath = s"${NodeJs.userHome}/.fast-links/config.json"

  val meta = Var[Option[Meta]](None)
  val config = Var[Option[Config]](None)

  val allCategories = meta.map(_.toSeq.flatMap(_.categories))

  def changeDataFilePath(dataFilePath: String): Unit = {
    config.update {
      case Some(cfg) => Some(cfg.copy(dataDirPath = dataFilePath))
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
            NodeJs.readFile(cfg.metaFilePath) match {
              case Some(metaContent) =>
                val m2 = upickle.read[Meta2](metaContent)
                meta := Some(sortProjects(new Meta(m2.categories.map(c2 => new Category(c2.id, c2.name, c2.projects.map(cfg.projectsDirPath + "/" + _).flatMap(readProjectFile), c2.description)))))
              case _ => meta := None
            }
          case _ => meta := None
        }
      case _ => meta := None
    }
  }

  private def readProjectFile(filepath: String): Option[Project] = {
    NodeJs.readFile(filepath) match {
      case Some(data) => Some(upickle.read[Project](data))
      case _ => None
    }
  }

  private def sortProjects(meta: Meta) = transform(meta)(transformCategory = {
    case c => Seq(c.copy(projects = c.projects.sortBy(_.name)))
  })

  def saveData(): Unit = {
    config.get.foreach { cfg =>
      NodeJs.writeFile(configFilePath, prettyJson(upickle.write[Config](cfg)))
      meta.get.foreach { m =>
        val m2 = new Meta2(m.categories.map(c => new Category2(c.id, c.name, c.projects.map(getProjectFileName), c.description)))
        NodeJs.writeFile(cfg.metaFilePath, prettyJson(upickle.write[Meta2](m2)))
        m.categories.flatMap(_.projects).foreach { project =>
          val targetProjectFile = s"${cfg.projectsDirPath}/${getProjectFileName(project)}"
          println(targetProjectFile)
          NodeJs.writeFile(targetProjectFile, prettyJson(upickle.write[Project](project)))
        }

        val invalidProjectFiles = nodejs.fs.readdirSync(cfg.projectsDirPath).filterNot(m2.categories.flatMap(_.projects).contains)
        invalidProjectFiles.map(filename => s"${cfg.projectsDirPath}/$filename").foreach(nodejs.fs.unlinkSync)
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
    meta := meta.get.map(transform(_)(transformLinkGroup = {
      case g if g.id == selectedLinkGroup.id && !g.links.exists(_.id == link.id) => Seq(g.copy(links = g.links :+ link))
    }, transformLink = {
      case l if l.id == link.id => Seq(link)
    }))
  }

  def deleteLink(link: Link): Unit = {
    meta := meta.get.map(transform(_)(transformLink = {
      case l if l.id == link.id => Nil
    }))
  }

  def changeLinkParent(link: Link, targetLinkGroup: LinkGroup): Unit = {
    deleteLink(link)
    addOrUpdateLink(targetLinkGroup, link)
  }

  def createNewLinkGroup(selectedProject: Project, linkGroup: LinkGroup): Unit = {
    meta := meta.get.map(transform(_)(transformProject = {
      case p if p.id == selectedProject.id => Seq(p.copy(linkGroups = p.linkGroups :+ linkGroup))
    }))
  }

  def updateLinkGroup(linkGroup: LinkGroup): Unit = {
    meta := meta.get.map(transform(_)(transformLinkGroup = {
      case g if g.id == linkGroup.id => Seq(linkGroup)
    }))
  }

  def changeLinkGroupParent(linkGroup: LinkGroup, targetProject: Project): Unit = {
    deleteLinkGroup(linkGroup)
    createNewLinkGroup(targetProject, linkGroup)
  }

  def deleteLinkGroup(linkGroup: LinkGroup): Unit = {
    meta := meta.get.map(transform(_)(transformLinkGroup = {
      case l if l.id == linkGroup.id => Nil
    }))
  }

  def findProject(id: String): Option[Project] = {
    meta.get.flatMap(_.categories.flatMap(_.projects).find(_.id == id))
  }

  def firstProject: Option[Project] = {
    meta.get.flatMap(_.categories.flatMap(_.projects).headOption)
  }

  def createNewProject(selectedCategory: Category, project: Project): Unit = {
    meta := meta.get.map(transform(_)(transformCategory = {
      case c if c.id == selectedCategory.id => Seq(c.copy(projects = c.projects :+ project))
    }))
  }

  def deleteProject(deleting: Project): Unit = {
    meta := meta.get.map(transform(_)(transformProject = {
      case p if p.id == deleting.id => Nil
    }))
  }

  def updateProject(project: Project): Unit = {
    meta := meta.get.map(transform(_)(transformProject = {
      case p if p.id == project.id => Seq(project)
    }))
  }

  def createCategory(category: Category): Unit = {
    meta := meta.get.map { m =>
      m.copy(categories = m.categories :+ category)
    }
  }

  def updateCategory(updating: Category): Unit = {
    meta := meta.get.map(transform(_)(transformCategory = {
      case c if c.id == updating.id => Seq(updating)
    }))
  }

  def deleteCategory(deleting: Category): Unit = {
    meta := meta.get.map(transform(_)(transformCategory = {
      case c if c.id == deleting.id => Nil
    }))
  }

  def moveLinkBefore(source: Link, target: Link): Unit = {
    meta := meta.get.map(transform(_)(transformLink = {
      case l if l.id == source.id => Nil
      case l if l.id == target.id => Seq(source, target)
    }))
  }

  private def transform(meta: Meta)(transformCategory: PartialFunction[Category, Seq[Category]] = {case c => Seq(c)},
                                    transformProject: PartialFunction[Project, Seq[Project]] = {case p => Seq(p)},
                                    transformLinkGroup: PartialFunction[LinkGroup, Seq[LinkGroup]] = {case g => Seq(g)},
                                    transformLink: PartialFunction[Link, Seq[Link]] = {case l => Seq(l)}): Meta = {
    meta.copy(categories = meta.categories.flatMap(transformCategory.orElse({ case c => Seq(c) })).map(c =>
      c.copy(projects = c.projects.flatMap(transformProject.orElse({ case p => Seq(p) })).map(p =>
        p.copy(linkGroups = p.linkGroups.flatMap(transformLinkGroup.orElse({ case g => Seq(g) })).map(g =>
          g.copy(links = g.links.flatMap(transformLink.orElse({ case l => Seq(l) })))))))))
  }

}
