package in.freewind.fastlinks

import libs.{NodeWebkit, NodeJs}
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.widok.bindings.Bootstrap._
import org.widok.{Channel, Buffer, InstantiatedRoute, Page, ReadChannel, Opt, Var, View}
import org.widok.html._
import upickle._
import LayoutWithSelectors._

import scala.concurrent.ExecutionContext.Implicits.global

case class EditPage() extends Page {

  val selectedProject = Opt[Project]()
  val toggled = Var(false)

  DataStore.meta.attach { _ =>
    selectedProject.update(p => DataStore.findProject(p.id).getOrElse(p))
  }

  override def ready(route: InstantiatedRoute): Unit = {
    DataStore.loadData()
    DataStore.allCategories.toBuffer.get.flatten.flatMap(_.projects).headOption.foreach(selectedProject := _)
  }

  override def view(): View = "#edit-page" >>> div(
    "#wrapper" >>> div(
      "#sidebar-wrapper" >>> div(
        ".sidebar" >>> sidebar()
      ),
      "#page-content-wrapper" >>> div(
        ".container-fluid" >>> div(
          ".row" >>> div(
            ".col-lg-12" >>> div(
              "#menu-toggle.btn.btn-default" >>> button("Toggle Menu").onClick(_ => toggled.update(!_)),
              ".main-content" >>> mainContent()
            )
          )
        )
      )
    ).cssState(toggled, "toggled")
  )

  private def sidebar() = div(DataStore.allCategories.map(categories =>
    ".categories" >>> div(
      categories.map { category =>
        val showCategoryOps = Var(false)
        val showCategoryForm = Var(false)
        ".category" >>> div(
          div(
            div(
              ".category-name" >>> span(category.name),
              ".category-ops" >>> span(
                button("edit").onClick(_ => showCategoryForm := true),
                button("delete").onClick(_ => if (dom.confirm("Are you sure to delete?")) {
                  DataStore.deleteCategory(category)
                })
              ).show(showCategoryOps)
            ).onMouseEnter(_ => showCategoryOps := true).onMouseLeave(_ => showCategoryOps := false)
              .show(showCategoryForm.map(!_)),
            ".category-form" >>> new CategoryForm(category).apply(showCategoryForm)
          ),
          ".projects" >>> div(
            category.projects.map(project => {
              val showProjectOps = Var(false)
              val showProjectForm = Var(false)
              ".project" >>> div(
                div(
                  ".project-name" >>> span(project.name),
                  ".project-ops" >>> span(
                    button("edit").onClick(_ => showProjectForm := true),
                    button("delete").onClick(_ => if (dom.confirm("Are you sure to delete?")) {
                      DataStore.deleteProject(project)
                    })
                  ).show(showProjectOps)
                ).onClick(_ => selectedProject := project)
                  .show(showProjectForm.map(!_))
                  .onMouseEnter(_ => showProjectOps := true)
                  .onMouseLeave(_ => showProjectOps := false),
                ".project-form" >>> new ProjectForm(project).apply(showProjectForm)
              )
            }
            ).toList: _*
          ),
          button("+ project").onClick(_ => DataStore.createNewProject(category, Project(Utils.newId(), "< new project >")))
        )
      },
      button("+ category").onClick(_ => DataStore.createCategory(Category(Utils.newId(), "< new category >")))
    )
  ))

  private def mainContent() = div(
    Button(Glyphicon.Ok()).size(Size.ExtraSmall).title("Done").onClick { _ => DataStore.saveData(); Entry.mainPage().go() },
    ".project-profile" >>> div(selectedProject.map { project =>
      div(
        ".project" >>> div(
          ".project-name" >>> div(project.name),
          ".link-groups" >>> div(project.linkGroups.map({ linkGroup =>
            val showCreatingForm = Var(false)
            val showChangeLinkGroupNameForm = Var(false)
            val showMoveLinkGroupNameForm = Var(false)
            val showLinkGroupOps = Var(false)
            ".link-group" >>> div(
              ".link-group-name" >>> div(
                div(
                  span(linkGroup.name),
                  span(
                    button("edit").onClick(_ => showChangeLinkGroupNameForm := true),
                    button("move").onClick(_ => showMoveLinkGroupNameForm := true)
                  ).show(showLinkGroupOps)
                ).onMouseEnter(_ => showLinkGroupOps := true).onMouseLeave(_ => showLinkGroupOps := false)
                  .show(showChangeLinkGroupNameForm.map(!_)),
                new ChangeLinkGroupNameForm(linkGroup).apply(showChangeLinkGroupNameForm),
                new MoveLinkGroupNameForm(project, linkGroup).apply(showMoveLinkGroupNameForm)
              ),
              ".link-group-links" >>> div(
                linkGroup.links.map { link =>
                  val showEditingForm = Var(false)
                  val showMovingForm = Var(false)
                  val showOps = Var(false)
                  div(
                    ".link" >>> div(
                      span(
                        ".link-name" >>> span(link.name.getOrElse[String]("")),
                        ".link-url" >>> a(link.url).url(link.url).attribute("target", "_blank")
                      ),
                      ".ops" >>> span(
                        button("edit").onClick(_ => showEditingForm := true),
                        button("move").onClick(_ => showMovingForm := true),
                        button("delete").onClick(_ => if (dom.confirm("Are you sure to delete?")) {
                          DataStore.deleteLink(link)
                        })
                      ).show(showOps)
                    ).show(showEditingForm.map(!_)).onMouseEnter(_ => showOps := true).onMouseLeave(_ => showOps := false),
                    new LinkForm(linkGroup, Some(link)).apply(showEditingForm),
                    new MovingForm(linkGroup, link).apply(showMovingForm)
                  )
                }
              ),
              Button(Glyphicon.Plus(), span(" Link")).size(Size.ExtraSmall)
                .onClick(_ => showCreatingForm.update(!_)).show(showCreatingForm.map(!_)),
              new LinkForm(linkGroup, None).apply(showCreatingForm)
            )
          })),
          button("new Link Group").onClick(_ => createNewLinkGroup(project))
        ),
        ".project-separator" >>> div()
      )
    })
  )

  private def createNewLinkGroup(project: Project): Unit = {
    val linkGroup = new LinkGroup(Utils.newId(), "< click to edit >", Nil)
    DataStore.createNewLinkGroup(project, linkGroup)
  }

  class LinkForm(linkGroup: LinkGroup, link: Option[Link]) {
    val newLinkTitle = Var[String](link.flatMap(_.name).getOrElse(""))
    val newLinkUrl = Var[String](link.map(_.url).getOrElse(""))
    val newLinkDescription = Var[String](link.flatMap(_.description).getOrElse(""))

    private def createOrUpdateLink(): Unit = {
      val newLink = new Link(link.map(_.id).getOrElse(Utils.newId()), name = Some(newLinkTitle.get), url = newLinkUrl.get, description = Some(newLinkDescription.get))
      DataStore.addOrUpdateLink(linkGroup, newLink)
    }

    def apply(showLinkForm: Var[Boolean]) = {
      ".link-form" >>> div(
        div("Link form"),
        div(
          div(text().bind(newLinkTitle).placeholder("Title")),
          div(text().bind(newLinkUrl).placeholder("URL")),
          div(text().bind(newLinkDescription).placeholder("description"))
        ),
        div(
          button("Cancel").onClick(_ => showLinkForm := false),
          button("OK").onClick { _ =>
            createOrUpdateLink()
            showLinkForm := false
          }
        )
      ).show(showLinkForm)
    }
  }

  class MovingForm(initLinkGroup: LinkGroup, link: Link) {
    val selectedLinkGroup = Var[Option[Either[Project, LinkGroup]]](Some(Right(initLinkGroup)))

    private def myOptions(): Buffer[Either[Project, LinkGroup]] = Buffer(DataStore.allCategories.map(cs => cs.flatMap(_.projects)).flatMapBuf { projects =>
      Buffer(projects.flatMap(p => Left(p) +: p.linkGroups.map(g => Right(g))): _*)
    }.get: _*)

    def showLinkGroupOptions(projectOrLinkGroup: Either[Project, LinkGroup]) = projectOrLinkGroup match {
      case Left(project) => option(project.name).enabled(false)
      case Right(linkGroup) => option(" - " + linkGroup.name)
    }

    def moveLink(): Unit = {
      selectedLinkGroup.get match {
        case Some(Right(targetLinkGroup)) if initLinkGroup != targetLinkGroup => DataStore.moveLink(link, targetLinkGroup)
        case _ =>
      }
    }

    def apply(showForm: Var[Boolean]) = div(
      select().bind(myOptions(), showLinkGroupOptions, selectedLinkGroup),
      div(
        button("cancel").onClick(_ => showForm := false),
        button("move!").onClick { _ =>
          moveLink()
          showForm := false
        }
      )
    ).show(showForm)
  }

  class ChangeLinkGroupNameForm(linkGroup: LinkGroup) {
    val newGroupName = Var(linkGroup.name)
    def apply(showForm: Var[Boolean]) = div(
      div(text().bind(newGroupName)),
      div(
        button("Update").onClick(_ => updateGroupName()),
        button("Cancel").onClick(_ => showForm := false)
      )
    ).show(showForm)

    private def updateGroupName(): Unit = {
      DataStore.updateLinkGroup(linkGroup.copy(name = newGroupName.get))
    }
  }

  class MoveLinkGroupNameForm(initProject: Project, linkGroup: LinkGroup) {
    private val selectedProject = Var[Option[Project]](Some(initProject))
    private def myOptions(): Buffer[Project] = Buffer(DataStore.allCategories.map(cs => cs.flatMap(_.projects)).flatMapBuf(Buffer(_: _*)).get: _*)

    def apply(showForm: Var[Boolean]) = div(
      select().bind(myOptions(), (p: Project) => option(p.name), selectedProject),
      button("Move!").onClick(_ => moveLinkGroup()),
      button("Cancel").onClick(_ => showForm := false)
    ).show(showForm)

    private def moveLinkGroup(): Unit = {
      selectedProject.get match {
        case Some(project) if project != initProject => DataStore.moveLinkGroup(linkGroup, project)
        case _ =>
      }
    }
  }

  class ProjectForm(project: Project) {
    val projectName = Var(project.name)
    def apply(showForm: Var[Boolean]) = div(
      text().bind(projectName).onKeyDown { event =>
        event.keyCode match {
          case KeyCode.Enter =>
            DataStore.updateProject(project.copy(name = projectName.get))
            showForm := false
          case KeyCode.Escape => showForm := false
          case _ =>
        }
      }
    ).show(showForm)
  }
  class CategoryForm(category: Category) {
    val categoryName = Var(category.name)
    def apply(showForm: Var[Boolean]) = div(
      text().bind(categoryName).onKeyDown { event =>
        event.keyCode match {
          case KeyCode.Enter =>
            DataStore.updateCategory(category.copy(name = categoryName.get))
            showForm := false
          case KeyCode.Escape => showForm := false
          case _ =>
        }
      }
    ).show(showForm)
  }
}
