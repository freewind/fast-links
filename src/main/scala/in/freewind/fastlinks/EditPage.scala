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
  val showSidebar = Var(true)

  override def ready(route: InstantiatedRoute): Unit = {
    DataStore.loadData()
  }

  override def view(): View = "#edit-page" >>> div(
    "#wrapper" >>> div(
      "#sidebar-wrapper" >>> div(
        ".sidebar-nav" >>> sidebar()
      ),
      "#page-content-wrapper" >>> div(
        ".container-fluid" >>> div(
          ".row" >>> div(
            ".col-lg-12" >>> div(
              "#menu-toggle.btn.btn-default" >>> button("Toggle Menu").onClick(_ => showSidebar.update(!_)),
              ".main-content" >>> mainContent()
            )
          )
        )
      )
    ).cssState(showSidebar, "toggled")
  )

  private def sidebar() = div(DataStore.allCategories.map(categories =>
    ".categories" >>> div(categories.map(category =>
      ".category" >>> ul(
        (".category-name.sidebar-brand" >>> li(a(category.name))) :: category.projects.map(p =>
          ".project" >>> li(a(p.name)).onClick(_ => selectedProject := p)
        ).toList: _*
      )
    ))
  ))

  private def mainContent() = div(
    Button(Glyphicon.Ok()).size(Size.ExtraSmall).title("Done").onClick { _ => DataStore.saveData(); Entry.mainPage().go() },
    ".project-profile" >>> div(selectedProject.map(project =>
      div(
        ".project" >>> div(
          ".project-name" >>> div(project.name),
          ".link-groups" >>> div(project.linkGroups.map({ linkGroup =>
            val showCreatingForm = Var(false)
            ".link-group" >>> div(
              ".link-group-name" >>> div(linkGroup.name),
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
                          updateSelectedProject()
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
          }))
        ),
        ".project-separator" >>> div()
      )
    ))
  )

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
            updateSelectedProject()
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
          updateSelectedProject()
          showForm := false
        }
      )
    ).show(showForm)
  }

  private def updateSelectedProject(): Unit = {
    selectedProject.update(p => DataStore.findProject(p.id).getOrElse(p))
  }

}
