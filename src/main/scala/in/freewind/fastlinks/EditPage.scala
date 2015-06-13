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
            val showForm = Var(false)

            ".link-group" >>> div(
              ".link-group-name" >>> div(linkGroup.name),
              ".link-group-links" >>> div(
                linkGroup.links.map(link =>
                  ".link" >>> div(
                    ".link-name" >>> span(link.name.getOrElse[String]("")),
                    ".link-url" >>> a(link.url).url(link.url).attribute("target", "_blank")
                  )
                )
              ),
              Button(Glyphicon.Plus(), span(" Link")).size(Size.ExtraSmall)
                .onClick(_ => showForm.update(!_)).show(showForm.map(!_)),
              new LinkForm().apply(linkGroup, showForm)
            )
          }))
        ),
        ".project-separator" >>> div()
      )
    ))
  )

  class LinkForm {
    val newLinkTitle = Var[String]("")
    val newLinkUrl = Var[String]("")
    val newLinkDescription = Var[String]("")
    val selectedLinkGroup = Opt[LinkGroup]()

    private def createLink(): Unit = {
      val newLink = new Link(Utils.newId(), name = Some(newLinkTitle.get), url = newLinkUrl.get, description = Some(newLinkDescription.get))
      selectedLinkGroup.toOption match {
        case Some(linkGroup) => DataStore.addLink(linkGroup, newLink)
        case _ =>
      }
    }

    //    private def myOptions(): Buffer[Either[Project, LinkGroup]] = Buffer(DataStore.allCategories.map(cs => cs.flatMap(_.projects)).flatMapBuf { projects =>
    //      Buffer(projects.flatMap(p => Left(p) +: p.linkGroups.map(g => Right(g))): _*)
    //    }.get: _*)
    //
    //    def showLinkGroupOptions(projectOrLinkGroup: Either[Project, LinkGroup]) = projectOrLinkGroup match {
    //      case Left(project) => option(project.name).enabled(false)
    //      case Right(linkGroup) => option(" - " + linkGroup.name)
    //    }
    //    div(selectedLinkGroup.map(_.toString)),
    //    select().bind(myOptions(), showLinkGroupOptions, selectedLinkGroup),

    def apply(linkGroup: LinkGroup, showLinkForm: Var[Boolean]) = {
      selectedLinkGroup := linkGroup
      ".link-form" >>> div(
        div("Link form"),
        div(
          div(text().bind(newLinkTitle).placeholder("Title")),
          div(text().bind(newLinkUrl).placeholder("URL")),
          div(text().bind(newLinkDescription).placeholder("description"))
        ),
        div(
          button("Close").onClick(_ => showLinkForm := false),
          button("OK").onClick { _ =>
            createLink()
            updateSelectedProject()
            showLinkForm := false
          }
        )
      ).show(showLinkForm)
    }
  }

  private def updateSelectedProject(): Unit = {
    selectedProject.update(p => DataStore.findProject(p.id).getOrElse(p))
  }

}
