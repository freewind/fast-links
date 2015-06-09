package in.freewind.fastlinks

import libs.{NodeWebkit, NodeJs}
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.widok.{InstantiatedRoute, Page, ReadChannel, Opt, Var, View}
import org.widok.html._
import upickle._
import LayoutWithSelectors._

import scala.concurrent.ExecutionContext.Implicits.global

case class EditPage() extends Page {

  val selectedProject = Opt[Project]()

  override def ready(route: InstantiatedRoute): Unit = {
    DataStore.loadData()
  }

  override def view(): View = "#edit-page" >>> div(
    ".sidebar" >>> sidebar(),
    ".main-content" >>> mainContent()
  )

  private def sidebar() = div(DataStore.allCategories.map(categories =>
    ".categories" >>> div(categories.map(category =>
      ".category" >>> div(
        ".category-name" >>> div(category.name),
        ".project-list" >>> div(category.projects.map(p =>
          ".project" >>> div(p.name).onClick(_ => selectedProject := p)
        ))
      )
    ))
  ))

  private def mainContent() = div(
    button("done").onClick(_ => Entry.mainPage().go()),
    ".project-profile" >>> div(selectedProject.map(_.name))
  )

  class LinkForm {
    val newLinkTitle = Var[String]("")
    val newLinkUrl = Var[String]("")
    val newLinkDescription = Var[String]("")
    val selectedLinkGroup = Opt[LinkGroup]()

    //    private def createLink(): Unit = {
    //      meta := meta.get.map { mmm =>
    //        mmm.copy(categories = mmm.categories.map { category =>
    //          category.copy(projects = category.projects.map { project =>
    //            project.copy(linkGroups = project.linkGroups.map { linkGroup =>
    //              val links = if (linkGroup == selectedLinkGroup.get) {
    //                linkGroup.links :+ new Link(Utils.newId(), name = Some(newLinkTitle.get), url = newLinkUrl.get, description = Some(newLinkDescription.get))
    //              } else {
    //                linkGroup.links
    //              }
    //              linkGroup.copy(links = links)
    //            })
    //          })
    //        })
    //      }
    //    }

    //    private def myOptions(): Buffer[Either[Project, LinkGroup]] = allCategories.map(cs => cs.flatMap(_.projects).flatMap { p =>
    //      select.Option(p.name).enabled(value = false) +: p.linkGroups.map(g => select.Option("-- " + g.name).onClick { _ =>
    //        println("clicked!!!")
    //        selectedLinkGroup := g
    //      })
    //    })

    def apply(linkGroup: LinkGroup, showLinkForm: Var[Boolean]) = {
      selectedLinkGroup := linkGroup
      ".link-form" >>> div(
        div(selectedLinkGroup.map(_.toString)),
        div("Link form"),
        div(
          div(text().bind(newLinkTitle).placeholder("Title")),
          div(text().bind(newLinkUrl).placeholder("URL")),
          div(text().bind(newLinkDescription).placeholder("description"))
        ),
        div(
          button("Close").onClick(_ => showLinkForm := false),
          button("OK").onClick { _ =>
            //            createLink()
            showLinkForm := false
          }
        )
      ).show(showLinkForm)
    }
  }

}
