package in.freewind.fastlinks

import libs.{NodeWebkit, NodeJs}
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.widok.bindings.Bootstrap._
import org.widok.{InstantiatedRoute, Page, ReadChannel, Opt, Var, View}
import org.widok.html._
import upickle._
import LayoutWithSelectors._

import scala.concurrent.ExecutionContext.Implicits.global

case class EditPage() extends Page {

  val selectedProject = Opt[Project]()
  val toggled = Var(false)

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
              "#menu-toggle.btn.btn-default" >>> button("Toggle Menu").onClick(_ => toggled.update(!_)),
              ".main-content" >>> mainContent()
            )
          )
        )
      )
    ).cssState(toggled, "toggled")
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
    Button(Glyphicon.Ok()).size(Size.ExtraSmall).title("Done").onClick(_ => Entry.mainPage().go()),
    ".project-profile" >>> div(selectedProject.map(project =>
      div(
        ".project" >>> div(
          ".project-name" >>> div(project.name),
          ".link-groups" >>> div(project.linkGroups.map({ linkGroup =>
            ".link-group" >>> div(
              ".link-group-name" >>> div(linkGroup.name),
              ".link-group-links" >>> div(
                linkGroup.links.map(link =>
                  ".link" >>> div(
                    ".link-name" >>> span(link.name.getOrElse[String]("")),
                    ".link-url" >>> a(link.url).url(link.url).attribute("target", "_blank")
                  )
                )
              )
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
