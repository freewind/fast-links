package in.freewind.fastlinks

import in.freewind.fastlinks.models._
import libs.NodeJs
import org.widok.html._
import org.widok.{Opt, PageApplication, Var}

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends PageApplication {

  val editing: Var[Boolean] = Var(false)
  val meta: Opt[Meta] = Opt()
  val selectedProject: Var[Option[Project]] = Var(None)

  meta.attach(meta => meta.categories.headOption.map(_.projects.headOption).foreach(selectedProject := _))

  def view() = {
    div(
      header(
        pageHeader(),
        searchPanel()
      ),
      div(
        sidebar(),
        projectProfile()
      ).css("content-wrapper").cssState(editing, "editing")
    )
  }

  private def sidebar() = div(
    meta.map(_.categories.map { category =>
      oneCategory(category)
    }).map(div(_).css("category-list"))
  ).css("sidebar")

  private def oneCategory(category: Category) = div(
    div(category.name).css("category-name"),
    div(category.projects.map(project => projectNameStars(project))).css("project-list")
  ).css("category")

  def projectNameStars(project: Project) = div(
    span(project.name).css("project-name"), projectStars(project)
  ).css("project")
    .cssState(selectedProject.is(Some(project)), "current-project")
    .onClick(_ => selectedProject := Some(project))

  private def projectStars(project: Project) = {
    project.stars.map(stars =>
      span((0 until stars).map(_ => i().css("icon-star"))).css("stars")
    ).getOrElse(span())
  }

  private def projectProfile() = div(
    selectedProject.map { pp => pp.map(project =>
      div(
        div(project.name).css("profile-header"),
        div(project.stars.getOrElse[Int](-1)).css("project-stars"),
        div(
          project.linkGroups.map { linkGroup =>
            div(
              linkGroup.links.map { link =>
                div(
                  span(link.name.getOrElse[String]("")),
                  span(link.url)
                ).css("link")
              }
            )
          }
        ).css("content")
      ).css("profile-content")
    )
    }
  ).css("project-profile")

  private def pageHeader() = div(
    span(button("Edit").css("edit")).css("config-panel")
  ).css("header")

  private def searchPanel() = div(
    text().placeholder("Search").css("search"),
    span(
      i().css("icon-cancel-circled")
    ).css("clear-search")
  ).css("search-panel")

  def ready() {
    NodeJs.readFile("/Users/twer/workspace/fast-links/data.json").map(upickle.read[Meta]).map { meta =>
      meta.copy(categories = meta.categories.map(category => category.copy(projects = category.projects.sortBy(_.name))))
    }.foreach(meta := _)
  }
}

