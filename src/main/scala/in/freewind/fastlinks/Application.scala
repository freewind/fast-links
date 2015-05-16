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

  def view() = div(
    div(
      div(button("Edit!!!"))
    ).css("header"),
    div(
      div(
        meta.map(_.categories.map { category =>
          div(
            div(category.name),
            div(category.projects.map(project =>
              div(
                div(project.name)
                  .css("project").cssState(selectedProject.is(Some(project)), "current-project")
                  .onClick(_ => selectedProject := Some(project))
              )
            )).css("project-list")
          ).css("category")
        }).map(div(_).css("category-list"))
      ).css("sidebar"),
      div(
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
    ).css("content-wrapper").cssState(editing, "editing")
  )

  def ready() {

    NodeJs.readFile("/Users/twer/workspace/fast-links/data.json").map(upickle.read[Meta]).map { meta =>
      meta.copy(categories = meta.categories.map(category => category.copy(projects = category.projects.sortBy(_.name))))
    }.foreach(meta := _)
  }
}

