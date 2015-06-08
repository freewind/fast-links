package in.freewind.fastlinks

import libs.{NodeWebkit, NodeJs}
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.widok.{ReadChannel, Opt, PageApplication, Var, View}
import org.widok.html._
import upickle._
import LayoutWithSelectors._

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends PageApplication {

  val keyword = Var[String]("")
  val selected: Opt[Link] = Opt()
  val editing = Var[Boolean](false)

  //  val creatingLinkInGroup = Var[Option[LinkGroup]](None)

  keyword.combine(selected).attach { case (key, link) =>
    // saveSnapshot(key, link)
  }

  val meta = Var[Option[Meta]](None)
  val allCategories = meta.map(_.toSeq.flatMap(_.categories))
  val tree: ReadChannel[Seq[Category]] = keyword.combine(meta).distinct.map({ case (k, m) => (k.trim.toLowerCase, m.toSeq.flatMap(_.categories)) }).map {
    case (key, cs) if key.isEmpty => cs
    case (key, cs) => cs.flatMap { category =>
      val myProjects = category.projects.flatMap { project =>
        if (projectMatches(project, key)) {
          Some(project)
        } else {
          val myLinkGroups = project.linkGroups.flatMap { linkGroup =>
            if (linkGroupMatches(linkGroup, key)) {
              Some(linkGroup)
            } else {
              linkGroup.links.filter(linkMatches(_, key)) match {
                case Nil => None
                case links => Some(linkGroup.copy(links = links))
              }
            }
          }
          myLinkGroups match {
            case Nil => None
            case linkGroups => Some(project.copy(linkGroups = linkGroups))
          }
        }
      }
      myProjects match {
        case Nil => None
        case projects => Some(category.copy(projects = projects))
      }
    }
  }
  val list: Var[Seq[Link]] = Var(Nil)
  tree.attach { t =>
    list := findAllItems(t)
  }


  val firstPossibleSelection: ReadChannel[Option[Link]] = list.map(_.headOption)
  firstPossibleSelection.attach {
    case Some(link) => if (selected.isEmpty$ || !list.get.contains(selected.get)) selected := link
    case _ =>
  }

  private def findAllItems(categories: Seq[Category]): Seq[Link] = {
    for {
      category <- categories
      project <- category.projects
      linkGroup <- project.linkGroups
      link <- linkGroup.links
    } yield link
  }

  def categoryMatches(category: Category, keyword: String): Boolean = textMatches(category.name, keyword)
  def projectMatches(project: Project, keyword: String): Boolean = textMatches(project.name, keyword)
  def linkGroupMatches(linkGroup: LinkGroup, keyword: String): Boolean = textMatches(linkGroup.name, keyword)
  def linkMatches(link: Link, keyword: String): Boolean = link.name.exists(textMatches(_, keyword)) || textMatches(link.url, keyword)
  private def textMatches(name: String, keyword: String): Boolean = {
    name.toLowerCase.contains(keyword) || Matches.flexMatches(name.toLowerCase, keyword)
  }

  def moveSelection(event: KeyboardEvent): Unit = {
    event.keyCode match {
      case KeyCode.up =>
        list.get.span(_ != selected.get)._1.lastOption.orElse(list.get.lastOption).foreach(selected := _)
      case KeyCode.down =>
        list.get.span(_ != selected.get)._2.tail.headOption.orElse(list.get.headOption).foreach(selected := _)
      case KeyCode.enter => {
        selected.toOption match {
          case Some(link) => openLink(link.url)
          case _ =>
        }
      }
      case KeyCode.escape => keyword := ""
      case _ =>
    }
  }

  private def openLink(url: String) = NodeWebkit.gui.Shell.openExternal(url)

  override def view(): View = "#main-page" >>> div(
    div(
      button("Edit").onClick(_ => editing := !editing.get)
    ),
    div(
      ".search-panel" >>> div(
        uiSearchInput(),
        ".clear-search" >>> button("Clear").onClick(_ => keyword := "")
      )
    ),
    ".search-results" >>> div(tree.map(categories =>
      div(categories.map(category =>
        ".category" >>> div(
          uiCategoryName(category),
          ".projects" >>> div(
            category.projects.map(project =>
              div(
                ".project" >>> div(
                  uiProjectName(project),
                  ".link-groups" >>> div(project.linkGroups.map({ linkGroup =>
                    val showLinkForm = Var(false)
                    ".link-group" >>> div(
                      uiGroupName(linkGroup),
                      ".link-group-links" >>> div(
                        linkGroup.links.map(uiLink),
                        div(
                          button("+ link").onClick(_ => showLinkForm := true)
                        ).show(editing),
                        new LinkForm().apply(linkGroup, showLinkForm)
                      )
                    )
                  })),
                  div(
                    button("+ group")
                  ).show(editing)
                ),
                ".project-separator" >>> div()
              )
            ).map(div(_))
          )
        )
      )))
    )
  )

  class LinkForm {
    val newLinkTitle = Var[String]("")
    val newLinkUrl = Var[String]("")
    val newLinkDescription = Var[String]("")
    val selectedLinkGroup = Opt[LinkGroup]()

    private def createLink(): Unit = {
      meta := meta.get.map { mmm =>
        mmm.copy(categories = mmm.categories.map { category =>
          category.copy(projects = category.projects.map { project =>
            project.copy(linkGroups = project.linkGroups.map { linkGroup =>
              val links = if (linkGroup == selectedLinkGroup.get) {
                linkGroup.links :+ new Link(Utils.newId(), name = Some(newLinkTitle.get), url = newLinkUrl.get, description = Some(newLinkDescription.get))
              } else {
                linkGroup.links
              }
              linkGroup.copy(links = links)
            })
          })
        })
      }
    }

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
            createLink()
            showLinkForm := false
          }
        )
      ).show(showLinkForm)
    }
  }

  private def uiSearchInput() = {
    ".search" >>> text()
      .bind(keyword)
      // Note: must be `onKeyUp` rather than `onKeyPress/onKeyDown`
      .onKeyUp(e => moveSelection(e))
      .placeholder("Search").autofocus(value = true)
  }

  private def uiCategoryName(category: Category) = {
    ".category-name" >>> div(highlight(category.name, keyword.get))
  }

  private def uiProjectName(project: Project) = {
    ".project-name" >>> div(span(highlight(project.name, keyword.get)), stars(project.stars))
  }

  private def uiGroupName(linkGroup: LinkGroup) = {
    ".link-group-name" >>> div(highlight(linkGroup.name, keyword.get))
  }

  private def uiLink(link: Link) = {
    ".link" >>> div(
      ".link-name" >>> span(highlight(link.name.getOrElse(""), keyword.get)),
      ".link-url" >>> a(highlight(link.url, keyword.get)).url(link.url).attribute("target", "_blank")
        .onClick { e: dom.MouseEvent =>
        e.preventDefault()
        openLink(link.url)
      })
      .cssState(selected.is(link), "highlight-search-item")
  }

  private def stars(value: Option[Int]) = value match {
    case Some(n) => ".stars" >>> span((1 to n).toList.map(_ => span("★")))
    case _ => span()
  }

  override def ready(): Unit = {
    NodeJs.readFile("/Users/twer/workspace/fast-links/data.json").map(upickle.read[Meta]).map { meta =>
      meta.copy(categories = meta.categories.map(category => category.copy(projects = category.projects.sortBy(_.name))))
    }.foreach(meta := Some(_))

  }

  private def highlight(content: String, keyword: String) = {
    Matches.containMatch(content, keyword) match {
      case Some(chars) => highlightChars(chars)
      case _ => Matches.flexMatchedResult(content, keyword) match {
        case Some(chars) => highlightChars(chars)
        case _ => Seq(span(content))
      }
    }
  }

  private def highlightChars(chars: Seq[ResultString]) = chars.map {
    case MatchedString(c) => ".highlight-char" >>> span(c)
    case NonMatchString(c) => span(c)
  }
}

object Matches {

  def flexMatches(text: String, keyword: String): Boolean = {
    keywordPossibilities(keyword).exists {
      case (left, right) => text.matches( s""".*$left.*$right.*""")
    }
  }

  private def keywordPossibilities(keyword: String): IndexedSeq[(String, String)] = {
    (1 to keyword.length - 1).map(keyword.splitAt)
  }

  def flexMatchedResult(text: String, keyword: String): Option[Seq[ResultString]] = {
    keywordPossibilities(keyword).foldRight(Option.empty[Seq[ResultString]]) {
      case (_, Some(r)) => Some(r)
      case ((left, right), _) => val Pattern = s"""(?i)(.*)($left)(.*)($right)(.*)""".r
        text match {
          case Pattern(n1, ll, n2, rr, n3) => Some(Seq(NonMatchString(n1), MatchedString(ll), NonMatchString(n2), MatchedString(rr), NonMatchString(n3)))
          case _ => None
        }
    }
  }

  def containMatch(text: String, keyword: String): Option[List[ResultString]] = {
    if (text.isEmpty || keyword.isEmpty) {
      None
    } else if (text.toLowerCase.contains(keyword.toLowerCase)) {
      val index = text.toLowerCase.indexOf(keyword.toLowerCase)
      Some(NonMatchString(text.substring(0, index))
        :: MatchedString(text.substring(index, index + keyword.length))
        :: NonMatchString(text.substring(index + keyword.length)) :: Nil)
    } else {
      None
    }
  }

}

sealed abstract class ResultString(str: String)
case class NonMatchString(str: String) extends ResultString(str)
case class MatchedString(str: String) extends ResultString(str)
