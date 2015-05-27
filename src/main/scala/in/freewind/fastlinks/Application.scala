package in.freewind.fastlinks

import libs.{NodeWebkit, NodeJs}
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.widok.{ReadChannel, Opt, PageApplication, Var, View}
import org.widok.html._
import upickle._

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends PageApplication {

  val keyword = Var[String]("")
  val selected: Opt[Link] = Opt()
  val editing = Var[Boolean](false)

  val creatingLinkInGroup = Var[Option[LinkGroup]](None)

  keyword.combine(selected).attach { case (key, link) =>
    // saveSnapshot(key, link)
  }

  val meta = Var[Option[Meta]](None)
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

  override def view(): View = div(
    div(creatingLinkInGroup.get.map(_.toString).getOrElse[String]("")),
    div(
      button("Edit").onClick(_ => editing := !editing.get)
    ),
    div(
      div(
        uiSearchInput(),
        button("Clear").css("clear-search").onClick(_ => keyword := "")
      ).css("search-panel")
    ),
    div(tree.map(categories =>
      div(categories.map(category =>
        div(
          uiCategoryName(category),
          div(
            category.projects.map(project =>
              div(
                div(
                  uiProjectName(project),
                  div(project.linkGroups.map(linkGroup =>
                    div(
                      uiGroupName(linkGroup),
                      div(
                        linkGroup.links.map(uiLink),
                        div(
                          button("+ link").onClick(_ => creatingLinkInGroup := Some(linkGroup))
                        ).show(editing)
                      ).css("link-group-links")
                    ).css("link-group")
                  )).css("link-groups"),
                  div(
                    button("+ group")
                  ).show(editing)
                ).css("project"),
                div().css("project-separator"))
            ).map(div(_))
          ).css("projects")
        ).css("category")
      )))
    ).css("search-results"),
    uiForms()
  ).id("main-page")


  private val newLinkTitle = Var[String]("")
  private val newLinkUrl = Var[String]("")
  private val newLinkDescription = Var[String]("")

  private def uiForms() = div(
    div(
      div("Link form"),
      div(
        div(text().bind(newLinkTitle).placeholder("Title")),
        div(text().bind(newLinkUrl).placeholder("URL")),
        div(text().bind(newLinkDescription).placeholder("description"))
      ),
      div(
        button("Close").onClick(_ => clearCreatingLinkVars()),
        button("OK").onClick { _ =>
          createLink(creatingLinkInGroup.get.get)
          clearCreatingLinkVars()
        }
      )
    ).css("link-form")
      .show(creatingLinkInGroup.map(_.isDefined))
  )

  private def clearCreatingLinkVars(): Unit = {
    newLinkTitle := ""
    newLinkUrl := ""
    newLinkDescription := ""
    creatingLinkInGroup := None
  }

  private def createLink(linkGroup: LinkGroup): Unit = {
    meta := meta.get.map { mmm =>
      mmm.copy(categories = mmm.categories.map { category =>
        category.copy(projects = category.projects.map { project =>
          project.copy(linkGroups = project.linkGroups.map { linkGroup =>
            linkGroup.copy(links = linkGroup.links :+
              new Link(Utils.newId(), name = Some(newLinkTitle.get), url = newLinkUrl.get, description = Some(newLinkDescription.get)))
          })
        })
      })
    }
  }

  private def uiSearchInput() = {
    text()
      .css("search")
      .bind(keyword)
      // Note: must be `onKeyUp` rather than `onKeyPress/onKeyDown`
      .onKeyUp(e => moveSelection(e))
      .placeholder("Search").autofocus(value = true)
  }

  private def uiCategoryName(category: Category) = {
    div(highlight(category.name, keyword.get))
      .css("category-name")
  }

  private def uiProjectName(project: Project) = {
    div(span(highlight(project.name, keyword.get)), stars(project.stars))
      .css("project-name")
  }

  private def uiGroupName(linkGroup: LinkGroup) = {
    div(highlight(linkGroup.name, keyword.get))
      .css("link-group-name")
  }

  private def uiLink(link: Link) = {
    div(
      span(highlight(link.name.getOrElse(""), keyword.get))
        .css("link-name"),
      a(highlight(link.url, keyword.get)).url(link.url).attribute("target", "_blank")
        .css("link-url")
        .onClick { e: dom.MouseEvent =>
        e.preventDefault()
        openLink(link.url)
      })
      .css("link")
      .cssState(selected.is(link), "highlight-search-item")
  }

  private def stars(value: Option[Int]) = value match {
    case Some(n) => span((1 to n).toList.map(_ => span("★"))).css("stars")
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
    case MatchedString(c) => span(c).css("highlight-char")
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
