package in.freewind.fastlinks

import libs.NodeWebkit
import org.scalajs.dom
import org.scalajs.dom.{KeyboardEvent}
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.HTMLInputElement
import org.widok.bindings.Bootstrap.{Table, Button}
import org.widok.{DOM, InstantiatedRoute, Page, ReadChannel, Opt, Var, View}
import org.widok.html._
import upickle._
import LayoutWithSelectors._

case class MainPage() extends Page {

  val projectUrl = "https://github.com/freewind/fast-links"

  val keyword = Var[String]("")
  val selected: Opt[Link] = Opt()

  val sidebarToggled = Var(true)
  val showHelp = Var(false)

  //  val creatingLinkInGroup = Var[Option[LinkGroup]](None)

  keyword.combine(selected).attach { case (key, link) =>
    // saveSnapshot(key, link)
  }

  NodeWebkit.gui.Window.get().on("focus", () => focusOnSearchInput())

  val tree: ReadChannel[Seq[Category]] = keyword.combine(DataStore.meta).distinct.map({ case (k, m) => (k.trim.toLowerCase, m.toSeq.flatMap(_.categories)) }).map {
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
      case KeyCode.Up =>
        list.get.span(_ != selected.get)._1.lastOption.orElse(list.get.lastOption).foreach(selected := _)
      case KeyCode.Down =>
        list.get.span(_ != selected.get)._2.tail.headOption.orElse(list.get.headOption).foreach(selected := _)
      case KeyCode.Enter => {
        selected.toOption match {
          case Some(link) => openLink(link.url)
          case _ =>
        }
      }
      case KeyCode.Escape => keyword := ""
      case _ =>
    }
  }

  private def openLink(url: String) = NodeWebkit.gui.Shell.openExternal(url)

  override def view(): View = "#main-page" >>> div(
    "#wrapper" >>> div(
      "#sidebar-wrapper" >>> div(
        ".sidebar" >>> sidebar()
      ),
      "#page-content-wrapper" >>> div(
        ".container-fluid" >>> div(
          ".row" >>> div(
            ".col-lg-12" >>> div(
              ".main-content" >>> mainContent()
            )
          )
        )
      ),
      "#help" >>> div(
        Table(
          tr(
            td("Show help"), td("cmd + /")
          ),
          tr(
            td("Toggle sidebar"), td("cmd + 1")
          ),
          tr(
            td("Focus on Search"), td("cmd + s")
          )
        ),
        div("project: ", a(projectUrl).url(projectUrl).onClick(_ => openLink(projectUrl))),
        div("local data file: ", span(DataStore.config.map(_.map(_.dataFilePath).getOrElse[String]("not chose"))))
      ).show(showHelp)
    ).cssState(sidebarToggled, "toggled")
  )

  document.keyDown.attach { event =>
    if (event.metaKey) {
      event.keyCode match {
        case KeyCode.Num1 => sidebarToggled.update(!_)
        case 191 /* slash */ => showHelp.update(!_)
        case KeyCode.S => focusOnSearchInput()
        case _ => println("keycode: " + event.keyCode)
      }
    }
  }

  private def toggleSidebarKey(event: dom.KeyboardEvent) = {
    event.metaKey && event.keyCode == KeyCode.Num1
  }

  private def toggleHelpKey(event: dom.KeyboardEvent) = {
    event.metaKey && event.keyCode == 191 // slash
  }

  private def sidebar() = div(DataStore.allCategories.map(categories =>
    ".categories" >>> div(categories.map(category =>
      ".category" >>> div(
        ".category-name" >>> div(category.name),
        ".projects" >>> div(category.projects.map(p =>
          ".project" >>> div(
            ".project-name" >>> div(p.name).onClick(_ => scrollTo(p.id))
          )
        ))
      )
    ))
  ))

  private def scrollTo(projectId: String): Unit = {
    dom.document.getElementById(projectId).scrollIntoView(true)
  }

  private def mainContent() = div(
    div(
      "#menu-toggle" >>> Button("Toggle Sidebar").title("cmd + 1").onClick(_ => sidebarToggled.update(!_)),
      Button("Edit").onClick(_ => Entry.editPage().go())
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
                s"#${project.id}.project" >>> div(
                  uiProjectName(project),
                  ".link-groups" >>> div(project.linkGroups.map({ linkGroup =>
                    ".link-group" >>> div(
                      uiGroupName(linkGroup),
                      ".link-group-links" >>> div(
                        linkGroup.links.map(uiLink)
                      )
                    )
                  }))
                ),
                ".project-separator" >>> div()
              )
            ).map(div(_))
          )
        )
      )))
    )
  )

  private def uiSearchInput() = {
    "#search-input.search" >>> text()
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

  override def ready(route: InstantiatedRoute): Unit = {
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

  private def focusOnSearchInput(): Unit = {
    DOM.getElement("search-input").foreach(text =>
      text.asInstanceOf[HTMLInputElement].focus()
    )
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
