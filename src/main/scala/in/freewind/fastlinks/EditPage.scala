package in.freewind.fastlinks

import in.freewind.fastlinks.LayoutWithSelectors._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.widok.bindings.Bootstrap
import org.widok.bindings.Bootstrap._
import org.widok.html._
import org.widok.{Buffer, InstantiatedRoute, Opt, Page, Var, View}

case class EditPage() extends Page {

  val selectedProject = Opt[Project]()
  val onDraggingLink = Var[Option[Link]](None)

  DataStore.meta.attach { _ =>
    selectedProject.update(p => DataStore.findProject(p.id).getOrElse(p))
  }

  override def ready(route: InstantiatedRoute): Unit = {
    DataStore.loadData()
    if (DataStore.meta.get.isEmpty) {
      println("######## meta is empty")
      Entry.chooseDataFilePage().go()
    } else {
      DataStore.allCategories.toBuffer.get.flatten.flatMap(_.projects).headOption.foreach(selectedProject := _)
    }
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
              ".main-content" >>> mainContent()
            )
          )
        )
      )
    )
  )

  private def sidebar() = div(DataStore.allCategories.map(categories =>
    ".categories" >>> div(
      categories.map { category =>
        val showCategoryForm = Var(false)
        ".category" >>> div(
          div(
            ".category-name" >>> div(
              ".name" >>> span(category.name),
              ".ops" >>> span(
                editButton.onClick(_ => showCategoryForm := true),
                deleteButton.onClick(_ => if (dom.confirm("Are you sure to delete?")) {
                  DataStore.deleteCategory(category)
                })
              )
            ).show(showCategoryForm.map(!_)),
            ".category-form" >>> new CategoryForm(category).apply(showCategoryForm)
          ),
          ".projects" >>> div(
            category.projects.map(project => {
              val showProjectForm = Var(false)
              ".project" >>> div(
                ".project-name" >>> div(
                  ".name" >>> span(project.name),
                  ".ops" >>> span(
                    editButton.onClick(_ => showProjectForm := true),
                    deleteButton.onClick { e =>
                      e.stopPropagation()
                      if (dom.confirm("Are you sure to delete?")) DataStore.deleteProject(project)
                    }
                  )
                ).onClick(_ => selectedProject := project).show(showProjectForm.map(!_)),
                ".project-form" >>> new ProjectForm(project).apply(showProjectForm)
              )
            }
            ).toList: _*
          ),
          addButton("Project").onClick(_ => DataStore.createNewProject(category, Project(Utils.newId(), "< new project >")))
        )
      },
      addButton("Category").onClick(_ => DataStore.createCategory(Category(Utils.newId(), "< new category >")))
    )
  ))

  val saveChangesButtonText = Var("Save changes")

  private def mainContent() = div(
    div(
      Button(saveChangesButtonText).onClick { _ =>
        saveChangesButtonText := "Saving ..."
        DataStore.saveData()
        dom.setTimeout(() => saveChangesButtonText := "Save changes", 1000)
      },
      Button("Save changes & Return").onClick { _ => DataStore.saveData(); Entry.mainPage().go() },
      Button("Cancel editing").onClick { _ => DataStore.loadData(); Entry.mainPage().go() },
      ".choose-data-file-panel" >>> span(
        span(DataStore.config.map(_.map(_.dataFilePath).getOrElse[String]("No data file, please choose one first!"))),
        Button("Choose data file").onClick(_ => Entry.chooseDataFilePage().go())
      )
    ),
    ".project-profile" >>> div(selectedProject.map { project =>
      div(
        ".project" >>> div(
          ".project-name" >>> div(project.name),
          ".link-groups" >>> div(project.linkGroups.map({ linkGroup =>
            val showCreatingForm = Var(false)
            val showChangeLinkGroupNameForm = Var(false)
            val showMoveLinkGroupNameForm = Var(false)
            ".link-group" >>> div(
              ".link-group-name" >>> div(
                div(
                  ".name" >>> span(linkGroup.name),
                  ".ops" >>> span(
                    editButton.onClick(_ => showChangeLinkGroupNameForm := true),
                    changeParentButton.onClick(_ => showMoveLinkGroupNameForm := true),
                    deleteButton.onClick(_ => if (dom.confirm("Are you sure to delete?")) {
                      DataStore.deleteLinkGroup(linkGroup)
                    })
                  )
                ).show(showChangeLinkGroupNameForm.map(!_)),
                new ChangeLinkGroupNameForm(linkGroup).apply(showChangeLinkGroupNameForm),
                new MoveLinkGroupNameForm(project, linkGroup).apply(showMoveLinkGroupNameForm)
              ),
              ".links" >>> div(
                linkGroup.links.map { link =>
                  val showEditingForm = Var(false)
                  val showMovingForm = Var(false)
                  div(
                    dropPlaceholder(linkGroup, Some(link)),
                    ".link" >>> div(
                      span(
                        ".link-name" >>> span(link.name.getOrElse[String]("")),
                        ".link-url" >>> a(link.url).url(link.url).attribute("target", "_blank")
                      ),
                      ".ops" >>> span(
                        editButton.onClick(_ => showEditingForm := true),
                        changeParentButton.onClick(_ => showMovingForm := true),
                        deleteButton.onClick(_ => if (dom.confirm("Are you sure to delete?")) {
                          DataStore.deleteLink(link)
                        })
                      )
                    ).show(showEditingForm.map(!_))
                      .attribute("draggable", "true")
                      .cssState(onDraggingLink.is(Some(link)), "dragging")
                      .onDragStart(_ => onDraggingLink := Some(link))
                      .onDragEnd(_ => onDraggingLink := None),
                    new LinkForm(linkGroup, Some(link)).apply(showEditingForm),
                    new MovingForm(linkGroup, link).apply(showMovingForm)
                  )
                } :+ dropPlaceholder(linkGroup, None)
              ),
              addButton("Link").onClick(_ => showCreatingForm.update(!_)).show(showCreatingForm.map(!_)),
              new LinkForm(linkGroup, None).apply(showCreatingForm)
            )
          })),
          addButton("Link Group").onClick(_ => createNewLinkGroup(project))
        ),
        ".project-separator" >>> div()
      )
    })
  )

  def dropPlaceholder(linkGroup: LinkGroup, link: Option[Link]) = {
    val dragOver = Var(false)
    div().css("drop-placeholder").cssState(dragOver, "drag-over").cssState(onDraggingLink.map(_.isDefined), "on-dragging")
      .onDragEnter(_ => dragOver := true)
      .onDragOver(e => e.preventDefault())
      .onDragLeave(_ => dragOver := false)
      .onDrop { e =>
      e.preventDefault()
      dragOver := false
      (link, onDraggingLink.get) match {
        case (Some(lnk), Some(onDragging)) if onDragging != lnk =>
          DataStore.moveLinkBefore(onDragging, lnk)
          onDraggingLink := None
        case (None, Some(onDragging)) =>
          DataStore.changeLinkParent(onDragging, linkGroup)
          onDraggingLink := None
        case _ =>
      }
    }
  }

  def okButton(title: String): Button = {
    Button(Glyphicon.OkCircle()).css("btn-form").title(title)
  }
  def cancelButton: Button = {
    Button(Glyphicon.RemoveCircle()).css("btn-form").title("Cancel")
  }
  def addButton(text: String): Button = {
    Button(Glyphicon.Plus(), span(" " + text)).css("btn-form")
  }

  def deleteButton: Button = {
    Button(Glyphicon.Remove()).css("btn-op").title("delete")
  }
  def changeParentButton: Button = {
    Button(Glyphicon.Random()).css("btn-op").title("change parent")
  }
  def editButton: Bootstrap.Button = {
    Button(Glyphicon.Edit()).css("btn-op").title("edit")
  }

  private def createNewLinkGroup(project: Project): Unit = {
    val linkGroup = new LinkGroup(Utils.newId(), "< new link group >", Nil)
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
      ".link-form" >>> HorizontalForm(
        FormGroup(
          Input.Text().bind(newLinkTitle).placeholder("Title")
        ),
        FormGroup(
          Input.Text().bind(newLinkUrl).placeholder("URL")
        ),
        FormGroup(
          Input.Text().bind(newLinkDescription).placeholder("Description")
        ),
        FormGroup(
          okButton("OK"),
          cancelButton.onClick(_ => showLinkForm := false)
        )
      ).show(showLinkForm).onSubmit { _ =>
        createOrUpdateLink()
        showLinkForm := false
      }
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
        case Some(Right(targetLinkGroup)) if initLinkGroup != targetLinkGroup => DataStore.changeLinkParent(link, targetLinkGroup)
        case _ =>
      }
    }

    def apply(showForm: Var[Boolean]) = div(
      select().bind(myOptions(), showLinkGroupOptions, selectedLinkGroup),
      div(
        okButton("OK").onClick { _ =>
          moveLink()
          showForm := false
        },
        cancelButton.onClick(_ => showForm := false)
      )
    ).show(showForm)
  }

  class ChangeLinkGroupNameForm(linkGroup: LinkGroup) {
    val newGroupName = Var(linkGroup.name)
    def apply(showForm: Var[Boolean]) = div(
      div(text().bind(newGroupName)),
      div(
        okButton("OK").onClick(_ => updateGroupName()),
        cancelButton.onClick(_ => showForm := false)
      )
    ).show(showForm)

    private def updateGroupName(): Unit = {
      DataStore.updateLinkGroup(linkGroup.copy(name = newGroupName.get))
    }
  }

  class MoveLinkGroupNameForm(initProject: Project, linkGroup: LinkGroup) {
    private val targetProject = Var[Option[Project]](Some(initProject))
    private def myOptions(): Buffer[Project] = Buffer(DataStore.allCategories.map(cs => cs.flatMap(_.projects)).flatMapBuf(Buffer(_: _*)).get: _*)

    def apply(showForm: Var[Boolean]) = div(
      select().bind(myOptions(), (p: Project) => option(p.name), targetProject),
      okButton("OK").onClick(_ => moveLinkGroup()),
      cancelButton.onClick(_ => showForm := false)
    ).show(showForm)

    private def moveLinkGroup(): Unit = {
      targetProject.get match {
        case Some(project) if project != initProject => DataStore.changeLinkGroupParent(linkGroup, project)
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
