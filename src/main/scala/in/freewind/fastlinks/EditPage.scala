package in.freewind.fastlinks

import org.widok.{InstantiatedRoute, View, Page}
import org.widok.html._

case class EditPage() extends Page {
  override def ready(route: InstantiatedRoute): Unit = ()
  override def view(): View = div(
    "edit page",
    button("done").onClick(_ => Entry.mainPage().go())
  )
}
