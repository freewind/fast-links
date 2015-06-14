package in.freewind.fastlinks

import org.widok.bindings.Bootstrap.Button
import org.widok.html.{file, _}
import org.widok.{InstantiatedRoute, Page, Var, View, WriteChannel}

case class ChooseDataFilePage() extends Page {

  val dataFilePath = Var("")

  override def ready(route: InstantiatedRoute): Unit = {
    DataStore.loadData()
    DataStore.config.get match {
      case Some(path) => dataFilePath := path.dataFilePath
      case _ =>
    }
  }

  override def view(): View = div(
    div(dataFilePath.map("Current data file: " + _)),
    div("Choose the data file:"),
    file().accept("application/json").bind(dataFilePath.asInstanceOf[WriteChannel[String]]),
    dataFilePath.filterNot(_.isEmpty).map { path =>
      Button("Save & Return").onClick(_ => {
        DataStore.changeDataFilePath(dataFilePath.get)
        Entry.mainPage().go()
      })
    }
  )

}
