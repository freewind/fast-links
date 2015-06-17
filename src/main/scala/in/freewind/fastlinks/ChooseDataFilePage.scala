package in.freewind.fastlinks

import org.widok.bindings.Bootstrap.Button
import org.widok.html.{file, _}
import org.widok.{InstantiatedRoute, Page, Var, View, WriteChannel}

case class ChooseDataFilePage() extends Page {

  val dataDirPath = Var("")

  override def ready(route: InstantiatedRoute): Unit = {
    DataStore.loadData()
    DataStore.config.get match {
      case Some(path) => dataDirPath := path.dataDirPath
      case _ =>
    }
  }

  override def view(): View = div(
    div(dataDirPath.map("Current data dir: " + _)),
    div("Choose the data dir (which might contains 'meta.json'):"),
    file().attribute("webkitdirectory", "").bind(dataDirPath.asInstanceOf[WriteChannel[String]]),
    dataDirPath.filterNot(_.isEmpty).map { path =>
      Button("Save & Return").onClick(_ => {
        DataStore.changeDataFilePath(dataDirPath.get)
        Entry.mainPage().go()
      })
    }
  )

}
