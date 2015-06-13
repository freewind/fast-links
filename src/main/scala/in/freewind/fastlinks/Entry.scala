package in.freewind.fastlinks

import org.widok.{Router, Route, Application}

object Entry extends Application {
  val mainPage = Route("/", MainPage)
  val editPage = Route("/edit", EditPage)
  val chooseDataFilePage = Route("/choose-data-file", ChooseDataFilePage)

  val routes = Set(mainPage, editPage, chooseDataFilePage)

  override def main() {
    val router = Router(routes, fallback = Some(mainPage))
    router.listen()
  }

}
