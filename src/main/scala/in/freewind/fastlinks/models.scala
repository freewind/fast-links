package in.freewind.fastlinks

case class Meta(categories: Seq[Category])

case class Categories(categories: Seq[Category])

case class Category(id: String, name: String, projects: Seq[Project] = Nil, description: Option[String] = None)

case class Project(id: String,
                   name: String,
                   linkGroups: Seq[LinkGroup] = Nil,
                   description: Option[String] = None,
                   stars: Option[Int] = None)

case class LinkGroup(id: String, name: String, links: Seq[Link] = Nil)

case class Link(id: String, name: Option[String] = None, url: String, description: Option[String] = None, showUrl: Boolean = true)

