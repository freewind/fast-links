package in.freewind.fastlinks

import upickle._

object DataConverter {

  def stringify(category: Category): String = {
    write(category)
  }

  def parse(json: String): Category = {
    read[Category](json)
  }

}

object DataConverter2 {

  def stringify(categories: Categories): String = {
    write(categories)
  }

  def parse(json: String): Categories = {
    read[Categories](json)
  }

}
