package in.freewind.fastlinks

import java.util.UUID

object Utils {
  def newId() = UUID.randomUUID().toString.replace("-", "").toUpperCase
}
