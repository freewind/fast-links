package in.freewind.fastlinks

import java.util.UUID

package object utils {

  def newId() = UUID.randomUUID().toString.replace("-", "").toUpperCase

}
