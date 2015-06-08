package in.freewind.fastlinks

import org.widok.Widget

object LayoutWithSelectors {

  implicit def selectors2layout(selector: String) = new {
    val (id, cssClasses) = selector.split('.').map(_.trim).filterNot(_.isEmpty).toList.partition(_.startsWith("#")) match {
      case (id :: Nil, classes) => (id.substring(1), classes)
      case (Nil, classes) => ("", classes)
      case (_ :: _, _) => throw new IllegalArgumentException("Too many ids for :" + selector)
    }
    def >>>[T <: Widget[T]](widget: T) = widget.id(id).css(cssClasses: _*)
  }

}
