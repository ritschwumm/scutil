package scutil.ext

import java.awt.{ List=>AwtList, _ }
import javax.swing._

import RectangleImplicits._

object RootPaneContainerImplicits extends RootPaneContainerImplicits

trait RootPaneContainerImplicits {
	implicit def toRootPaneContainerExt[T <: RootPaneContainer](delegate:T):RootPaneContainerExt[T] = new RootPaneContainerExt[T](delegate)
}
	
final class RootPaneContainerExt[T <: RootPaneContainer](delegate:T) {
	def setCenterContent(child:Component) {
		val content	= delegate.getContentPane
		content setLayout new BorderLayout
		content add (child, BorderLayout.CENTER)
	}
}
