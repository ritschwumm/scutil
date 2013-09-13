package scutil.pimp

import java.awt.{ List=>AwtList, _ }
import javax.swing._

import RectangleImplicits._

object RootPaneContainerImplicits extends RootPaneContainerImplicits

trait RootPaneContainerImplicits {
	implicit def toRootPaneContainerExt(delegate:RootPaneContainer):RootPaneContainerExt	= new RootPaneContainerExt(delegate)
}
	
final class RootPaneContainerExt(delegate:RootPaneContainer) {
	def setCenterContent(child:Component) {
		val content	= delegate.getContentPane
		content setLayout	new BorderLayout
		content add			(child, BorderLayout.CENTER)
	}
}
