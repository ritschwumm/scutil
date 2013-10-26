package scutil.pimp

import java.awt.{ List=>AwtList, _ }
import javax.swing._

import scutil.pimp.RectangleImplicits._

object RootPaneContainerImplicits extends RootPaneContainerImplicits

trait RootPaneContainerImplicits {
	implicit def toRootPaneContainerExt(peer:RootPaneContainer):RootPaneContainerExt	= new RootPaneContainerExt(peer)
}
	
final class RootPaneContainerExt(peer:RootPaneContainer) {
	def setCenterContent(child:Component) {
		val content	= peer.getContentPane
		content setLayout	new BorderLayout
		content add			(child, BorderLayout.CENTER)
	}
}
