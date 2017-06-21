package scutil.gui.pimp

import java.awt.{ List=>_, _ }
import javax.swing._

object RootPaneContainerImplicits extends RootPaneContainerImplicits

trait RootPaneContainerImplicits {
	implicit final class RootPaneContainerExt(peer:RootPaneContainer) {
		def setCenterContent(child:Component) {
			val content	= peer.getContentPane
			content setLayout	new BorderLayout
			content add			(child, BorderLayout.CENTER)
		}
	}
}
