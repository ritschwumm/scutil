package scutil.gui.extension

import java.awt.{ List as _, * }
import javax.swing.*

object RootPaneContainerExtensions {
	implicit final class RootPaneContainerExt(peer:RootPaneContainer) {
		def setCenterContent(child:Component):Unit	= {
			val content	= peer.getContentPane
			content.setLayout(new BorderLayout)
			content.add(child, BorderLayout.CENTER)
		}
	}
}
