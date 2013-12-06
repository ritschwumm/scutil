package scutil.pimp

import java.awt.{ List=>AwtList, _ }
import javax.swing._

import scutil.geom._

object WindowImplicits extends WindowImplicits

trait WindowImplicits {
	implicit def toWindowExt(peer:Window):WindowExt	= new WindowExt(peer)
}
	
final class WindowExt(peer:Window) {
	def restrictToScreen() {
		val frame	= IntRect fromAwtRectangle peer.getBounds
		val screen	= IntRect fromAwtRectangle peer.getGraphicsConfiguration.getBounds
		peer setBounds (frame restrictTo screen).toAwtRectangle
	}
}
