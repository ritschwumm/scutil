package scutil.ext

import java.awt.{ List=>AwtList, _ }
import javax.swing._

import scutil.geom._

object WindowImplicits extends WindowImplicits

trait WindowImplicits {
	implicit def toWindowExt(delegate:Window):WindowExt	= new WindowExt(delegate)
}
	
final class WindowExt(delegate:Window) {
	def restrictToScreen() {
		val frame	= IntRect fromAwtRectangle delegate.getBounds
		val screen	= IntRect fromAwtRectangle delegate.getGraphicsConfiguration.getBounds
		delegate setBounds (frame restrictTo screen).toAwtRectangle
	}
}
