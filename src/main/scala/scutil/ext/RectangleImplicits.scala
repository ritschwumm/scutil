package scutil.ext

import java.awt.{ List=>AwtList, _ }

object RectangleImplicits extends RectangleImplicits

trait RectangleImplicits {
	implicit def toRectangleExt[T <: Rectangle](delegate:T):RectangleExt[T] = new RectangleExt[T](delegate)
}
	
final class RectangleExt[T <: Rectangle](delegate:T) {
	def inset(insets:Insets):Rectangle =
			new Rectangle(
					delegate.x + insets.left,
					delegate.y + insets.top,
					delegate.width - insets.left - insets.right,
					delegate.height - insets.top - insets.bottom)
}
