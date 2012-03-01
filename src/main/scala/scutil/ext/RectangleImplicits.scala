package scutil.ext

import java.awt.{ List=>AwtList, _ }

object RectangleImplicits extends RectangleImplicits

trait RectangleImplicits {
	implicit def toRectangleExt(delegate:Rectangle):RectangleExt	= new RectangleExt(delegate)
}
	
final class RectangleExt(delegate:Rectangle) {
	def topLeft:Point	= new Point(
			delegate.x,
			delegate.y)
	
	def topRight:Point	= new Point(
			delegate.x + delegate.width,
			delegate.y)
			
	def bottomLeft:Point	= new Point(
			delegate.x,
			delegate.y + delegate.height)
			
	def bottomRight:Point	= new Point(
			delegate.x + delegate.width,
			delegate.y + delegate.height)
			
	//------------------------------------------------------------------------------
			
	def inset(insets:Insets):Rectangle	= new Rectangle(
			delegate.x + insets.left,
			delegate.y + insets.top,
			delegate.width	- insets.left	- insets.right,
			delegate.height	- insets.top	- insets.bottom)
}
