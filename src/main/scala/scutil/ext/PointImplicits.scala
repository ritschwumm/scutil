package scutil.ext

import java.awt.{ List=>AwtList, _ }

import scutil.geom._

object PointImplicits extends PointImplicits

trait PointImplicits {
	implicit def toPointExt(delegate:Point):PointExt	= new PointExt(delegate)
}
	
final class PointExt(delegate:Point) {
	def unary_- :Point	= new Point(
			-delegate.x,
			-delegate.y)
			
	def +(that:Point):Point	=  new Point(
			delegate.x + that.x,
			delegate.y + that.y)
	
	def -(that:Point):Point	=  new Point(
			delegate.x	- that.x,
			delegate.y	- that.y)
			
	def *(that:Point):Point	=  new Point(
			delegate.x  * that.x,
			delegate.y	* that.y)
			
	def /(that:Point):Point	=  new Point(
			delegate.x / that.x,
			delegate.y / that.y)
			
	/*
	def *(factor:Int):Point	=  new Point(
			delegate.x  * factor,
			delegate.y * factor)
			
	def /(factor:Int):Point	=  new Point(
			delegate.x / factor,
			delegate.y / factor)
	*/

	//------------------------------------------------------------------------------
			
	def moveBy(dimension:Dimension):Point	= new Point(
			delegate.x + dimension.width,
			delegate.y + dimension.height)
			
	def sizeTo(that:Point):Dimension	= new Dimension(
			that.x - delegate.x,
			that.y - delegate.y)
	
	def rectangleTo(that:Point):Rectangle	= new Rectangle(
			delegate.x,
			delegate.y,
			that.x-delegate.x,
			that.y-delegate.y)
	
	def rectangleWith(dimension:Dimension):Rectangle	= 
			new Rectangle(delegate, dimension)
			
	//------------------------------------------------------------------------------
			
	def toDimension:Dimension	= new Dimension(
			delegate.x,
			delegate.y)
			
	def toIntPoint:IntPoint	=
			IntPoint fromAwtPoint delegate
}
