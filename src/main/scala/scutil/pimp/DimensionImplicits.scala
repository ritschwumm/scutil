package scutil.pimp

import java.awt.{ List=>AwtList, _ }

import scutil.geom._

object DimensionImplicits extends DimensionImplicits

trait DimensionImplicits {
	implicit def toDimensionExt(delegate:Dimension):DimensionExt	= new DimensionExt(delegate)
}
	
final class DimensionExt(delegate:Dimension) {
	def unary_- :Dimension			= new Dimension(
			-delegate.width,
			-delegate.height)
			
	def +(that:Dimension):Dimension	=  new Dimension(
			delegate.width  + that.width,
			delegate.height + that.height)
	
	def -(that:Dimension):Dimension	=  new Dimension(
			delegate.width	- that.width,
			delegate.height	- that.height)
			
	def *(that:Dimension):Dimension	=  new Dimension(
			delegate.width  * that.width,
			delegate.height * that.height)
			
	def /(that:Dimension):Dimension	=  new Dimension(
			delegate.width  / that.width,
			delegate.height / that.height)
			
	/*
	def *(factor:Int):Dimension	=  new Dimension(
			delegate.width  * factor,
			delegate.height * factor)
			
	def /(factor:Int):Dimension	=  new Dimension(
			delegate.width  / factor,
			delegate.height / factor)
	*/
	
	//------------------------------------------------------------------------------
			
	def toPoint:Point	= new Point(
			delegate.width,
			delegate.height)
			
	def toIntPoint:IntPoint	=
			IntPoint fromAwtDimension delegate
}
