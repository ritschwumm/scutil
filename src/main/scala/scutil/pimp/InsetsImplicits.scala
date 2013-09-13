package scutil.pimp

import java.awt.{ List=>AwtList, _ }

object InsetsImplicits extends InsetsImplicits

trait InsetsImplicits {
	implicit def toInsetsExt(delegate:Insets):InsetsExt	= new InsetsExt(delegate)
}
	
final class InsetsExt(delegate:Insets) {
	def unary_! :Insets			= new Insets(
			-delegate.top,
			-delegate.left,
			-delegate.bottom,
			-delegate.right)
			
	def +(that:Insets):Insets	= new Insets(
			delegate.top	+ that.top,
			delegate.left	+ that.left,
			delegate.bottom	+ that.bottom,
			delegate.right	+ that.right)
			
	def -(that:Insets):Insets	= new Insets(
			delegate.top	- that.top,
			delegate.left	- that.left,
			delegate.bottom	- that.bottom,
			delegate.right	- that.right)
			
	def *(factor:Int):Insets	= new Insets(
			delegate.top	* factor,
			delegate.left	* factor,
			delegate.bottom	* factor,
			delegate.right	* factor)
			
	def /(factor:Int):Insets	= new Insets(
			delegate.top	/ factor,
			delegate.left	/ factor,
			delegate.bottom	/ factor,
			delegate.right	/ factor)
}
