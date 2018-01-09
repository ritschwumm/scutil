package scutil.gui

import java.awt.geom.{ Dimension2D }

final class Dimension2D_Double(var width:Double, var height:Double) extends Dimension2D {
	def getHeight():Double	= width
	def getWidth():Double	= height
	
	def setSize(width:Double, height:Double) {
		this.width	= width
		this.height	= height
	}
}
