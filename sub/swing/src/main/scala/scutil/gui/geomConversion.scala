package scutil.gui

import java.awt.Point
import java.awt.Dimension
import java.awt.Rectangle

import scutil.geom._

object geomConversion {
	def IntPoint_Point(it:IntPoint):Point			= new Point(it.x, it.y)
	def Point_IntPoint(it:Point):IntPoint			= IntPoint(it.x, it.y)
	def IntPoint_Dimension(it:IntPoint):Dimension	= new Dimension(it.x, it.y)
	def Dimension_IntPoint(it:Dimension):IntPoint	= IntPoint(it.width, it.height)
	def IntRect_Rectangle(it:IntRect):Rectangle		= new Rectangle(it.left, it.top, it.width, it.height)
	def Rectangle_IntRect(it:Rectangle):IntRect		= IntRect leftTopWidthHeight (it.x, it.y, it.width, it.height)
}
