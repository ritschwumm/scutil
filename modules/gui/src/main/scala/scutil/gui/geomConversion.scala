package scutil.gui

import java.awt._
import java.awt.geom._

import scutil.geom._

object geomConversion {
	def IntPoint_Point(it:IntPoint):Point			= new Point(it.x, it.y)
	def Point_IntPoint(it:Point):IntPoint			= IntPoint(it.x, it.y)

	def IntPoint_Dimension(it:IntPoint):Dimension	= new Dimension(it.x, it.y)
	def Dimension_IntPoint(it:Dimension):IntPoint	= IntPoint(it.width, it.height)

	def IntRect_Rectangle(it:IntRect):Rectangle		= new Rectangle(it.left, it.top, it.width, it.height)
	def Rectangle_IntRect(it:Rectangle):IntRect		= IntRect leftTopWidthHeight (it.x, it.y, it.width, it.height)

	//------------------------------------------------------------------------------

	def DoublePoint_Point2D(it:DoublePoint):Point2D			= new Point2D.Double(it.x, it.y)
	def Point2D_DoublePoint(it:Point2D):DoublePoint			= DoublePoint(it.getX, it.getY)

	def DoublePoint_Dimension2D(it:DoublePoint):Dimension2D	= new Dimension2D_Double(it.x, it.y)
	def Dimension2D_DoublePoint(it:Dimension2D):DoublePoint	= DoublePoint(it.getWidth, it.getHeight)

	def DoubleRect_Rectangle2D(it:DoubleRect):Rectangle2D	= new Rectangle2D.Double(it.left, it.top, it.width, it.height)
	def Rectangle2D_DoubleRect(it:Rectangle2D):DoubleRect	= DoubleRect leftTopWidthHeight (it.getX, it.getY, it.getWidth, it.getHeight)
}
