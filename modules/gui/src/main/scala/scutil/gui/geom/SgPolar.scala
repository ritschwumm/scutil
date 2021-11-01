package scutil.gui.geom

import scala.math._

object SgPolar {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= SgPolar(0, 0)
	val one		= SgPolar(1, 1)
}

final case class SgPolar(length:Double, angle:Double) {
	def +(that:SgPolar):SgPolar	= (this.toCartesian + that.toCartesian).toPolar
	def -(that:SgPolar):SgPolar	= (this.toCartesian - that.toCartesian).toPolar

	def *(value:Double):SgPolar	= SgPolar(length * value, angle)
	def /(value:Double):SgPolar	= SgPolar(length / value, angle)

	def normalize:SgPolar				= SgPolar(1, angle)
	def rotate(angle:Double):SgPolar	= SgPolar(length, this.angle + angle)

	def x:Double	= length * cos(angle)
	def y:Double	= length * sin(angle)

	//------------------------------------------------------------------------------
	//## kartesian conversion

	def toCartesian:SgPoint	= SgPoint(x, y)
}
