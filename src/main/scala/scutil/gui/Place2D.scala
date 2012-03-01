package scutil.gui

import java.awt.Insets

import scutil.Implicits._

object Place2D {
	import Place1D._
	
	val TopLeft			= Place2D(Start,	Start)
	val TopCenter		= Place2D(Center,	Start)
	val TopRight		= Place2D(End,		Start)
	val CenterLeft		= Place2D(Start,	Center)
	val CenterCenter	= Place2D(Center,	Center)
	val CenterRight		= Place2D(End,		Center)
	val BottomLeft		= Place2D(Start,	End)
	val BottomCenter	= Place2D(Center,	End)
	val BottomRight		= Place2D(End,		End)
	
	val AloneLeft		= Place2D(Start,	Alone)
	val AloneCenter		= Place2D(Center,	Alone)
	val AloneRight		= Place2D(End,		Alone)
	
	val TopAlone		= Place2D(Alone,	Start)
	val CenterAlone		= Place2D(Alone,	Center)
	val BottomAlone		= Place2D(Alone,	End)
	
	val AloneAlone		= Place2D(Alone,	Alone)
}

case class Place2D(horizontal:Place1D, vertical:Place1D) {
	def insets(hgap:Int, vgap:Int):Insets	= new Insets(
		vertical.before		cata (vgap, 0),
		horizontal.before	cata (hgap, 0),
		vertical.after		cata (vgap, 0),
		horizontal.after	cata (hgap, 0)
	)
}
