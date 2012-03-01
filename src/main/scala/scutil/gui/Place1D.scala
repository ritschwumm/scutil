package scutil.gui

object Place1D {
	val Alone	= Place1D(false,	false)
	val Start	= Place1D(false,	true)
	val Center	= Place1D(true,		true)
	val End		= Place1D(true,		false)
}

case class Place1D(before:Boolean, after:Boolean)
