package scutil.gui

import java.awt.{ List=>_, _ }

import scutil.base.implicits._

/** layouts two components horizontally or vertically such that the available space is divided at a fixed ratio */
final class RatioLayout(horizontal:Boolean, gap:Int, parts:Seq[Double]) extends LayoutManager {
	require(gap >= 0,			"precondition violated: gap >= 0")
	require(parts.nonEmpty,		"precondition violated: parts.size > 0")
	parts foreach { part =>
		require(part >= 0.0,	"precondition violated: part >= 0.0")
	}

	def addLayoutComponent(constraint:String, c:Component) {}
	def removeLayoutComponent(c:Component) {}

	// NOTE requires at least one part
	private def getPart(index:Int):Double	=
			parts(index % parts.size)

	// NOTE requires at least one component
	private def normalizedParts(count:Int):Seq[Double]	=
			0 until count map getPart into { all =>
				all map { _ / all.sum }
			}

	private def allGaps(count:Int):Int	=
			gap * (count-1)

	// TODO find a better way to calculate this
	def preferredLayoutSize(c:Container):Dimension	=
			minimumLayoutSize(c)

	@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
	def minimumLayoutSize(c:Container):Dimension	= {
		val components	= c.getComponents.toVector
		if (components.isEmpty)	return new Dimension(0, 0)

		val minSizes	= components map { _.getMinimumSize }

		// in the split direction the mimimum size is choosen such that the minimum size of both component is respected
		val parts		= normalizedParts(components.size)
		val gaps		= allGaps(components.size)
		val splits		= minSizes map (dir.dimensionSize)
		val mores		= (splits zipWith parts) { _ / _ } map { _.toInt }
		val split		= (splits.sum max mores.max) + gaps

		// in the non-split direction make sure all components fit
		val keeps		= minSizes map (rid.dimensionSize)
		val keep		= keeps.max

		dir mkDimension (split, keep)
	}

	def layoutContainer(c:Container) {
		val components	= c.getComponents.toVector
		if (components.isEmpty)	return

		val size:Dimension	= c.getSize
		val insets:Insets	= c.getInsets
		val bounds:Rectangle	= new Rectangle(
				insets.left,
				insets.bottom,
				size.width	- insets.left	- insets.right,
				size.height - insets.top	- insets.bottom)

		val start	= dir rectangleLocation	bounds
		val full	= dir rectangleSize		bounds

		val parts	= normalizedParts(components.size)
		val gaps	= allGaps(components.size)

		// space to distribute for components
		val dist	= full - gaps
		// component sizes
		val sizes	= parts map { it => (it * dist).toInt }
		// component starts
		val starts	= (sizes scanLeft start) { (start, size) => start + size + gap }
		// component spans
		val spans	= starts dropRight 1 zip sizes

		// other direction
		val startO	= rid rectangleLocation	bounds
		val sizeO	= rid rectangleSize		bounds

		val boundss	= spans map { case (start, size) => dir mkRectangle (start, startO, size, sizeO) }

		(components zip boundss) foreach { case (component, bounds) => component setBounds bounds }
	}

	//------------------------------------------------------------------------------

	private val dir	= Direction trueHorizontal horizontal
	private val rid	= dir.inverted

	private object Direction {
		def trueHorizontal(cond:Boolean):Direction	=
				if (cond)	Horizontal else Vertical

		def trueVertical(cond:Boolean):Direction	=
				trueHorizontal(cond).inverted
	}

	private sealed trait Direction {
		def inverted:Direction	= this match {
			case Horizontal	=> Vertical
			case Vertical	=> Horizontal
		}

		def pointPosition(p:Point):Int
		def dimensionSize(d:Dimension):Int
		def rectangleLocation(r:Rectangle):Int	= pointPosition(r.getLocation)
		def rectangleSize(r:Rectangle):Int		= dimensionSize(r.getSize)

		def mkPoint(active:Int, inactive:Int):Point
		def mkDimension(active:Int, inactive:Int):Dimension
		def mkRectangle(activePos:Int, inactivePos:Int, activeSize:Int, inactiveSize:Int):Rectangle
	}

	private case object Horizontal extends Direction {
		def pointPosition(p:Point):Int		= p.x
		def dimensionSize(d:Dimension):Int	= d.width

		def mkPoint(active:Int, inactive:Int):Point			= new Point(active, inactive)
		def mkDimension(active:Int, inactive:Int):Dimension	= new Dimension(active, inactive)
		def mkRectangle(activePos:Int, inactivePos:Int, activeSize:Int, inactiveSize:Int):Rectangle	=
				new Rectangle(activePos, inactivePos, activeSize, inactiveSize)
	}

	private case object Vertical extends Direction {
		def pointPosition(p:Point):Int			= p.y
		def dimensionSize(d:Dimension):Int	= d.height

		def mkPoint(active:Int, inactive:Int):Point			= new Point(active, inactive)
		def mkDimension(active:Int, inactive:Int):Dimension	= new Dimension(inactive, active)
		def mkRectangle(activePos:Int, inactivePos:Int, activeSize:Int, inactiveSize:Int):Rectangle	=
				new Rectangle(inactivePos, activePos, inactiveSize, activeSize)
	}
}
