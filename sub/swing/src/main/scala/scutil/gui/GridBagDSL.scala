package scutil.gui

import java.awt.GridBagConstraints
import java.awt.Insets

import scutil.implicits._

object GridBagDSL {
	def GBC = new GridBagConstraints
	
	implicit def toGridBagDSL(c:GridBagConstraints):GridBagDSL	= new GridBagDSL(c)
	
	sealed abstract class GridBagConstraintsExtra(val v:Int)
	case object RELATIVE	extends GridBagConstraintsExtra(GridBagConstraints.RELATIVE)
	case object REMAINDER	extends GridBagConstraintsExtra(GridBagConstraints.REMAINDER)
	
	case class GridBagConstraintsPosition(val v:Int)
	implicit def toGridBagConstraintsPosition(v:Int):GridBagConstraintsPosition				= GridBagConstraintsPosition(v)
	implicit def toGridBagConstraintsPosition(v:RELATIVE.type):GridBagConstraintsPosition	= GridBagConstraintsPosition(v.v)
	
	case class GridBagConstraintsSize(val v:Int)
	implicit def toGridBagConstraintsSize(v:Int):GridBagConstraintsSize						= GridBagConstraintsSize(v)
	implicit def toGridBagConstraintsSize(v:RELATIVE.type):GridBagConstraintsSize			= GridBagConstraintsSize(v.v)
	implicit def toGridBagConstraintsSize(v:REMAINDER.type):GridBagConstraintsSize			= GridBagConstraintsSize(v.v)

	sealed abstract class GridBagConstraintsAnchor(val v:Int)
	case object CENTER					extends GridBagConstraintsAnchor(GridBagConstraints.CENTER)
	case object NORTH					extends GridBagConstraintsAnchor(GridBagConstraints.NORTH)
	case object SOUTH					extends GridBagConstraintsAnchor(GridBagConstraints.SOUTH)
	case object EAST					extends GridBagConstraintsAnchor(GridBagConstraints.EAST)
	case object WEST					extends GridBagConstraintsAnchor(GridBagConstraints.WEST)
	case object NORTHEAST				extends GridBagConstraintsAnchor(GridBagConstraints.NORTHEAST)
	case object NORTHWEST				extends GridBagConstraintsAnchor(GridBagConstraints.NORTHWEST)
	case object SOUTHEAST				extends GridBagConstraintsAnchor(GridBagConstraints.SOUTHEAST)
	case object SOUTHWEST				extends GridBagConstraintsAnchor(GridBagConstraints.SOUTHWEST)
	case object PAGE_START				extends GridBagConstraintsAnchor(GridBagConstraints.PAGE_START)
	case object PAGE_END				extends GridBagConstraintsAnchor(GridBagConstraints.PAGE_END)
	case object LINE_START				extends GridBagConstraintsAnchor(GridBagConstraints.LINE_START)
	case object LINE_END				extends GridBagConstraintsAnchor(GridBagConstraints.LINE_END)
	case object FIRST_LINE_START		extends GridBagConstraintsAnchor(GridBagConstraints.FIRST_LINE_START)
	case object FIRST_LINE_END			extends GridBagConstraintsAnchor(GridBagConstraints.FIRST_LINE_END)
	case object LAST_LINE_START			extends GridBagConstraintsAnchor(GridBagConstraints.LAST_LINE_START)
	case object LAST_LINE_END			extends GridBagConstraintsAnchor(GridBagConstraints.LAST_LINE_END)
	case object BASELINE				extends GridBagConstraintsAnchor(GridBagConstraints.BASELINE)
	case object BASELINE_LEADING		extends GridBagConstraintsAnchor(GridBagConstraints.BASELINE_LEADING)
	case object BASELINE_TRAILING		extends GridBagConstraintsAnchor(GridBagConstraints.BASELINE_TRAILING)
	case object ABOVE_BASELINE			extends GridBagConstraintsAnchor(GridBagConstraints.ABOVE_BASELINE)
	case object ABOVE_BASELINE_LEADING	extends GridBagConstraintsAnchor(GridBagConstraints.ABOVE_BASELINE_LEADING)
	case object ABOVE_BASELINE_TRAILING	extends GridBagConstraintsAnchor(GridBagConstraints.ABOVE_BASELINE_TRAILING)
	case object BELOW_BASELINE			extends GridBagConstraintsAnchor(GridBagConstraints.BELOW_BASELINE)
	case object BELOW_BASELINE_LEADING	extends GridBagConstraintsAnchor(GridBagConstraints.BELOW_BASELINE_LEADING)
	case object BELOW_BASELINE_TRAILING	extends GridBagConstraintsAnchor(GridBagConstraints.BELOW_BASELINE_TRAILING)
	
	sealed abstract class GridBagConstraintsFill(val v:Int)
	case object NONE		extends GridBagConstraintsFill(GridBagConstraints.NONE)
	case object HORIZONTAL	extends GridBagConstraintsFill(GridBagConstraints.HORIZONTAL)
	case object VERTICAL	extends GridBagConstraintsFill(GridBagConstraints.VERTICAL)
	case object BOTH		extends GridBagConstraintsFill(GridBagConstraints.BOTH)
	
	def TopLeft(hgap:Int, vgap:Int):Insets		= new Insets(0,		0,		vgap,	hgap)
	def TopCenter(hgap:Int, vgap:Int):Insets	= new Insets(0,		hgap,	vgap,	hgap)
	def TopRight(hgap:Int, vgap:Int):Insets		= new Insets(0,		hgap,	vgap,	0)
	def CenterLeft(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		vgap,	hgap)
	def CenterCenter(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	vgap,	hgap)
	def CenterRight(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	vgap,	0)
	def BottomLeft(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		0,		hgap)
	def BottomCenter(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	0,		hgap)
	def BottomRight(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	0,		0)
	def AloneLeft(hgap:Int, vgap:Int):Insets	= new Insets(0,		0,		0,		hgap)
	def AloneCenter(hgap:Int, vgap:Int):Insets	= new Insets(0,		hgap,	0,		hgap)
	def AloneRight(hgap:Int, vgap:Int):Insets	= new Insets(0,		hgap,	0,		0)
	def TopAlone(hgap:Int, vgap:Int):Insets		= new Insets(0,		0,		vgap,	0)
	def CenterAlone(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		vgap,	0)
	def BottomAlone(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		0,		0)
	def AloneAlone(hgap:Int, vgap:Int):Insets	= new Insets(0,		0,		0,		0)
}
	
final class GridBagDSL(peer:GridBagConstraints) {
	import GridBagDSL._
	
	def pos(x:GridBagConstraintsPosition, y:GridBagConstraintsPosition):GridBagConstraints	= modified { c => c.gridx		= x.v;	c.gridy			= y.v	}
	def size(x:GridBagConstraintsSize, y:GridBagConstraintsSize):GridBagConstraints			= modified { c => c.gridwidth	= x.v;	c.gridheight	= y.v	}
	def weight(x:Double, y:Double):GridBagConstraints										= modified { c => c.weightx		= x;	c.weighty		= y		}
	def ipad(x:Int, y:Int):GridBagConstraints												= modified { c => c.ipadx		= x; 	c.ipady			= y 	}
	
	def anchor(v:GridBagConstraintsAnchor):GridBagConstraints								= modified { c => c.anchor	= v.v	}
	def fill(v:GridBagConstraintsFill):GridBagConstraints									= modified { c => c.fill	= v.v	}
	
	def insets(v:Insets):GridBagConstraints													= modified { c => c.insets	= v.clone.asInstanceOf[Insets]			}
	def insetsTLBR(top:Int, left:Int, bottom:Int, right:Int):GridBagConstraints				= modified { c => c.insets	= new Insets(top, left, bottom, right)	}
	
	def modified(effect:GridBagConstraints=>Unit):GridBagConstraints =
			cloned |>> effect
	
	def cloned:GridBagConstraints	= 
			peer.clone.asInstanceOf[GridBagConstraints]
}
