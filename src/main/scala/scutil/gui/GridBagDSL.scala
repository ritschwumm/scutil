package scutil.gui

import java.awt.GridBagConstraints
import java.awt.Insets

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

	// BASELINE BASELINE_LEADING BASELINE_TRAILING 
	// ABOVE_BASELINE ABOVE_BASELINE_LEADING ABOVE_BASELINE_TRAILING 
	// BELOW_BASELINE BELOW_BASELINE_LEADING BELOW_BASELINE_TRAILING 
	sealed abstract class GridBagConstraintsAnchor(val v:Int)
	case object CENTER				extends GridBagConstraintsAnchor(GridBagConstraints.CENTER)
	case object NORTH				extends GridBagConstraintsAnchor(GridBagConstraints.NORTH)
	case object SOUTH				extends GridBagConstraintsAnchor(GridBagConstraints.SOUTH)
	case object EAST				extends GridBagConstraintsAnchor(GridBagConstraints.EAST)
	case object WEST				extends GridBagConstraintsAnchor(GridBagConstraints.WEST)
	case object NORTHEAST			extends GridBagConstraintsAnchor(GridBagConstraints.NORTHEAST)
	case object NORTHWEST			extends GridBagConstraintsAnchor(GridBagConstraints.NORTHWEST)
	case object SOUTHEAST			extends GridBagConstraintsAnchor(GridBagConstraints.SOUTHEAST)
	case object SOUTHWEST			extends GridBagConstraintsAnchor(GridBagConstraints.SOUTHWEST)
	case object PAGE_START			extends GridBagConstraintsAnchor(GridBagConstraints.PAGE_START)
	case object PAGE_END			extends GridBagConstraintsAnchor(GridBagConstraints.PAGE_END)
	case object LINE_START			extends GridBagConstraintsAnchor(GridBagConstraints.LINE_START)
	case object LINE_END			extends GridBagConstraintsAnchor(GridBagConstraints.LINE_END)
	case object FIRST_LINE_START	extends GridBagConstraintsAnchor(GridBagConstraints.FIRST_LINE_START)
	case object FIRST_LINE_END		extends GridBagConstraintsAnchor(GridBagConstraints.FIRST_LINE_END)
	case object LAST_LINE_START		extends GridBagConstraintsAnchor(GridBagConstraints.LAST_LINE_START)
	case object LAST_LINE_END		extends GridBagConstraintsAnchor(GridBagConstraints.LAST_LINE_END)
	
	sealed abstract class GridBagConstraintsFill(val v:Int)
	case object NONE		extends GridBagConstraintsFill(GridBagConstraints.NONE)
	case object HORIZONTAL	extends GridBagConstraintsFill(GridBagConstraints.HORIZONTAL)
	case object VERTICAL	extends GridBagConstraintsFill(GridBagConstraints.VERTICAL)
	case object BOTH		extends GridBagConstraintsFill(GridBagConstraints.BOTH)
}
	
final class GridBagDSL(delegate:GridBagConstraints) {
	import GridBagDSL._
	
	def pos(x:GridBagConstraintsPosition, y:GridBagConstraintsPosition):GridBagConstraints		= modify { c => c.gridx		= x.v;	c.gridy			= y.v	}
	def size(x:GridBagConstraintsSize, y:GridBagConstraintsSize):GridBagConstraints				= modify { c => c.gridwidth	= x.v;	c.gridheight	= y.v	}
	def weight(x:Double, y:Double):GridBagConstraints											= modify { c => c.weightx	= x;	c.weighty		= y		}
	def ipad(x:Int, y:Int):GridBagConstraints													= modify { c => c.ipadx		= x; 	c.ipady			= y 	}
	
	def anchor(v:GridBagConstraintsAnchor):GridBagConstraints									= modify { c => c.anchor	= v.v	}
	def fill(v:GridBagConstraintsFill):GridBagConstraints										= modify { c => c.fill		= v.v	}
	
	def insets(v:Insets):GridBagConstraints														= modify { c => c.insets	= v.clone.asInstanceOf[Insets]			}
	def insets(top:Int, left:Int, bottom:Int, right:Int):GridBagConstraints						= modify { c => c.insets	= new Insets(top, left, bottom, right)	}
	def insets(p:Place2D, hgap:Int, vgap:Int):GridBagConstraints								= modify { c => c.insets	= p insets (hgap, vgap) }
	
	// implicit def toInsets(v:Tuple4[Int,Int,Int,Int]):Insets	= new Insets(v._1, v._2, v._3, v._4)
		
	def modify(effect:GridBagConstraints=>Unit):GridBagConstraints = {
		val out	= delegate.clone.asInstanceOf[GridBagConstraints]
		effect(out)
		out
	}
}
