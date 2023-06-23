package scutil.gui.gridbag

import java.awt.GridBagConstraints
import java.awt.Insets

import scutil.core.implicits.*

def GBC = new GridBagConstraints

import GridBag.*

extension(peer:GridBagConstraints) {
	def pos(x:Position, y:Position):GridBagConstraints	= modified { c => c.gridx		= Position(x);	c.gridy			= Position(y)	}
	def size(x:Size, y:Size):GridBagConstraints			= modified { c => c.gridwidth	= Size(x);		c.gridheight	= Size(y)		}
	def weight(x:Double, y:Double):GridBagConstraints	= modified { c => c.weightx		= x;			c.weighty		= y				}
	def ipad(x:Int, y:Int):GridBagConstraints			= modified { c => c.ipadx		= x;			c.ipady			= y				}

	def anchor(v:Anchor):GridBagConstraints				= modified { c => c.anchor	= Anchor(v)	}
	def fill(v:Fill):GridBagConstraints					= modified { c => c.fill	= Fill(v)	}

	def insets(v:Insets):GridBagConstraints										= modified { c => c.insets	= new Insets(v.top, v.left, v.bottom, v.right)	}
	def insetsTLBR(top:Int, left:Int, bottom:Int, right:Int):GridBagConstraints	= modified { c => c.insets	= new Insets(top, left, bottom, right)			}

	def modified(effect:GridBagConstraints=>Unit):GridBagConstraints = cloned doto effect

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def cloned:GridBagConstraints	= peer.clone.asInstanceOf[GridBagConstraints]
}

object GridBag {
	type Position	= Int|"RELATIVE";
	def Position(it:Position):Int	=
		it match {
			case x:Int			=> x
			case _:"RELATIVE"	=> GridBagConstraints.RELATIVE
		}

	type Size	= Int|"RELATIVE"|"REMAINDER";
	def Size(it:Size):Int	=
		it match {
			case x:Int			=> x
			case _:"RELATIVE"	=> GridBagConstraints.RELATIVE
			case _:"REMAINDER"	=> GridBagConstraints.REMAINDER
		}

	type Anchor	= "CENTER"|"NORTH"|"SOUTH"|"EAST"|"WEST"|"NORTHEAST"|"NORTHWEST"|"SOUTHEAST"|"SOUTHWEST"|"PAGE_START"|"PAGE_END"|"LINE_START"|"LINE_END"|"FIRST_LINE_START"|"FIRST_LINE_END"|"LAST_LINE_START"|"LAST_LINE_END"|"BASELINE"|"BASELINE_LEADING"|"BASELINE_TRAILING"|"ABOVE_BASELINE"|"ABOVE_BASELINE_LEADING"|"ABOVE_BASELINE_TRAILING"|"BELOW_BASELINE"|"BELOW_BASELINE_LEADING"|"BELOW_BASELINE_TRAILING"
	def Anchor(it:Anchor):Int	=
		it match {
			case "CENTER"					=> GridBagConstraints.CENTER
			case "NORTH"					=> GridBagConstraints.NORTH
			case "SOUTH"					=> GridBagConstraints.SOUTH
			case "EAST"						=> GridBagConstraints.EAST
			case "WEST"						=> GridBagConstraints.WEST
			case "NORTHEAST"				=> GridBagConstraints.NORTHEAST
			case "NORTHWEST"				=> GridBagConstraints.NORTHWEST
			case "SOUTHEAST"				=> GridBagConstraints.SOUTHEAST
			case "SOUTHWEST"				=> GridBagConstraints.SOUTHWEST
			case "PAGE_START"				=> GridBagConstraints.PAGE_START
			case "PAGE_END"					=> GridBagConstraints.PAGE_END
			case "LINE_START"				=> GridBagConstraints.LINE_START
			case "LINE_END"					=> GridBagConstraints.LINE_END
			case "FIRST_LINE_START"			=> GridBagConstraints.FIRST_LINE_START
			case "FIRST_LINE_END"			=> GridBagConstraints.FIRST_LINE_END
			case "LAST_LINE_START"			=> GridBagConstraints.LAST_LINE_START
			case "LAST_LINE_END"			=> GridBagConstraints.LAST_LINE_END
			case "BASELINE"					=> GridBagConstraints.BASELINE
			case "BASELINE_LEADING"			=> GridBagConstraints.BASELINE_LEADING
			case "BASELINE_TRAILING"		=> GridBagConstraints.BASELINE_TRAILING
			case "ABOVE_BASELINE"			=> GridBagConstraints.ABOVE_BASELINE
			case "ABOVE_BASELINE_LEADING"	=> GridBagConstraints.ABOVE_BASELINE_LEADING
			case "ABOVE_BASELINE_TRAILING"	=> GridBagConstraints.ABOVE_BASELINE_TRAILING
			case "BELOW_BASELINE"			=> GridBagConstraints.BELOW_BASELINE
			case "BELOW_BASELINE_LEADING"	=> GridBagConstraints.BELOW_BASELINE_LEADING
			case "BELOW_BASELINE_TRAILING"	=> GridBagConstraints.BELOW_BASELINE_TRAILING
		}

	type Fill	= "NONE"|"HORIZONTAL"|"VERTICAL"|"BOTH"
	def Fill(it:Fill):Int	=
		it match {
			case "NONE"			=> GridBagConstraints.NONE
			case "HORIZONTAL"	=> GridBagConstraints.HORIZONTAL
			case "VERTICAL"		=> GridBagConstraints.VERTICAL
			case "BOTH"			=> GridBagConstraints.BOTH
		}
}
