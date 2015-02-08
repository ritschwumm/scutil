package scutil.gui

import java.awt.{ List=>AwtList, _ }

object Colors {
	val transparentBlack:Color	= new Color(0, 0, 0, 0)
	val transparentWhite:Color	= new Color(255, 255, 255, 0)
	
	def decodeOption(s:String):Option[Color]	=
			try {
				Some(Color decode s)
			}
			catch { case e:NumberFormatException =>
				None
			}
}
