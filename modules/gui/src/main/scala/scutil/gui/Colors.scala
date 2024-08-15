package scutil.gui

import java.awt.Color

object Colors {
	val transparentBlack:Color	= new Color(0, 0, 0, 0)
	val transparentWhite:Color	= new Color(255, 255, 255, 0)

	def decodeOption(s:String):Option[Color]	=
		decodeEither(s).toOption

	def decodeEither(s:String):Either[NumberFormatException,Color]	=
		try {
			Right(Color.decode(s))
		}
		catch { case e:NumberFormatException =>
			Left(e)
		}
}
