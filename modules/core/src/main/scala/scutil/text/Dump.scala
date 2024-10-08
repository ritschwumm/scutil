package scutil.text

import scutil.core.implicits.*

object Dump {
	def hexLines(bytesPerLine:Int, bytes:Iterable[Byte]):Iterator[String]	=
		bytes.grouped(bytesPerLine).map(hexLine(bytesPerLine, _))

	def hexLine(bytesPerLine:Int, bytes:Iterable[Byte]):String	=
		HexFormat	.string(bytes).padTo(HexFormat.width(bytesPerLine),		' ')	+ " |"	+
		AsciiFormat	.string(bytes).padTo(AsciiFormat.width(bytesPerLine),	' ')	+ "|"

	trait DumpFormat {
		def string(line:Iterable[Byte]):String
		def width(count:Int):Int
	}

	object HexFormat extends DumpFormat {
		def string(line:Iterable[Byte])	= line.map("%02x".format(_)).mkString(" ")
		def width(size:Int):Int			= (size != 0).cata(0, size*3-1)
	}

	object AsciiFormat extends DumpFormat {
		def string(line:Iterable[Byte])	= line.map(_.toChar.optionBy(printable).getOrElse(".")).mkString("")
		def width(size:Int):Int			= size
		def printable(c:Char):Boolean	= c >= 32 && c <= 127
	}
}
