package scutil

import Implicits._

object Dump {
	def hexLines(bytesPerLine:Int, bytes:Seq[Byte]):Iterator[String]	=
			bytes grouped bytesPerLine map { hexLine(bytesPerLine, _) }
	
	def hexLine(bytesPerLine:Int, bytes:Seq[Byte]):String	=
			((HexFormat		string bytes) padTo (HexFormat		width bytesPerLine, ' '))	+ " |"	+
			((AsciiFormat	string bytes) padTo (AsciiFormat	width bytesPerLine, ' '))	+ "|"
			
	trait DumpFormat {
		def string(line:Seq[Byte]):String
		def width(count:Int):Int
	}
	
	object HexFormat extends DumpFormat {
		def string(line:Seq[Byte])	= line map { it => "%02x" format (it.toInt & 0xff) }	mkString " "
		def width(size:Int):Int		= size != 0 cata (size*3-1, 0)
	}
	
	object AsciiFormat extends DumpFormat {
		def string(line:Seq[Byte])		= line map { _.toChar guardBy printable getOrElse "." } mkString ""
		def width(size:Int):Int			= size
		def printable(c:Char):Boolean	= c >= 32 && c <= 127
	}
}
