package scutil

import scala.math._

/** width is bytes per line */
final class HexDump(bytesPerLine:Int) {
	def print(bytes:Array[Byte]) {
		bytes grouped bytesPerLine foreach { group =>
			println(line(group))
		}
	}
	
	def line(line:Array[Byte]):String	= hex(line) + " |" + ascii(line) + "|"
	def hex(line:Array[Byte]):String	= line map { it => "%02x" format (it.toInt & 0xff) }				mkString " "	padTo (bytesPerLine*3-1,	' ')
	def ascii(line:Array[Byte]):String	= line map { it => (if (printable(it.toChar)) it.toChar else ".") } mkString ""		padTo (bytesPerLine,		' ')
	
	def printable(c:Char):Boolean		= c >= 32 && c <= 127
}
