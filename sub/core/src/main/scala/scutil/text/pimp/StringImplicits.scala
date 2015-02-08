package scutil.text.pimp

import java.lang.{
	Byte => JByte, Short => JShort, Integer => JInt, Long => JLong,
	Float => JFloat, Double => JDouble, Boolean => JBoolean
}
import java.util.regex.Pattern

import scala.collection.mutable

import scutil.lang._

object StringImplicits extends StringImplicits

trait StringImplicits {
	implicit def toStringExt(peer:String) = new StringExt(peer)
}

final class StringExt(peer:String) {
	def toBooleanOption:Option[Boolean]	= toNumberOption(_.toBoolean)
	def toByteOption:Option[Byte]		= toNumberOption(_.toByte)
	def toShortOption:Option[Short]		= toNumberOption(_.toShort)
	def toIntOption:Option[Int]			= toNumberOption(_.toInt)
	def toLongOption:Option[Long]		= toNumberOption(_.toLong)
	def toFloatOption:Option[Float]		= toNumberOption(_.toFloat)
	def toDoubleOption:Option[Double]	= toNumberOption(_.toDouble)
	
	private def toNumberOption[T](func:String=>T):Option[T]	=
			toNumberTried(func).toOption
		
	def toBooleanTried:Tried[NumberFormatException,Boolean]	= toNumberTried(JBoolean.parseBoolean)
	def toByteTried:Tried[NumberFormatException,Byte]		= toNumberTried(JByte.parseByte)
	def toShortTried:Tried[NumberFormatException,Short]		= toNumberTried(JShort.parseShort)
	def toIntTried:Tried[NumberFormatException,Int]			= toNumberTried(JInt.parseInt)
	def toLongTried:Tried[NumberFormatException,Long]		= toNumberTried(JLong.parseLong)
	def toFloatTried:Tried[NumberFormatException,Float]		= toNumberTried(JFloat.parseFloat)
	def toDoubleTried:Tried[NumberFormatException,Double]	= toNumberTried(JDouble.parseDouble)
		
	private def toNumberTried[T](func:String=>T):Tried[NumberFormatException,T]	=
			Catch.byType[NumberFormatException] in func(peer)
	
	//------------------------------------------------------------------------------
	
	def guardNonEmpty:Option[String]	=
			if (peer.nonEmpty)	Some(peer)
			else				None
	
	//------------------------------------------------------------------------------
		
	def cutPrefix(prefix:String):Option[String] =
			if (peer startsWith prefix)	Some(peer substring prefix.length)
			else						None
				
	def cutSuffix(suffix:String):Option[String] =
			if (peer endsWith suffix)	Some(peer substring (0, peer.length - suffix.length))
			else						None
		
	def pastePrefix(prefix:String):Option[String] =
			if (peer startsWith prefix)	None
			else						Some(prefix + peer)
	
	def pasteSuffix(suffix:String):Option[String] =
			if (peer endsWith suffix)	None
			else						Some(peer + suffix)
	
	//------------------------------------------------------------------------------
	
	/** excludes the separator char itself */
	def splitAroundChar(separator:Char):ISeq[String] =
			splitAroundString(separator.toString)
	
	/** excludes the separator string itself */
	def splitAroundString(separator:String):ISeq[String] = {
		val	out	= new mutable.ArrayBuffer[String]
		@scala.annotation.tailrec
		def loop(pos:Int):ISeq[String]	=
				peer indexOf (separator, pos) match {
					case -1 =>
						out	+= peer substring pos
						out.toVector
					case index	=>
						out	+= peer substring (pos, index)
						loop(index + separator.length)
				}
		loop(0)
	}
	
	//------------------------------------------------------------------------------
	
	/** excludes the indexed char itself */
	def splitAroundIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index < peer.length)	Some((peer substring (0,index), peer substring index+1))
			else									None
	
	/** excludes the separator char itself */
	def splitAroundFirstChar(separator:Char):Option[(String,String)] =
			splitAroundIndex(peer indexOf separator)
		
	/** excludes the separator char itself */
	def splitAroundLastChar(separator:Char):Option[(String,String)] =
			splitAroundIndex(peer lastIndexOf separator)
		
	/** excludes the separator itself */
	def splitAroundFirstString(separator:String):Option[(String,String)] =
			splitAroundIndex(peer indexOf separator)
		
	/** excludes the separator itself */
	def splitAroundLastString(separator:String):Option[(String,String)] =
			splitAroundIndex(peer lastIndexOf separator)
		
	//------------------------------------------------------------------------------
		
	/** like splitAt, but None for indices outside the String's boundaries */
	def splitAtIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index <= peer.length)	Some((peer substring (0,index), peer substring index))
			else									None
		
	//------------------------------------------------------------------------------
	
	/** like indexOf, but None if not found */
	def indexOfOption(ch:Int, fromIndex:Int=0):Option[Int]	=
			peer indexOf (ch, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}
			
	/** like lastIndexOf, but None if not found */
	def lastIndexOfOption(ch:Int, fromIndex:Int=peer.length-1):Option[Int]	=
			peer lastIndexOf (ch, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}
	
	//------------------------------------------------------------------------------
	
	/** quote to use as a literal in a Regex */
	def quoteRegex:String	= Pattern quote peer
	
	/** quote to use within a character class in a Regex */
	def quoteCharacterClass:String	= {
		val b	= new StringBuilder
		var i	= 0
		while (i < peer.length) {
			peer charAt i match {
				case '\\'	=> b append "\\\\"
				case '-'	=> b append "\\-"
				case '^'	=> b append "\\^"
				case '['	=> b append "\\["
				case ']'	=> b append "\\]"
				case x		=> b += x
			}
			i	+= 1
		}
		b.toString
	}
	
	/** quote for use in XML */
	def quoteXML(quot:Boolean = false, apos:Boolean = false):String	= {
		val b	= new StringBuilder
		var i	= 0
		while (i < peer.length) {
			peer charAt i match {
				case '<'			=> b append "&lt;"
				case '>'			=> b append "&gt;"
				case '&'			=> b append "&amp;"
				case '"'	if quot	=> b append "&quot;"
				case '\''	if apos	=> b append "&apos;"
				case x				=> b += x
			}
			i	+= 1
		}
		b.toString
	}
}
