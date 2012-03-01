package scutil.ext

import java.util.regex.Pattern

import scala.annotation.tailrec 
import scala.collection.mutable
import scala.util.control.Exception._

import scutil.Functions.neverComesHere

object StringImplicits extends StringImplicits

trait StringImplicits {
	implicit def toStringExt(delegate:String) = new StringExt(delegate)
}

final class StringExt(delegate:String) {
	def toBooleanOption:Option[Boolean]	= toNumberOption(_.toBoolean)
	def toByteOption:Option[Byte]		= toNumberOption(_.toByte)
	def toShortOption:Option[Short]		= toNumberOption(_.toShort)
	def toIntOption:Option[Int]			= toNumberOption(_.toInt)
	def toLongOption:Option[Long]		= toNumberOption(_.toLong)
	def toFloatOption:Option[Float]		= toNumberOption(_.toFloat)
	def toDoubleOption:Option[Double]	= toNumberOption(_.toDouble)
	
	private def toNumberOption[T](func:String=>T):Option[T]	=
			catching(classOf[NumberFormatException]) opt func(delegate)
	
	def guardNonEmpty:Option[String]	= 
			if (delegate.nonEmpty)	Some(delegate)
			else					None 
	
	def cutPrefix(prefix:String):Option[String] =
			if (delegate startsWith prefix)	Some(delegate substring prefix.length)
			else							None
				
	def cutSuffix(suffix:String):Option[String] = 
			if (delegate endsWith suffix)	Some(delegate substring (0, delegate.length-suffix.length))
			else							None
	
	/** exludes the separator chars themselves */
	def splitAround(separator:Char):Seq[String] = {
		val	out	= new mutable.ArrayBuffer[String]
		var pos	= 0
		while (true) {
			val	index	= delegate indexOf (separator, pos)
			if (index == -1) {
				out	+= delegate substring pos
				return out
			}
			else {
				out	+= delegate substring (pos, index)
				pos	= index + 1
			}
		}
		neverComesHere
	}
	
	/** exludes the separator strings themselves */
	def splitAround(separator:String):Seq[String] = {
		val	out	= new mutable.ArrayBuffer[String]
		var pos	= 0
		while (true) {
			val	index	= delegate indexOf (separator, pos)
			if (index == -1) {
				out	+= delegate substring pos
				return out
			}
			else {
				out	+= delegate substring (pos, index)
				pos	= index + separator.length
			}
		}
		neverComesHere
	}
	
	/*
	def splitAround(separator:Char):List[String] = {
		@tailrec 
		def unfold(s:String, accu:List[String]):List[String]	= s lastIndexOf separator match {
			case -1	=> s :: accu
			case n	=> unfold(s substring (0,n), (s substring (n+1)) :: accu)
		}
		unfold(delegate, Nil)
	}
	*/
	
	/** like splitAt, but None for indizes outside the String's boundaries */
	def splitAtIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index <= delegate.length)	Some((delegate substring (0,index), delegate substring index))
			else										None
			
	/** before index and after index excluding the index itself */
	def splitAroundIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index < delegate.length)	Some((delegate substring (0,index), delegate substring index+1))
			else										None
	
	/** exludes the separator char itself */
	def splitAroundFirst(separator:Char):Option[(String,String)] =
			splitAroundIndex(delegate indexOf separator)
		
	/** exludes the separator char itself */
	def splitAroundLast(separator:Char):Option[(String,String)] =
			splitAroundIndex(delegate lastIndexOf separator)
		
	/** like indexOf, but None if not found */
	def indexOfOption(ch:Int, fromIndex:Int=0)	=
			delegate indexOf (ch, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}
			
	/** like lastIndexOf, but None if not found */
	def lastIndexOfOption(ch:Int, fromIndex:Int=delegate.length-1)	=
			delegate lastIndexOf (ch, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}
	
	/** quote to use as a literal in a Regex */
	def quoteRegex:String	= Pattern quote delegate
	
	/** quote to use within a character class in a Regex */
	def quoteCharacterClass:String	= {
		val b	= new StringBuilder
		var i	= 0
		while (i < delegate.length) {
			delegate charAt i match {
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
		while (i < delegate.length) {
			delegate charAt i match {
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
