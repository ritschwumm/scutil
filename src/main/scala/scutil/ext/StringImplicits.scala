package scutil.ext

import java.util.regex.Pattern

import scala.annotation.tailrec 
import scala.collection.mutable
import scala.util.control.Exception._

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
	def splitAround(separator:Char):List[String] = {
		@tailrec 
		def unfold(s:String, accu:List[String]):List[String]	= s lastIndexOf separator match {
			case -1	=> s :: accu
			case n	=> unfold(s substring (0,n), (s substring (n+1)) :: accu)
		}
		unfold(delegate, Nil)
	}
	
	/** before index and after index excluding the index itself */
	def splitAroundIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index <= delegate.length)	Some((delegate substring (0,index), delegate substring index+1))
			else										None
			
	/** exludes the separator char itself */
	def splitAroundFirst(separator:Char):Option[(String,String)] =
			splitAroundIndex(delegate indexOf separator)
		
	/** exludes the separator char itself */
	def splitAroundLast(separator:Char):Option[(String,String)] =
			splitAroundIndex(delegate lastIndexOf separator)
		
	/*
	def indexOfOption(separator:Char)	=
			delegate indexOf separator match {
				case -1		=> None
				case index	=> Some(index)
			}
			
	def lastIndexOfOption(separator:Char)	=
			delegate lastIndexOf separator match {
				case -1		=> None
				case index	=> Some(index)
			}
	*/
	
	def quoteRegex:String	= Pattern quote delegate
	
	def quoteXML(quot:Boolean = false, apos:Boolean = false):String	= {
		val b	= new StringBuilder
		var i	= 0
		while (i < delegate.length) {
			delegate charAt i match {
				case '<'				=> b append "&lt;"
				case '>'				=> b append "&gt;"
				case '&'				=> b append "&amp;"
				case '"'	if quot		=> b append "&quot;"
				case '\''	if apos		=> b append "&apos;"
				case x					=> b += x
			}
			i	+= 1
		}
		b.toString
	}
}
