package scutil.ext

import scala.collection.mutable
import scala.util.control.Exception._

import BooleanImplicits._

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
	
	// TODO rename to stripPrefixOpt and stripSuffixOpt, or even return Eithers
	
	def cutPrefix(prefix:String):Option[String] = 
			delegate startsWith prefix guard (delegate substring prefix.length) 
				
	def cutSuffix(suffix:String):Option[String] = 
			delegate endsWith suffix guard (delegate substring (0, delegate.length-suffix.length)) 
			
	def splitAround(separator:Char):List[String] = {
		val	out	= new mutable.ArrayBuffer[String]
		val	len	= delegate.length
		var	pos	= 0
		for (i <- 0 until len) {
			val	c	= delegate charAt i
			if (c == separator) {
				out	+= (delegate substring (pos, i))
				pos	= i + 1
			}
		}
		if (pos <= len) {
			out	+= (delegate substring (pos, len))
		}
		out.toList
	}
}
