package scutil.lang.pimp

import scala.collection.mutable

import scutil.lang._

object StringImplicits extends StringImplicits

trait StringImplicits {
	implicit final class LangStringExt(peer:String) {
		def toBooleanOption:Option[Boolean]	= toBooleanEither.toOption
		def toByteOption:Option[Byte]		= toByteEither.toOption
		def toShortOption:Option[Short]		= toShortEither.toOption
		def toIntOption:Option[Int]			= toIntEither.toOption
		def toLongOption:Option[Long]		= toLongEither.toOption
		def toFloatOption:Option[Float]		= toFloatEither.toOption
		def toDoubleOption:Option[Double]	= toDoubleEither.toOption
		def toBigIntOption:Option[BigInt]	= toBigIntEither.toOption
		
		// toBoolean throws an IllegalArgumentException, not a NumberFormatException
		def toBooleanEither:Either[NumberFormatException,Boolean]	=
				peer match {
					case "true"		=> Right(true)
					case "false"	=> Right(false)
					case null		=> Left(new NumberFormatException("null is not a boolean"))
					case x			=> Left(new NumberFormatException("not a boolean: " + x))
				}
		def toByteEither:Either[NumberFormatException,Byte]		= toNumberEither(_.toByte)
		def toShortEither:Either[NumberFormatException,Short]	= toNumberEither(_.toShort)
		def toIntEither:Either[NumberFormatException,Int]		= toNumberEither(_.toInt)
		def toLongEither:Either[NumberFormatException,Long]		= toNumberEither(_.toLong)
		def toFloatEither:Either[NumberFormatException,Float]	= toNumberEither(_.toFloat)
		def toDoubleEither:Either[NumberFormatException,Double]	= toNumberEither(_.toDouble)
		def toBigIntEither:Either[NumberFormatException,BigInt]	= toNumberEither(BigInt(_))
			
		private def toNumberEither[T](func:String=>T):Either[NumberFormatException,T]	=
				Catch.byType[NumberFormatException] in func(peer)
		
		//------------------------------------------------------------------------------
		
		def optionNonEmpty:Option[String]	=
				if (peer.nonEmpty)	Some(peer)
				else				None
		
		@deprecated("use optionNonEmpty", "0.121.0")
		def guardNonEmpty:Option[String]	=
				optionNonEmpty
			
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
		def indexOfString(part:String, fromIndex:Int=0):Option[Int]	=
				peer indexOf (part, fromIndex) match {
					case -1		=> None
					case index	=> Some(index)
				}
				
		/** like lastIndexOf, but None if not found */
		def lastIndexOfStringOption(part:String, fromIndex:Int=peer.length-1):Option[Int]	=
				peer lastIndexOf (part, fromIndex) match {
					case -1		=> None
					case index	=> Some(index)
				}
	
		/** like indexOf, but None if not found */
		def indexOfCharOption(ch:Int, fromIndex:Int=0):Option[Int]	=
				peer indexOf (ch, fromIndex) match {
					case -1		=> None
					case index	=> Some(index)
				}
				
		/** like lastIndexOf, but None if not found */
		def lastIndexOfCharOption(ch:Int, fromIndex:Int=peer.length-1):Option[Int]	=
				peer lastIndexOf (ch, fromIndex) match {
					case -1		=> None
					case index	=> Some(index)
				}
		
		//------------------------------------------------------------------------------
		
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
}
