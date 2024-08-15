package scutil.lang.extension

import java.nio.charset.Charset

import scala.annotation.tailrec
import scala.collection.mutable

import scutil.lang.*

object StringExtensions {
	extension (peer:String) {
		def toBigIntOption:Option[BigInt]	= parseBigInt.toOption

		// toBoolean throws an IllegalArgumentException, not a NumberFormatException
		def parseBoolean:Either[NumberFormatException,Boolean]	=
			if		(caselessTrue(peer))	Right(true)
			else if	(caselessFalse(peer))	Right(false)
			else							Left(new NumberFormatException("not a boolean: " + peer))

		// NOTE does not use String.toLowercase because that would depend on the default Locale,
		// and other Locales might not be supported in scala-js
		private def caselessTrue(it:String):Boolean	=
			it.length == 4 &&
			{	val	c = it.charAt(0);	(c == 't' || c == 'T')	} &&
			{	val	c = it.charAt(1);	(c == 'r' || c == 'R')	} &&
			{	val	c = it.charAt(2);	(c == 'u' || c == 'U')	} &&
			{	val	c = it.charAt(3);	(c == 'e' || c == 'E')	}

		// NOTE does not use String.toLowercase because that would depend on the default Locale,
		// and other Locales might not be supported in scala-js
		private def caselessFalse(it:String):Boolean	=
			it.length == 5 &&
			{	val	c = it.charAt(0);	(c == 'f' || c == 'F')	}	&&
			{	val	c = it.charAt(1);	(c == 'a' || c == 'A')	}	&&
			{	val	c = it.charAt(2);	(c == 'l' || c == 'L')	}	&&
			{	val	c = it.charAt(3);	(c == 's' || c == 'S')	}	&&
			{	val	c = it.charAt(4);	(c == 'e' || c == 'E')	}

		def parseByte:Either[NumberFormatException,Byte]		= parseNumber(_.toByte)
		def parseShort:Either[NumberFormatException,Short]		= parseNumber(_.toShort)
		def parseInt:Either[NumberFormatException,Int]			= parseNumber(_.toInt)
		def parseLong:Either[NumberFormatException,Long]		= parseNumber(_.toLong)
		def parseFloat:Either[NumberFormatException,Float]		= parseNumber(_.toFloat)
		def parseDouble:Either[NumberFormatException,Double]	= parseNumber(_.toDouble)
		def parseBigInt:Either[NumberFormatException,BigInt]	= parseNumber(BigInt(_))

		private def parseNumber[T](func:String=>T):Either[NumberFormatException,T]	=
			Catch.byType[NumberFormatException].in(func(peer))

		//------------------------------------------------------------------------------

		def toByteString(charset:Charset):ByteString	=
			ByteString.fromString(peer, charset)

		def toUtf8ByteString:ByteString	=
			ByteString.fromUtf8String(peer)

		def toIArray:IArray[Char]	=
			IArray.unsafeFromArray(peer.toCharArray)

		//------------------------------------------------------------------------------

		def optionNonEmpty:Option[String]	=
			if (peer.nonEmpty)	Some(peer)
			else				None

		//------------------------------------------------------------------------------

		def cutPrefix(prefix:String):Option[String] =
			if (peer.startsWith(prefix))	Some(peer.substring(prefix.length))
			else							None

		def cutSuffix(suffix:String):Option[String] =
			if (peer.endsWith(suffix))	Some(peer.substring(0, peer.length - suffix.length))
			else						None

		def pastePrefix(prefix:String):Option[String] =
			if (peer.startsWith(prefix))	None
			else							Some(prefix + peer)

		def pasteSuffix(suffix:String):Option[String] =
			if (peer.endsWith(suffix))	None
			else						Some(peer + suffix)

		//------------------------------------------------------------------------------

		/** excludes the separator char itself */
		def splitAroundChar(separator:Char):Seq[String] =
			splitAroundString(separator.toString)

		/** excludes the separator string itself */
		def splitAroundString(separator:String):Seq[String] = {
			val	out	= new mutable.ArrayBuffer[String]
			@tailrec
			def loop(pos:Int):Seq[String]	=
				peer.indexOf(separator, pos) match {
					case -1 =>
						out	+= peer.substring(pos)
						out.toVector
					case index	=>
						out	+= peer.substring(pos, index)
						loop(index + separator.length)
				}
			loop(0)
		}

		//------------------------------------------------------------------------------

		/** excludes the indexed char itself */
		def splitAroundIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index < peer.length)	Some((peer.substring(0,index), peer.substring(index+1)))
			else									None

		/** excludes the separator char itself */
		def splitAroundFirstChar(separator:Char):Option[(String,String)] =
			splitAroundIndex(peer.indexOf(separator))

		/** excludes the separator char itself */
		def splitAroundLastChar(separator:Char):Option[(String,String)] =
			splitAroundIndex(peer.lastIndexOf(separator))

		/** excludes the separator itself */
		def splitAroundFirstString(separator:String):Option[(String,String)] =
			splitAroundIndex(peer.indexOf(separator))

		/** excludes the separator itself */
		def splitAroundLastString(separator:String):Option[(String,String)] =
			splitAroundIndex(peer.lastIndexOf(separator))

		//------------------------------------------------------------------------------

		/** like splitAt, but None for indices outside the String's boundaries */
		def splitAtIndex(index:Int):Option[(String,String)]	=
			if (index >= 0 && index <= peer.length)	Some((peer.substring(0,index), peer.substring(index)))
			else									None

		//------------------------------------------------------------------------------

		/** like indexOf, but None if not found */
		def indexOfString(part:String):Option[Int]	= indexOfStringFrom(part, 0)
		def indexOfStringFrom(part:String, fromIndex:Int):Option[Int]	=
			peer.indexOf(part, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}

		/** like lastIndexOf, but None if not found */
		def lastIndexOfStringOption(part:String):Option[Int]	= lastIndexOfStringOptionFrom(part, peer.length-1)
		def lastIndexOfStringOptionFrom(part:String, fromIndex:Int):Option[Int]	=
			peer.lastIndexOf(part, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}

		/** like indexOf, but None if not found */
		def indexOfChar(ch:Int):Option[Int]	= indexOfCharFrom(ch, 0)
		def indexOfCharFrom(ch:Int, fromIndex:Int):Option[Int]	=
			peer.indexOf(ch, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}

		/** like lastIndexOf, but None if not found */
		def lastIndexOfChar(ch:Int):Option[Int]	= lastIndexOfCharFrom(ch, peer.length-1)
		def lastIndexOfCharFrom(ch:Int, fromIndex:Int):Option[Int]	=
			peer.lastIndexOf(ch, fromIndex) match {
				case -1		=> None
				case index	=> Some(index)
			}

		//------------------------------------------------------------------------------

		/** quote for use in XML */
		def quoteXML(quot:Boolean = false, apos:Boolean = false):String	= {
			val b	= new StringBuilder
			var i	= 0
			while (i < peer.length) {
				peer.charAt(i) match {
					case '<'			=> b append "&lt;"
					case '>'			=> b append "&gt;"
					case '&'			=> b append "&amp;"
					case '"'	if quot	=> b append "&quot;"
					case '\''	if apos	=> b append "&apos;"
					case x				=> b += x
				}
				i	+= 1
			}
			b.result
		}
	}
}
