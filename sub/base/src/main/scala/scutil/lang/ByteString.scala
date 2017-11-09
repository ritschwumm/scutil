package scutil.lang

import scala.annotation.tailrec

import java.util.{ Arrays => JArrays }
import java.nio.charset.Charset

object ByteString {
	val empty:ByteString	= new ByteString(Array.empty)
	
	//------------------------------------------------------------------------------
	
	def fromByteArray(it:Array[Byte]):ByteString	= {
		val tmp	= new Array[Byte](it.length)
		System arraycopy (it, 0, tmp, 0, it.length)
		new ByteString(tmp)
	}
	
	def fromSeq(it:Seq[Byte]):ByteString	= {
		val tmp	= new Array[Byte](it.size)
		val iter = it.iterator
		var i = 0
		while (iter.hasNext) {
			tmp(i) = iter.next()
			i	= i + 1
		}
		new ByteString(tmp)
	}
	
	def fromISeq(it:ISeq[Byte]):ByteString	=
			fromSeq(it)
	
	def fromString(it:String, charset:Charset):ByteString	=
			new ByteString(it getBytes charset)
		
	def fromUtf8String(it:String):ByteString	=
			fromString(it, Charsets.utf_8)
		
	def sliceFromByteArray(it:Array[Byte], srcPos:Int, copyLength:Int):Option[ByteString]	=
			if (!containsSlice(srcPos, srcPos+copyLength, it.length))	None
			else Some {
				val tmp	= new Array[Byte](copyLength)
				System arraycopy (it, srcPos, tmp, 0, copyLength)
				new ByteString(tmp)
			}
	
	//------------------------------------------------------------------------------
		
	def unsafeFromByteArray(it:Array[Byte]):ByteString	=
			new ByteString(it)
		
	//------------------------------------------------------------------------------
	
	def apply(its:Byte*):ByteString	=
			fromSeq(its)
		
	def unapplySeq(it:ByteString):Option[Seq[Byte]]	=
			Some(it.toByteArray)
		
	//------------------------------------------------------------------------------
	
	private def containsIndex(index:Int, size:Int):Boolean	=
			index >= 0 && index < size
		
	private def containsSlice(begin:Int, end:Int, size:Int):Boolean	=
			begin >= 0 && begin <= end && end <= size
		
}

/** wraps an Array[Byte] to be immutable and provide sensible equals and hashCode implementations */
final class ByteString private (private val value:Array[Byte]) {
	require(value != null, "value must not be null")
	
	def size:Int			= value.length
	def isEmpty:Boolean		= size == 0
	def nonEmpty:Boolean	= size != 0
	
	def containsIndex(index:Int):Boolean	=
			ByteString containsIndex (index, size)
		
	def containsSlice(begin:Int, end:Int):Boolean	=
			ByteString containsSlice (begin, end, size)
		
	def safeIndex(rawIndex:Int):Option[Int]	=
			if (containsIndex(rawIndex))	Some(rawIndex)
			else							None
		
	def safeSlice(rawBegin:Int, rawEnd:Int):Option[(Int,Int)]	=
			if (containsSlice(rawBegin, rawEnd))	Some((rawBegin, rawEnd))
			else									None
		
	def get(index:Int):Option[Byte]	=
			if (containsIndex(index))	Some(unsafeGet(index))
			else						None
		
	def slice(begin:Int, end:Int):Option[ByteString]	=
				 if (!containsSlice(begin, end))	None
			else if (begin == end)					Some(ByteString.empty)
			else if (begin == 0 && end == size)		Some(this)
			else Some {
				val length	= end - begin
				val tmp	= new Array[Byte](length)
				System arraycopy (value, begin, tmp, 0, length)
				new ByteString(tmp)
			}
		
	def containsAt(index:Int, that:ByteString):Boolean	=
			containsSlice(index, index+that.size) && {
				@tailrec def loop(i:Int):Boolean	=
					(i == that.size) 						||
					(this value index+i) == (that value i)	&&
					loop(i+1)
				loop(0)
			}
		
	def concat(that:ByteString):ByteString	= {
		val tmp	 = new Array[Byte](this.size + that.size)
		System arraycopy (this.value, 0, tmp, 0, this.size)
		System arraycopy (that.value, 0, tmp, this.size, that.size)
		new ByteString(tmp)
	}
	
	//------------------------------------------------------------------------------
			
	def asString(charset:Charset):String		= new String(value, charset)
	def asUtf8String(charset:Charset):String	= asString(Charsets.utf_8)
	
	def toByteArray:Array[Byte]	= {
		val tmp	= new Array[Byte](size)
		System arraycopy (value, 0, tmp, 0, size)
		tmp
	}
	
	def toISeq:ISeq[Byte]	= value.to[ISeq]
	def toVector:ISeq[Byte]	= value.to[Vector]
	def toList:ISeq[Byte]	= value.to[List]
	def toSet:Set[Byte]		= value.to[Set]
	
	def copyIntoByteArray(dest:Array[Byte], destPos:Int):Boolean	=
			sliceIntoByteArray(0, dest, destPos, size)
		
	def sliceIntoByteArray(srcPos:Int, dest:Array[Byte], destPos:Int, copyLength:Int):Boolean	= {
		val ok	=
				(ByteString containsSlice (srcPos,	srcPos	+ copyLength, size)) &&
				(ByteString containsSlice (destPos,	destPos	+ copyLength, dest.length))
		if (ok)	System arraycopy (this.value, srcPos, dest, destPos, copyLength)
		ok
	}
	
	//------------------------------------------------------------------------------
	
	def unsafeGet(index:Int):Byte	= value(index)
	def unsafeValue:Array[Byte]		= value
	
	//------------------------------------------------------------------------------
	
	override def equals(that:Any):Boolean	= that match {
		case that:ByteString	=> JArrays equals (this.value, that.value)
		case _					=> false
	}
	
	override def hashCode():Int		= JArrays hashCode value
	
	override def toString:String	= "[" + value.size.toString + " bytes]"
}
