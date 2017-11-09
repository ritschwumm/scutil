package scutil.lang

import scala.annotation.tailrec

import java.util.{ Arrays => JArrays }
import java.nio.ByteBuffer
import java.nio.charset.Charset

import scala.collection.mutable

import scutil.lang.tc._


object ByteString extends ByteStringInstances {
	val empty:ByteString	= new ByteString(Array.empty)
	
	def single(it:Byte):ByteString	= apply(it)
	
	//------------------------------------------------------------------------------
	
	def fromArray(it:Array[Byte]):ByteString	=
			makeWithArray(it.length) { tmp =>
				System arraycopy (it, 0, tmp, 0, it.length)
			}
	
	def fromSeq(it:Seq[Byte]):ByteString	=
			makeWithArray(it.size) { tmp =>
				val iter = it.iterator
				var i = 0
				while (iter.hasNext) {
					tmp(i) = iter.next()
					i	= i + 1
				}
			}
	
	def fromISeq(it:ISeq[Byte]):ByteString	=
			fromSeq(it)
	
	def fromString(it:String, charset:Charset):ByteString	=
			new ByteString(it getBytes charset)
		
	def fromUtf8String(it:String):ByteString	=
			fromString(it, Charsets.utf_8)
		
	def fromArrayBuffer(it:mutable.ArrayBuffer[Byte]):ByteString	=
			new ByteString(it.toArray)
		
	def sliceFromArray(it:Array[Byte], srcPos:Int, copyLength:Int):Option[ByteString]	=
			if (!containsSlice(srcPos, srcPos+copyLength, it.length))	None
			else Some {
				makeWithArray(copyLength) { tmp =>
					System arraycopy (it, srcPos, tmp, 0, copyLength)
				}
			}
	
	//------------------------------------------------------------------------------
		
	def makeWithArray(size:Int)(effect:Effect[Array[Byte]]):ByteString	= {
		val tmp	= new Array[Byte](size)
		effect(tmp)
		new ByteString(tmp)
	}
	
	def makeWithByteBuffer(size:Int)(effect:Effect[ByteBuffer]):ByteString	= {
		val tmp	= ByteBuffer allocate size
		effect(tmp)
		new ByteString(tmp.array())
	}
	
	def unsafeFromArray(it:Array[Byte]):ByteString	=
			new ByteString(it)
		
	def unsafeFromByteBuffer(it:ByteBuffer):ByteString	=
			new ByteString(it.array())
		
	val unsafeArrayBijection:Bijection[Array[Byte],ByteString]	=
			Bijection(ByteString.unsafeFromArray, _.unsafeValue)
		
	//------------------------------------------------------------------------------
	
	def apply(its:Byte*):ByteString	=
			fromSeq(its)
		
	def unapplySeq(it:ByteString):Option[Seq[Byte]]	=
			Some(it.toArray)
		
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
				(ByteString makeWithArray length) { tmp =>
					System arraycopy (value, begin, tmp, 0, length)
				}
			}
		
	def containsAt(index:Int, that:ByteString):Boolean	=
			containsSlice(index, index+that.size) && {
				@tailrec def loop(i:Int):Boolean	=
					(i == that.size) 						||
					(this value index+i) == (that value i)	&&
					loop(i+1)
				loop(0)
			}
		
	@inline def ++(that:ByteString):ByteString	=
			this concat that
	
	@inline def +:(that:Byte):ByteString	=
			this prepend that
	
	@inline def :+(that:Byte):ByteString	=
			this append that
		
	def concat(that:ByteString):ByteString	=
			(ByteString makeWithArray (this.size + that.size)) { tmp =>
				System arraycopy (this.value, 0, tmp, 0, this.size)
				System arraycopy (that.value, 0, tmp, this.size, that.size)
			}
			
	def prepend(value:Byte):ByteString	=
			(ByteString makeWithArray (size+1)) { tmp =>
				tmp(0)	= value
				System arraycopy (this.value, 0, tmp, 1, size)
			}
			
	def append(value:Byte):ByteString	=
			(ByteString makeWithArray (size+1)) { tmp =>
				System arraycopy (this.value, 0, tmp, 0, size)
				tmp(size)	= value
			}
			
	def constantTimeEquals(that:ByteString):Boolean	= {
		val length	= this.size min that.size
		var diff	= this.size ^ that.size
		var i = 0
		while (i < length) {
			diff |= (this unsafeGet i) ^ (that unsafeGet i)
			i	+= 1
		}
		diff == 0
	}
	
	//------------------------------------------------------------------------------
			
	def asString(charset:Charset):String		= new String(value, charset)
	def asUtf8String(charset:Charset):String	= asString(Charsets.utf_8)
	
	def toArray:Array[Byte]	= {
		val tmp	= new Array[Byte](size)
		System arraycopy (value, 0, tmp, 0, size)
		tmp
	}
	
	def toByteBuffer:ByteBuffer	= {
		 ByteBuffer wrap toArray
	}
	
	def toISeq:ISeq[Byte]	= value.to[ISeq]
	def toVector:ISeq[Byte]	= value.to[Vector]
	def toList:ISeq[Byte]	= value.to[List]
	def toSet:Set[Byte]		= value.to[Set]
	
	def copyIntoArray(dest:Array[Byte], destPos:Int):Boolean	=
			sliceIntoArray(0, dest, destPos, size)
		
	def sliceIntoArray(srcPos:Int, dest:Array[Byte], destPos:Int, copyLength:Int):Boolean	= {
		val ok	=
				(ByteString containsSlice (srcPos,	srcPos	+ copyLength, size)) &&
				(ByteString containsSlice (destPos,	destPos	+ copyLength, dest.length))
		if (ok)	System arraycopy (this.value, srcPos, dest, destPos, copyLength)
		ok
	}
	
	def modifyAArray(func:Array[Byte]=>Unit):ByteString	= {
		val tmp	= toArray
		func(tmp)
		ByteString unsafeFromArray tmp
	}
	
	//------------------------------------------------------------------------------
	
	def unsafeGet(index:Int):Byte	= value(index)
	def unsafeValue:Array[Byte]		= value
	def unsafeByteBuffer:ByteBuffer	= ByteBuffer wrap value
	
	//------------------------------------------------------------------------------
	
	override def equals(that:Any):Boolean	=
			that match {
				case that:ByteString	=> JArrays equals (this.value, that.value)
				case _					=> false
			}
	
	override def hashCode():Int		= JArrays hashCode value
	
	override def toString:String	= "[" + value.size.toString + " bytes]"
}

trait ByteStringInstances {
	implicit val ByteStringMonoid:Monoid[ByteString]	=
			Monoid instance (ByteString.empty, _ ++ _)
}