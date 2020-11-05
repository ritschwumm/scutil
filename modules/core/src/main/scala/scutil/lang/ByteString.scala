package scutil.lang

import scala.annotation.tailrec

import java.util.{ Arrays => JArrays }
import java.nio.ByteBuffer
import java.nio.charset.Charset

import scala.collection.mutable

import scutil.lang.tc._
import scutil.bit._

object ByteString {
	val empty:ByteString			= new ByteString(Array.empty)

	def single(it:Byte):ByteString	= apply(it)

	def of(its:Byte*):ByteString	= fromCollectionSeq(its)

	//------------------------------------------------------------------------------

	def fromArray(it:Array[Byte]):ByteString	=
		makeWithArray(it.length) { tmp =>
			System.arraycopy(it, 0, tmp, 0, it.length)
		}

	def fromSeq(it:Seq[Byte]):ByteString	=
		fromCollectionSeq(it)

	/** works even with mutable collections */
	def fromCollectionSeq(it:scala.collection.Seq[Byte]):ByteString	=
		makeWithArray(it.size) { tmp =>
			val iter = it.iterator
			var i = 0
			while (iter.hasNext) {
				tmp(i) = iter.next()
				i	= i + 1
			}
		}

	def fromString(it:String, charset:Charset):ByteString	=
		new ByteString(it getBytes charset)

	def fromUtf8String(it:String):ByteString	=
		fromString(it, Charsets.utf_8)

	def fromByteBuffer(it:ByteBuffer):ByteString	=
		fromArray(it.array)

	def fromArrayBuffer(it:mutable.ArrayBuffer[Byte]):ByteString	=
		new ByteString(it.toArray)

	def sliceFromArray(it:Array[Byte], srcPos:Int, copyLength:Int):Option[ByteString]	=
		if (containsSlice(srcPos, srcPos+copyLength, it.length))	Some(unsafeSliceFromArray(it, srcPos, copyLength))
		else														None

	//------------------------------------------------------------------------------

	def fromBigEndianShort(it:Short):ByteString	=
		unsafeFromArray(ByteArrayUtil fromBigEndianShort it)

	/*
	// NOTE this would work, too:
	(ByteString makeWithByteBuffer 4) { buffer =>
		buffer putInt it
	}
	*/
	def fromBigEndianInt(it:Int):ByteString	=
		unsafeFromArray(ByteArrayUtil fromBigEndianInt it)

	def fromBigEndianLong(it:Long):ByteString	=
		unsafeFromArray(ByteArrayUtil fromBigEndianLong it)

	def fromLittleEndianShort(it:Short):ByteString	=
		unsafeFromArray(ByteArrayUtil fromLittleEndianShort it)

	def fromLittleEndianInt(it:Int):ByteString	=
		unsafeFromArray(ByteArrayUtil fromLittleEndianInt it)

	def fromLittleEndianLong(it:Long):ByteString	=
		unsafeFromArray(ByteArrayUtil fromLittleEndianLong it)

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

	def unsafeSliceFromArray(it:Array[Byte], srcPos:Int, copyLength:Int):ByteString	=
		makeWithArray(copyLength) { tmp =>
			System.arraycopy(it, srcPos, tmp, 0, copyLength)
		}

	//------------------------------------------------------------------------------

	def apply(its:Byte*):ByteString	=
		fromSeq(its)

	def unapplySeq(it:ByteString):Option[Seq[Byte]]	=
		Some(it.toSeq)

	//------------------------------------------------------------------------------

	private def containsIndex(index:Int, size:Int):Boolean	=
		index >= 0 && index < size

	private def containsGap(gap:Int, size:Int):Boolean	=
		gap >= 0 && gap <= size

	private def containsSlice(begin:Int, end:Int, size:Int):Boolean	=
		begin >= 0 && begin <= end && end <= size

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val ByteStringMonoid:Monoid[ByteString]	=
		Monoid.instance(ByteString.empty, _ ++ _)
}

/** wraps an Array[Byte] to be immutable and provide sensible equals and hashCode implementations */
final class ByteString private (private val value:Array[Byte]) {
	require(value != null, "value must not be null")

	def size:Int			= value.length
	def last:Int			= size-1
	def isEmpty:Boolean		= size == 0
	def nonEmpty:Boolean	= size != 0

	def containsIndex(index:Int):Boolean	=
		ByteString.containsIndex(index, size)

	def containsGap(gap:Int):Boolean	=
		ByteString.containsGap(gap, size)

	def containsSlice(begin:Int, end:Int):Boolean	=
		ByteString.containsSlice(begin, end, size)

	def safeIndex(rawIndex:Int):Option[Int]	=
		if (containsIndex(rawIndex))	Some(rawIndex)
		else							None

	def safeGap(rawGap:Int):Option[Int]	=
		if (ByteString.containsGap(rawGap, size))	Some(rawGap)
		else										None

	def safeSlicing(rawBegin:Int, rawEnd:Int):Option[(Int,Int)]	=
		if (containsSlice(rawBegin, rawEnd))	Some((rawBegin, rawEnd))
		else									None

	def get(index:Int):Option[Byte]	=
		if (containsIndex(index))	Some(unsafeGet(index))
		else						None

	def slice(begin:Int, end:Int):Option[ByteString]	=
			 if (!containsSlice(begin, end))	None
		else if (begin == end)					Some(ByteString.empty)
		else if (begin == 0 && end == size)		Some(this)
		else 									Some(unsafeSlice(begin, end))

	def splitFirst:Option[(ByteString,Byte)]	=
		if (size > 0)	Some((unsafeSlice(1, size), unsafeGet(0)))
		else			None

	def splitLast:Option[(ByteString,Byte)]	=
		if (size > 0)	Some((unsafeSlice(0, last), unsafeGet(last)))
		else			None

	def splitAt(index:Int):Option[(ByteString,ByteString)]	=
			 if (!containsGap(index))	None
		else if (index == 0)			Some((ByteString.empty, this))
		else if (index == size)			Some((this, ByteString.empty))
		else 							Some((unsafeSlice(0, index), unsafeSlice(index, size)))

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
			System.arraycopy(this.value, 0, tmp, 0, this.size)
			System.arraycopy(that.value, 0, tmp, this.size, that.size)
		}

	def prepend(value:Byte):ByteString	=
		(ByteString makeWithArray (size+1)) { tmp =>
			tmp(0)	= value
			System.arraycopy(this.value, 0, tmp, 1, size)
		}

	def append(value:Byte):ByteString	=
		(ByteString makeWithArray (size+1)) { tmp =>
			System.arraycopy(this.value, 0, tmp, 0, size)
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

	def reverse:ByteString	=
		ByteString unsafeFromArray (ByteArrayUtil reverse value)

	def filter(pred:Byte=>Boolean):ByteString	=
		ByteString unsafeFromArray (unsafeValue filter pred)

	def filterNot(pred:Byte=>Boolean):ByteString	=
		filter(!pred(_))

	//------------------------------------------------------------------------------

	def asString(charset:Charset):String	= new String(value, charset)
	def asUtf8String:String					= asString(Charsets.utf_8)

	def toArray:Array[Byte]	= {
		val tmp	= new Array[Byte](size)
		System.arraycopy(value, 0, tmp, 0, size)
		tmp
	}

	def toByteBuffer:ByteBuffer	=
		ByteBuffer wrap toArray

	def toSeq:Seq[Byte]			= value.to(Seq)
	def toVector:Vector[Byte]	= value.to(Vector)
	def toList:List[Byte	]	= value.to(List)
	def toSet:Set[Byte]			= value.to(Set)

	def copyIntoArray(dest:Array[Byte], destPos:Int):Boolean	=
		sliceIntoArray(0, dest, destPos, size)

	def sliceIntoArray(srcPos:Int, dest:Array[Byte], destPos:Int, copyLength:Int):Boolean	= {
		val ok	=
				(ByteString.containsSlice(srcPos,	srcPos	+ copyLength, size)) &&
				(ByteString.containsSlice(destPos,	destPos	+ copyLength, dest.length))
		if (ok)	System.arraycopy(this.value, srcPos, dest, destPos, copyLength)
		ok
	}

	def modifyArray(func:Array[Byte]=>Unit):ByteString	= {
		val tmp	= toArray
		func(tmp)
		ByteString unsafeFromArray tmp
	}

	//------------------------------------------------------------------------------

	def toByte:Option[Byte]	=
		if (size == 1)	Some(value(0))
		else			None

	def toBigEndianShort:Option[Short]	=
		if (size == 2)	Some(ByteArrayUtil toBigEndianShort value)
		else			None

	def toBigEndianInt:Option[Int]		=
		if (size == 4)	Some(ByteArrayUtil toBigEndianInt value)
		else			None

	def toBigEndianLong:Option[Long]		=
		if (size == 8)	Some(ByteArrayUtil toBigEndianLong value)
		else			None

	def toLittleEndianShort:Option[Short]	=
		if (size == 2)	Some(ByteArrayUtil toLittleEndianShort value)
		else			None

	def toLittleEndianInt:Option[Int]		=
		if (size == 4)	Some(ByteArrayUtil toLittleEndianInt value)
		else			None

	def toLittleEndianLong:Option[Long]		=
		if (size == 8)	Some(ByteArrayUtil toLittleEndianLong value)
		else			None

	//------------------------------------------------------------------------------

	// only exists for symmetry
	def getByte(offset:Int):Option[Byte]	= get(offset)

	def getBigEndianShort(offset:Int):Option[Short]	=
		if (containsSlice(offset, offset+2))	Some(ByteArrayUtil.getBigEndianShort(value, offset))
		else									None

	def getBigEndianInt(offset:Int):Option[Int]		=
		if (containsSlice(offset, offset+4))	Some(ByteArrayUtil.getBigEndianInt(value, offset))
		else									None

	def getBigEndianLong(offset:Int):Option[Long]		=
		if (containsSlice(offset, offset+8))	Some(ByteArrayUtil.getBigEndianLong(value, offset))
		else									None

	def getLittleEndianShort(offset:Int):Option[Short]	=
		if (containsSlice(offset, offset+2))	Some(ByteArrayUtil.getLittleEndianShort(value, offset))
		else									None

	def getLittleEndianInt(offset:Int):Option[Int]		=
		if (containsSlice(offset, offset+4))	Some(ByteArrayUtil.getLittleEndianInt(value, offset))
		else									None

	def getLittleEndianLong(offset:Int):Option[Long]		=
		if (containsSlice(offset, offset+8))	Some(ByteArrayUtil.getLittleEndianLong(value, offset))
		else									None

	//------------------------------------------------------------------------------

	def unsafeGet(index:Int):Byte	= value(index)
	def unsafeValue:Array[Byte]		= value
	def unsafeByteBuffer:ByteBuffer	= ByteBuffer wrap value

	private def unsafeSlice(begin:Int, end:Int):ByteString	= {
		val length	= end - begin
		(ByteString makeWithArray length) { tmp =>
			System.arraycopy(value, begin, tmp, 0, length)
		}
	}

	//------------------------------------------------------------------------------

	override def equals(that:Any):Boolean	=
		that match {
			case that:ByteString	=> JArrays.equals(this.value, that.value)
			case _					=> false
		}

	override def hashCode():Int		= JArrays hashCode value

	override def toString:String	= "[" + value.size.toString + " bytes]"
}
