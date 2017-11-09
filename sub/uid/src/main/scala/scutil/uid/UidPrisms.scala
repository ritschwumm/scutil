package scutil.uid

import java.nio.ByteBuffer

import scutil.lang._
import scutil.base.implicits._

object UidPrisms {
	val ByteString:Prism[ByteString,Uid]	=
			Prism(
				(it:ByteString)	=> {
					it.size == 4*8 option {
						val bb	= it.unsafeByteBuffer
						Uid(
							machine	= bb.getLong,
							counter	= bb.getLong,
							time	= bb.getLong,
							random	= bb.getLong
						)
					}
				},
				(it:Uid)	=> {
					(scutil.lang.ByteString makeWithByteBuffer 4*8) (
						_ putLong it.counter putLong it.time putLong it.random
					)
				}
			)
			
	@deprecated("0.126.0", "use ByteString")
	val ByteArray:Prism[Array[Byte],Uid]	=
			Prism(
				(it:Array[Byte])	=> {
					it.size == 4*8 option {
						val bb	= ByteBuffer wrap it
						Uid(
							machine	= bb.getLong,
							counter	= bb.getLong,
							time	= bb.getLong,
							random	= bb.getLong
						)
					}
				},
				(it:Uid)	=> {
					ByteBuffer allocate 4*8 putLong it.counter putLong it.time  putLong it.random array()
				}
			)

	val String:Prism[String,Uid]	=
			Prism(
				(it:String)	=> {
					try {
						val number	= BigInt(it, 16)
						Some(Uid(
							machine	= (number >> 192).toLong,
							counter	= (number >> 128).toLong,
							time	= (number >>  64).toLong,
							random	= (number >>   0).toLong
						))
					}
					catch { case e:NumberFormatException	=>
						None
					}
				},
				(it:Uid)	=> {
					"%016x%016x%016x%016x" format (
						it.machine,
						it.counter,
						it.time,
						it.random
					)
				}
			)
}
