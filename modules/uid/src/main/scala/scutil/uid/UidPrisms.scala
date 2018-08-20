package scutil.uid

import scutil.lang._
import scutil.base.implicits._

@deprecated("use scutil.guid.Guid", "0.144.0")
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

	val String:Prism[String,Uid]	=
			Prism(
				(it:String)	=> {
					try {
						val number	= BigInt(it, 16)
						Some(
							Uid(
								machine	= (number >> 192).toLong,
								counter	= (number >> 128).toLong,
								time	= (number >>  64).toLong,
								random	= (number >>   0).toLong
							)
						)
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
