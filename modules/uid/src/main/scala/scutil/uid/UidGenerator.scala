package scutil.uid

import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicLong

import scutil.platform.MachineId

/** creates randomized, unique ids. no synchronization necessary. */
@deprecated("use scutil.guid.Guid", "0.144.0")
object UidGenerator {
	// NOTE new SecureRandom works, too.
	// NOTE why does SecureRandom.getInstanceStrong() make this hang?
	// NOTE SecureRandom is guaranteed to be thread safe
	private val random	= SecureRandom getInstance "SHA1PRNG"
			
	private val counter:AtomicLong	=
			new AtomicLong(random.nextLong())
	
	def next():Uid	=
			Uid(
				MachineId.long,
				counter.incrementAndGet,
				System.currentTimeMillis,
				random.nextLong()
			)
}
