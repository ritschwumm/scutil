package scutil.uid

import java.security.SecureRandom
import java.util.Random
import java.util.concurrent.atomic.AtomicLong

import scutil.platform.MachineId

/** creates randomized, unique ids. no synchronization necessary. */
final class UidGenerator(secure:Boolean) {
	private val random	=
			if (secure)	SecureRandom getInstance "SHA1PRNG"
			else		new Random
			
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
