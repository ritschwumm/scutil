package scutil

import scutil.lang._

final class Resource[+T](value:T, close:Task) {
	def use[U](work:T=>U):U = {
		var thrown	= false
		try {
			work(value) 
		}
		catch { case e:Throwable	=> 
			thrown	= true
			throw e
		}
		finally {
			try { 
				close()
			}
			catch { case e:Throwable	=> 
				if (!thrown)	throw e
				// NOTE the exception from close has been swallowed
			}
		}
	}
}
