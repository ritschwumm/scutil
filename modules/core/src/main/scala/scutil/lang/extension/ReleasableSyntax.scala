package scutil.lang.syntaxes

import scala.util.Using.Releasable

object ReleaseableSyntax {
	implicit final class ReleaseableSyntaxExt[T](peer:T)(using RS:Releasable[T]) {
		/** do something to us, then dispose */
		def use[U](consume:T=>U):U	= {
			var primary:Throwable	= null
			try {
				consume(peer)
			}
			catch { case e:Throwable	=>
				primary	= e
				throw e
			}
			finally {
				try {
					RS release peer
				}
				catch { case e:Throwable	=>
					if (primary ne null)	primary addSuppressed e
					else					throw e
				}
			}
		}
	}
}
