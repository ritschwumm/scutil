package scutil.lang.pimp

import scutil.lang._

object Function2Implicits extends Function2Implicits

trait Function2Implicits {
	implicit final class Function2Ext[S1,S2,T](peer:Function2[S1,S2,T]) {
		def flip:Function2[S2,S1,T]	= (s2:S2, s1:S1) => peer(s1,s2)

		def toThunk(s1:S1, s2:S2):Thunk[T]	= () => peer(s1, s2)
	}
}
