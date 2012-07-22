package scutil.ext

object Function2Implicits extends Function2Implicits

trait Function2Implicits {
	implicit def toFunction2Ext[S1,S2,T](delegate:Function2[S1,S2,T])	= new Function2Ext[S1,S2,T](delegate)
}

final class Function2Ext[S1,S2,T](delegate:Function2[S1,S2,T]) {
	def flip:Function2[S2,S1,T]	= (s2:S2, s1:S1) => delegate(s1,s2)
}