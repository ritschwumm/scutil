package scutil.ext

import scala.annotation._

object Function1Implicits extends Function1Implicits

trait Function1Implicits {
	implicit def toFunction1Ext[S,T](delegate:Function1[S,T])	= new Function1Ext[S,T](delegate)
}

final class Function1Ext[S,T](delegate:Function1[S,T]) {
	def partial(predicate:S=>Boolean):PartialFunction[S,T]	= new PartialFunction[S,T] {
		def isDefinedAt(s:S):Boolean	= predicate(s)
		def apply(s:S):T				= delegate(s)
	}
	
	/** inverse to PartialFunction#lift */
	def unlift[X](implicit ev:T=>Option[X]):PartialFunction[S,X]	=
			Function unlift delegate.asInstanceOf[S=>Option[X]]
		
	/** apply until the value doesn't change any more */
	@tailrec
	def fixpoint(it:S)(implicit ev:T=>S):S	= {
		val tmp	= delegate(it)
		if (tmp != it)	fixpoint(tmp)
		else			it
	}
	
	/** apply until the value doesn't change any more */
	@tailrec
	def fixpointEq(it:S, equal:(S,S)=>Boolean)(implicit ev:T=>S):S	= {
		val tmp	= delegate(it)
		if (!equal(tmp,it))	fixpointEq(tmp, equal)
		else				it
	}
	
	/** apply until the value becomes None */
	@tailrec
	def rewrite(it:S)(implicit ev:T=>Option[S]):S	= {
		ev(delegate(it)) match {
			case Some(next)	=> rewrite(next)
			case None		=> it
		}
	}
}
