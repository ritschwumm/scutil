package scutil.ext

object AnyImplicits extends AnyImplicits

trait AnyImplicits {
	implicit def toAnyExt[T](delegate:T):AnyExt[T] = new AnyExt[T](delegate)
}

final class AnyExt[T](delegate:T) {
	/** match lifted to an Option */ 
	def matchOption[U](pf:PartialFunction[T,U]):Option[U] =
			pf.lift apply delegate	
			// Some(delegate) collect pf
			// PartialFunction.condOpt(pf)(delegate)
			
	/** apply an effect, then return the delegate itself. */
	def doto(effect:Function1[T,Unit]):T = {
		effect apply delegate
		delegate
	}
	
	/** apply a number of effects, then return the delegate itself. inspired by clojure's doto */
	def dotoN(effects:Function1[T,Unit]*):T	= {
		effects foreach { _ apply delegate }
		delegate
	}
	
	/** apply a single unary function, just like F#'s operator */
	def |>[U](f:T=>U)(implicit witness:T <%< U)	= f(delegate)
	
	/** Some if the predicate matches, else None */
	def guardBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(delegate)) Some(delegate) else None
			
	/** None if the predicate matches, else Some */
	def preventBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(delegate)) Some(delegate) else None
}
