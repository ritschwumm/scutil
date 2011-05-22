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
			
	/** apply a single unary function, just like F#'s operator */
	def |>[U](f:T=>U):U	= f(delegate)
	
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
	
	/** type-invariant equality */
	def ====[U](that:U)(implicit ev:T=:=U):Boolean	= delegate == that
	
	/** type-invariant inequality */
	def !===[U](that:T)(implicit ev:T=:=U):Boolean	= delegate != that
	
	/** Some if the predicate matches, else None */
	def guardBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(delegate)) Some(delegate) else None
			
	/** None if the predicate matches, else Some */
	def preventBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(delegate)) Some(delegate) else None
			
	/** Right if the predicate matches, else Left */
	def eitherBy(predicate:T=>Boolean):Either[T,T]	= 
			if (predicate(delegate)) Right(delegate) else Left(delegate)
}
