package scutil.ext

object AnyImplicits extends AnyImplicits

trait AnyImplicits {
	implicit def toAnyExt[T](delegate:T):AnyExt[T] = new AnyExt[T](delegate)
}

final class AnyExt[T](delegate:T) {
	/** ensure we get the java wrapper */
	def boxed:AnyRef	= delegate.asInstanceOf[AnyRef]
	
	/** apply a single unary function, just like F#'s operator or ruby's thrush */
	def |>[U](f:T=>U):U	= f(delegate)
	
	/** same as |> but with different precendence */
	def into[U](f:T=>U):U	= f(delegate)
	
	/** apply an effect, then return the delegate itself. */
	def |>>(effect:T=>Unit):T = {
		effect apply delegate
		delegate
	}
	
	/** apply a number of effects, then return the delegate itself; inspired by clojure's doto */
	def doto(effects:(T=>Unit)*):T	= {
		effects foreach { _ apply delegate }
		delegate
	}
	
	/** type-invariant equality */
	def ====[U](that:U)(implicit ev:T=:=U):Boolean	= delegate == that
	
	/** type-invariant inequality */
	def !===[U](that:T)(implicit ev:T=:=U):Boolean	= delegate != that
	
	/** match lifted to an Option */ 
	def matchOption[U](pf:PartialFunction[T,U]):Option[U] =
			if (pf isDefinedAt delegate)	Some(pf(delegate))
			else							None
			// pf.lift apply delegate	
			// Some(delegate) collect pf
			// PartialFunction.condOpt(pf)(delegate)
			
	/*
	// NOTE works only for covariant type parameters
	// typesafe casting
	def guardInstanceOf[U](implicit tm:Manifest[T], um:Manifest[U]):Option[U] = {
		def sameArgs	= (um.typeArguments zip tm.typeArguments) forall { case (ua,ta) => ua >:> ta }
		if (um >:> tm && sameArgs)	Some(delegate.asInstanceOf[U])
		else						None
	}
	*/
	
	/** Some if the predicate matches, else None */
	def guardBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(delegate)) Some(delegate) else None
			
	/** None if the predicate matches, else Some */
	def preventBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(delegate)) None else Some(delegate)
			
	/** Right if the predicate matches, else Left */
	def eitherBy(predicate:T=>Boolean):Either[T,T]	= 
			if (predicate(delegate)) Right(delegate) else Left(delegate)
	
	/** Right if Some else original value in Left */
	def rightBy(func:T=>Option[T]):Either[T,T]	=
			func(delegate) toRight delegate

	/** Left if Some else original value in Right */
	def leftBy(func:T=>Option[T]):Either[T,T]	=
			func(delegate) toLeft delegate	
				
	/** pair with function applied */
	def firstBy[U](func:T=>U):(T,U)	=
			(delegate, func(delegate))

	/** pair with function applied */
	def secondBy[U](func:T=>U):(U,T)	=
			(func(delegate), delegate)
	
	/** pair with itself */ 
	def duplicate:(T,T)	= (delegate,delegate)
}
