package scutil.pimp

import scala.annotation.tailrec

import scutil.lang._

object AnyImplicits extends AnyImplicits

trait AnyImplicits {
	implicit def toAnyExt[T](peer:T):AnyExt[T] = new AnyExt[T](peer)
}

final class AnyExt[T](peer:T) {
	/** ensure we get the java wrapper */
	def boxed:AnyRef	= peer.asInstanceOf[AnyRef]
	
	/** symbolic alias for into */
	@inline
	def |>[U](f:T=>U):U		= into(f)

	/** symbolic alias for doto */
	@inline
	def |>>(effect:T=>Unit):T	= doto(effect)
	
	/** apply a single unary function, just like F#'s operator or ruby's thrush */
	def into[U](f:T=>U):U	= f(peer)
	
	/** apply an effect, then return the peer itself; inspired by clojure's doto */
	def doto(effect:T=>Unit):T	= {
		effect apply peer
		peer
	}
	
	/** do something to us, then dispose */
	def use[U](func:T=>U)(implicit ev:T=>Disposable):U = {
		var thrown	= false
		try {
			func(peer) 
		}
		catch { case e:Throwable	=> 
			thrown	= true
			throw e
		}
		finally {
			try { 
				ev(peer).dispose()
			}
			catch { case e:Throwable	=> 
				if (!thrown)	throw e
				// NOTE the exception from close has been swallowed
			}
		}
	}
	
	/** type-invariant equality */
	def ====[U](that:U)(implicit ev:T=:=U):Boolean	= peer == that
	
	/** type-invariant inequality */
	def !===[U](that:T)(implicit ev:T=:=U):Boolean	= peer != that
	
	/** match lifted to an Option */ 
	def matchOption[U](pf:PartialFunction[T,U]):Option[U] =
			if (pf isDefinedAt peer)	Some(pf(peer))
			else						None
			// pf.lift apply peer	
			// Some(peer) collect pf
			// PartialFunction.condOpt(pf)(peer)
			
	/*
	// NOTE works only for covariant type parameters
	// typesafe casting
	def guardInstanceOf[U](implicit tm:Manifest[T], um:Manifest[U]):Option[U] = {
		def sameArgs	= (um.typeArguments zip tm.typeArguments) forall { case (ua,ta) => ua >:> ta }
		if (um >:> tm && sameArgs)	Some(peer.asInstanceOf[U])
		else						None
	}
	*/
	
	/** Some if the predicate matches, else None */
	def guardBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(peer)) Some(peer) else None
			
	/** None if the predicate matches, else Some */
	def preventBy(predicate:T=>Boolean):Option[T]	= 
			if (predicate(peer)) None else Some(peer)
			
	/** Right if the predicate matches, else Left */
	def eitherBy(predicate:T=>Boolean):Either[T,T]	= 
			if (predicate(peer)) Right(peer) else Left(peer)
			
	/** Right if Some else original value in Left */
	def rightBy[U](func:PFunction[T,U]):Either[T,U]	=
			func(peer) toRight peer

	/** Left if Some else original value in Right */
	def leftBy[U](func:PFunction[T,U]):Either[U,T]	=
			func(peer) toLeft peer	
		
	/** Win if the predicate matches, else Fail */
	def triedBy(predicate:T=>Boolean):Tried[T,T]	= 
			if (predicate(peer)) Win(peer) else Fail(peer)
	
	/** Win if Some else original value in Fail */
	def winBy[U](func:PFunction[T,U]):Tried[T,U]	=
			func(peer) match {
				case Some(x)	=> Win(x)
				case None		=> Fail(peer)
			}

	/** Fail if Some else original value in Win */
	def failBy[U](func:PFunction[T,U]):Tried[U,T]	=
			func(peer) match {
				case Some(x)	=> Fail(x)
				case None		=> Win(peer)
			}
				
	/** pair with function applied */
	def firstBy[U](func:T=>U):(T,U)	=
			(peer, func(peer))

	/** pair with function applied */
	def secondBy[U](func:T=>U):(U,T)	=
			(func(peer), peer)
	
	/** pair with itself */ 
	def duplicate:(T,T)	= (peer, peer)
	
	/** apply until the value doesn't change any more */
	def fixpoint(func:Endo[T]):T	= {
		@tailrec
		def loop(it:T):T	= {
			val tmp	= func(it)
			if (tmp != it)	loop(tmp)
			else			it
		}
		loop(peer)
	}
	
	/** apply until the value doesn't change any more */
	def fixpointEq(func:Endo[T], equal:(T,T)=>Boolean):T	= {
		@tailrec
		def loop(it:T):T	= {
			val tmp	= func(it)
			if (!equal(tmp,it))	loop(tmp)
			else				it
		}
		loop(peer)
	}
	
	/** apply until the value becomes None */
	def rewrite(func:PEndo[T]):T	= {
		@tailrec
		def loop(it:T):T	=
				func(it) match {
					case Some(next)	=> loop(next)
					case None		=> it
				}
		loop(peer)
	}
	
	/** apply until the value becomes None */
	def rewritePartial(func:PartialFunction[T,T]):T	= {
		@tailrec
		def loop(it:T):T	= 
				if (func isDefinedAt it)	loop(func(it))
				else						it
		loop(peer)
	}
}
