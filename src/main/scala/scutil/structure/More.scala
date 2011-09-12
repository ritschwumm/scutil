/*
package scutil.structure.versatz

trait Functor[Z[_]] {
	def map[S,T](t:Z[S], f:S=>T):Z[T]
}

trait Pointed[Z[_]] {
	def pure[S](s:S):Z[S]
}

trait PointedFunctor[Z[_]] extends Pointed[Z] with Functor[Z]

trait Applicative[Z[_]] extends PointedFunctor[Z] {
	def ap[S,T](v:Z[S], f:Z[S=>T]):Z[T]
	def map[S,T](s:Z[S], f:S=>T):Z[T]	= ap(s, pure(f))
}

trait Monad[Z[_]] extends Applicative[Z] {
	// NOTE either map+flatten or flatMap has to be implemented
	override def map[S,T](s:Z[S], f:S=>T):Z[T]	= flatMap(s, f andThen pure[T])
	def ap[S,T](s:Z[S], f:Z[S=>T]):Z[T]			= flatMap(f, (ff:S=>T) => map(s, (ss:S) => ff(ss)))
	def flatMap[S,T](s:Z[S], f:S=>Z[T]):Z[T]	= flatten(map(s, f))
	def flatten[T](s:Z[Z[T]]):Z[T]				= flatMap(s, identity[Z[T]])
}

// TODO filter, Alternative, MonadPlus, Traverse

trait Foldable[Z[_]] extends Functor[Z] {
	def fold[S,T](t:T, s:Z[S], f:(T,S)=>T):T
}

trait CoFunctor[Z[_]] {
	def map[S,T](s:Z[S], f:T=>S):Z[T]
}

//------------------------------------------------------------------------------

// Magma?

trait Semigroup[T] {
	def append(a:T, b:T):T
}

trait Monoid[T] extends Semigroup[T] {
	def zero:T
}
*/
