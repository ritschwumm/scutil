package scutil

object Lens {
	def apply[S,T](getFunc:S=>T, setFunc:(S,T)=>S):Lens[S,T] = 
			new FunctionLens[S,T](getFunc, setFunc)
			
	def trivial[T]:Lens[T,Unit]	= Lens(
			t		=> (),
			(t,_)	=> t)
	
	def identity[T]:Lens[T,T]	= Lens(
			t => t,
			(_,t) => t)
			
	def first[S,T]:Lens[(S,T),S]	= Lens(
			p		=> p._1,
			(p,t)	=> (t, p._2))

	def second[S,T]:Lens[(S,T),T]	= Lens(
			p		=> p._2,
			(p,t)	=> (p._1, t))
}

trait Lens[S,T] {
	// can be used as scala getter - but not as a setter
	final def apply(s:S):T			= get(s)
	// final def update(s:S, t:T):S	= put(s,T)
	
	def get(s:S):T
	def put(s:S, t:T):S
	
	def modify(s:S, func:T=>T):S	=
			put(s, func(get(s)))
	
	/** map the value in both directions */
	def mapValue[U](bijection:Bijection[T,U]):Lens[S,U]	= Lens(
			s		=> bijection write (this get s),
			(s,u)	=> this put (s, bijection read u))
		
	/** map the container in both directions */
	def mapContainer[R](bijection:Bijection[R,S]):Lens[R,T]	= Lens(
			r		=> this get (bijection write r),
			(r,t)	=> bijection read (this put ((bijection write r), t)))
	
	def compose[R](that:Lens[R,S]):Lens[R,T]	= Lens(
			r		=> this get (that get r),
			(r,t)	=> that put (r, this put (that get r, t)))	// that.modify(r, set(_,t)) 
	
	def andThen[U](that:Lens[T,U]):Lens[S,U]	= 
			that compose this
		
	/** set two values from a pair */
	def zip[U](that:Lens[S,U]):Lens[S,(T,U)]	= Lens(
			s		=> (this get s, that get s),
			(s,tu)	=> that put (this put (s,tu._1), tu._2))
			
	/*
	// TODO generalize to any Functor
	def liftSeq:Lens[Seq[S],Seq[T]]	= Lens(
			(c)		=> c map get,
			(c,v)	=> c zip v map (put _ tupled))
	*/
}

private final class FunctionLens[S,T](getFunc:S=>T, putFunc:(S,T)=>S) extends Lens[S,T] {
	def get(s:S):T		= getFunc(s)
	def put(s:S, t:T):S	= putFunc(s,t)
}
