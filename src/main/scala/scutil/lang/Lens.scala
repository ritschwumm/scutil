package scutil.lang

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
			
	def codiag[T]:Lens[Either[T,T],T]	=
			identity[T] ||| identity[T]
			
	def map[K,V](k:K):Lens[Map[K,V],Option[V]]	= Lens(
			_ get k,
			(c,v)	=> v match {
				case Some(v)	=> c + (k -> v)
				case None		=> c - k
			})
			
	def set[T](t:T):Lens[Set[T],Boolean]	= Lens(
			_ contains t,
			(c,v)	=> if (v) c + t else c - t)
}

trait Lens[S,T] {
	/** get the detail value */
	def get(s:S):T
	/** set the detail value */
	def put(s:S, t:T):S
	
	/** put with flipped arguments, curried */
	def putter(t:T):S=>S	= 
			s => put(s,t)
	
	def modify(s:S, func:T=>T):S	=
			put(s, func(get(s)))
		
	/** modify with flipped arguments, curried */
	def modifier(func:T=>T):S=>S	= 
			modify(_, func)
	
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
			
	// either
	def |||[SS](that:Lens[SS,T]):Lens[Either[S,SS],T]	= Lens(
			sss	=> sss match {
				case Left(s)	=> this get s
				case Right(ss)	=> that get ss
			},
			(sss,t)	=> sss match {
				case Left(s)	=> Left(this put (s, t))
				case Right(ss)	=> Right(that put (ss, t))
			})
	
	// pair
	def ***[SS,TT](that:Lens[SS,TT]):Lens[(S,SS),(T,TT)]	= Lens(
		(sss)		=> (this get sss._1,			that get sss._2),
		(sss,ttt)	=> (this put (sss._1, ttt._1),	that put (sss._2, ttt._2))
	)
	
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
