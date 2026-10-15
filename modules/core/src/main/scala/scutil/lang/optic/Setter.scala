package scutil.lang

object Setter {
	def identity[T]:Setter[T,T]		=
		Setter(it => it)

	def trivial[T]:Setter[T,Unit]	=
		Setter(_ => it => it)

	/*
	def first[S,T]:Setter[(S,T), S]	= ???
	def second[S,T]:Setter[(S,T), T]	= ???
	*/
}

final case class Setter[S,T](mod:(T=>T)=>(S=>S)) {
	def set(value:T):S=>S	= mod(constant(value))

	//-----------------------------------------------------------------------------

	/** symbolic alias for andThen */
	def >=>[U](that:Setter[T,U]):Setter[S,U]	=
		this.andThen(that)

	/** symbolic alias for compose */
	def <=<[R](that:Setter[R,S]):Setter[R,T]	=
		this.compose(that)

	def compose[R](that:Setter[R,S]):Setter[R,T]	=
		that.andThen(this)

	def andThen[U](that:Setter[T,U]):Setter[S,U]	=
		Setter(that.mod `andThen` this.mod)

	//-----------------------------------------------------------------------------

	/*
	def zip[U](that:Setter[S,U]):Setter[S,(T,U)]	=
		Setter { tutu => s0 =>
			val tt:T=>T	= ???
			val ss1		= this.mod(tt)
			val s1:S	= ss1(s0)

			val uu:U=>U	= ???
			val ss2		= that.mod(uu)
			val s2:S	= ss2(s1)

			val out:S	= s2
			out
		}
	*/

	def sum[SS](that:Setter[SS,T]):Setter[Either[S,SS],T]	=
		Setter { tt => e0 =>
			e0 match {
				case Left(s)	=> Left(this.mod(tt)(s))
				case Right(ss)	=> Right(that.mod(tt)(ss))
			}
		}

	/*
	def product[SS,TT](that:Setter[SS,TT]):Setter[(S,SS),(T,TT)]	=
		Setter { (tp:((T, TT)) => (T, TT))	=>
			val out:((S,SS))=>(S,SS)	=
				(s:S, ss:SS)	=> {
					this.mod { t=>
						that.mod { tt =>
							val (t2, tt2)	= tp(t, tt)
							tt2
						}
						t
					}

					val so	= ???
					val sso	= ???
					(so, sso)
				}
			out
		}
	*/
}
