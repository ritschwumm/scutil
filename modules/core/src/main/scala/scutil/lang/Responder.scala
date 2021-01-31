package scutil.lang

import scutil.lang.tc._

object Responder {
	@deprecated("use never", "0.201.0")
	def empty[T]:Responder[T]	=
		never

	def never[T]:Responder[T]	=
		Responder { cont => }

	def pure[T](value:T):Responder[T]	=
		Responder { cont =>
			cont(value)
		}

	def delay[T](value: =>T):Responder[T]	=
		thunk(() => value)

	def thunk[T](value:()=>T):Responder[T]	=
		Responder { cont =>
			cont(value())
		}

	def optional[T](value:Option[T]):Responder[T]	=
		Responder { cont =>
			value foreach cont
		}

	def many[T](value:Iterable[T]):Responder[T]	=
		Responder { cont =>
			value foreach cont
		}

	def of[T](values:T*):Responder[T]	=
		many(values)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val ResponderMonad:Monad[Responder]	=
		new Monad[Responder] {
			override def pure[R](it:R):Responder[R]												= Responder pure it
			override def map[R,RR](it:Responder[R])(func:R=>RR):Responder[RR]					= it map func
			override def flatMap[R,RR](it:Responder[R])(func:R=>Responder[RR]):Responder[RR]	= it flatMap func
		}

	implicit val ResponderDelay:Delay[Responder]	=
		new Delay[Responder] {
			override def delay[R](it: =>R):Responder[R]	= Responder delay it
		}
}

final case class Responder[T](unsafeRun:(T=>Unit)=>Unit) {
	def foreach(handler:T=>Unit):Unit	=
		unsafeRun(handler)

	def runUnit():Unit	=
		runFold(())((_,_)=>())

	def runVector():Vector[T]	=
		runFold(Vector.empty[T])(_ :+ _)

	def runFold[U](initial:U)(combine:(U,T)=>U):U	= {
		var state	= initial
		unsafeRun { item =>
			state	= combine(state, item)
		}
		state
	}

	//------------------------------------------------------------------------------

	def map[U](func:T=>U):Responder[U]	=
		Responder { cont =>
			unsafeRun { item =>
				cont(func(item))
			}
		}

	def flatMap[U](func:T=>Responder[U]):Responder[U]	=
		Responder { cont =>
			unsafeRun { item =>
				func(item) unsafeRun cont
			}
		}

	def flatten[U](implicit ev:T=>Responder[U]):Responder[U]	=
		flatMap(ev)

	def mapFilter[U](func:T=>Option[U]):Responder[U]	=
		Responder { cont =>
			unsafeRun { item =>
				func(item) foreach cont
			}
		}

	def flattenOption[U](implicit ev:T=>Option[U]):Responder[U]	=
		mapFilter(ev)

	def collect[U](pf:PartialFunction[T,U]):Responder[U]	=
		mapFilter(pf.lift)

	def withFilter(pred:T=>Boolean):Responder[T]	=
		filter(pred)

	def filter(pred:T=>Boolean):Responder[T]	=
		Responder { cont =>
			unsafeRun(t => if (pred(t)) cont(t))
		}

	def filterNot(pred:T=>Boolean):Responder[T]	=
		Responder { cont =>
			unsafeRun(t => if (!pred(t)) cont(t))
		}

	def concat(that:Responder[T]):Responder[T]	=
		Responder { cont =>
			this unsafeRun cont
			that unsafeRun cont
		}

	def prepend(item:T):Responder[T]	=
		Responder { cont =>
			cont(item)
			unsafeRun(cont)
		}

	def append(item:T):Responder[T]	=
		Responder { cont =>
			unsafeRun(cont)
			cont(item)
		}

	def exists(pred:T=>Boolean):Responder[Boolean]	=
		Responder { cont =>
			var state	= false
			unsafeRun { item =>
				state	= state || pred(item)
			}
			cont(state)
		}

	def forall(pred:T=>Boolean):Responder[Boolean]	=
		Responder { cont =>
			var state	= true
			unsafeRun { item =>
				state	= state && pred(item)
			}
			cont(state)
		}

	def take(size:Long):Responder[T]	=
		whereIndex(_ < size)

	def drop(size:Long):Responder[T]	=
		whereIndex(_ >= size)

	def whereIndex(pred:Long=>Boolean):Responder[T]	=
		zipWithIndex
		.filter { case (_, index)	=> pred(index) }
		.map	{ case (value, _)	=> value }

	def zipWithIndex:Responder[(T,Long)]	=
		scan(0L) { (index, _) => index + 1 }

	def fold[U](initial:U)(combine:(U,T)=>U):Responder[U]	=
		scan(initial)(combine) map (_._2)

	def scan[U](initial:U)(combine:(U,T)=>U):Responder[(T,U)]	=
		Responder { cont =>
			var state	= initial
			unsafeRun { item =>
				cont(item -> state)
				state	= combine(state, item)
			}
		}

	def grouped(size:Long):Responder[Vector[T]]	=
		Responder { cont =>
			var batch	= Vector.empty[T]
			unsafeRun { item =>
				batch	:+= item
				if (batch.size >= size) {
					cont(batch)
					batch	= Vector.empty
				}
			}
			if (batch.nonEmpty) {
				cont(batch)
			}
		}
}
