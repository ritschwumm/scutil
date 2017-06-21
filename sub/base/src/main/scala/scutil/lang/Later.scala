package scutil.lang

import scutil.lang.tc._

object Later extends LaterInstances {
	def empty[T]:Later[T]	=
			Later { cont => }
		
	def pure[T](value:T):Later[T]	=
			Later { cont =>
				cont(value)
			}
			
	def delay[T](value: =>T):Later[T]	=
			thunk(() => value)
		
	def thunk[T](value:()=>T):Later[T]	=
			Later { cont =>
				cont(value())
			}
		
	def many[T](value:Iterable[T]):Later[T]	=
			Later { cont =>
				value foreach cont
			}
}

final case class Later[T](run:(T=>Unit)=>Unit) {
	def runUnit():Unit	=
			runFold(())((_,_)=>())
	
	def runVector():Vector[T]	=
			runFold(Vector.empty[T])(_ :+ _)
		
	// TODO merge with fold somehow?
	def runFold[U](initial:U)(combine:(U,T)=>U):U	= {
		var state	= initial
		run { item =>
			state	= combine(state, item)		
		}
		state
	}
		
	//------------------------------------------------------------------------------
	
	def map[U](func:T=>U):Later[U]	=
			Later { cont =>
				run { item =>
					cont(func(item))
				}
			}
			
	def flatMap[U](func:T=>Later[U]):Later[U]	=
			Later { cont =>
				run { item =>
					func(item) run cont
				}
			}
			
	def flatten[U](implicit ev:T=>Later[U]):Later[U]	=
			flatMap(ev)
		
	def collapseMap[U](func:T=>Option[U]):Later[U]	=
			Later { cont =>
				run { item =>
					func(item) foreach cont
				}
			}
			
	def collapse[U](implicit ev:T=>Option[U]):Later[U]	=
			collapseMap(ev)
			
	def filter(pred:T=>Boolean):Later[T]	=
			Later { cont =>
				run(t => if (pred(t)) cont(t))
			}
		
	def filterNot(pred:T=>Boolean):Later[T]	=
			Later { cont =>
				run(t => if (!pred(t)) cont(t))
			}
		
	def concat(that:Later[T]):Later[T]	=
			Later { cont =>
				this run cont
				that run cont
			}
			
	def prepend(item:T):Later[T]	=
			Later { cont =>
				cont(item)
				run(cont)
			}
			
	def append(item:T):Later[T]	=
			Later { cont =>
				run(cont)
				cont(item)
			}
			
	def exists(pred:T=>Boolean):Later[Boolean]	=
			Later { cont =>
				var state	= false
				run { item =>
					state	= state || pred(item)
				}
				cont(state)
			}
			
	def forall(pred:T=>Boolean):Later[Boolean]	=
			Later { cont =>
				var state	= true
				run { item =>
					state	= state && pred(item)
				}
				cont(state)
			}
		
	def take(size:Long):Later[T]	=
			whereIndex(_ < size)
		
	def drop(size:Long):Later[T]	=
			whereIndex(_ >= size)
		
	def whereIndex(pred:Long=>Boolean):Later[T]	=
			zipWithIndex
			.filter { case (_, index)	=> pred(index) }
			.map	{ case (value, _)	=> value }
			
	def zipWithIndex:Later[(T,Long)]	=
			scan(0L) { (index, _) => index + 1 }
		
	def fold[U](initial:U)(combine:(U,T)=>U):Later[U]	=
			scan(initial)(combine) map (_._2)
		
	def scan[U](initial:U)(combine:(U,T)=>U):Later[(T,U)]	=
			Later { cont =>
				var state	= initial
				run { item =>
					cont(item -> state)
					state	= combine(state, item)
				}
			}
			
	def grouped(size:Long):Later[Vector[T]]	=
			Later { cont =>
				var batch	= Vector.empty[T]
				run { item =>
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

trait LaterInstances {
	implicit val LaterDelay:Delay[Later]	=
			new Delay[Later] {
				override def delay[R](it: =>R):Later[R]		= Later delay it
				override def thunk[R](it:Thunk[R]):Later[R]	= Later thunk it
			}
			
	implicit val LaterMonad:Monad[Later]	=
			new Monad[Later] {
				override def pure[R](it:R):Later[R]										= Later pure it
				override def map[R,RR](it:Later[R])(func:R=>RR):Later[RR]				= it map func
				override def flatMap[R,RR](it:Later[R])(func:R=>Later[RR]):Later[RR]	= it flatMap func
			}
}
