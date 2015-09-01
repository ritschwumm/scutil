package scutil.lang.pimp

import scutil.lang._

object StatefulImplicits extends StatefulImplicits

trait StatefulImplicits {
	implicit def toStatefulExt[T,X](peer:Stateful[T,X])	= new StatefulExt[T,X](peer)
}

final class StatefulExt[T,X](peer:Stateful[T,X]) {
	/** symbolic alias for andThenFixed */
	def >=>[Y](that:Stateful[T,Y]):Stateful[T,(X,Y)]	=
			andThenFixed(that)
			
	/** symbolic alias for composeFixed */
	def <=<[W](that:Stateful[T,W]):Stateful[T,(W,X)]	=
			composeFixed(that)
			
	def andThenFixed[Y](that:Stateful[T,Y]):Stateful[T,(X,Y)]	=
			t	=> {
				val (t2, x)	= peer(t)
				val (t3, y)	= that(t2)
				(t3, (x, y))
			}
			
	def composeFixed[W](that:Stateful[T,W]):Stateful[T,(W,X)]	=
			t	=> {
				val (t2, w)	= that(t)
				val (t3, x)	= peer(t2)
				(t3, (w, x))
			}
}
