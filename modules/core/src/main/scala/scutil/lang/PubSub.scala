package scutil.lang

import scutil.core.implicits.*
import scutil.lang.*

object PubSub {
	private final case class Item[T](key:Long, subscriber:T=>Io[Unit])

	def create[T]:Io[PubSub[T]]	=
		for {
			subscribers	<-	AtomicRef[Io,Io,Vector[Item[T]]](Vector.empty)
		}
		yield PubSub[T](
			publish	= (value:T) =>
				subscribers.get.flatMap(_.traverseVoid(_.subscriber(value))),
				// this would work, too
				//subscribers.get.flatMap(_.map(_.listener(value)).combineAll),

			subscribe	= (subscriber:T=>Io[Unit]) =>
				IoResource.disposing(
					subscribers.modify { old =>
						val key	= old.map(_.key).maxOption.getOrElse(0L)
						val out	= old :+ Item(key, subscriber)
						out -> key
					}
				)(
					(key:Long) => subscribers.update(_.filterNot(it => it.key == key)).void
				).void
		)
}

final case class PubSub[T](
	publish:	T => Io[Unit],
	/** the subscriber must not throw exceptions */
	subscribe:	(T => Io[Unit]) => IoResource[Unit]
)
