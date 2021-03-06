package scutil.lang

import scala.reflect.ClassTag

/** similar to scala.util.control.Exception.Catch but much simpler and specialised to Either */
object Catch {
	def apply[E<:Throwable](pfunc:Throwable=>Option[E]):Catch[E]	=
		new Catch[E](pfunc)

	def partial[E<:Throwable,T](pf:PartialFunction[Throwable,E]):Catch[E]	=
		Catch(pf.lift)

	//------------------------------------------------------------------------------

	def throwable:Catch[Throwable]	=
		Catch(Some.apply)

	def exception:Catch[Exception]	=
		partial { case e:Exception => e }

	def byType[E<:Throwable:ClassTag]:Catch[E]	=
		partial { case e:E	=> e }

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def byClass[E<:Throwable](clazz:Class[E]):Catch[E]	=
		Catch { e =>
			if (clazz isInstance e)	Some(e.asInstanceOf[E])
			else					None
		}

	def byPrism[E<:Throwable](prism:Prism[Throwable,E]):Catch[E]	=
		Catch(prism.get)
}

final class Catch[E<:Throwable](func:Throwable=>Option[E]) {
	def in[T](value: =>T):Either[E,T]	=
		try {
			Right(value)
		}
		catch { case e:Throwable =>
			func(e) map Left.apply getOrElse (throw e)
		}

	def get[T](value:Thunk[T]):Either[E,T]	=
		in(value())
}
