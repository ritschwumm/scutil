package scutil.lang.extension

import scala.quoted.*

import scutil.lang.tc.Show

object ShowLiterals {
	extension (inline peer:StringContext) {
		/** provide a string interpolator "show" that requires a Show instance for all arguments */
		inline def show(inline args:Any*):String	= ${ showImpl('peer, 'args) }
	}

	private def showImpl(context:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String]	= {
		import quotes.reflect.*

		/*
		// NOTE probably not needed, we just use the standard s interpolator
		val literals	=
			context.valueOrAbort.parts map { raw =>
				try {
					StringContext processEscapes raw
				}
				catch { case e:Exception =>
					report.errorAndAbort(s"failed to escape part: ${e.getMessage}")
				}
			}
		*/

		val varArgs	=
			args match {
				case Varargs(x)	=> x
				case _			=> report.errorAndAbort("expected varargs")
			}

		val shown	=
			varArgs map { case '{ $arg: typ } =>
				/*
				// NOTE for an invariant show, this requires widening
				val t = TypeRepr.of[typ].widen
				println(s"type ${t} dealias ${t.dealias} widen ${t.widen} simplified ${t.simplified}")
				*/

				val instance	=
					Expr.summon[Show[typ]].getOrElse(report.errorAndAbort(s"cannot summon an instance of ${Type.show[Show[typ]]}", arg))

				'{ $instance.show($arg) }
			}

		val varShown = Varargs(shown)

		'{
			$context.s($varShown*)
		}
	}
}
