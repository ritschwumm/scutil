package scutil.lang.extension

import scala.reflect.macros.blackbox.Context

private final class ShowMacros(val c:Context) {
	import c.universe._

	def showImpl(args:c.Expr[Any]*):c.Expr[String]	=
		c.prefix.tree match {
			case Apply(_, List(Apply(_, literals)))	=>
				// literals are a Seq of Literal(Constant(String))
				val escapeds:Seq[c.Tree]	=
						literals map {
							case Literal(const@Constant(raw:String)) =>
								val escaped	=
										try {
											StringContext processEscapes raw
										}
										catch { case e:Exception =>
											// TODO have a better position here
											c.abort(c.enclosingPosition, e.getMessage)
										}
								q"""$escaped"""
							case x	=>
								c.abort(c.enclosingPosition,  s"expected a string literal, found ${x.toString}")
						}

				val inserts:Seq[c.Tree]		= args	map { expr => q"""_root_.scutil.lang.tc.Show.doit($expr)""" }

				@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
				val parts:Seq[c.Tree]		= (escapeds zip inserts flatMap { case (lit, ins) => Seq(lit, ins) }) :+ q"""${escapeds.last}"""
				@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
				val full:c.Tree				= parts reduce { (a, b) => q"""$a + $b""" }
				c.Expr[String](full)
			case x =>
				c.abort(c.enclosingPosition, s"expected a string literal, found ${x.toString}")
	}
}
