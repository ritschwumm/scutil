package scutil.lang.tc

import java.util.UUID
import java.net.URL
import java.net.URI
import java.io.File
import java.nio.file.Path
import java.math.{ BigInteger as JBigInteger, BigDecimal as JBigDecimal}

object Show {
	def apply[T](using ev:Show[T]):Show[T]	= ev

	def instance[T](func:T=>String):Show[T]	=
		new Show[T] {
			def show(it:T):String	= func(it)
		}

	def toStringInstance[T]:Show[T]	=
		Show instance (_.toString)

	def doit[T](value:T)(using S:Show[T]):String	= S show value

	//------------------------------------------------------------------------------

	given Show[Byte]		= Show.toStringInstance
	given Show[Short]		= Show.toStringInstance
	given Show[Int]			= Show.toStringInstance
	given Show[Long]		= Show.toStringInstance
	given Show[Float]		= Show.toStringInstance
	given Show[Double]		= Show.toStringInstance
	given Show[Char]		= Show.toStringInstance
	given Show[Boolean]		= Show.toStringInstance

	given Show[String]		= Show instance identity

	given BigIntShow:Show[BigInt]			= Show.toStringInstance
	given BigDecimalShow:Show[BigDecimal]	= Show.toStringInstance

	given JBigIntegerShow:Show[JBigInteger]	= Show.toStringInstance
	given JBigDecimalShow:Show[JBigDecimal]	= Show.toStringInstance

	given Show[Thread]		= Show.toStringInstance
	given Show[Class[?]]	= Show.toStringInstance
	given Show[UUID]		= Show.toStringInstance
	given Show[URL]			= Show.toStringInstance
	given Show[URI]			= Show.toStringInstance
	given Show[File]		= Show instance (_.getPath)
	given Show[Path]		= Show.toStringInstance
}

trait Show[-T] {
	def show(it:T):String

	def contraMap[S](func:S=>T):Show[S]	=
		Show instance { func andThen show }
}
