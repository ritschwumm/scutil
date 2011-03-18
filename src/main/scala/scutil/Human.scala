package scutil

/** format numbers in a human readable way */
object Human {
	def binary(value:BigDecimal, precision:Int):String = 
			format(value, precision, 1024, List("", "k", "M", "G", "T", "P", "E", "Z", "Y"))
	
	def decimal(value:BigDecimal, precision:Int):String =
			format(value, precision, 1000, List("", "k", "M", "G", "T", "P", "E", "Z", "Y"))
	
	def format(value:BigDecimal, precision:Int, factor:Int, prefixes:List[String]) = {
		val pair	= shorten(value, factor, prefixes)
		precise(pair._1, precision) + pair._2
	}
	
	private def shorten(value:BigDecimal, factor:Int, prefixes:List[String]):Pair[BigDecimal,String] =
			if (value > factor && prefixes.nonEmpty)	shorten(value / factor, factor, prefixes.tail)
			else										Pair(value, prefixes.head)
			
	private def precise(value:BigDecimal, digits:Int):String = 
			String.format("%." + digits + "f", Array(value.bigDecimal):_*)
}
