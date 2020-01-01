package scutil.text

import java.util.Locale

import scutil.lang._
import scutil.base.implicits._

/** format (positive) numbers in a human readable way */
object Human {
	private val zero	= BigDecimal(0)
	private val one		= BigDecimal(1)

	def multi(head:HumanUnit, tail:HumanUnit*):Human	=
		Human(Nes(head, tail.toVector))

	//------------------------------------------------------------------------------
	//## predefined tables

	val binary:Human	=
		Human multi (
			HumanUnit("yotta",	"Y",	one*1024*1024*1024*1024*1024*1024*1024*1024),
			HumanUnit("zeta",	"Z",	one*1024*1024*1024*1024*1024*1024*1024),
			HumanUnit("exa",	"E",	one*1024*1024*1024*1024*1024*1024),
			HumanUnit("peta",	"P",	one*1024*1024*1024*1024*1024),
			HumanUnit("tera",	"T",	one*1024*1024*1024*1024),
			HumanUnit("giga",	"G",	one*1024*1024*1024),
			HumanUnit("mega",	"M",	one*1024*1024),
			HumanUnit("kilo",	"k",	one*1024),
			HumanUnit("",		"",		one),
			HumanUnit("milli",	"m",	one/1024),
			HumanUnit("micro",	"µ",	one/1024/1024),
			HumanUnit("nano",	"n",	one/1024/1024/1024/1024),
			HumanUnit("pico",	"p",	one/1024/1024/1024/1024/1024),
			HumanUnit("femto",	"f",	one/1024/1024/1024/1024/1024/1024),
			HumanUnit("atto",	"a",	one/1024/1024/1024/1024/1024/1024/1024),
			HumanUnit("zepto",	"z",	one/1024/1024/1024/1024/1024/1024/1024/1024),
			HumanUnit("yocto",	"y",	one/1024/1024/1024/1024/1024/1024/1024/1024/1024)
		)

	val decimal:Human	=
		Human multi (
			HumanUnit("yotta",	"Y",	one*1000*1000*1000*1000*1000*1000*1000*1000),
			HumanUnit("zeta",	"Z",	one*1000*1000*1000*1000*1000*1000*1000),
			HumanUnit("exa",	"E",	one*1000*1000*1000*1000*1000*1000),
			HumanUnit("peta",	"P",	one*1000*1000*1000*1000*1000),
			HumanUnit("tera",	"T",	one*1000*1000*1000*1000),
			HumanUnit("giga",	"G",	one*1000*1000*1000),
			HumanUnit("mega",	"M",	one*1000*1000),
			HumanUnit("kilo",	"k",	one*1000),
			HumanUnit("",		"",		one),
			HumanUnit("milli",	"m",	one/1000),
			HumanUnit("micro",	"µ",	one/1000/1000),
			HumanUnit("nano",	"n",	one/1000/1000/1000/1000),
			HumanUnit("pico",	"p",	one/1000/1000/1000/1000/1000),
			HumanUnit("femto",	"f",	one/1000/1000/1000/1000/1000/1000),
			HumanUnit("atto",	"a",	one/1000/1000/1000/1000/1000/1000/1000),
			HumanUnit("zepto",	"z",	one/1000/1000/1000/1000/1000/1000/1000/1000),
			HumanUnit("yocto",	"y",	one/1000/1000/1000/1000/1000/1000/1000/1000/1000)
		)

	val time:Human	=
		Human multi (
			HumanUnit("year",			"y",	one*60*60*24*365.24219052),	// tropical year
			HumanUnit("day",			"d",	one*60*60*24),
			HumanUnit("hour",			"h",	one*60*60),
			HumanUnit("minute",			"m",	one*60),
			HumanUnit("second",			"s",	one),
			HumanUnit("millisecond",	"ms",	one/1000),
			HumanUnit("nanosecond",		"ns",	one/1000/1000)
		)

	val degrees:Human	=
		Human multi (
			HumanUnit("degree",	"°",	one),
			HumanUnit("minute",	"'",	one/60),
			HumanUnit("second",	"''",	one/60/60)
		)

	//------------------------------------------------------------------------------
	//## convenience functions

	val full	= HumanConfig(smallUnits = 0, decimalPlaces = 0)
	val rounded	= HumanConfig(smallUnits = 0, maxUnits = 1, decimalPlaces = 2)

	val fullBinary:BigDecimal=>String		= binary	renderer full
	val fullDecimal:BigDecimal=>String		= decimal	renderer full
	val fullTime:BigDecimal=>String			= time		renderer full

	val roundedBinary:BigDecimal=>String	= binary	renderer rounded
	val roundedDecimal:BigDecimal=>String	= decimal	renderer rounded
	val roundedTime:BigDecimal=>String		= time		renderer rounded

	//------------------------------------------------------------------------------
	//## more convenience functions

	val roundedDms:BigDecimal=>String	=
		degrees renderer HumanConfig(decimalPlaces=3)

	private val millisToSeconds:Endo[BigDecimal]	=
		_ * (time divisor 1)

	val fullMilliDuration:BigDecimal=>String	=
		millisToSeconds andThen (time renderer HumanConfig(smallUnits = 1, decimalPlaces = 0))

	val roundedMilliDuration:BigDecimal=>String	=
		millisToSeconds andThen (time renderer HumanConfig(smallUnits = 1, maxUnits = 1, decimalPlaces = 2))
}

// BETTER ensure table is sorted by construction
final case class Human(table:Nes[HumanUnit]) {
	private val smallCount	= table count { _.divisor < Human.one }

	private def smallCut(smallUnits:Int):Nes[HumanUnit]	=
		table.reverse drop (smallCount - smallUnits) cata (Nes single table.head, _.reverse)

	//------------------------------------------------------------------------------

	def divisor(smallUnits:Int):BigDecimal	=
		smallCut(smallUnits).last.divisor

	def renderer(config:HumanConfig):BigDecimal=>String	= {
		val limit1	= smallCut(config.smallUnits)

		value => {
			require(value >= Human.zero, "value must be positive or zero")

			val limit2	= limit1 dropWhile	{ _.divisor > value }	getOrElse (Nes single limit1.last)
			val limit3	= limit2 take		config.maxUnits			getOrElse (Nes single limit2.head)

			renderRaw(limit3, config.decimalPlaces, value)
		}
	}

	private def renderRaw(table:Nes[HumanUnit], decimalPlaces:Int, value:BigDecimal):String	=
		table.tailNes match {
			case None	=>
				s"%.${decimalPlaces.toString}f${table.head.short}" formatLocal (Locale.US, value / table.head.divisor)
			case Some(tail)	=>
				val (div, mod)	= value /% table.head.divisor
				val prefix		= div.toBigInt.toString + table.head.short
				if (mod == Human.zero)	prefix
				else					prefix + " " + renderRaw(tail, decimalPlaces, mod)
		}
}
