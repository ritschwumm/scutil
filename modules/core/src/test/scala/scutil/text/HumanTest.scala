package scutil.text

import minitest.*

object HumanTest extends SimpleTestSuite {
	test("Human should full decode multiple binary units") {
		assertEquals(
			Human fullBinary 47110815,
			"44M 950k 671"
		)
	}

	test("Human should full decode a single binary unit") {
		assertEquals(
			Human fullBinary 1023,
			"1023"
		)
	}

	test("Human should rounded decode binary units") {
		assertEquals(
			Human roundedBinary 47110815,
			"44.93M"
		)
	}

	//------------------------------------------------------------------------------

	test("Human should full decode multiple decimal units") {
		assertEquals(
			Human fullDecimal 47110815,
			"47M 110k 815"
		)
	}

	test("Human should full decode a single decimal unit") {
		assertEquals(
			Human fullDecimal 999,
			"999"
		)
	}

	test("Human should rounded decode decimal units") {
		assertEquals(
			Human roundedDecimal 47110815,
			"47.11M"
		)
	}

	//------------------------------------------------------------------------------

	test("Human should full decode multiple milli time units") {
		assertEquals(
			Human fullMilliDuration 47110815L,
			"13h 5m 10s 815ms"
		)
	}

	test("Human should rounded decode milli time units") {
		assertEquals(
			Human roundedMilliDuration 47110815L,
			"13.09h"
		)
	}

	//------------------------------------------------------------------------------

	// val dms	= Human render (Human.table.degree, decimalPlaces=3)

	test("Human should properly format DMS") {
		assertEquals(
			Human roundedDms 360,
			"360°"
		)
	}

	test("Human should properly format DMS") {
		assertEquals(
			Human roundedDms 10.5,
			"10° 29' 60.000''"
		)
	}

	test("Human should properly format DMS") {
		assertEquals(
			Human roundedDms 48.125268,
			"48° 7' 30.965''"
		)
	}
}
