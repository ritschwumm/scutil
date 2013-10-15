package scutil.text

import org.specs2.mutable._

class HumanTest extends Specification {
	"binary human" should {
		"full decode multiple binary units" in {
			Human fullBinary 47110815 mustEqual "44M 950k 671"
		}
		"full decode a single binary unit" in {
			Human fullBinary 1023 mustEqual "1023"
		}
		"rounded decode binary units" in {
			Human roundedBinary (2, 47110815) mustEqual "44.93M"
		}
		
		"full decode multiple decimal units" in {
			Human fullDecimal 47110815 mustEqual "47M 110k 815"
		}
		"full decode a single decimal unit" in {
			Human fullDecimal 999 mustEqual "999"
		}
		"rounded decode decimal units" in {
			Human roundedDecimal (2, 47110815) mustEqual "47.11M"
		}
		
		
		"full decode multiple milli time units" in {
			Human fullMilliTime 47110815L mustEqual "13h 5m 10s 815ms"
		}
		"rounded decode milli time units" in {
			Human roundedMilliTime (2, 47110815L) mustEqual "13.09h"
		}
		
		"allow rounding without comma digits" in {
			Human roundedMilliTime (0, 47110815L) mustEqual "13h"
		}
	}
}
