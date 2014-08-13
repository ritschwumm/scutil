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
			Human roundedBinary 47110815 mustEqual "44.93M"
		}
		
		//------------------------------------------------------------------------------
		
		"full decode multiple decimal units" in {
			Human fullDecimal 47110815 mustEqual "47M 110k 815"
		}
		"full decode a single decimal unit" in {
			Human fullDecimal 999 mustEqual "999"
		}
		"rounded decode decimal units" in {
			Human roundedDecimal 47110815 mustEqual "47.11M"
		}
		
		//------------------------------------------------------------------------------
		
		"full decode multiple milli time units" in {
			Human fullMilliTime 47110815L mustEqual "13h 5m 10s 815ms"
		}
		"rounded decode milli time units" in {
			Human roundedMilliTime 47110815L mustEqual "13.09h"
		}
		
		//------------------------------------------------------------------------------
		
		// val dms	= Human render (Human.table.degree, decimalPlaces=3)
		
		"properly format DMS" in {
			Human roundedDms 360 mustEqual "360°" 
		}
		
		"properly format DMS" in {
			Human roundedDms 10.5 mustEqual "10° 29' 60.000''" 
		}
		
		"properly format DMS" in {
			Human roundedDms 48.125268 mustEqual "48° 7' 30.965''" 
		}
	}
}
