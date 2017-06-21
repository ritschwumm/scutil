package scutil.time

import org.specs2.mutable._

class TimeTest extends Specification {
	"JulianDay" should {
		"convert 2457700" in {
			JulianDay(2457700).toGregorianDate mustEqual GregorianDate(7,11,2016)
		}
	}
	
	"GregorianDate" should {
		"compare equal days correctly" in {
			GregorianDate(7,8,2012) compare GregorianDate(7,8,2012) mustEqual 0
		}
		"compare earlier day correctly" in {
			GregorianDate(6,7,2012) compare GregorianDate(7,8,2012) mustEqual -1
		}
		"compare later day correctly" in {
			GregorianDate(8,8,2012) compare GregorianDate(7,8,2012) mustEqual +1
		}
		"compare earlier month correctly" in {
			GregorianDate(7,7,2012) compare GregorianDate(7,8,2012) mustEqual -1
		}
		"compare later month correctly" in {
			GregorianDate(7,9,2012) compare GregorianDate(7,8,2012) mustEqual +1
		}
		"compare earlier year correctly" in {
			GregorianDate(7,8,2011) compare GregorianDate(7,8,2012) mustEqual -1
		}
		"compare later year correctly" in {
			GregorianDate(7,8,2013) compare GregorianDate(7,8,2012) mustEqual +1
		}
		"prefer year over month" in {
			GregorianDate(7,8,2012) compare GregorianDate(7,9,2013) mustEqual -1
		}
		"prefer month over day" in {
			GregorianDate(7,8,2012) compare GregorianDate(6,9,2012) mustEqual -1
		}
	}
}
