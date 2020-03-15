package scutil.text

import org.specs2.mutable._

import scutil.text.literals._

class BlockTest extends Specification {
	"cooked Block interpolator" should {
		"apply escape codes" in {
			tb"""test\ntest""" mustEqual "test\ntest"
		}
	}

	"raw Block interpolator" should {
		"leave escape codes alone" in {
			rtb"""test\ntest""" mustEqual "test\\ntest"
		}
	}

	"Block interpolator" should {
		"generate 0" in {
			tb"""foo""" mustEqual "foo"
		}
		"generate 1" in {
			tb"""
			foo
			""" mustEqual "foo"
		}
		"generate 2" in {
			tb"""
			foo
			bar
			""" mustEqual "foo\nbar"
		}
		"generate 3" in {
			tb"""
			${"foo"}
			bar
			""" mustEqual "foo\nbar"
		}
		"generate 4" in {
			tb"""
			foo
			${"bar"}
			""" mustEqual "foo\nbar"
		}
		"generate 5" in {
			tb"""
			foo
			${"bar"}
			quux
			""" mustEqual "foo\nbar\nquux"
		}
		"generate 6" in {
			tb"""
			${"foo"}
			bar
			${"quux"}
			""" mustEqual "foo\nbar\nquux"
		}

		"generate 7" in {
			tb"""
				foo
			bar
			""" mustEqual "\tfoo\nbar"
		}
		"generate 8" in {
			tb"""
			foo
				bar
			""" mustEqual "foo\n\tbar"
		}
		"generate 9" in {
			tb"""
			foo
				${"bar"}
			""" mustEqual "foo\n\tbar"
		}
		"generate 10" in {
			tb"""
				${"foo"}
			bar
			""" mustEqual "\tfoo\nbar"
		}

		"generate 11" in {
			tb"""
			foo
			${"bar\nquux\nwibble"}
			xyzzy
			""" mustEqual "foo\nbar\nquux\nwibble\nxyzzy"
		}
		"generate 12" in {
			tb"""
			foo
				${"bar\nquux\nwibble"}
			xyzzy
			""" mustEqual "foo\n\tbar\n\tquux\n\twibble\nxyzzy"
		}

		"generate 13" in {
			tb"""
			${"bar"} ${"quux"}
			""" mustEqual "bar quux"
		}

		"generate 14" in {
			tb"""
			foo
				${"bar"} ${"quux"}
			wibble
			""" mustEqual "foo\n\tbar quux\nwibble"
		}

		"generate 15" in {
			tb"""
			${"foo\nbar"} ${"quux\nwibble"}
			""" mustEqual "foo\nbar quux\nwibble"
		}
		"generate 16" in {
			tb"""
			a
				${"foo\nbar"} ${"quux\nwibble"}
			b
			""" mustEqual "a\n\tfoo\n\tbar quux\n\twibble\nb"
		}

		"generate 17" in {
			tb"""
			""" mustEqual ""
		}

		"generate 18" in {
			tb"""
				foo
			""" mustEqual "foo"
		}

		"generate 19" in {
			tb"""
				${"foo\nbar"}
			""" mustEqual "foo\nbar"
		}

		"generate 20" in {
			tb"""
				${"foo\n\nbar"}
			""" mustEqual "foo\n\nbar"
		}

		"generate 20" in {
			tb"""
			a
				${"foo\n\nbar"}
			b
			""" mustEqual "a\n\tfoo\n\t\n\tbar\nb"
		}

		"generate 21" in {
			tb"""
				${""}
				${"a"}
			""" mustEqual "\na"
		}

		"generate 22" in {
			tb"""
				${"a"}
				${""}
			""" mustEqual "a\n"
		}

		"generate 23" in {
			tb"""
				${""}
				${""}
			""" mustEqual "\n"
		}
	}
}
