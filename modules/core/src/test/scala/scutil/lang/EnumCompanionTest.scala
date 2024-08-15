package scutil.lang

import minitest.*

object EnumCompanionTest extends SimpleTestSuite {
	enum Test {
		case Foo, Bar, Quux
	}

	enum Bad {
		case Foo
		case Bar
		case Quux
		case Xxx(num:Int)
	}

	//-----------------------------------------------------------------------------

	test("EnumCompanion should fail with non-simple enums") {
		// EnumCompanion[Bad].values
		assertDoesNotCompile("EnumCompanion[Bad].values")
	}

	//-----------------------------------------------------------------------------

	test("EnumCompanion should provide values") {
		assertEquals(
			EnumCompanion[Test].values.toVector,
			Vector(Test.Foo, Test.Bar, Test.Quux)
		)
	}

	test("EnumCompanion should provide valuesNes") {
		assertEquals(
			EnumCompanion[Test].valuesNes,
			Nes.of(Test.Foo, Test.Bar, Test.Quux)
		)
	}

	//-----------------------------------------------------------------------------

	test("EnumCompanion should support fromOrdinal") {
		assertEquals(
			EnumCompanion[Test].fromOrdinal(1),
			Test.Bar
		)
	}

	test("EnumCompanion should provide ordinal") {
		assertEquals(
			EnumCompanion[Test].ordinal(Test.Quux),
			2
		)
	}

	//-----------------------------------------------------------------------------


	test("EnumCompanion should support valueOf") {
		assertEquals(
			EnumCompanion[Test].valueOf("Quux"),
			Test.Quux
		)
	}

	test("EnumCompanion should provide enumLabel") {
		assertEquals(
			EnumCompanion[Test].enumLabel(Test.Foo),
			"Foo"
		)
	}
}
