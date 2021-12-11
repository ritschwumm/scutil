package scutil.lang

import minitest.*

import scutil.core.implicits.*

object StringTest extends SimpleTestSuite {
	test("string extension should parse 'false' to Boolean") {
		assertEquals(
			"false".parseBoolean,
			Right(false)
		)
	}

	test("string extension should parse 'true' to Boolean") {
		assertEquals(
			"true".parseBoolean,
			Right(true)
		)
	}

	test("string extension should parse 'FALSE' to Boolean") {
		assertEquals(
			"FALSE".parseBoolean,
			Right(false)
		)
	}

	test("string extension should parse 'TRUE' to Boolean") {
		assertEquals(
			"TRUE".parseBoolean,
			Right(true)
		)
	}

	test("string extension should parse 'False' to Boolean") {
		assertEquals(
			"False".parseBoolean,
			Right(false)
		)
	}

	test("string extension should parse 'True' to Boolean") {
		assertEquals(
			"True".parseBoolean,
			Right(true)
		)
	}

	test("string extension should parse 'fAlse' to Boolean") {
		assertEquals(
			"fAlse".parseBoolean,
			Right(false)
		)
	}
	test("string extension should parse 'tRrue' to Boolean") {
		assertEquals(
			"tRue".parseBoolean,
			Right(true)
		)
	}

	test("string extension should parse 'faLse' to Boolean") {
		assertEquals(
			"faLse".parseBoolean,
			Right(false)
		)
	}

	test("string extension should parse 'trUe' to Boolean") {
		assertEquals(
			"trUe".parseBoolean,
			Right(true)
		)
	}

	test("string extension should parse 'falSe' to Boolean") {
		assertEquals(
			"falSe".parseBoolean,
			Right(false)
		)
	}

	test("string extension should parse 'truE' to Boolean") {
		assertEquals(
			"truE".parseBoolean,
			Right(true)
		)
	}

	test("string extension should parse 'falsE' to Boolean") {
		assertEquals(
			"falsE".parseBoolean,
			Right(false)
		)
	}
}
