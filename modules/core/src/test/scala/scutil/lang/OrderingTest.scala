package scutil.lang

import minitest._

import scutil.core.implicits._

object OrderingTest extends SimpleTestSuite {
	test("seqOrdering should order by base order (same)") {
		assertEquals(
			Seq(Seq(1), Seq(2), Seq(1), Seq(2)) sorted (Ordering sequence[Int] true),
			Seq(Seq(1), Seq(1), Seq(2), Seq(2))
		)
	}

	test("seqOrdering should order missing first") {
		assertEquals(
			Seq(Seq(1), Seq(1,2), Seq(1), Seq(1,2)) sorted (Ordering sequence[Int] true),
			Seq(Seq(1), Seq(1), Seq(1,2), Seq(1,2))
		)
	}

	test("seqOrdering should order missing last") {
		assertEquals(
			Seq(Seq(1), Seq(1,2), Seq(1), Seq(1,2)) sorted (Ordering sequence[Int] false),
			Seq(Seq(1,2), Seq(1,2), Seq(1), Seq(1))
		)
	}

	test("seqOrdering should order left to right") {
		assertEquals(
			Seq(Seq(1,1), Seq(1,2), Seq(2,1), Seq(2,2), Seq(1,1), Seq(1,2), Seq(2,1), Seq(2,2)) sorted (Ordering sequence[Int] false),
			Seq(Seq(1,1), Seq(1,1), Seq(1,2), Seq(1,2), Seq(2,1), Seq(2,1), Seq(2,2), Seq(2,2))
		)
	}
}
