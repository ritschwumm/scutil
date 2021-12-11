package scutil.collection

import minitest.*

import scutil.lang.*
import scutil.collection.implicits.*

object SeqExtensionsTest extends SimpleTestSuite {
	test("equivalentSpans should be empty for empty input") {
		//(Seq.empty[String] equivalentSpans equivalentSpansCriterium) must haveTheSameElementsAs(Seq.empty)
		assertEquals(
			(Seq.empty[String] equivalentSpans equivalentSpansCriterium),
			Seq.empty
		)
	}

	test("equivalentSpans should be simple for a 1-element input") {
		assertEquals(
			(Seq("hallo") equivalentSpans equivalentSpansCriterium),
			Seq(Seq("hallo"))
		)
	}

	test("equivalentSpans should group together 2 equivalent elements") {
		assertEquals(
			(Seq("hallo", "hello") equivalentSpans equivalentSpansCriterium),
			Seq(Seq("hallo", "hello"))
		)
	}

	test("equivalentSpans should group separate 2 non-equivalent elements") {
		assertEquals(
			(Seq("hallo", "ballo") equivalentSpans equivalentSpansCriterium),
			Seq(Seq("hallo"), Seq("ballo"))
		)
	}

	test("equivalentSpans should leave 1 non-equivalent element at the end") {
		assertEquals(
			(Seq("hallo", "hello", "ballo") equivalentSpans equivalentSpansCriterium),
			Seq(Seq("hallo", "hello"), Seq("ballo"))
		)
	}

	private def equivalentSpansCriterium(a:String, b:String):Boolean	=
		(a charAt 0) == (b charAt 0)

	//------------------------------------------------------------------------------

	/*
	test("splitAround should be empty for empty input") {
		(Seq.empty[Int] splitAround 1) must haveTheSameElementsAs(Seq.empty)
	}

	test("splitAround should be simple for a 1-element input") {
		(Seq(0) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0)))
	}

	test("splitAround should split into two for a single separator") {
		(Seq(1) splitAround 1) must haveTheSameElementsAs(Seq(Seq(),Seq()))
	}

	test("splitAround should split an empty Seq before a leading separator") {
		(Seq(1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(),Seq(2)))
	}

	test("splitAround should split an empty Seq after a trailing separator") {
		(Seq(0,1) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq()))
	}

	test("splitAround should split a simple Seq correctly") {
		(Seq(0,1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq(2)))
	}

	test("splitAround should create an empty Seq between two adjacent separators") {
		(Seq(0,1,1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq(),Seq(2)))
	}
	*/

	//------------------------------------------------------------------------------

	test("adjacents should work with 0 elements") {
		assertEquals(
			Seq.empty[Int].adjacents,
			Seq.empty
		)
	}

	test("adjacents should work with 1 element") {
		assertEquals(
			Seq(1).adjacents,
			Seq((None,1,None))
		)
	}

	test("adjacents should work with 2 elements") {
		assertEquals(
			Seq(1,2).adjacents,
			Seq((None,1,Some(2)), (Some(1),2,None))
		)
	}

	test("adjacents should work with 3 elements") {
		assertEquals(
			Seq(1,2,3).adjacents,
			Seq((None,1,Some(2)), (Some(1),2,Some(3)), (Some(2),3,None))
		)
	}

	test("adjacents should return the right type") {
		val a	= Vector(1,2,3).adjacents
		typed[ Vector[(Option[Int],Int,Option[Int])] ](a)
		assertEquals(
			a,
			Seq((None,1,Some(2)), (Some(1),2,Some(3)), (Some(2),3,None))
		)
	}

	//------------------------------------------------------------------------------

	test("insertBetween should work with 0 elements") {
		assertEquals(
			Seq.empty[Int].insertBetween(it => it.toString, (a,b) => Some("x")),
			Seq.empty
		)
	}

	test("insertBetween should work with 1 element") {
		assertEquals(
			Seq(1).insertBetween(it => it.toString, (a,b) => Some("x")),
			Seq("1")
		)
	}

	test("insertBetween should work with 2 elements") {
		assertEquals(
			Seq(1,2).insertBetween(it => it.toString, (a,b) => Some("x")),
			Seq("1", "x", "2")
		)
	}

	test("insertBetween should work with 3 elements") {
		assertEquals(
			Seq(1,2,3).insertBetween(it => it.toString, (a,b) => Some("x")),
			Seq("1", "x", "2", "x", "3")
		)
	}

	test("insertBetween should return the right type") {
		val a	= Vector(1,2,3).insertBetween(it => it.toString, (a,b) => Some("x"))
		typed[ Vector[String] ](a)
		assertEquals(
			a,
			Vector("1", "x", "2", "x", "3")
		)
	}

	//------------------------------------------------------------------------------

	private val splitWherePredicate:Int=>Boolean	= _ == 1

	test("splitWhere should be empty for empty input") {
		assertEquals(
			Seq.empty[Int] splitWhere splitWherePredicate,
			Seq.empty
		)
	}

	test("splitWhere should be simple for a 1-element input") {
		assertEquals(
			Seq(0) splitWhere splitWherePredicate,
			Seq(Right(Seq(0)))
		)
	}

	test("splitWhere should split into two for a single separator") {
		assertEquals(
			Seq(1) splitWhere splitWherePredicate,
			Seq(Right(Seq()),Left(1), Right(Seq()))
		)
	}

	test("splitWhere should split an empty Seq before a leading separator") {
		assertEquals(
			Seq(1,2) splitWhere splitWherePredicate,
			Seq(Right(Seq()),Left(1),Right(Seq(2)))
		)
	}

	test("splitWhere should split an empty Seq after a trailing separator") {
		assertEquals(
			Seq(0,1) splitWhere splitWherePredicate,
			Seq(Right(Seq(0)),Left(1),Right(Seq()))
		)
	}

	test("splitWhere should split a simple Seq correctly") {
		assertEquals(
			Seq(0,1,2) splitWhere splitWherePredicate,
			Seq(Right(Seq(0)),Left(1),Right(Seq(2)))
		)
	}

	test("splitWhere should create an empty Seq between two adjacent separators") {
		assertEquals(
			Seq(0,1,1,2) splitWhere splitWherePredicate,
			Seq(Right(Seq(0)),Left(1),Right(Seq()),Left(1),Right(Seq(2)))
		)
	}

	//------------------------------------------------------------------------------

	test("moveAt should fail without enough elements") {
		assertEquals(
			Seq().moveAt(0,0),
			None
		)
	}

	test("moveAt should move from start to end") {
		assertEquals(
			Seq(1,2,3).moveAt(0,3),
			Some(Seq(2,3,1))
		)
	}

	test("moveAt should move from end to start") {
		assertEquals(
			Seq(1,2,3).moveAt(2,0),
			Some(Seq(3,1,2))
		)
	}

	test("moveAt should not move to gap left") {
		// TODO this would work as a no-op, why do we reject it?
		assertEquals(
			Seq(1,2,3,4).moveAt(1,1),
			None
		)
	}

	test("moveAt should not move to gap right") {
		// TODO this would work as a no-op, why do we reject it?
		assertEquals(
			Seq(1,2,3,4).moveAt(1,2),
			None
		)
	}

	test("moveAt should move to gap further left") {
		assertEquals(
			Seq(1,2,3,4).moveAt(1,0),
			Some(Seq(2,1,3,4))
		)
	}

	test("moveAt should move to gap further right") {
		assertEquals(
			Seq(1,2,3,4).moveAt(1,3),
			Some(Seq(1,3,2,4))
		)
	}

	//------------------------------------------------------------------------------

	// TODO dotty actually move more than 1
	test("moveManyAt should fail without enough elements") {
		assertEquals(
			Seq().moveManyAt(0,1,0),
			None
		)
	}

	test("moveManyAt should move from start to end") {
		assertEquals(
			Seq(1,2,3).moveManyAt(0,1,3),
			Some(Seq(2,3,1))
		)
	}

	test("moveManyAt should move from end to start") {
		assertEquals(
			Seq(1,2,3).moveManyAt(2,1,0),
			Some(Seq(3,1,2))
		)
	}

	test("moveManyAt should not move to gap left") {
		// TODO moveMany rejects this, probably without a good reason
		assertEquals(
			Seq(1,2,3,4).moveManyAt(1,1,1),
			Some(Seq(1,2,3,4))
		)
	}

	test("moveManyAt should not move to gap right") {
		// TODO moveMany rejects this, probably without a good reason
		assertEquals(
			Seq(1,2,3,4).moveManyAt(1,1,2),
			Some(Seq(1,2,3,4))
		)
	}

	test("moveManyAt should move to gap further left") {
		assertEquals(
			Seq(1,2,3,4).moveManyAt(1,1,0),
			Some(Seq(2,1,3,4))
		)
	}

	test("moveManyAt should move to gap further right") {
		assertEquals(
			Seq(1,2,3,4).moveManyAt(1,1,3),
			Some(Seq(1,3,2,4))
		)
	}

	//------------------------------------------------------------------------------

	test("zipTail should just work") {
		assertEquals(
			Vector(1,2,3).zipTail,
			Vector((1,2),(2,3))
		)
	}
}
