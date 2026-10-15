package scutil.lang

import minitest.*

object PrismTest extends SimpleTestSuite {
	object DragData {
		object P {
			val WorkMove:Prism[DragData,Int]			= Prism.partial({ case PrismTest.DragData.WorkMove(workId)				=> workId				},	PrismTest.DragData.WorkMove.apply)
			val WorkInject:Prism[DragData,(Int,String)]	= Prism.partial({ case PrismTest.DragData.WorkInject(workId, projectId)	=> (workId, projectId)	},	PrismTest.DragData.WorkInject.apply.tupled)
		}
	}
	enum DragData {
		case WorkMove(workId:Int)
		case WorkInject(workId:Int, projectId:String)
	}

	sealed trait PrismSuper
	case object			PrismObject						extends PrismSuper
	final case class	PrismZero()						extends PrismSuper
	final case class	PrismOne(a:Int)					extends PrismSuper
	final case class	PrismTwo(a:String, b:Boolean)	extends PrismSuper

	//-----------------------------------------------------------------------------

	test("Prism should do write in partial") {
		val data:DragData	= DragData.WorkMove(1)
		val opt:Option[Int]	= DragData.P.WorkMove.get(data)
		assertEquals(
			opt,
			Some(1)
		)
	}

	test("Prism should change the non-matching side when setting") {
		assertEquals(
			DragData.P.WorkMove.set(2),
			DragData.WorkMove(2)
		)
	}

	test("Prism should switch from the non-matching to the matchiong side when setting") {
		assertEquals(
			DragData.P.WorkInject.set((1,"")),
			DragData.WorkInject(1,"")
		)
	}

	test("Prism should change the non-matching side when setting the converted Optional") {
		val orig:DragData	= DragData.WorkMove(1)
		assertEquals(
			DragData.P.WorkMove.toOptional.set(2)(orig),
			DragData.WorkMove(2)
		)
	}

	test("Prism should leave the non-matching side alone when setting the converted Optional") {
		val orig:DragData	= DragData.WorkMove(1)
		assertEquals(
			DragData.P.WorkInject.toOptional.set((1,""))(orig),
			orig
		)
	}

	//------------------------------------------------------------------------------

	test("Prism.subType should not get the wrong variant") {
		val prism	= Prism.subType[PrismSuper,PrismTwo]
		assertEquals(
			prism.get(PrismOne(1)),
			None
		)
	}

	test("Prism.subType should get a case object") {
		val prism	= Prism.subType[PrismSuper,PrismObject.type]
		assertEquals(
			prism.get(PrismObject),
			Some(PrismObject)
		)
	}
	test("Prism.subType should put a case object") {
		val prism	= Prism.subType[PrismSuper,PrismObject.type]
		assertEquals(
			prism.set(PrismObject),
			PrismObject
		)
	}

	test("Prism.subType should get a case class with 0 fields") {
		val prism	= Prism.subType[PrismSuper,PrismZero]
		assertEquals(
			prism.get(PrismZero()),
			Some(PrismZero())
		)
	}
	test("Prism.subType should put a case class with 0 fields") {
		val prism	= Prism.subType[PrismSuper,PrismZero]
		assertEquals(
			prism.set(PrismZero()),
			PrismZero()
		)
	}

	test("Prism.subType should get a case class with 1 field") {
		val prism	= Prism.subType[PrismSuper,PrismOne]
		assertEquals(
			prism.get(PrismOne(1)),
			Some(PrismOne(1))
		)
	}
	test("Prism.subType should put a case class with 1 field") {
		val prism	= Prism.subType[PrismSuper,PrismOne]
		assertEquals(
			prism.set(PrismOne(1)),
			PrismOne(1)
		)
	}

	test("Prism.subType should get a case class with 2 fields") {
		val prism	= Prism.subType[PrismSuper,PrismTwo]
		assertEquals(
			prism.get(PrismTwo("hello", true)),
			Some(PrismTwo("hello", true))
		)
	}
	test("Prism.subType should put a case class with 2 fields") {
		val prism	= Prism.subType[PrismSuper,PrismTwo]
		assertEquals(
			prism.set( PrismTwo("hello", true)),
			PrismTwo("hello", true)
		)
	}

	//-----------------------------------------------------------------------------

	test("Prism.subTypes should put a case class with 2 fields") {
		val prisms	= Prism.subTypes[PrismSuper]
		val prism	= prisms[PrismTwo]
		assertEquals(
			prism.set( PrismTwo("hello", true)),
			PrismTwo("hello", true)
		)
	}
}
