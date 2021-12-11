package scutil.lang

import minitest.*

object DragData {
	object P {
		val WorkMove:Prism[DragData,Int]			= Prism.partial({ case scutil.lang.WorkMove(workId)					=> workId				},	scutil.lang.WorkMove.apply)
		val WorkInject:Prism[DragData,(Int,String)]	= Prism.partial({ case scutil.lang.WorkInject(workId, projectId)	=> (workId, projectId)	},	(scutil.lang.WorkInject.apply _).tupled)
	}
}
sealed trait DragData
final case class WorkMove(workId:Int)						extends DragData
final case class WorkInject(workId:Int, projectId:String)	extends DragData

sealed trait PrismSuper
case object			PrismObject			extends PrismSuper
final case class	PrismClass(a:Int)	extends PrismSuper

object PrismTest extends SimpleTestSuite {
	test("Prism should do write in partial") {
		val data:DragData	= WorkMove(1)
		val opt:Option[Int]	= DragData.P.WorkMove get data
		assertEquals(
			opt,
			Some(1)
		)
	}

	test("Prism should change the non-matching side when setting") {
		assertEquals(
			DragData.P.WorkMove.set(2),
			WorkMove(2)
		)
	}

	test("Prism should switch from the non-matching to the matchiong side when setting") {
		assertEquals(
			DragData.P.WorkInject.set((1,"")),
			WorkInject(1,"")
			)
	}

	test("Prism should change the non-matching side when setting the converted Optional") {
		val orig:DragData	= WorkMove(1)
		assertEquals(
			DragData.P.WorkMove.toOptional.set(2)(orig),
			WorkMove(2)
		)
	}

	test("Prism should leave the non-matching side alone when setting the converted Optional") {
		val orig:DragData	= WorkMove(1)
		assertEquals(
			DragData.P.WorkInject.toOptional.set((1,""))(orig),
			orig
		)
	}

	//------------------------------------------------------------------------------

	test("Prism.subType should get a case object") {
		val prism					= Prism.subType[PrismSuper,PrismObject.type]
		val sub:PrismObject.type	= PrismObject
		val sup:PrismSuper			= sub
		assertEquals(
			prism get sup,
			Some(sub)
		)
	}

	test("Prism.subType should put a case object") {
		val prism					= Prism.subType[PrismSuper,PrismObject.type]
		val sub:PrismObject.type	= PrismObject
		val sup:PrismSuper			= sub
		assertEquals(
			prism set sub,
			sup
		)
	}

	test("Prism.subType should get a case class") {
		val prism			= Prism.subType[PrismSuper,PrismClass]
		val sub:PrismClass	= PrismClass(1)
		val sup:PrismSuper	= sub
		assertEquals(
			prism get sup,
			Some(sub)
		)
	}
	test("Prism.subType should put a case class") {
		val prism			= Prism.subType[PrismSuper,PrismClass]
		val sub:PrismClass	= PrismClass(1)
		val sup:PrismSuper	= sub
		assertEquals(
			prism set sub,
			sup
		)
	}
}
