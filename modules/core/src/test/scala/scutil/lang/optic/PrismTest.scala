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
	case object			PrismObject			extends PrismSuper
	final case class	PrismClass(a:Int)	extends PrismSuper

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

	test("Prism.subType should get a case object") {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		val prism					= Prism.subType[PrismSuper,PrismObject.type]
		val sub:PrismObject.type	= PrismObject
		val sup:PrismSuper			= sub
		assertEquals(
			prism.get(sup),
			Some(sub)
		)
	}

	test("Prism.subType should put a case object") {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		val prism					= Prism.subType[PrismSuper,PrismObject.type]
		val sub:PrismObject.type	= PrismObject
		val sup:PrismSuper			= sub
		assertEquals(
			prism.set(sub),
			sup
		)
	}

	test("Prism.subType should get a case class") {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		val prism			= Prism.subType[PrismSuper,PrismClass]
		val sub:PrismClass	= PrismClass(1)
		val sup:PrismSuper	= sub
		assertEquals(
			prism.get(sup),
			Some(sub)
		)
	}
	test("Prism.subType should put a case class") {
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		val prism			= Prism.subType[PrismSuper,PrismClass]
		val sub:PrismClass	= PrismClass(1)
		val sup:PrismSuper	= sub
		assertEquals(
			prism.set(sub),
			sup
		)
	}
}
