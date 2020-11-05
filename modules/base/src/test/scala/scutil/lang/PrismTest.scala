package scutil.lang

import org.specs2.mutable._

object DragData {
	object P {
		val WorkMove:Prism[DragData,Int]			= Prism.partial({ case scutil.lang.WorkMove(workId)					=> workId				},	scutil.lang.WorkMove.apply)
		val WorkInject:Prism[DragData,(Int,String)]	= Prism.partial({ case scutil.lang.WorkInject(workId, projectId)	=> (workId, projectId)	},	(scutil.lang.WorkInject.apply _).tupled)
	}
}
sealed trait DragData
final case class WorkMove(workId:Int)						extends DragData
final case class WorkInject(workId:Int, projectId:String)	extends DragData

class PrismTest extends Specification {
	"Prism" should {
		"do write in partial" in {
			val data:DragData	= WorkMove(1)
			val opt:Option[Int]	= DragData.P.WorkMove get data
			opt mustEqual Some(1)
		}

		"change the non-matching side when setting" in {
			DragData.P.WorkMove.set(2).mustEqual(WorkMove(2))
		}

		"switch from the non-matching to the matchiong side when setting" in {
			DragData.P.WorkInject.set((1,"")).mustEqual(WorkInject(1,""))
		}

		"change the non-matching side when setting the converted Optional" in {
			val orig:DragData	= WorkMove(1)
			DragData.P.WorkMove.toOptional.set(2)(orig).mustEqual(WorkMove(2))
		}

		"leave the non-matching side alone when setting the converted Optional" in {
			val orig:DragData	= WorkMove(1)
			DragData.P.WorkInject.toOptional.set((1,""))(orig).mustEqual(orig)
		}
	}
}
