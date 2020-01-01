package scutil.lang

import org.specs2.mutable._

object DragData {
	object P {
		val WorkMove:Prism[DragData,Int]			= Prism partial ({ case scutil.lang.WorkMove(workId)				=> workId				},	scutil.lang.WorkMove.apply)
		val WorkInject:Prism[DragData,(Int,String)]	= Prism partial ({ case scutil.lang.WorkInject(workId, projectId)	=> (workId, projectId)	},	(scutil.lang.WorkInject.apply _).tupled)
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
	}
}
