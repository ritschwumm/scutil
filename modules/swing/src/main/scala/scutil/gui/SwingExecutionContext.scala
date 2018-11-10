package scutil.gui

import java.util.concurrent.{ Executor=>JExecutor }
import javax.swing.SwingUtilities

import scala.concurrent._

object SwingExecutionContext {
	implicit val self:ExecutionContext	=
			ExecutionContext fromExecutor new JExecutor {
				def execute(runnable:Runnable) {
					SwingUtilities invokeLater runnable
				}
			}
}
