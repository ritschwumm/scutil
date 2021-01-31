package scutil.gui

import javax.swing.SwingUtilities

import scala.concurrent._

object SwingExecutionContext {
	implicit val self:ExecutionContext	=
		ExecutionContext fromExecutor (SwingUtilities invokeLater _)
}
