package scutil.gui

import javax.swing.SwingUtilities

import scala.concurrent._

object SwingExecutionContext {
	given self:ExecutionContext	=
		ExecutionContext fromExecutor (SwingUtilities invokeLater _)
}
