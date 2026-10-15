package scutil.gui

import java.awt.event.*
import java.awt.dnd.*

object AwtAdapters {
	abstract class DropTargetAdapter2 extends DropTargetAdapter {
		// missing in swing, for some reason
		override def drop(ev:DropTargetDropEvent):Unit = ()
	}

	abstract class WindowFocusAdapter extends WindowFocusListener {
		override def windowGainedFocus(ev:WindowEvent):Unit	= {}
		override def windowLostFocus(ev:WindowEvent):Unit	= {}
	}

	abstract class InputMethodAdapter extends InputMethodListener {
		override def inputMethodTextChanged(ev:InputMethodEvent):Unit	= {}
		override def caretPositionChanged(ev:InputMethodEvent):Unit		= {}
	}
}
