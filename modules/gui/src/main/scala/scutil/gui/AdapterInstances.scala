package scutil.gui

import java.awt.event.*
import javax.swing.event.*

object AdapterInstances {
	//------------------------------------------------------------------------------
	//## awt

	class WindowFocusAdapter extends WindowFocusListener {
		def windowGainedFocus(ev:WindowEvent):Unit	= {}
		def windowLostFocus(ev:WindowEvent):Unit	= {}
	}

	class InputMethodAdapter extends InputMethodListener {
		def inputMethodTextChanged(ev:InputMethodEvent):Unit	= {}
		def caretPositionChanged(ev:InputMethodEvent):Unit		= {}
	}

	//------------------------------------------------------------------------------
	//## swing

	class CellEditorAdapter extends CellEditorListener {
		def editingStopped(ev:ChangeEvent):Unit		= {}
		def editingCanceled(ev:ChangeEvent):Unit	= {}
	}

	class MenuDragMouseAdapter extends MenuDragMouseListener {
		def menuDragMouseEntered(ev:MenuDragMouseEvent):Unit	= {}
		def menuDragMouseExited(ev:MenuDragMouseEvent):Unit		= {}
		def menuDragMouseDragged(ev:MenuDragMouseEvent):Unit	= {}
		def menuDragMouseReleased(ev:MenuDragMouseEvent):Unit	= {}
	}

	class MenuKeyAdapter extends MenuKeyListener {
		def menuKeyTyped(ev:MenuKeyEvent):Unit		= {}
		def menuKeyPressed(ev:MenuKeyEvent):Unit	= {}
		def menuKeyReleased(ev:MenuKeyEvent):Unit	= {}
	}

	class ListDataAdapter extends ListDataListener {
		def intervalAdded(ev:ListDataEvent):Unit	= {}
		def intervalRemoved(ev:ListDataEvent):Unit	= {}
		def contentsChanged(ev:ListDataEvent):Unit	= {}
	}

	class AncestorAdapter extends AncestorListener {
		def ancestorAdded(ev:AncestorEvent):Unit	= {}
		def ancestorRemoved(ev:AncestorEvent):Unit	= {}
		def ancestorMoved(ev:AncestorEvent):Unit	= {}
	}

	class DocumentAdapter extends DocumentListener {
		def insertUpdate(ev:DocumentEvent):Unit		= {}
		def removeUpdate(ev:DocumentEvent):Unit		= {}
		def changedUpdate(ev:DocumentEvent):Unit	= {}
	}

	class MenuAdapter extends MenuListener {
		def menuSelected(ev:MenuEvent):Unit		= {}
		def menuDeselected(ev:MenuEvent):Unit	= {}
		def menuCanceled(ev:MenuEvent):Unit		= {}
	}

	class PopupMenuAdapter extends PopupMenuListener {
		def popupMenuWillBecomeVisible(ev:PopupMenuEvent):Unit		= {}
		def popupMenuWillBecomeInvisible(ev:PopupMenuEvent):Unit	= {}
		def popupMenuCanceled(ev:PopupMenuEvent):Unit				= {}
	}

	class TableColumnModelAdapter extends TableColumnModelListener {
		def columnAdded(ev:TableColumnModelEvent):Unit			= {}
		def columnRemoved(ev:TableColumnModelEvent):Unit		= {}
		def columnMoved(ev:TableColumnModelEvent):Unit			= {}
		def columnMarginChanged(ev:ChangeEvent):Unit			= {}
		def columnSelectionChanged(ev:ListSelectionEvent):Unit	= {}
	}

	class TreeModelAdapter extends TreeModelListener {
		def treeNodesChanged(ev:TreeModelEvent):Unit		= {}
		def treeNodesInserted(ev:TreeModelEvent):Unit		= {}
		def treeNodesRemoved(ev:TreeModelEvent):Unit		= {}
		def treeStructureChanged(ev:TreeModelEvent):Unit	= {}
	}

	class TreeWillExpandAdapter extends TreeWillExpandListener {
		def treeWillExpand(ev:TreeExpansionEvent):Unit		= {}
		def treeWillCollapse(ev:TreeExpansionEvent):Unit	= {}
	}

	class TreeExpansionAdapter extends TreeExpansionListener {
		def treeExpanded(ev:TreeExpansionEvent):Unit	= {}
		def treeCollapsed(ev:TreeExpansionEvent):Unit	= {}
	}
}
