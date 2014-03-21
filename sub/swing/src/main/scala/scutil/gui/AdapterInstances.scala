package scutil.gui

import java.awt.event._
import javax.swing.event._

object AdapterInstances {
	//------------------------------------------------------------------------------
	//## awt
	
	class WindowFocusAdapter extends WindowFocusListener {
		def windowGainedFocus(ev:WindowEvent)	{}
		def windowLostFocus(ev:WindowEvent)		{}
	}
	
	class InputMethodAdapter extends InputMethodListener {
		def inputMethodTextChanged(ev:InputMethodEvent)	{}
		def caretPositionChanged(ev:InputMethodEvent)	{}
	}
	
	//------------------------------------------------------------------------------
	//## swing
	
	class CellEditorAdapter extends CellEditorListener {
		def editingStopped(ev:ChangeEvent)	{}
		def editingCanceled(ev:ChangeEvent)	{}
	}
	
	class MenuDragMouseAdapter extends MenuDragMouseListener {
		def menuDragMouseEntered(ev:MenuDragMouseEvent)		{}
		def menuDragMouseExited(ev:MenuDragMouseEvent)		{}
		def menuDragMouseDragged(ev:MenuDragMouseEvent)		{}
		def menuDragMouseReleased(ev:MenuDragMouseEvent)	{}
	}
	
	class MenuKeyAdapter extends MenuKeyListener {
		def menuKeyTyped(ev:MenuKeyEvent)		{}
		def menuKeyPressed(ev:MenuKeyEvent)		{}
		def menuKeyReleased(ev:MenuKeyEvent)	{}
	}
	
	class ListDataAdapter extends ListDataListener {
		def intervalAdded(ev:ListDataEvent)		{}
		def intervalRemoved(ev:ListDataEvent)	{}
		def contentsChanged(ev:ListDataEvent)	{}
	}
	
	class AncestorAdapter extends AncestorListener {
		def ancestorAdded(ev:AncestorEvent)		{}
		def ancestorRemoved(ev:AncestorEvent)	{}
		def ancestorMoved(ev:AncestorEvent)		{}
	}
	
	class DocumentAdapter extends DocumentListener {
		def insertUpdate(ev:DocumentEvent)	{}
		def removeUpdate(ev:DocumentEvent)	{}
		def changedUpdate(ev:DocumentEvent)	{}
	}
	
	class MenuAdapter extends MenuListener {
		def menuSelected(ev:MenuEvent)		{}
		def menuDeselected(ev:MenuEvent)	{}
		def menuCanceled(ev:MenuEvent)		{}
	}
	
	class PopupMenuAdapter extends PopupMenuListener {
		def popupMenuWillBecomeVisible(ev:PopupMenuEvent)	{}
		def popupMenuWillBecomeInvisible(ev:PopupMenuEvent)	{}
		def popupMenuCanceled(ev:PopupMenuEvent)			{}
	}
	
	class TableColumnModelAdapter extends TableColumnModelListener {
		def columnAdded(ev:TableColumnModelEvent)			{}
		def columnRemoved(ev:TableColumnModelEvent)			{}
		def columnMoved(ev:TableColumnModelEvent)			{}
		def columnMarginChanged(ev:ChangeEvent)				{}
		def columnSelectionChanged(ev:ListSelectionEvent)	{}
	}
	
	class TreeModelAdapter extends TreeModelListener {
		def treeNodesChanged(ev:TreeModelEvent)		{}
		def treeNodesInserted(ev:TreeModelEvent)	{}
		def treeNodesRemoved(ev:TreeModelEvent)		{}
		def treeStructureChanged(ev:TreeModelEvent)	{}
	}
	
	class TreeWillExpandAdapter extends TreeWillExpandListener {
		def treeWillExpand(ev:TreeExpansionEvent)	{}
		def treeWillCollapse(ev:TreeExpansionEvent)	{}
	}
	
	class TreeExpansionAdapter extends TreeExpansionListener {
		def treeExpanded(ev:TreeExpansionEvent)		{}
		def treeCollapsed(ev:TreeExpansionEvent)	{}
	}
}
