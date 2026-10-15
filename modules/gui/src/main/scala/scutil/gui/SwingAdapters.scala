package scutil.gui

import javax.swing.event.*

object SwingAdapters {
	abstract class CellEditorAdapter extends CellEditorListener {
		override def editingStopped(ev:ChangeEvent):Unit	= {}
		override def editingCanceled(ev:ChangeEvent):Unit	= {}
	}

	abstract class MenuDragMouseAdapter extends MenuDragMouseListener {
		override def menuDragMouseEntered(ev:MenuDragMouseEvent):Unit	= {}
		override def menuDragMouseExited(ev:MenuDragMouseEvent):Unit	= {}
		override def menuDragMouseDragged(ev:MenuDragMouseEvent):Unit	= {}
		override def menuDragMouseReleased(ev:MenuDragMouseEvent):Unit	= {}
	}

	abstract class MenuKeyAdapter extends MenuKeyListener {
		override def menuKeyTyped(ev:MenuKeyEvent):Unit		= {}
		override def menuKeyPressed(ev:MenuKeyEvent):Unit	= {}
		override def menuKeyReleased(ev:MenuKeyEvent):Unit	= {}
	}

	abstract class ListDataAdapter extends ListDataListener {
		override def intervalAdded(ev:ListDataEvent):Unit	= {}
		override def intervalRemoved(ev:ListDataEvent):Unit	= {}
		override def contentsChanged(ev:ListDataEvent):Unit	= {}
	}

	abstract class AncestorAdapter extends AncestorListener {
		override def ancestorAdded(ev:AncestorEvent):Unit	= {}
		override def ancestorRemoved(ev:AncestorEvent):Unit	= {}
		override def ancestorMoved(ev:AncestorEvent):Unit	= {}
	}

	abstract class DocumentAdapter extends DocumentListener {
		override def insertUpdate(ev:DocumentEvent):Unit	= {}
		override def removeUpdate(ev:DocumentEvent):Unit	= {}
		override def changedUpdate(ev:DocumentEvent):Unit	= {}
	}

	abstract class MenuAdapter extends MenuListener {
		override def menuSelected(ev:MenuEvent):Unit	= {}
		override def menuDeselected(ev:MenuEvent):Unit	= {}
		override def menuCanceled(ev:MenuEvent):Unit	= {}
	}

	abstract class PopupMenuAdapter extends PopupMenuListener {
		override def popupMenuWillBecomeVisible(ev:PopupMenuEvent):Unit		= {}
		override def popupMenuWillBecomeInvisible(ev:PopupMenuEvent):Unit	= {}
		override def popupMenuCanceled(ev:PopupMenuEvent):Unit				= {}
	}

	abstract class TableColumnModelAdapter extends TableColumnModelListener {
		override def columnAdded(ev:TableColumnModelEvent):Unit			= {}
		override def columnRemoved(ev:TableColumnModelEvent):Unit		= {}
		override def columnMoved(ev:TableColumnModelEvent):Unit			= {}
		override def columnMarginChanged(ev:ChangeEvent):Unit			= {}
		override def columnSelectionChanged(ev:ListSelectionEvent):Unit	= {}
	}

	abstract class TreeModelAdapter extends TreeModelListener {
		override def treeNodesChanged(ev:TreeModelEvent):Unit		= {}
		override def treeNodesInserted(ev:TreeModelEvent):Unit		= {}
		override def treeNodesRemoved(ev:TreeModelEvent):Unit		= {}
		override def treeStructureChanged(ev:TreeModelEvent):Unit	= {}
	}

	abstract class TreeWillExpandAdapter extends TreeWillExpandListener {
		override def treeWillExpand(ev:TreeExpansionEvent):Unit		= {}
		override def treeWillCollapse(ev:TreeExpansionEvent):Unit	= {}
	}

	abstract class TreeExpansionAdapter extends TreeExpansionListener {
		override def treeExpanded(ev:TreeExpansionEvent):Unit	= {}
		override def treeCollapsed(ev:TreeExpansionEvent):Unit	= {}
	}
}
