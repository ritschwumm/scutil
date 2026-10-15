package scutil.gui

import scala.annotation.targetName
import scala.reflect.Selectable.reflectiveSelectable

import javax.swing.event.*

import scutil.lang.*

import  scutil.gui.SwingAdapters.*

object SwingCasters {
	type CellEditorCaster = {
		def addCellEditorListener(listener:CellEditorListener):Unit
		def removeCellEditorListener(listener:CellEditorListener):Unit
	}

	extension (peer:CellEditorCaster) {
		def cellEditorCaster:Caster[CellEditorListener]	= new Caster(peer.addCellEditorListener, peer.removeCellEditorListener)

		def onChangeEvent(callback:ChangeEvent=>Unit):Disposer	=
			cellEditorCaster.listen(new CellEditorListener {
				def editingStopped(ev:ChangeEvent):Unit	= callback(ev)
				def editingCanceled(ev:ChangeEvent):Unit	= callback(ev)
			})

		def onEditingStopped(callback:ChangeEvent=>Unit):Disposer	=
			cellEditorCaster.listen(new CellEditorAdapter {
				override def editingStopped(ev:ChangeEvent):Unit	= callback(ev)
			})

		def onEditingCanceled(callback:ChangeEvent=>Unit):Disposer	=
			cellEditorCaster.listen(new CellEditorAdapter {
				override def editingCanceled(ev:ChangeEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type InternalFrameCaster = {
		def addInternalFrameListener(listener:InternalFrameListener):Unit
		def removeInternalFrameListener(listener:InternalFrameListener):Unit
	}

	extension (peer:InternalFrameCaster) {
		def internalFrameCaster:Caster[InternalFrameListener]	= new Caster(peer.addInternalFrameListener, peer.removeInternalFrameListener)

		def onInternalFrameEvent(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameListener {
				def internalFrameOpened(ev:InternalFrameEvent):Unit			= callback(ev)
				def internalFrameClosing(ev:InternalFrameEvent):Unit		= callback(ev)
				def internalFrameClosed(ev:InternalFrameEvent):Unit			= callback(ev)
				def internalFrameIconified(ev:InternalFrameEvent):Unit		= callback(ev)
				def internalFrameDeiconified(ev:InternalFrameEvent):Unit	= callback(ev)
				def internalFrameActivated(ev:InternalFrameEvent):Unit		= callback(ev)
				def internalFrameDeactivated(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameOpened(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameOpened(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameClosing(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameClosing(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameClosed(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameClosed(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameIconified(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameIconified(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameDeiconified(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameDeiconified(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameActivated(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameActivated(ev:InternalFrameEvent):Unit	= callback(ev)
			})

		def onInternalFrameDeactivated(callback:InternalFrameEvent=>Unit):Disposer	=
			internalFrameCaster.listen(new InternalFrameAdapter {
				override def internalFrameDeactivated(ev:InternalFrameEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type MenuDragMouseCaster = {
		def addMenuDragMouseListener(listener:MenuDragMouseListener):Unit
		def removeMenuDragMouseListener(listener:MenuDragMouseListener):Unit
	}

	extension (peer:MenuDragMouseCaster) {
		def menuDragMouseCaster:Caster[MenuDragMouseListener]	= new Caster(peer.addMenuDragMouseListener, peer.removeMenuDragMouseListener)

		def onMenuDragMouseEvent(callback:MenuDragMouseEvent=>Unit):Disposer	=
			menuDragMouseCaster.listen(new MenuDragMouseListener {
				def menuDragMouseEntered(ev:MenuDragMouseEvent):Unit	= callback(ev)
				def menuDragMouseExited(ev:MenuDragMouseEvent):Unit		= callback(ev)
				def menuDragMouseDragged(ev:MenuDragMouseEvent):Unit	= callback(ev)
				def menuDragMouseReleased(ev:MenuDragMouseEvent):Unit	= callback(ev)
			})

		def onMenuDragMouseEntered(callback:MenuDragMouseEvent=>Unit):Disposer	=
			menuDragMouseCaster.listen(new MenuDragMouseAdapter {
				override def menuDragMouseEntered(ev:MenuDragMouseEvent):Unit	= callback(ev)
			})

		def onMenuDragMouseExited(callback:MenuDragMouseEvent=>Unit):Disposer	=
			menuDragMouseCaster.listen(new MenuDragMouseAdapter {
				override def menuDragMouseExited(ev:MenuDragMouseEvent):Unit	= callback(ev)
			})

		def onMenuDragMouseDragged(callback:MenuDragMouseEvent=>Unit):Disposer	=
			menuDragMouseCaster.listen(new MenuDragMouseAdapter {
				override def menuDragMouseDragged(ev:MenuDragMouseEvent):Unit	= callback(ev)
			})

		def onMenuDragMouseReleased(callback:MenuDragMouseEvent=>Unit):Disposer	=
			menuDragMouseCaster.listen(new MenuDragMouseAdapter {
				override def menuDragMouseReleased(ev:MenuDragMouseEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type MenuKeyCaster = {
		def addMenuKeyListener(listener:MenuKeyListener):Unit
		def removeMenuKeyListener(listener:MenuKeyListener):Unit
	}

	extension (peer:MenuKeyCaster) {
		def menuKeyCaster:Caster[MenuKeyListener]	= new Caster(peer.addMenuKeyListener, peer.removeMenuKeyListener)

		def onMenuKeyEvent(callback:MenuKeyEvent=>Unit):Disposer	=
			menuKeyCaster.listen(new MenuKeyListener {
				def menuKeyTyped(ev:MenuKeyEvent):Unit		= callback(ev)
				def menuKeyPressed(ev:MenuKeyEvent):Unit	= callback(ev)
				def menuKeyReleased(ev:MenuKeyEvent):Unit	= callback(ev)
			})

		def onMenuKeyTyped(callback:MenuKeyEvent=>Unit):Disposer	=
			menuKeyCaster.listen(new MenuKeyAdapter {
				override def menuKeyTyped(ev:MenuKeyEvent):Unit	= callback(ev)
			})

		def onMenuKeyPressed(callback:MenuKeyEvent=>Unit):Disposer	=
			menuKeyCaster.listen(new MenuKeyAdapter {
				override def menuKeyPressed(ev:MenuKeyEvent):Unit	= callback(ev)
			})

		def onMenuKeyReleased(callback:MenuKeyEvent=>Unit):Disposer	=
			menuKeyCaster.listen(new MenuKeyAdapter {
				override def menuKeyReleased(ev:MenuKeyEvent):Unit	= callback(ev)
			})

	}

	//-----------------------------------------------------------------------------

	type RowSorterCaster = {
		def addRowSorterListener(listener:RowSorterListener):Unit
		def removeRowSorterListener(listener:RowSorterListener):Unit
	}

	extension (peer:RowSorterCaster) {
		def rowSorterCaster:Caster[RowSorterListener]	= new Caster(peer.addRowSorterListener, peer.removeRowSorterListener)

		// def onRowSorterEvent(callback:RowSorterEvent=>Unit):Disposer	= rowSorterCaster.listen(callback(_))

		def onSorterChanged(callback:RowSorterEvent=>Unit):Disposer	= rowSorterCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type ListDataCaster = {
		def addListDataListener(listener:ListDataListener):Unit
		def removeListDataListener(listener:ListDataListener):Unit
	}

	extension (peer:ListDataCaster) {
		def listDataCaster:Caster[ListDataListener]	= new Caster(peer.addListDataListener, peer.removeListDataListener)

		def onListDataEvent(callback:ListDataEvent=>Unit):Disposer	=
			listDataCaster.listen(new ListDataListener {
				def intervalAdded(ev:ListDataEvent):Unit	= callback(ev)
				def intervalRemoved(ev:ListDataEvent):Unit	= callback(ev)
				def contentsChanged(ev:ListDataEvent):Unit	= callback(ev)
			})

		def onIntervalAdded(callback:ListDataEvent=>Unit):Disposer	=
			listDataCaster.listen(new ListDataAdapter {
				override def intervalAdded(ev:ListDataEvent):Unit	= callback(ev)
			})

		def onIntervalRemoved(callback:ListDataEvent=>Unit):Disposer	=
			listDataCaster.listen(new ListDataAdapter {
				override def intervalRemoved(ev:ListDataEvent):Unit	= callback(ev)
			})

		def onContentsChanged(callback:ListDataEvent=>Unit):Disposer	=
			listDataCaster.listen(new ListDataAdapter {
				override def contentsChanged(ev:ListDataEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type ListSelectionCaster = {
		def addListSelectionListener(listener:ListSelectionListener):Unit
		def removeListSelectionListener(listener:ListSelectionListener):Unit
	}

	extension (peer:ListSelectionCaster) {
		def listSelectionCaster:Caster[ListSelectionListener]	= new Caster(peer.addListSelectionListener, peer.removeListSelectionListener)

		// def onListSelectionEvent(callback:ListSelectionEvent=>Unit):Disposer	= listSelectionCaster.listen(callback(_))

		@targetName("listSelection_onValueChanged")
		def onValueChanged(callback:ListSelectionEvent=>Unit):Disposer	= listSelectionCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type AncestorCaster = {
		def addAncestorListener(listener:AncestorListener):Unit
		def removeAncestorListener(listener:AncestorListener):Unit
	}

	extension (peer:AncestorCaster) {
		def ancestorCaster:Caster[AncestorListener]	= new Caster(peer.addAncestorListener, peer.removeAncestorListener)

		def onAncestorEvent(callback:AncestorEvent=>Unit):Disposer	=
			ancestorCaster.listen(new AncestorListener {
				def ancestorAdded(ev:AncestorEvent):Unit	= callback(ev)
				def ancestorRemoved(ev:AncestorEvent):Unit	= callback(ev)
				def ancestorMoved(ev:AncestorEvent):Unit	= callback(ev)
			})

		def onAncestorAdded(callback:AncestorEvent=>Unit):Disposer	=
			ancestorCaster.listen(new AncestorAdapter {
				override def ancestorAdded(ev:AncestorEvent):Unit	= callback(ev)
			})

		def onAncestorRemoved(callback:AncestorEvent=>Unit):Disposer	=
			ancestorCaster.listen(new AncestorAdapter {
				override def ancestorRemoved(ev:AncestorEvent):Unit	= callback(ev)
			})

		@targetName("ancestor_onAncestorMoved")
		def onAncestorMoved(callback:AncestorEvent=>Unit):Disposer	=
			ancestorCaster.listen(new AncestorAdapter {
				override def ancestorMoved(ev:AncestorEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type CaretCaster = {
		def addCaretListener(listener:CaretListener):Unit
		def removeCaretListener(listener:CaretListener):Unit
	}

	extension (peer:CaretCaster) {
		def caretCaster:Caster[CaretListener]	= new Caster(peer.addCaretListener, peer.removeCaretListener)

		// def onCaretEvent(callback:CaretEvent=>Unit):Disposer	= caretCaster.listen(callback(_))

		def onCaretUpdate(callback:CaretEvent=>Unit):Disposer	= caretCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type HyperlinkCaster = {
		def addHyperlinkListener(listener:HyperlinkListener):Unit
		def removeHyperlinkListener(listener:HyperlinkListener):Unit
	}

	extension (peer:HyperlinkCaster) {
		def hyperlinkCaster:Caster[HyperlinkListener]	= new Caster(peer.addHyperlinkListener, peer.removeHyperlinkListener)

		// def onHyperlinkEvent(callback:HyperlinkEvent=>Unit):Disposer	= hyperlinkCaster.listen(callback(_))

		def onHyperlinkUpdate(callback:HyperlinkEvent=>Unit):Disposer	= hyperlinkCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type ChangeCaster = {
		def addChangeListener(listener:ChangeListener):Unit
		def removeChangeListener(listener:ChangeListener):Unit
	}

	extension (peer:ChangeCaster) {
		def changeCaster:Caster[ChangeListener]	= new Caster(peer.addChangeListener, peer.removeChangeListener)

		// def onChangeEvent(callback:ChangeEvent=>Unit):Disposer	= changeCaster.listen(callback(_))

		def onStateChanged(callback:ChangeEvent=>Unit):Disposer	= changeCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type DocumentCaster = {
		def addDocumentListener(listener:DocumentListener):Unit
		def removeDocumentListener(listener:DocumentListener):Unit
	}

	extension (peer:DocumentCaster) {
		def documentCaster:Caster[DocumentListener]	= new Caster(peer.addDocumentListener, peer.removeDocumentListener)

		def onDocumentEvent(callback:DocumentEvent=>Unit):Disposer	=
			documentCaster.listen(new DocumentListener {
				def insertUpdate(ev:DocumentEvent):Unit		= callback(ev)
				def removeUpdate(ev:DocumentEvent):Unit		= callback(ev)
				def changedUpdate(ev:DocumentEvent):Unit	= callback(ev)
	})

		def onInsertUpdate(callback:DocumentEvent=>Unit):Disposer	=
			documentCaster.listen(new DocumentAdapter {
				override def insertUpdate(ev:DocumentEvent):Unit	= callback(ev)
			})

		def onRemoveUpdate(callback:DocumentEvent=>Unit):Disposer	=
			documentCaster.listen(new DocumentAdapter {
				override def removeUpdate(ev:DocumentEvent):Unit	= callback(ev)
			})

		def onChangedUpdate(callback:DocumentEvent=>Unit):Disposer	=
			documentCaster.listen(new DocumentAdapter {
				override def changedUpdate(ev:DocumentEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type MenuCaster = {
		def addMenuListener(listener:MenuListener):Unit
		def removeMenuListener(listener:MenuListener):Unit
	}

	extension (peer:MenuCaster) {
		def menuCaster:Caster[MenuListener]	= new Caster(peer.addMenuListener, peer.removeMenuListener)

		def onMenuEvent(callback:MenuEvent=>Unit):Disposer	=
			menuCaster.listen( new MenuListener {
				def menuSelected(ev:MenuEvent):Unit		= callback(ev)
				def menuDeselected(ev:MenuEvent):Unit	= callback(ev)
				def menuCanceled(ev:MenuEvent):Unit		= callback(ev)
			})

		def onMenuSelected(callback:MenuEvent=>Unit):Disposer	=
			menuCaster.listen(new MenuAdapter {
				override def menuSelected(ev:MenuEvent):Unit	= callback(ev)
			})

		def onMenuDeselected(callback:MenuEvent=>Unit):Disposer	=
			menuCaster.listen(new MenuAdapter {
				override def menuDeselected(ev:MenuEvent):Unit	= callback(ev)
			})

		def onMenuCanceled(callback:MenuEvent=>Unit):Disposer	=
			menuCaster.listen(new MenuAdapter {
				override def menuCanceled(ev:MenuEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type PopupMenuCaster = {
		def addPopupMenuListener(listener:PopupMenuListener):Unit
		def removePopupMenuListener(listener:PopupMenuListener):Unit
	}

	extension (peer:PopupMenuCaster) {
		def popupMenuCaster:Caster[PopupMenuListener]	= new Caster(peer.addPopupMenuListener, peer.removePopupMenuListener)

		def onPopupMenuEvent(callback:PopupMenuEvent=>Unit):Disposer	=
			popupMenuCaster.listen(new PopupMenuListener {
				def popupMenuWillBecomeVisible(ev:PopupMenuEvent):Unit		= callback(ev)
				def popupMenuWillBecomeInvisible(ev:PopupMenuEvent):Unit	= callback(ev)
				def popupMenuCanceled(ev:PopupMenuEvent):Unit				= callback(ev)
			})

		def onPopupMenuWillBecomeVisible(callback:PopupMenuEvent=>Unit):Disposer	=
			popupMenuCaster.listen(new PopupMenuAdapter {
				override def popupMenuWillBecomeVisible(ev:PopupMenuEvent):Unit	= callback(ev)
			})

		def onPopupMenuWillBecomeInvisible(callback:PopupMenuEvent=>Unit):Disposer	=
			popupMenuCaster.listen(new PopupMenuAdapter {
				override def popupMenuWillBecomeInvisible(ev:PopupMenuEvent):Unit	= callback(ev)
			})

		def onPopupMenuCanceled(callback:PopupMenuEvent=>Unit):Disposer	=
			popupMenuCaster.listen(new PopupMenuAdapter {
				override def popupMenuCanceled(ev:PopupMenuEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type TableModelCaster = {
		def addTableModelListener(listener:TableModelListener):Unit
		def removeTableModelListener(listener:TableModelListener):Unit
	}

	extension (peer:TableModelCaster) {
		def tableModelCaster:Caster[TableModelListener]	= new Caster(peer.addTableModelListener, peer.removeTableModelListener)

		// def onTableModelEvent(callback:TableModelEvent=>Unit):Disposer	= tableModelCaster.listen(callback(_))

		def onTableChanged(callback:TableModelEvent=>Unit):Disposer	= tableModelCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type TableColumnModelCaster = {
		def addTableColumnModelListener(listener:TableColumnModelListener):Unit
		def removeTableColumnModelListener(listener:TableColumnModelListener):Unit
	}

	extension (peer:TableColumnModelCaster) {
		def tableColumnModelCaster:Caster[TableColumnModelListener]	= new Caster(peer.addTableColumnModelListener, peer.removeTableColumnModelListener)

		// NOTE no generic listener, this caster uses multiple different event types

		def onColumnAdded(callback:TableColumnModelEvent=>Unit):Disposer	=
			tableColumnModelCaster.listen(new TableColumnModelAdapter {
			override def columnAdded(ev:TableColumnModelEvent):Unit	= callback(ev)
		})

		def onColumnRemoved(callback:TableColumnModelEvent=>Unit):Disposer	=
			tableColumnModelCaster.listen(new TableColumnModelAdapter {
				override def columnRemoved(ev:TableColumnModelEvent):Unit	= callback(ev)
			})

		def onColumnMoved(callback:TableColumnModelEvent=>Unit):Disposer	=
			tableColumnModelCaster.listen(new TableColumnModelAdapter {
				override def columnMoved(ev:TableColumnModelEvent):Unit	= callback(ev)
			})

		def onColumnMarginChanged(callback:ChangeEvent=>Unit):Disposer	=
			tableColumnModelCaster.listen(new TableColumnModelAdapter {
				override def columnMarginChanged(ev:ChangeEvent):Unit	= callback(ev)
			})

		def onColumnSelectionChanged(callback:ListSelectionEvent=>Unit):Disposer	=
			tableColumnModelCaster.listen(new TableColumnModelAdapter {
				override def columnSelectionChanged(ev:ListSelectionEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type TreeModelCaster = {
		def addTreeModelListener(listener:TreeModelListener):Unit
		def removeTreeModelListener(listener:TreeModelListener):Unit
	}

	extension (peer:TreeModelCaster) {
		def treeModelCaster:Caster[TreeModelListener]	= new Caster(peer.addTreeModelListener, peer.removeTreeModelListener)

		def onTreeModelEvent(callback:TreeModelEvent=>Unit):Disposer	=
			treeModelCaster.listen(new TreeModelListener {
				def treeNodesChanged(ev:TreeModelEvent):Unit		= callback(ev)
				def treeNodesInserted(ev:TreeModelEvent):Unit		= callback(ev)
				def treeNodesRemoved(ev:TreeModelEvent):Unit		= callback(ev)
				def treeStructureChanged(ev:TreeModelEvent):Unit	= callback(ev)
			})

		def onTreeNodesChanged(callback:TreeModelEvent=>Unit):Disposer	=
			treeModelCaster.listen(new TreeModelAdapter {
				override def treeNodesChanged(ev:TreeModelEvent):Unit	= callback(ev)
			})

		def onTreeNodesInserted(callback:TreeModelEvent=>Unit):Disposer	=
			treeModelCaster.listen(new TreeModelAdapter {
				override def treeNodesInserted(ev:TreeModelEvent):Unit	= callback(ev)
			})

		def onTreeNodesRemoved(callback:TreeModelEvent=>Unit):Disposer	=
			treeModelCaster.listen(new TreeModelAdapter {
				override def treeNodesRemoved(ev:TreeModelEvent):Unit	= callback(ev)
			})

		def onTreeStructureChanged(callback:TreeModelEvent=>Unit):Disposer	=
			treeModelCaster.listen(new TreeModelAdapter {
				override def treeStructureChanged(ev:TreeModelEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type TreeSelectionCaster = {
		def addTreeSelectionListener(listener:TreeSelectionListener):Unit
		def removeTreeSelectionListener(listener:TreeSelectionListener):Unit
	}

	extension (peer:TreeSelectionCaster) {
		def treeSelectionCaster:Caster[TreeSelectionListener]	= new Caster(peer.addTreeSelectionListener, peer.removeTreeSelectionListener)

		// def onTreeSelectionEvent(callback:TreeSelectionEvent=>Unit):Disposer	= treeSelectionCaster.listen(callback(_))

		@targetName("treeSelection_onValueChanged")
		def onValueChanged(callback:TreeSelectionEvent=>Unit):Disposer	= treeSelectionCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type TreeWillExpandCaster = {
		def addTreeWillExpandListener(listener:TreeWillExpandListener):Unit
		def removeTreeWillExpandListener(listener:TreeWillExpandListener):Unit
	}

	extension (peer:TreeWillExpandCaster) {
		def treeWillExpandCaster:Caster[TreeWillExpandListener]	= new Caster(peer.addTreeWillExpandListener, peer.removeTreeWillExpandListener)

		@targetName("treeWillExpand_onTreeExpansionEvent")
		def onTreeExpansionEvent(callback:TreeExpansionEvent=>Unit):Disposer	=
			treeWillExpandCaster.listen(new TreeWillExpandListener {
				def treeWillExpand(ev:TreeExpansionEvent):Unit	= callback(ev)
				def treeWillCollapse(ev:TreeExpansionEvent):Unit	= callback(ev)
			})

		def onTreeWillExpand(callback:TreeExpansionEvent=>Unit):Disposer	=
			treeWillExpandCaster.listen(new TreeWillExpandAdapter {
				override def treeWillExpand(ev:TreeExpansionEvent):Unit	= callback(ev)
			})

		def onTreeWillCollapse(callback:TreeExpansionEvent=>Unit):Disposer	=
			treeWillExpandCaster.listen(new TreeWillExpandAdapter {
				override def treeWillCollapse(ev:TreeExpansionEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	// NOTE TreeExpansionEvent doesn't have an ID
	type TreeExpansionCaster = {
		def addTreeExpansionListener(listener:TreeExpansionListener):Unit
		def removeTreeExpansionListener(listener:TreeExpansionListener):Unit
	}

	extension (peer:TreeExpansionCaster) {
		def treeExpansionCaster:Caster[TreeExpansionListener]	= new Caster(peer.addTreeExpansionListener, peer.removeTreeExpansionListener)

		@targetName("treeExpansion_onTreeExpansionEvent")
		def onTreeExpansionEvent(callback:TreeExpansionEvent=>Unit):Disposer	=
			treeExpansionCaster.listen(new TreeExpansionListener {
				def treeExpanded(ev:TreeExpansionEvent):Unit	= callback(ev)
				def treeCollapsed(ev:TreeExpansionEvent):Unit	= callback(ev)
			})

		def onTreeExpanded(callback:TreeExpansionEvent=>Unit):Disposer	=
			treeExpansionCaster.listen(new TreeExpansionAdapter {
				override def treeExpanded(ev:TreeExpansionEvent):Unit	= callback(ev)
			})

		def onTreeCollapsed(callback:TreeExpansionEvent=>Unit):Disposer	=
			treeExpansionCaster.listen(new TreeExpansionAdapter {
				override def treeCollapsed(ev:TreeExpansionEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type UndoableEditCaster = {
		def addUndoableEditListener(listener:UndoableEditListener):Unit
		def removeUndoableEditListener(listener:UndoableEditListener):Unit
	}

	extension (peer:UndoableEditCaster) {
		def undoableEditCaster:Caster[UndoableEditListener]	= new Caster(peer.addUndoableEditListener, peer.removeUndoableEditListener)

		// def onUndoableEditEvent(callback:UndoableEditEvent=>Unit):Disposer	= undoableEditCaster.listen(callback(_))

		def onUndoableEditHappened(callback:UndoableEditEvent=>Unit):Disposer	= undoableEditCaster.listen(callback(_))
	}
}
