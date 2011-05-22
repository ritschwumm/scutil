package scutil.gui

import javax.swing.event._

trait SwingCasterInstances {
	type CellEditorCaster = {
		def addCellEditorListener(listener:CellEditorListener):Unit
		def removeCellEditorListener(listener:CellEditorListener):Unit
	}
	def mkCellEditorListener(callback:ChangeEvent=>Unit):CellEditorListener = new CellEditorListener {
		def editingStopped(ev:ChangeEvent)	{ callback(ev) }
		def editingCanceled(ev:ChangeEvent)	{ callback(ev) }
	}
	implicit def generifyCellEditorCaster(caster:CellEditorCaster):Caster[CellEditorListener,ChangeEvent] = Caster(
			caster.addCellEditorListener,
			caster.removeCellEditorListener,
			mkCellEditorListener)
	
	type InternalFrameCaster = {
		def addInternalFrameListener(listener:InternalFrameListener):Unit
		def removeInternalFrameListener(listener:InternalFrameListener):Unit
	}
	def mkInternalFrameListener(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameListener {
		def internalFrameOpened(ev:InternalFrameEvent)		{ callback(ev) }
		def internalFrameClosing(ev:InternalFrameEvent)		{ callback(ev) }
		def internalFrameClosed(ev:InternalFrameEvent)		{ callback(ev) }
		def internalFrameIconified(ev:InternalFrameEvent)	{ callback(ev) }
		def internalFrameDeiconified(ev:InternalFrameEvent)	{ callback(ev) }
		def internalFrameActivated(ev:InternalFrameEvent)	{ callback(ev) }
		def internalFrameDeactivated(ev:InternalFrameEvent)	{ callback(ev) }
	}
	implicit def generifyInternalFrameCaster(caster:InternalFrameCaster):Caster[InternalFrameListener,InternalFrameEvent] = Caster(
			caster.addInternalFrameListener,
			caster.removeInternalFrameListener,
			mkInternalFrameListener)
	
	type MenuDragMouseCaster = {
		def addMenuDragMouseListener(listener:MenuDragMouseListener):Unit
		def removeMenuDragMouseListener(listener:MenuDragMouseListener):Unit
	}
	def mkMenuDragMouseListener(callback:MenuDragMouseEvent=>Unit):MenuDragMouseListener = new MenuDragMouseListener {
		def menuDragMouseEntered(ev:MenuDragMouseEvent)		{ callback(ev) }
		def menuDragMouseExited(ev:MenuDragMouseEvent)		{ callback(ev) }
		def menuDragMouseDragged(ev:MenuDragMouseEvent)		{ callback(ev) }
		def menuDragMouseReleased(ev:MenuDragMouseEvent)	{ callback(ev) }
	}
	implicit def generifyMenuDragMouseCaster(caster:MenuDragMouseCaster):Caster[MenuDragMouseListener,MenuDragMouseEvent] = Caster(
			caster.addMenuDragMouseListener,
			caster.removeMenuDragMouseListener,
			mkMenuDragMouseListener)
	
	type MenuKeyCaster = {
		def addMenuKeyListener(listener:MenuKeyListener):Unit
		def removeMenuKeyListener(listener:MenuKeyListener):Unit
	}
	def mkMenuKeyListener(callback:MenuKeyEvent=>Unit):MenuKeyListener = new MenuKeyListener {
		def menuKeyTyped(ev:MenuKeyEvent)		{ callback(ev) }
		def menuKeyPressed(ev:MenuKeyEvent)		{ callback(ev) }
		def menuKeyReleased(ev:MenuKeyEvent)	{ callback(ev) }
	}
	implicit def generifyMenuKeyCaster(caster:MenuKeyCaster):Caster[MenuKeyListener,MenuKeyEvent] = Caster(
			caster.addMenuKeyListener,
			caster.removeMenuKeyListener,
			mkMenuKeyListener)
	
	type RowSorterCaster = {
		def addRowSorterListener(listener:RowSorterListener):Unit
		def removeRowSorterListener(listener:RowSorterListener):Unit
	}
	def mkRowSorterListener(callback:RowSorterEvent=>Unit):RowSorterListener = new RowSorterListener {
		def sorterChanged(ev:RowSorterEvent) { callback(ev) }
	}
	implicit def generifyRowSorterCaster(caster:RowSorterCaster):Caster[RowSorterListener,RowSorterEvent] = Caster(
			caster.addRowSorterListener,
			caster.removeRowSorterListener,
			mkRowSorterListener)
	
	type ListDataCaster = {
		def addListDataListener(listener:ListDataListener):Unit
		def removeListDataListener(listener:ListDataListener):Unit
	}
	def mkListDataListener(callback:ListDataEvent=>Unit):ListDataListener = new ListDataListener {
		def intervalAdded(ev:ListDataEvent)		{ callback(ev) }
		def intervalRemoved(ev:ListDataEvent)	{ callback(ev) }
		def contentsChanged(ev:ListDataEvent)	{ callback(ev) }
	}
	implicit def generifyListDataCaster(caster:ListDataCaster):Caster[ListDataListener,ListDataEvent] = Caster(
			caster.addListDataListener,
			caster.removeListDataListener,
			mkListDataListener)
	
	type ListSelectionCaster = {
		def addListSelectionListener(listener:ListSelectionListener):Unit
		def removeListSelectionListener(listener:ListSelectionListener):Unit
	}
	def mkListSelectionListener(callback:ListSelectionEvent=>Unit):ListSelectionListener = new ListSelectionListener {
		def valueChanged(ev:ListSelectionEvent) { callback(ev) }
	}
	implicit def generifyListSelectionCaster(caster:ListSelectionCaster):Caster[ListSelectionListener,ListSelectionEvent] = Caster(
			caster.addListSelectionListener,
			caster.removeListSelectionListener,
			mkListSelectionListener)
	
	type AncestorCaster = {
		def addAncestorListener(listener:AncestorListener):Unit
		def removeAncestorListener(listener:AncestorListener):Unit
	}
	def mkAncestorListener(callback:AncestorEvent=>Unit):AncestorListener = new AncestorListener {
		def ancestorAdded(ev:AncestorEvent)		{ callback(ev) }
		def ancestorRemoved(ev:AncestorEvent)	{ callback(ev) }
		def ancestorMoved(ev:AncestorEvent)		{ callback(ev) }
	}
	implicit def generifyAncestorCaster(caster:AncestorCaster):Caster[AncestorListener,AncestorEvent] = Caster(
			caster.addAncestorListener,
			caster.removeAncestorListener,
			mkAncestorListener)
	
	type CaretCaster = {
		def addCaretListener(listener:CaretListener):Unit
		def removeCaretListener(listener:CaretListener):Unit
	}
	def mkCaretListener(callback:CaretEvent=>Unit):CaretListener = new CaretListener {
		def caretUpdate(ev:CaretEvent) { callback(ev) }
	}
	implicit def generifyCaretCaster(caster:CaretCaster):Caster[CaretListener,CaretEvent] = Caster(
			caster.addCaretListener,
			caster.removeCaretListener,
			mkCaretListener)
	
	type HyperlinkCaster = {
		def addHyperlinkListener(listener:HyperlinkListener):Unit
		def removeHyperlinkListener(listener:HyperlinkListener):Unit
	}
	def mkHyperlinkListener(callback:HyperlinkEvent=>Unit):HyperlinkListener = new HyperlinkListener {
		def hyperlinkUpdate(ev:HyperlinkEvent) { callback(ev) }
	}
	implicit def generifyHyperlinkCaster(caster:HyperlinkCaster):Caster[HyperlinkListener,HyperlinkEvent] = Caster(
			caster.addHyperlinkListener,
			caster.removeHyperlinkListener,
			mkHyperlinkListener)
	
	type ChangeCaster = {
		def addChangeListener(listener:ChangeListener):Unit
		def removeChangeListener(listener:ChangeListener):Unit
	}
	def mkChangeListener(callback:ChangeEvent=>Unit):ChangeListener = new ChangeListener {
		def stateChanged(ev:ChangeEvent) { callback(ev) }
	}
	implicit def generifyChangeCaster(caster:ChangeCaster):Caster[ChangeListener,ChangeEvent] = Caster(
			caster.addChangeListener,
			caster.removeChangeListener,
			mkChangeListener)
	
	type DocumentCaster = {
		def addDocumentListener(listener:DocumentListener):Unit
		def removeDocumentListener(listener:DocumentListener):Unit
	}
	def mkDocumentListener(callback:DocumentEvent=>Unit):DocumentListener = new DocumentListener {
		def insertUpdate(ev:DocumentEvent)	{ callback(ev) }
		def removeUpdate(ev:DocumentEvent)	{ callback(ev) }
		def changedUpdate(ev:DocumentEvent)	{ callback(ev) }
	}
	implicit def generifyDocumentCaster(caster:DocumentCaster):Caster[DocumentListener,DocumentEvent] = Caster(
			caster.addDocumentListener,
			caster.removeDocumentListener,
			mkDocumentListener)
	
	type MenuCaster = {
		def addMenuListener(listener:MenuListener):Unit
		def removeMenuListener(listener:MenuListener):Unit
	}
	def mkMenuListener(callback:MenuEvent=>Unit):MenuListener = new MenuListener {
		def menuSelected(ev:MenuEvent)		{ callback(ev) }
		def menuDeselected(ev:MenuEvent)	{ callback(ev) }
		def menuCanceled(ev:MenuEvent)		{ callback(ev) }
	}
	implicit def generifyMenuCaster(caster:MenuCaster):Caster[MenuListener,MenuEvent] = Caster(
			caster.addMenuListener,
			caster.removeMenuListener,
			mkMenuListener)
	
	type PopupMenuCaster = {
		def addPopupMenuListener(listener:PopupMenuListener):Unit
		def removePopupMenuListener(listener:PopupMenuListener):Unit
	}
	def mkPopupMenuListener(callback:PopupMenuEvent=>Unit):PopupMenuListener = new PopupMenuListener {
		def popupMenuWillBecomeVisible(ev:PopupMenuEvent)	{ callback(ev) }
		def popupMenuWillBecomeInvisible(ev:PopupMenuEvent)	{ callback(ev) }
		def popupMenuCanceled(ev:PopupMenuEvent)			{ callback(ev) }
	}
	implicit def generifyPopupMenuCaster(caster:PopupMenuCaster):Caster[PopupMenuListener,PopupMenuEvent] = Caster(
			caster.addPopupMenuListener,
			caster.removePopupMenuListener,
			mkPopupMenuListener)
	
	type TableModelCaster = {
		def addTableModelListener(listener:TableModelListener):Unit
		def removeTableModelListener(listener:TableModelListener):Unit
	}
	def mkTableModelListener(callback:TableModelEvent=>Unit):TableModelListener = new TableModelListener {
		def tableChanged(ev:TableModelEvent) { callback(ev) }
	}
	implicit def generifyTableModelCaster(caster:TableModelCaster):Caster[TableModelListener,TableModelEvent] = Caster(
			caster.addTableModelListener,
			caster.removeTableModelListener,
			mkTableModelListener)
	
	type TableColumnModelCaster = {
		def addTableColumnModelListener(listener:TableColumnModelListener):Unit
		def removeTableColumnModelListener(listener:TableColumnModelListener):Unit
	}
	def mkTableColumnModelListener(callback:TableColumnModelEvent=>Unit, marginCallback:ChangeEvent=>Unit, selectionCallback:ListSelectionEvent=>Unit):TableColumnModelListener = new TableColumnModelListener {
		def columnAdded(ev:TableColumnModelEvent)	{ callback(ev) }
		def columnRemoved(ev:TableColumnModelEvent)	{ callback(ev) }
		def columnMoved(ev:TableColumnModelEvent)	{ callback(ev) }
		def columnMarginChanged(ev:ChangeEvent)				{ marginCallback(ev) }
		def columnSelectionChanged(ev:ListSelectionEvent)	{ selectionCallback(ev) }
	}
	implicit def generifyTableColumnModelCaster(caster:TableColumnModelCaster):Caster[TableColumnModelListener,TableColumnModelEvent] = Caster(
			caster.addTableColumnModelListener,
			caster.removeTableColumnModelListener,
			// TODO does not work because we have multiple event types here
			sys error "not implemented")
	
	type TreeModelCaster = {
		def addTreeModelListener(listener:TreeModelListener):Unit
		def removeTreeModelListener(listener:TreeModelListener):Unit
	}
	def mkTreeModelListener(callback:TreeModelEvent=>Unit):TreeModelListener = new TreeModelListener {
		def treeNodesChanged(ev:TreeModelEvent)		{ callback(ev) }
		def treeNodesInserted(ev:TreeModelEvent)	{ callback(ev) }
		def treeNodesRemoved(ev:TreeModelEvent)		{ callback(ev) }
		def treeStructureChanged(ev:TreeModelEvent)	{ callback(ev) }
	}
	implicit def generifyTreeModelCaster(caster:TreeModelCaster):Caster[TreeModelListener,TreeModelEvent] = Caster(
			caster.addTreeModelListener,
			caster.removeTreeModelListener,
			mkTreeModelListener)

	type TreeSelectionCaster = {
		def addTreeSelectionListener(listener:TreeSelectionListener):Unit
		def removeTreeSelectionListener(listener:TreeSelectionListener):Unit
	}
	def mkTreeSelectionListener(callback:TreeSelectionEvent=>Unit):TreeSelectionListener = new TreeSelectionListener {
		def valueChanged(ev:TreeSelectionEvent) { callback(ev) }
	}	
	implicit def generifyTreeSelectionCaster(caster:TreeSelectionCaster):Caster[TreeSelectionListener,TreeSelectionEvent] = Caster(
			caster.addTreeSelectionListener,
			caster.removeTreeSelectionListener,
			mkTreeSelectionListener)
	
	type TreeWillExpandCaster = {
		def addTreeWillExpandListener(listener:TreeWillExpandListener):Unit
		def removeTreeWillExpandListener(listener:TreeWillExpandListener):Unit
	}
	def mkTreeWillExpandListener(callback:TreeExpansionEvent=>Unit):TreeWillExpandListener = new TreeWillExpandListener {
		def treeWillExpand(ev:TreeExpansionEvent)	{ callback(ev) }
		def treeWillCollapse(ev:TreeExpansionEvent)	{ callback(ev) }
	}
	implicit def generifyTreeWillExpandCaster(caster:TreeWillExpandCaster):Caster[TreeWillExpandListener,TreeExpansionEvent] = Caster(
			caster.addTreeWillExpandListener,
			caster.removeTreeWillExpandListener,
			mkTreeWillExpandListener)

	// TODO TreeExpansionEvent doesn't have an ID
	type TreeExpansionCaster = {
		def addTreeExpansionListener(listener:TreeExpansionListener):Unit
		def removeTreeExpansionListener(listener:TreeExpansionListener):Unit
	}
	def mkTreeExpansionListener(callback:TreeExpansionEvent=>Unit):TreeExpansionListener = new TreeExpansionListener {
		def treeExpanded(ev:TreeExpansionEvent)		{ callback(ev) }
		def treeCollapsed(ev:TreeExpansionEvent)	{ callback(ev) }
	}
	implicit def generifyTreeExpansionCaster(caster:TreeExpansionCaster):Caster[TreeExpansionListener,TreeExpansionEvent] = Caster(
			caster.addTreeExpansionListener,
			caster.removeTreeExpansionListener,
			mkTreeExpansionListener)
	
	type UndoableEditCaster = {
		def addUndoableEditListener(listener:UndoableEditListener):Unit
		def removeUndoableEditListener(listener:UndoableEditListener):Unit
	}
	def mkUndoableEditListener(callback:UndoableEditEvent=>Unit):UndoableEditListener = new UndoableEditListener {
		def undoableEditHappened(ev:UndoableEditEvent) { callback(ev) }
	}
	implicit def generifyUndoableEditCaster(caster:UndoableEditCaster):Caster[UndoableEditListener,UndoableEditEvent] = Caster(
			caster.addUndoableEditListener,
			caster.removeUndoableEditListener,
			mkUndoableEditListener)
}
