package scutil.gui

import java.beans.*
import java.awt.event.*
import java.awt.dnd.*
import javax.swing.event.*

import AdapterInstances.*

object ListenerInstances {
	//==============================================================================
	//## bean

	def mkPropertyChangeListener(callback:PropertyChangeEvent=>Unit):PropertyChangeListener = new PropertyChangeListener {
		def propertyChange(ev:PropertyChangeEvent):Unit = callback(ev)
	}
	def on_propertyChange(callback:PropertyChangeEvent=>Unit):PropertyChangeListener = mkPropertyChangeListener(callback)

	//==============================================================================
	//## awt

	// NOTE a generic mk*Listener does not work because we have multiple event types here
	// NOTE DropTargetAdapter does not implement drop itself
	def on_dragEnter(callback:DropTargetDragEvent=>Unit):DropTargetListener	= new DropTargetAdapter {
		override def dragEnter(ev:DropTargetDragEvent):Unit =	callback(ev)
		override def drop(ev:DropTargetDropEvent):Unit = {}
	}
	def on_dragExit(callback:DropTargetEvent=>Unit):DropTargetListener	= new DropTargetAdapter {
		override def dragExit(ev:DropTargetEvent):Unit =	callback(ev)
		override def drop(ev:DropTargetDropEvent):Unit = {}
	}
	def on_dragOver(callback:DropTargetDragEvent=>Unit):DropTargetListener	= new DropTargetAdapter {
		override def dragOver(ev:DropTargetDragEvent):Unit =	callback(ev)
		override def drop(ev:DropTargetDropEvent):Unit = {}
	}
	def on_drop(callback:DropTargetDropEvent=>Unit):DropTargetListener	= new DropTargetAdapter {
		override def drop(ev:DropTargetDropEvent):Unit =	callback(ev)
	}
	def on_dropActionChanged(callback:DropTargetDragEvent=>Unit):DropTargetListener	= new DropTargetAdapter {
		override def dropActionChanged(ev:DropTargetDragEvent):Unit =	callback(ev)
		override def drop(ev:DropTargetDropEvent):Unit = {}
	}

	//------------------------------------------------------------------------------

	def mkAdjustmentListener(callback:AdjustmentEvent=>Unit):AdjustmentListener = new AdjustmentListener {
		def adjustmentValueChanged(ev:AdjustmentEvent):Unit = callback(ev)
	}
	def on_adjustmentValueChanged(callback:AdjustmentEvent=>Unit):AdjustmentListener = mkAdjustmentListener(callback)

	//------------------------------------------------------------------------------

	def mkWindowListener(callback:WindowEvent=>Unit):WindowListener = new WindowListener {
		def windowOpened(ev:WindowEvent):Unit		= callback(ev)
		def windowClosing(ev:WindowEvent):Unit		= callback(ev)
		def windowClosed(ev:WindowEvent):Unit		= callback(ev)
		def windowIconified(ev:WindowEvent):Unit	= callback(ev)
		def windowDeiconified(ev:WindowEvent):Unit	= callback(ev)
		def windowActivated(ev:WindowEvent):Unit	= callback(ev)
		def windowDeactivated(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowOpened(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowOpened(ev:WindowEvent):Unit		= callback(ev)
	}
	def on_windowClosing(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowClosing(ev:WindowEvent):Unit		= callback(ev)
	}
	def on_windowClosed(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowClosed(ev:WindowEvent):Unit		= callback(ev)
	}
	def on_windowIconified(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowIconified(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowDeiconified(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowDeiconified(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowActivated(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowActivated(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowDeactivated(callback:WindowEvent=>Unit):WindowListener = new WindowAdapter {
		override def windowDeactivated(ev:WindowEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkWindowStateListener(callback:WindowEvent=>Unit):WindowStateListener = new WindowStateListener {
		def windowStateChanged(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowStateChanged(callback:WindowEvent=>Unit):WindowStateListener = mkWindowStateListener(callback)

	//------------------------------------------------------------------------------

	def mkWindowFocusListener(callback:WindowEvent=>Unit):WindowFocusListener = new WindowFocusListener {
		def windowGainedFocus(ev:WindowEvent):Unit	= callback(ev)
		def windowLostFocus(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowGainedFocus(callback:WindowEvent=>Unit):WindowFocusListener = new WindowFocusAdapter {
		override def windowGainedFocus(ev:WindowEvent):Unit	= callback(ev)
	}
	def on_windowLostFocus(callback:WindowEvent=>Unit):WindowFocusListener = new WindowFocusAdapter {
		override def windowLostFocus(ev:WindowEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkFocusListener(callback:FocusEvent=>Unit):FocusListener = new FocusListener {
		def focusGained(ev:FocusEvent):Unit	= callback(ev)
		def focusLost(ev:FocusEvent):Unit	= callback(ev)
	}
	def on_focusGained(callback:FocusEvent=>Unit):FocusListener = new FocusAdapter {
		override def focusGained(ev:FocusEvent):Unit	= callback(ev)
	}
	def on_focusLost(callback:FocusEvent=>Unit):FocusListener = new FocusAdapter {
		override def focusLost(ev:FocusEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkComponentListener(callback:ComponentEvent=>Unit):ComponentListener = new ComponentListener {
		def componentResized(ev:ComponentEvent):Unit	= callback(ev)
		def componentMoved(ev:ComponentEvent):Unit		= callback(ev)
		def componentShown(ev:ComponentEvent):Unit		= callback(ev)
		def componentHidden(ev:ComponentEvent):Unit		= callback(ev)
	}
	def on_componentResized(callback:ComponentEvent=>Unit):ComponentListener = new ComponentAdapter {
		override def componentResized(ev:ComponentEvent):Unit	= callback(ev)
	}
	def on_componentMoved(callback:ComponentEvent=>Unit):ComponentListener = new ComponentAdapter {
		override def componentMoved(ev:ComponentEvent):Unit		= callback(ev)
	}
	def on_componentShown(callback:ComponentEvent=>Unit):ComponentListener = new ComponentAdapter {
		override def componentShown(ev:ComponentEvent):Unit		= callback(ev)
	}
	def on_componentHidden(callback:ComponentEvent=>Unit):ComponentListener = new ComponentAdapter {
		override def componentHidden(ev:ComponentEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkContainerListener(callback:ContainerEvent=>Unit):ContainerListener = new ContainerListener {
		def componentAdded(ev:ContainerEvent):Unit	= callback(ev)
		def componentRemoved(ev:ContainerEvent):Unit	= callback(ev)
	}
	def on_componentAdded(callback:ContainerEvent=>Unit):ContainerListener = new ContainerAdapter {
		override def componentAdded(ev:ContainerEvent):Unit	= callback(ev)
	}
	def on_componentRemoved(callback:ContainerEvent=>Unit):ContainerListener = new ContainerAdapter {
		override def componentRemoved(ev:ContainerEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkItemListener(callback:ItemEvent=>Unit):ItemListener = new ItemListener {
		def itemStateChanged(ev:ItemEvent):Unit	= callback(ev)
	}
	def on_itemStateChanged(callback:ItemEvent=>Unit):ItemListener = mkItemListener(callback)

	//------------------------------------------------------------------------------

	def mkActionListener(callback:ActionEvent=>Unit):ActionListener = new ActionListener {
		def actionPerformed(ev:ActionEvent):Unit	= callback(ev)
	}
	def on_actionPerformed(callback:ActionEvent=>Unit):ActionListener = mkActionListener(callback)

	//------------------------------------------------------------------------------

	def mkKeyListener(callback:KeyEvent=>Unit):KeyListener = new KeyListener {
		def keyTyped(ev:KeyEvent):Unit		= callback(ev)
		def keyPressed(ev:KeyEvent):Unit	= callback(ev)
		def keyReleased(ev:KeyEvent):Unit	= callback(ev)
	}
	def on_keyTyped(callback:KeyEvent=>Unit):KeyListener = new KeyAdapter {
		override def keyTyped(ev:KeyEvent):Unit	= callback(ev)
	}
	def on_keyPressed(callback:KeyEvent=>Unit):KeyListener = new KeyAdapter {
		override def keyPressed(ev:KeyEvent):Unit	= callback(ev)
	}
	def on_keyReleased(callback:KeyEvent=>Unit):KeyListener = new KeyAdapter {
		override def keyReleased(ev:KeyEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkMouseListener(callback:MouseEvent=>Unit):MouseListener = new MouseListener {
		def mouseClicked(ev:MouseEvent):Unit	= callback(ev)
		def mousePressed(ev:MouseEvent):Unit	= callback(ev)
		def mouseReleased(ev:MouseEvent):Unit	= callback(ev)
		def mouseEntered(ev:MouseEvent):Unit	= callback(ev)
		def mouseExited(ev:MouseEvent):Unit		= callback(ev)
	}
	def on_mouseClicked(callback:MouseEvent=>Unit):MouseListener = new MouseAdapter {
		override def mouseClicked(ev:MouseEvent):Unit	= callback(ev)
	}
	def on_mousePressed(callback:MouseEvent=>Unit):MouseListener = new MouseAdapter {
		override def mousePressed(ev:MouseEvent):Unit	= callback(ev)
	}
	def on_mouseReleased(callback:MouseEvent=>Unit):MouseListener = new MouseAdapter {
		override def mouseReleased(ev:MouseEvent):Unit	= callback(ev)
	}
	def on_mouseEntered(callback:MouseEvent=>Unit):MouseListener = new MouseAdapter {
		override def mouseEntered(ev:MouseEvent):Unit	= callback(ev)
	}
	def on_mouseExited(callback:MouseEvent=>Unit):MouseListener = new MouseAdapter {
		override def mouseExited(ev:MouseEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkMouseMotionListener(callback:MouseEvent=>Unit):MouseMotionListener = new MouseMotionListener {
		def mouseDragged(ev:MouseEvent):Unit	= callback(ev)
		def mouseMoved(ev:MouseEvent):Unit	= callback(ev)
	}
	def on_mouseDragged(callback:MouseEvent=>Unit):MouseMotionListener = new MouseMotionAdapter {
		override def mouseDragged(ev:MouseEvent):Unit	= callback(ev)
	}
	def on_mouseMoved(callback:MouseEvent=>Unit):MouseMotionListener = new MouseMotionAdapter {
		override def mouseMoved(ev:MouseEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkMouseWheelListener(callback:MouseWheelEvent=>Unit):MouseWheelListener = new MouseWheelListener {
		def mouseWheelMoved(ev:MouseWheelEvent):Unit	= callback(ev)
	}
	def on_mouseWheelMoved(callback:MouseWheelEvent=>Unit):MouseWheelListener = mkMouseWheelListener(callback)

	//------------------------------------------------------------------------------

	def mkHierarchyBoundsListener(callback:HierarchyEvent=>Unit):HierarchyBoundsListener = new HierarchyBoundsListener {
		def ancestorMoved(ev:HierarchyEvent):Unit	= callback(ev)
		def ancestorResized(ev:HierarchyEvent):Unit	= callback(ev)
	}
	def on_ancestorMoved(callback:HierarchyEvent=>Unit):HierarchyBoundsListener = new HierarchyBoundsAdapter {
		override def ancestorMoved(ev:HierarchyEvent):Unit	= callback(ev)
	}
	def on_ancestorResized(callback:HierarchyEvent=>Unit):HierarchyBoundsListener = new HierarchyBoundsAdapter {
		override def ancestorResized(ev:HierarchyEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkHierarchyListener(callback:HierarchyEvent=>Unit):HierarchyListener = new HierarchyListener {
		def hierarchyChanged(ev:HierarchyEvent):Unit	= callback(ev)
	}
	def on_hierarchyChanged(callback:HierarchyEvent=>Unit):HierarchyListener = mkHierarchyListener(callback)

	//------------------------------------------------------------------------------

	def mkInputMethodListener(callback:InputMethodEvent=>Unit):InputMethodListener = new InputMethodListener {
		def inputMethodTextChanged(ev:InputMethodEvent):Unit	= callback(ev)
		def caretPositionChanged(ev:InputMethodEvent):Unit	= callback(ev)
	}
	def on_inputMethodTextChanged(callback:InputMethodEvent=>Unit):InputMethodListener = new InputMethodAdapter {
		override def inputMethodTextChanged(ev:InputMethodEvent):Unit	= callback(ev)
	}
	def on_caretPositionChanged(callback:InputMethodEvent=>Unit):InputMethodListener = new InputMethodAdapter {
		override def caretPositionChanged(ev:InputMethodEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkTextListener(callback:TextEvent=>Unit):TextListener = new TextListener {
		def textValueChanged(ev:TextEvent):Unit	= callback(ev)
	}
	def on_textValueChanged(callback:TextEvent=>Unit):TextListener = mkTextListener(callback)

	//==============================================================================
	//## swing

	def mkCellEditorListener(callback:ChangeEvent=>Unit):CellEditorListener = new CellEditorListener {
		def editingStopped(ev:ChangeEvent):Unit	= callback(ev)
		def editingCanceled(ev:ChangeEvent):Unit	= callback(ev)
	}
	def on_editingStopped(callback:ChangeEvent=>Unit):CellEditorListener = new CellEditorAdapter {
		override def editingStopped(ev:ChangeEvent):Unit	= callback(ev)
	}
	def on_editingCanceled(callback:ChangeEvent=>Unit):CellEditorListener = new CellEditorAdapter {
		override def editingCanceled(ev:ChangeEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkInternalFrameListener(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameListener {
		def internalFrameOpened(ev:InternalFrameEvent):Unit			= callback(ev)
		def internalFrameClosing(ev:InternalFrameEvent):Unit		= callback(ev)
		def internalFrameClosed(ev:InternalFrameEvent):Unit			= callback(ev)
		def internalFrameIconified(ev:InternalFrameEvent):Unit		= callback(ev)
		def internalFrameDeiconified(ev:InternalFrameEvent):Unit	= callback(ev)
		def internalFrameActivated(ev:InternalFrameEvent):Unit		= callback(ev)
		def internalFrameDeactivated(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameOpened(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameOpened(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameClosing(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameClosing(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameClosed(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameClosed(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameIconified(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameIconified(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameDeiconified(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameDeiconified(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameActivated(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameActivated(ev:InternalFrameEvent):Unit	= callback(ev)
	}
	def on_internalFrameDeactivated(callback:InternalFrameEvent=>Unit):InternalFrameListener = new InternalFrameAdapter {
		override def internalFrameDeactivated(ev:InternalFrameEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkMenuDragMouseListener(callback:MenuDragMouseEvent=>Unit):MenuDragMouseListener = new MenuDragMouseListener {
		def menuDragMouseEntered(ev:MenuDragMouseEvent):Unit	= callback(ev)
		def menuDragMouseExited(ev:MenuDragMouseEvent):Unit		= callback(ev)
		def menuDragMouseDragged(ev:MenuDragMouseEvent):Unit	= callback(ev)
		def menuDragMouseReleased(ev:MenuDragMouseEvent):Unit	= callback(ev)
	}
	def on_menuDragMouseEntered(callback:MenuDragMouseEvent=>Unit):MenuDragMouseListener = new MenuDragMouseAdapter {
		override def menuDragMouseEntered(ev:MenuDragMouseEvent):Unit	= callback(ev)
	}
	def on_menuDragMouseExited(callback:MenuDragMouseEvent=>Unit):MenuDragMouseListener = new MenuDragMouseAdapter {
		override def menuDragMouseExited(ev:MenuDragMouseEvent):Unit	= callback(ev)
	}
	def on_menuDragMouseDragged(callback:MenuDragMouseEvent=>Unit):MenuDragMouseListener = new MenuDragMouseAdapter {
		override def menuDragMouseDragged(ev:MenuDragMouseEvent):Unit	= callback(ev)
	}
	def on_menuDragMouseReleased(callback:MenuDragMouseEvent=>Unit):MenuDragMouseListener = new MenuDragMouseAdapter {
		override def menuDragMouseReleased(ev:MenuDragMouseEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkMenuKeyListener(callback:MenuKeyEvent=>Unit):MenuKeyListener = new MenuKeyListener {
		def menuKeyTyped(ev:MenuKeyEvent):Unit		= callback(ev)
		def menuKeyPressed(ev:MenuKeyEvent):Unit	= callback(ev)
		def menuKeyReleased(ev:MenuKeyEvent):Unit	= callback(ev)
	}
	def on_menuKeyTyped(callback:MenuKeyEvent=>Unit):MenuKeyListener = new MenuKeyAdapter {
		override def menuKeyTyped(ev:MenuKeyEvent):Unit	= callback(ev)
	}
	def on_menuKeyPressed(callback:MenuKeyEvent=>Unit):MenuKeyListener = new MenuKeyAdapter {
		override def menuKeyPressed(ev:MenuKeyEvent):Unit	= callback(ev)
	}
	def on_menuKeyReleased(callback:MenuKeyEvent=>Unit):MenuKeyListener = new MenuKeyAdapter {
		override def menuKeyReleased(ev:MenuKeyEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkRowSorterListener(callback:RowSorterEvent=>Unit):RowSorterListener = new RowSorterListener {
		def sorterChanged(ev:RowSorterEvent):Unit	= callback(ev)
	}
	def on_sorterChanged(callback:RowSorterEvent=>Unit):RowSorterListener = mkRowSorterListener(callback)

	//------------------------------------------------------------------------------

	def mkListDataListener(callback:ListDataEvent=>Unit):ListDataListener = new ListDataListener {
		def intervalAdded(ev:ListDataEvent):Unit	= callback(ev)
		def intervalRemoved(ev:ListDataEvent):Unit	= callback(ev)
		def contentsChanged(ev:ListDataEvent):Unit	= callback(ev)
	}
	def on_intervalAdded(callback:ListDataEvent=>Unit):ListDataListener = new ListDataAdapter {
		override def intervalAdded(ev:ListDataEvent):Unit	= callback(ev)
	}
	def on_intervalRemoved(callback:ListDataEvent=>Unit):ListDataListener = new ListDataAdapter {
		override def intervalRemoved(ev:ListDataEvent):Unit	= callback(ev)
	}
	def on_contentsChanged(callback:ListDataEvent=>Unit):ListDataListener = new ListDataAdapter {
		override def contentsChanged(ev:ListDataEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkListSelectionListener(callback:ListSelectionEvent=>Unit):ListSelectionListener = new ListSelectionListener {
		def valueChanged(ev:ListSelectionEvent):Unit	= callback(ev)
	}
	def on_valueChanged(callback:ListSelectionEvent=>Unit):ListSelectionListener = mkListSelectionListener(callback)

	//------------------------------------------------------------------------------

	def mkAncestorListener(callback:AncestorEvent=>Unit):AncestorListener = new AncestorListener {
		def ancestorAdded(ev:AncestorEvent):Unit	= callback(ev)
		def ancestorRemoved(ev:AncestorEvent):Unit	= callback(ev)
		def ancestorMoved(ev:AncestorEvent):Unit	= callback(ev)
	}
	def on_ancestorAdded(callback:AncestorEvent=>Unit):AncestorListener = new AncestorAdapter {
		override def ancestorAdded(ev:AncestorEvent):Unit	= callback(ev)
	}
	def on_ancestorRemoved(callback:AncestorEvent=>Unit):AncestorListener = new AncestorAdapter {
		override def ancestorRemoved(ev:AncestorEvent):Unit	= callback(ev)
	}
	def on_ancestorMoved(callback:AncestorEvent=>Unit):AncestorListener = new AncestorAdapter {
		override def ancestorMoved(ev:AncestorEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkCaretListener(callback:CaretEvent=>Unit):CaretListener = new CaretListener {
		def caretUpdate(ev:CaretEvent):Unit	= callback(ev)
	}
	def on_caretUpdate(callback:CaretEvent=>Unit):CaretListener = mkCaretListener(callback)

	//------------------------------------------------------------------------------

	def mkHyperlinkListener(callback:HyperlinkEvent=>Unit):HyperlinkListener = new HyperlinkListener {
		def hyperlinkUpdate(ev:HyperlinkEvent):Unit	= callback(ev)
	}
	def on_hyperlinkUpdate(callback:HyperlinkEvent=>Unit):HyperlinkListener = mkHyperlinkListener(callback)

	//------------------------------------------------------------------------------

	def mkChangeListener(callback:ChangeEvent=>Unit):ChangeListener = new ChangeListener {
		def stateChanged(ev:ChangeEvent):Unit	= callback(ev)
	}
	def on_stateChanged(callback:ChangeEvent=>Unit):ChangeListener = mkChangeListener(callback)

	//------------------------------------------------------------------------------

	def mkDocumentListener(callback:DocumentEvent=>Unit):DocumentListener = new DocumentListener {
		def insertUpdate(ev:DocumentEvent):Unit		= callback(ev)
		def removeUpdate(ev:DocumentEvent):Unit		= callback(ev)
		def changedUpdate(ev:DocumentEvent):Unit	= callback(ev)
	}
	def on_insertUpdate(callback:DocumentEvent=>Unit):DocumentListener = new DocumentAdapter {
		override def insertUpdate(ev:DocumentEvent):Unit	= callback(ev)
	}
	def on_removeUpdate(callback:DocumentEvent=>Unit):DocumentListener = new DocumentAdapter {
		override def removeUpdate(ev:DocumentEvent):Unit	= callback(ev)
	}
	def on_changedUpdate(callback:DocumentEvent=>Unit):DocumentListener = new DocumentAdapter {
		override def changedUpdate(ev:DocumentEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkMenuListener(callback:MenuEvent=>Unit):MenuListener = new MenuListener {
		def menuSelected(ev:MenuEvent):Unit		= callback(ev)
		def menuDeselected(ev:MenuEvent):Unit	= callback(ev)
		def menuCanceled(ev:MenuEvent):Unit		= callback(ev)
	}
	def on_menuSelected(callback:MenuEvent=>Unit):MenuListener = new MenuAdapter {
		override def menuSelected(ev:MenuEvent):Unit	= callback(ev)
	}
	def on_menuDeselected(callback:MenuEvent=>Unit):MenuListener = new MenuAdapter {
		override def menuDeselected(ev:MenuEvent):Unit	= callback(ev)
	}
	def on_menuCanceled(callback:MenuEvent=>Unit):MenuListener = new MenuAdapter {
		override def menuCanceled(ev:MenuEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkPopupMenuListener(callback:PopupMenuEvent=>Unit):PopupMenuListener = new PopupMenuListener {
		def popupMenuWillBecomeVisible(ev:PopupMenuEvent):Unit		= callback(ev)
		def popupMenuWillBecomeInvisible(ev:PopupMenuEvent):Unit	= callback(ev)
		def popupMenuCanceled(ev:PopupMenuEvent):Unit				= callback(ev)
	}
	def on_popupMenuWillBecomeVisible(callback:PopupMenuEvent=>Unit):PopupMenuListener = new PopupMenuAdapter {
		override def popupMenuWillBecomeVisible(ev:PopupMenuEvent):Unit	= callback(ev)
	}
	def on_popupMenuWillBecomeInvisible(callback:PopupMenuEvent=>Unit):PopupMenuListener = new PopupMenuAdapter {
		override def popupMenuWillBecomeInvisible(ev:PopupMenuEvent):Unit	= callback(ev)
	}
	def on_popupMenuCanceled(callback:PopupMenuEvent=>Unit):PopupMenuListener = new PopupMenuAdapter {
		override def popupMenuCanceled(ev:PopupMenuEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkTableModelListener(callback:TableModelEvent=>Unit):TableModelListener = new TableModelListener {
		def tableChanged(ev:TableModelEvent):Unit	= callback(ev)
	}
	def on_tableChanged(callback:TableModelEvent=>Unit):TableModelListener = mkTableModelListener(callback)

	//------------------------------------------------------------------------------

	/*
	// NOTE a generic mk*Listener does not work because we have multiple event types here
	def mkTableColumnModelListener(callback:TableColumnModelEvent=>Unit, marginCallback:ChangeEvent=>Unit, selectionCallback:ListSelectionEvent=>Unit):TableColumnModelListener = new TableColumnModelListener {
		def columnAdded(ev:TableColumnModelEvent):Unit	= callback(ev)
		def columnRemoved(ev:TableColumnModelEvent):Unit	= callback(ev)
		def columnMoved(ev:TableColumnModelEvent):Unit	= callback(ev)
		def columnMarginChanged(ev:ChangeEvent):Unit	= { marginCallback(ev) }
		def columnSelectionChanged(ev:ListSelectionEvent):Unit	= { selectionCallback(ev) }
	}
	*/
	def on_columnAdded(callback:TableColumnModelEvent=>Unit):TableColumnModelListener = new TableColumnModelAdapter {
		override def columnAdded(ev:TableColumnModelEvent):Unit	= callback(ev)
	}
	def on_columnRemoved(callback:TableColumnModelEvent=>Unit):TableColumnModelListener = new TableColumnModelAdapter {
		override def columnRemoved(ev:TableColumnModelEvent):Unit	= callback(ev)
	}
	def on_columnMoved(callback:TableColumnModelEvent=>Unit):TableColumnModelListener = new TableColumnModelAdapter {
		override def columnMoved(ev:TableColumnModelEvent):Unit	= callback(ev)
	}
	def on_columnMarginChanged(callback:ChangeEvent=>Unit):TableColumnModelListener = new TableColumnModelAdapter {
		override def columnMarginChanged(ev:ChangeEvent):Unit	= callback(ev)
	}
	def on_columnSelectionChanged(callback:ListSelectionEvent=>Unit):TableColumnModelListener = new TableColumnModelAdapter {
		override def columnSelectionChanged(ev:ListSelectionEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkTreeModelListener(callback:TreeModelEvent=>Unit):TreeModelListener = new TreeModelListener {
		def treeNodesChanged(ev:TreeModelEvent):Unit		= callback(ev)
		def treeNodesInserted(ev:TreeModelEvent):Unit		= callback(ev)
		def treeNodesRemoved(ev:TreeModelEvent):Unit		= callback(ev)
		def treeStructureChanged(ev:TreeModelEvent):Unit	= callback(ev)
	}
	def on_treeNodesChanged(callback:TreeModelEvent=>Unit):TreeModelListener = new TreeModelAdapter {
		override def treeNodesChanged(ev:TreeModelEvent):Unit	= callback(ev)
	}
	def on_treeNodesInserted(callback:TreeModelEvent=>Unit):TreeModelListener = new TreeModelAdapter {
		override def treeNodesInserted(ev:TreeModelEvent):Unit	= callback(ev)
	}
	def on_treeNodesRemoved(callback:TreeModelEvent=>Unit):TreeModelListener = new TreeModelAdapter {
		override def treeNodesRemoved(ev:TreeModelEvent):Unit	= callback(ev)
	}
	def on_treeStructureChanged(callback:TreeModelEvent=>Unit):TreeModelListener = new TreeModelAdapter {
		override def treeStructureChanged(ev:TreeModelEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkTreeSelectionListener(callback:TreeSelectionEvent=>Unit):TreeSelectionListener = new TreeSelectionListener {
		def valueChanged(ev:TreeSelectionEvent):Unit	= callback(ev)
	}
	def on_valueChanged(callback:TreeSelectionEvent=>Unit):TreeSelectionListener = mkTreeSelectionListener(callback)

	//------------------------------------------------------------------------------

	def mkTreeWillExpandListener(callback:TreeExpansionEvent=>Unit):TreeWillExpandListener = new TreeWillExpandListener {
		def treeWillExpand(ev:TreeExpansionEvent):Unit	= callback(ev)
		def treeWillCollapse(ev:TreeExpansionEvent):Unit	= callback(ev)
	}
	def on_treeWillExpand(callback:TreeExpansionEvent=>Unit):TreeWillExpandListener = new TreeWillExpandAdapter {
		override def treeWillExpand(ev:TreeExpansionEvent):Unit	= callback(ev)
	}
	def on_treeWillCollapse(callback:TreeExpansionEvent=>Unit):TreeWillExpandListener = new TreeWillExpandAdapter {
		override def treeWillCollapse(ev:TreeExpansionEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkTreeExpansionListener(callback:TreeExpansionEvent=>Unit):TreeExpansionListener = new TreeExpansionListener {
		def treeExpanded(ev:TreeExpansionEvent):Unit	= callback(ev)
		def treeCollapsed(ev:TreeExpansionEvent):Unit	= callback(ev)
	}
	def on_treeExpanded(callback:TreeExpansionEvent=>Unit):TreeExpansionListener = new TreeExpansionAdapter {
		override def treeExpanded(ev:TreeExpansionEvent):Unit	= callback(ev)
	}
	def on_treeCollapsed(callback:TreeExpansionEvent=>Unit):TreeExpansionListener = new TreeExpansionAdapter {
		override def treeCollapsed(ev:TreeExpansionEvent):Unit	= callback(ev)
	}

	//------------------------------------------------------------------------------

	def mkUndoableEditListener(callback:UndoableEditEvent=>Unit):UndoableEditListener = new UndoableEditListener {
		def undoableEditHappened(ev:UndoableEditEvent):Unit	= callback(ev)
	}
	def on_undoableEditHappened(callback:UndoableEditEvent=>Unit):UndoableEditListener = mkUndoableEditListener(callback)
}
