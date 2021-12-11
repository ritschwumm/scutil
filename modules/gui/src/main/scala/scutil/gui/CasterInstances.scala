package scutil.gui

import scala.reflect.Selectable.reflectiveSelectable

import java.beans._
import java.awt.event._
import java.awt.dnd._
import javax.swing.event._

import scutil.lang._

import ListenerInstances._

object CasterInstances {
	//==============================================================================
	//## bean

	type PropertyChangeCaster = {
		def addPropertyChangeListener(listener:PropertyChangeListener):Unit
		def removePropertyChangeListener(listener:PropertyChangeListener):Unit
	}
	final class PropertyChangeCasterExt(caster:PropertyChangeCaster) extends Caster(caster.addPropertyChangeListener, caster.removePropertyChangeListener, mkPropertyChangeListener) {
		def onPropertyChange(callback:PropertyChangeEvent=>Unit):Disposer	= listen(on_propertyChange(callback))
	}
	implicit def generifyPropertyChangeCaster(caster:PropertyChangeCaster):PropertyChangeCasterExt	= new PropertyChangeCasterExt(caster)

	/*
	type NamedPropertyChangeCaster = {
		def addPropertyChangeListener(name:String, listener:PropertyChangeListener):Unit
		def removePropertyChangeListener(name:String, listener:PropertyChangeListener):Unit
	}
	// NOTE this cannot be made implicit
	def generifyNamedPropertyChangeCaster(name:String, caster:NamedPropertyChangeCaster):Caster[PropertyChangeListener,PropertyChangeEvent] = new Caster[PropertyChangeListener,PropertyChangeEvent] {
		def addListener(listener:PropertyChangeListener)	{ caster addPropertyChangeListener		(name, listener) }
		def removeListener(listener:PropertyChangeListener)	{ caster removePropertyChangeListener	(name, listener) }
		def createListener(callback:PropertyChangeEvent=>Unit)	= mkPropertyChangeListener(callback)
	}
	*/

	//==============================================================================
	//## awt

	type DropTargetCaster = {
		def addDropTargetListener(listener:DropTargetListener):Unit
		def removeDropTargetListener(listener:DropTargetListener):Unit
	}
	implicit def generifyDropTargetCaster(caster:DropTargetCaster):DropTargetCasterExt	= new DropTargetCasterExt(caster)
	// NOTE a generic mk*Listener does not work because we have multiple event types here
	final class DropTargetCasterExt(caster:DropTargetCaster) extends Caster(caster.addDropTargetListener, caster.removeDropTargetListener, sorryNotImplemented[DropTargetListener]) {
		def onDragEnter(callback:DropTargetDragEvent=>Unit):Disposer			= listen(on_dragEnter(callback))
		def onDragExit(callback:DropTargetEvent=>Unit):Disposer					= listen(on_dragExit(callback))
		def onDragOver(callback:DropTargetDragEvent=>Unit):Disposer				= listen(on_dragOver(callback))
		def onDrop(callback:DropTargetDropEvent=>Unit):Disposer					= listen(on_drop(callback))
		def onDropActionChanged(callback:DropTargetDragEvent=>Unit):Disposer	= listen(on_dropActionChanged(callback))
	}

	type AdjustmentCaster = {
		def addAdjustmentListener(listener:AdjustmentListener):Unit
		def removeAdjustmentListener(listener:AdjustmentListener):Unit
	}
	implicit def generifyAdjustmentCaster(caster:AdjustmentCaster):AdjustmentCasterExt	= new AdjustmentCasterExt(caster)
	final class AdjustmentCasterExt(caster:AdjustmentCaster) extends Caster(caster.addAdjustmentListener, caster.removeAdjustmentListener, mkAdjustmentListener) {
		def onAdjustmentValueChanged(callback:AdjustmentEvent=>Unit):Disposer	= listen(on_adjustmentValueChanged(callback))
	}

	type WindowCaster = {
		def addWindowListener(listener:WindowListener):Unit
		def removeWindowListener(listener:WindowListener):Unit
	}
	implicit def generifyWindowCaster(caster:WindowCaster):WindowCasterExt = new WindowCasterExt(caster)
	final class WindowCasterExt(caster:WindowCaster) extends Caster(caster.addWindowListener, caster.removeWindowListener, mkWindowListener) {
		def onWindowOpened(callback:WindowEvent=>Unit):Disposer			= listen(on_windowOpened(callback))
		def onWindowClosing(callback:WindowEvent=>Unit):Disposer		= listen(on_windowClosing(callback))
		def onWindowClosed(callback:WindowEvent=>Unit):Disposer			= listen(on_windowClosed(callback))
		def onWindowIconified(callback:WindowEvent=>Unit):Disposer		= listen(on_windowIconified(callback))
		def onWindowDeiconified(callback:WindowEvent=>Unit):Disposer	= listen(on_windowDeiconified(callback))
		def onWindowActivated(callback:WindowEvent=>Unit):Disposer		= listen(on_windowActivated(callback))
		def onWindowDeactivated(callback:WindowEvent=>Unit):Disposer	= listen(on_windowDeactivated(callback))
	}

	type WindowStateCaster = {
		def addWindowStateListener(listener:WindowStateListener):Unit
		def removeWindowStateListener(listener:WindowStateListener):Unit
	}
	implicit def generifyWindowStateCaster(caster:WindowStateCaster):WindowStateCasterExt	= new WindowStateCasterExt(caster)
	final class WindowStateCasterExt(caster:WindowStateCaster) extends Caster(caster.addWindowStateListener, caster.removeWindowStateListener, mkWindowStateListener) {
		def onWindowStateChanged(callback:WindowEvent=>Unit):Disposer	= listen(on_windowStateChanged(callback))
	}

	type WindowFocusCaster = {
		def addWindowFocusListener(listener:WindowFocusListener):Unit
		def removeWindowFocusListener(listener:WindowFocusListener):Unit
	}
	implicit def generifyWindowFocusCaster(caster:WindowFocusCaster):WindowFocusCasterExt	= new WindowFocusCasterExt(caster)
	final class WindowFocusCasterExt(caster:WindowFocusCaster) extends Caster(caster.addWindowFocusListener, caster.removeWindowFocusListener, mkWindowFocusListener) {
		def onWindowGainedFocus(callback:WindowEvent=>Unit):Disposer	= listen(on_windowGainedFocus(callback))
		def onWindowLostFocus(callback:WindowEvent=>Unit):Disposer		= listen(on_windowLostFocus(callback))
	}

	type FocusCaster = {
		def addFocusListener(listener:FocusListener):Unit
		def removeFocusListener(listener:FocusListener):Unit
	}
	implicit def generifyFocusCaster(caster:FocusCaster):FocusCasterExt	= new FocusCasterExt(caster)
	final class FocusCasterExt(caster:FocusCaster) extends Caster(caster.addFocusListener, caster.removeFocusListener, mkFocusListener) {
		def onFocusGained(callback:FocusEvent=>Unit):Disposer	= listen(on_focusGained(callback))
		def onFocusLost(callback:FocusEvent=>Unit):Disposer		= listen(on_focusLost(callback))
	}

	type ComponentCaster = {
		def addComponentListener(listener:ComponentListener):Unit
		def removeComponentListener(listener:ComponentListener):Unit
	}
	implicit def generifyComponentCaster(caster:ComponentCaster):ComponentCasterExt	= new ComponentCasterExt(caster)
	final class ComponentCasterExt(caster:ComponentCaster) extends Caster(caster.addComponentListener, caster.removeComponentListener, mkComponentListener) {
		def onComponentResized(callback:ComponentEvent=>Unit):Disposer	= listen(on_componentResized(callback))
		def onComponentMoved(callback:ComponentEvent=>Unit):Disposer	= listen(on_componentMoved(callback))
		def onComponentShown(callback:ComponentEvent=>Unit):Disposer	= listen(on_componentShown(callback))
		def onComponentHidden(callback:ComponentEvent=>Unit):Disposer	= listen(on_componentHidden(callback))
	}

	type ContainerCaster = {
		def addContainerListener(listener:ContainerListener):Unit
		def removeContainerListener(listener:ContainerListener):Unit
	}
	implicit def generifyContainerCaster(caster:ContainerCaster):ContainerCasterExt	= new ContainerCasterExt(caster)
	final class ContainerCasterExt(caster:ContainerCaster) extends Caster(caster.addContainerListener, caster.removeContainerListener, mkContainerListener) {
		def onComponentAdded(callback:ContainerEvent=>Unit):Disposer	= listen(on_componentAdded(callback))
		def onComponentRemoved(callback:ContainerEvent=>Unit):Disposer	= listen(on_componentRemoved(callback))
	}

	type ItemCaster = {
		def addItemListener(listener:ItemListener):Unit
		def removeItemListener(listener:ItemListener):Unit
	}
	implicit def generifyItemCaster(caster:ItemCaster):ItemCasterExt	= new ItemCasterExt(caster)
	final class ItemCasterExt(caster:ItemCaster) extends Caster(caster.addItemListener, caster.removeItemListener, mkItemListener) {
		def onItemStateChanged(callback:ItemEvent=>Unit):Disposer	= listen(on_itemStateChanged(callback))
	}

	type ActionCaster = {
		def addActionListener(listener:ActionListener):Unit
		def removeActionListener(listener:ActionListener):Unit
	}
	implicit def generifyActionCaster(caster:ActionCaster):ActionCasterExt	= new ActionCasterExt(caster)
	final class ActionCasterExt(caster:ActionCaster) extends Caster(caster.addActionListener, caster.removeActionListener, mkActionListener) {
		def onActionPerformed(callback:ActionEvent=>Unit):Disposer	= listen(on_actionPerformed(callback))
	}

	type KeyCaster = {
		def addKeyListener(listener:KeyListener):Unit
		def removeKeyListener(listener:KeyListener):Unit
	}
	implicit def generifyKeyCaster(caster:KeyCaster):KeyCasterExt	= new KeyCasterExt(caster)
	final class KeyCasterExt(caster:KeyCaster) extends Caster(caster.addKeyListener, caster.removeKeyListener, mkKeyListener) {
		def onKeyTyped(callback:KeyEvent=>Unit):Disposer	= listen(on_keyTyped(callback))
		def onKeyPressed(callback:KeyEvent=>Unit):Disposer	= listen(on_keyPressed(callback))
		def onKeyReleased(callback:KeyEvent=>Unit):Disposer	= listen(on_keyReleased(callback))
	}

	type MouseCaster = {
		def addMouseListener(listener:MouseListener):Unit
		def removeMouseListener(listener:MouseListener):Unit
	}
	implicit def generifyMouseCaster(caster:MouseCaster):MouseCasterExt	= new MouseCasterExt(caster)
	final class MouseCasterExt(caster:MouseCaster) extends Caster(caster.addMouseListener, caster.removeMouseListener, mkMouseListener) {
		def onMouseClicked(callback:MouseEvent=>Unit):Disposer	= listen(on_mouseClicked(callback))
		def onMousePressed(callback:MouseEvent=>Unit):Disposer	= listen(on_mousePressed(callback))
		def onMouseReleased(callback:MouseEvent=>Unit):Disposer	= listen(on_mouseReleased(callback))
		def onMouseEntered(callback:MouseEvent=>Unit):Disposer	= listen(on_mouseEntered(callback))
		def onMouseExited(callback:MouseEvent=>Unit):Disposer	= listen(on_mouseExited(callback))
	}

	type MouseMotionCaster = {
		def addMouseMotionListener(listener:MouseMotionListener):Unit
		def removeMouseMotionListener(listener:MouseMotionListener):Unit
	}
	implicit def generifyMouseMotionCaster(caster:MouseMotionCaster):MouseMotionCasterExt	= new MouseMotionCasterExt(caster)
	final class MouseMotionCasterExt(caster:MouseMotionCaster) extends Caster(caster.addMouseMotionListener, caster.removeMouseMotionListener, mkMouseMotionListener) {
		def onMouseDragged(callback:MouseEvent=>Unit):Disposer	= listen(on_mouseDragged(callback))
		def onMouseMoved(callback:MouseEvent=>Unit):Disposer	= listen(on_mouseMoved(callback))
	}

	type MouseWheelCaster = {
		def addMouseWheelListener(listener:MouseWheelListener):Unit
		def removeMouseWheelListener(listener:MouseWheelListener):Unit
	}
	implicit def generifyMouseWheelCaster(caster:MouseWheelCaster):MouseWheelCasterExt	= new MouseWheelCasterExt(caster)
	final class MouseWheelCasterExt(caster:MouseWheelCaster) extends Caster(caster.addMouseWheelListener, caster.removeMouseWheelListener, mkMouseWheelListener) {
		def onMouseWheelMoved(callback:MouseWheelEvent=>Unit):Disposer	= listen(on_mouseWheelMoved(callback))
	}

	type HierarchyBoundsCaster = {
		def addHierarchyBoundsListener(listener:HierarchyBoundsListener):Unit
		def removeHierarchyBoundsListener(listener:HierarchyBoundsListener):Unit
	}
	implicit def generifyHierarchyBoundsCaster(caster:HierarchyBoundsCaster):HierarchyBoundsCasterExt	= new HierarchyBoundsCasterExt(caster)
	final class HierarchyBoundsCasterExt(caster:HierarchyBoundsCaster) extends Caster(caster.addHierarchyBoundsListener, caster.removeHierarchyBoundsListener, mkHierarchyBoundsListener) {
		def onAncestorMoved(callback:HierarchyEvent=>Unit):Disposer		= listen(on_ancestorMoved(callback))
		def onAncestorResized(callback:HierarchyEvent=>Unit):Disposer	= listen(on_ancestorResized(callback))
	}

	type HierarchyCaster = {
		def addHierarchyListener(listener:HierarchyListener):Unit
		def removeHierarchyListener(listener:HierarchyListener):Unit
	}
	implicit def generifyHierarchyCaster(caster:HierarchyCaster):HierarchyCasterExt	= new HierarchyCasterExt(caster)
	final class HierarchyCasterExt(caster:HierarchyCaster) extends Caster(caster.addHierarchyListener, caster.removeHierarchyListener, mkHierarchyListener) {
		def onHierarchyChanged(callback:HierarchyEvent=>Unit):Disposer	= listen(on_hierarchyChanged(callback))
	}

	type InputMethodCaster = {
		def addInputMethodListener(listener:InputMethodListener):Unit
		def removeInputMethodListener(listener:InputMethodListener):Unit
	}
	implicit def generifyInputMethodCaster(caster:InputMethodCaster):InputMethodCasterExt	= new InputMethodCasterExt(caster)
	final class InputMethodCasterExt(caster:InputMethodCaster) extends Caster(caster.addInputMethodListener, caster.removeInputMethodListener, mkInputMethodListener) {
		def onInputMethodTextChanged(callback:InputMethodEvent=>Unit):Disposer	= listen(on_inputMethodTextChanged(callback))
		def onCaretPositionChanged(callback:InputMethodEvent=>Unit):Disposer	= listen(on_caretPositionChanged(callback))
	}

	type TextCaster = {
		def addTextListener(listener:TextListener):Unit
		def removeTextListener(listener:TextListener):Unit
	}
	implicit def generifyTextCaster(caster:TextCaster):TextCasterExt	= new TextCasterExt(caster)
	final class TextCasterExt(caster:TextCaster) extends Caster(caster.addTextListener, caster.removeTextListener, mkTextListener) {
		def onTextValueChanged(callback:TextEvent=>Unit):Disposer	= listen(on_textValueChanged(callback))
	}

	//==============================================================================
	//## swing

	type CellEditorCaster = {
		def addCellEditorListener(listener:CellEditorListener):Unit
		def removeCellEditorListener(listener:CellEditorListener):Unit
	}
	implicit def generifyCellEditorCaster(caster:CellEditorCaster):CellEditorCasterExt	= new CellEditorCasterExt(caster)
	final class CellEditorCasterExt(caster:CellEditorCaster) extends Caster(caster.addCellEditorListener, caster.removeCellEditorListener, mkCellEditorListener) {
		def onEditingStopped(callback:ChangeEvent=>Unit):Disposer	= listen(on_editingStopped(callback))
		def onEditingCanceled(callback:ChangeEvent=>Unit):Disposer	= listen(on_editingCanceled(callback))
	}

	type InternalFrameCaster = {
		def addInternalFrameListener(listener:InternalFrameListener):Unit
		def removeInternalFrameListener(listener:InternalFrameListener):Unit
	}
	implicit def generifyInternalFrameCaster(caster:InternalFrameCaster):InternalFrameCasterExt	= new InternalFrameCasterExt(caster)
	final class InternalFrameCasterExt(caster:InternalFrameCaster) extends Caster(caster.addInternalFrameListener, caster.removeInternalFrameListener, mkInternalFrameListener) {
		def onInternalFrameOpened(callback:InternalFrameEvent=>Unit):Disposer		= listen(on_internalFrameOpened(callback))
		def onInternalFrameClosing(callback:InternalFrameEvent=>Unit):Disposer		= listen(on_internalFrameClosing(callback))
		def onInternalFrameClosed(callback:InternalFrameEvent=>Unit):Disposer		= listen(on_internalFrameClosed(callback))
		def onInternalFrameIconified(callback:InternalFrameEvent=>Unit):Disposer	= listen(on_internalFrameIconified(callback))
		def onInternalFrameDeiconified(callback:InternalFrameEvent=>Unit):Disposer	= listen(on_internalFrameDeiconified(callback))
		def onInternalFrameActivated(callback:InternalFrameEvent=>Unit):Disposer	= listen(on_internalFrameActivated(callback))
		def onInternalFrameDeactivated(callback:InternalFrameEvent=>Unit):Disposer	= listen(on_internalFrameDeactivated(callback))
	}

	type MenuDragMouseCaster = {
		def addMenuDragMouseListener(listener:MenuDragMouseListener):Unit
		def removeMenuDragMouseListener(listener:MenuDragMouseListener):Unit
	}
	implicit def generifyMenuDragMouseCaster(caster:MenuDragMouseCaster):MenuDragMouseCasterExt	= new MenuDragMouseCasterExt(caster)
	final class MenuDragMouseCasterExt(caster:MenuDragMouseCaster) extends Caster(caster.addMenuDragMouseListener, caster.removeMenuDragMouseListener, mkMenuDragMouseListener) {
		def onMenuDragMouseEntered(callback:MenuDragMouseEvent=>Unit):Disposer	= listen(on_menuDragMouseEntered(callback))
		def onMenuDragMouseExited(callback:MenuDragMouseEvent=>Unit):Disposer	= listen(on_menuDragMouseExited(callback))
		def onMenuDragMouseDragged(callback:MenuDragMouseEvent=>Unit):Disposer	= listen(on_menuDragMouseDragged(callback))
		def onMenuDragMouseReleased(callback:MenuDragMouseEvent=>Unit):Disposer	= listen(on_menuDragMouseReleased(callback))
	}

	type MenuKeyCaster = {
		def addMenuKeyListener(listener:MenuKeyListener):Unit
		def removeMenuKeyListener(listener:MenuKeyListener):Unit
	}
	implicit def generifyMenuKeyCaster(caster:MenuKeyCaster):MenuKeyCasterExt	= new MenuKeyCasterExt(caster)
	final class MenuKeyCasterExt(caster:MenuKeyCaster) extends Caster(caster.addMenuKeyListener, caster.removeMenuKeyListener, mkMenuKeyListener) {
		def onMenuKeyTyped(callback:MenuKeyEvent=>Unit):Disposer	= listen(on_menuKeyTyped(callback))
		def onMenuKeyPressed(callback:MenuKeyEvent=>Unit):Disposer	= listen(on_menuKeyPressed(callback))
		def onMenuKeyReleased(callback:MenuKeyEvent=>Unit):Disposer	= listen(on_menuKeyReleased(callback))
	}

	type RowSorterCaster = {
		def addRowSorterListener(listener:RowSorterListener):Unit
		def removeRowSorterListener(listener:RowSorterListener):Unit
	}
	implicit def generifyRowSorterCaster(caster:RowSorterCaster):RowSorterCasterExt	= new RowSorterCasterExt(caster)
	final class RowSorterCasterExt(caster:RowSorterCaster) extends Caster(caster.addRowSorterListener, caster.removeRowSorterListener, mkRowSorterListener) {
		def onSorterChanged(callback:RowSorterEvent=>Unit):Disposer	= listen(on_sorterChanged(callback))
	}

	type ListDataCaster = {
		def addListDataListener(listener:ListDataListener):Unit
		def removeListDataListener(listener:ListDataListener):Unit
	}
	implicit def generifyListDataCaster(caster:ListDataCaster):ListDataCasterExt	= new ListDataCasterExt(caster)
	final class ListDataCasterExt(caster:ListDataCaster) extends Caster(caster.addListDataListener, caster.removeListDataListener, mkListDataListener) {
		def onIntervalAdded(callback:ListDataEvent=>Unit):Disposer		= listen(on_intervalAdded(callback))
		def onIntervalRemoved(callback:ListDataEvent=>Unit):Disposer	= listen(on_intervalRemoved(callback))
		def onContentsChanged(callback:ListDataEvent=>Unit):Disposer	= listen(on_contentsChanged(callback))
	}

	type ListSelectionCaster = {
		def addListSelectionListener(listener:ListSelectionListener):Unit
		def removeListSelectionListener(listener:ListSelectionListener):Unit
	}
	implicit def generifyListSelectionCaster(caster:ListSelectionCaster):ListSelectionCasterExt	= new ListSelectionCasterExt(caster)
	final class ListSelectionCasterExt(caster:ListSelectionCaster) extends Caster(caster.addListSelectionListener, caster.removeListSelectionListener, mkListSelectionListener) {
		def onValueChanged(callback:ListSelectionEvent=>Unit):Disposer	= listen(on_valueChanged(callback))
	}

	type AncestorCaster = {
		def addAncestorListener(listener:AncestorListener):Unit
		def removeAncestorListener(listener:AncestorListener):Unit
	}
	implicit def generifyAncestorCaster(caster:AncestorCaster):AncestorCasterExt	= new AncestorCasterExt(caster)
	final class AncestorCasterExt(caster:AncestorCaster) extends Caster(caster.addAncestorListener, caster.removeAncestorListener, mkAncestorListener) {
		def onAncestorAdded(callback:AncestorEvent=>Unit):Disposer		= listen(on_ancestorAdded(callback))
		def onAncestorRemoved(callback:AncestorEvent=>Unit):Disposer	= listen(on_ancestorRemoved(callback))
		def onAncestorMoved(callback:AncestorEvent=>Unit):Disposer		= listen(on_ancestorMoved(callback))
	}

	type CaretCaster = {
		def addCaretListener(listener:CaretListener):Unit
		def removeCaretListener(listener:CaretListener):Unit
	}
	implicit def generifyCaretCaster(caster:CaretCaster):CaretCasterExt	= new CaretCasterExt(caster)
	final class CaretCasterExt(caster:CaretCaster) extends Caster(caster.addCaretListener, caster.removeCaretListener, mkCaretListener) {
		def onCaretUpdate(callback:CaretEvent=>Unit):Disposer	= listen(on_caretUpdate(callback))
	}

	type HyperlinkCaster = {
		def addHyperlinkListener(listener:HyperlinkListener):Unit
		def removeHyperlinkListener(listener:HyperlinkListener):Unit
	}
	implicit def generifyHyperlinkCaster(caster:HyperlinkCaster):HyperlinkCasterExt	= new HyperlinkCasterExt(caster)
	final class HyperlinkCasterExt(caster:HyperlinkCaster) extends Caster(caster.addHyperlinkListener, caster.removeHyperlinkListener, mkHyperlinkListener) {
		def onHyperlinkUpdate(callback:HyperlinkEvent=>Unit):Disposer	= listen(on_hyperlinkUpdate(callback))
	}

	type ChangeCaster = {
		def addChangeListener(listener:ChangeListener):Unit
		def removeChangeListener(listener:ChangeListener):Unit
	}
	implicit def generifyChangeCaster(caster:ChangeCaster):ChangeCasterExt	= new ChangeCasterExt(caster)
	final class ChangeCasterExt(caster:ChangeCaster) extends Caster(caster.addChangeListener, caster.removeChangeListener, mkChangeListener) {
		def onStateChanged(callback:ChangeEvent=>Unit):Disposer	= listen(on_stateChanged(callback))
	}

	type DocumentCaster = {
		def addDocumentListener(listener:DocumentListener):Unit
		def removeDocumentListener(listener:DocumentListener):Unit
	}
	implicit def generifyDocumentCaster(caster:DocumentCaster):DocumentCasterExt	= new DocumentCasterExt(caster)
	final class DocumentCasterExt(caster:DocumentCaster) extends Caster(caster.addDocumentListener, caster.removeDocumentListener, mkDocumentListener) {
		def onInsertUpdate(callback:DocumentEvent=>Unit):Disposer	= listen(on_insertUpdate(callback))
		def onRemoveUpdate(callback:DocumentEvent=>Unit):Disposer	= listen(on_removeUpdate(callback))
		def onChangedUpdate(callback:DocumentEvent=>Unit):Disposer	= listen(on_changedUpdate(callback))
	}

	type MenuCaster = {
		def addMenuListener(listener:MenuListener):Unit
		def removeMenuListener(listener:MenuListener):Unit
	}
	implicit def generifyMenuCaster(caster:MenuCaster):MenuCasterExt	= new MenuCasterExt(caster)
	final class MenuCasterExt(caster:MenuCaster) extends Caster(caster.addMenuListener, caster.removeMenuListener, mkMenuListener) {
		def onMenuSelected(callback:MenuEvent=>Unit):Disposer	= listen(on_menuSelected(callback))
		def onMenuDeselected(callback:MenuEvent=>Unit):Disposer	= listen(on_menuDeselected(callback))
		def onMenuCanceled(callback:MenuEvent=>Unit):Disposer	= listen(on_menuCanceled(callback))
	}

	type PopupMenuCaster = {
		def addPopupMenuListener(listener:PopupMenuListener):Unit
		def removePopupMenuListener(listener:PopupMenuListener):Unit
	}
	implicit def generifyPopupMenuCaster(caster:PopupMenuCaster):PopupMenuCasterExt	= new PopupMenuCasterExt(caster)
	final class PopupMenuCasterExt(caster:PopupMenuCaster) extends Caster(caster.addPopupMenuListener, caster.removePopupMenuListener, mkPopupMenuListener) {
		def onPopupMenuWillBecomeVisible(callback:PopupMenuEvent=>Unit):Disposer	= listen(on_popupMenuWillBecomeVisible(callback))
		def onPopupMenuWillBecomeInvisible(callback:PopupMenuEvent=>Unit):Disposer	= listen(on_popupMenuWillBecomeInvisible(callback))
		def onPopupMenuCanceled(callback:PopupMenuEvent=>Unit):Disposer				= listen(on_popupMenuCanceled(callback))
	}

	type TableModelCaster = {
		def addTableModelListener(listener:TableModelListener):Unit
		def removeTableModelListener(listener:TableModelListener):Unit
	}
	implicit def generifyTableModelCaster(caster:TableModelCaster):TableModelCasterExt	= new TableModelCasterExt(caster)
	final class TableModelCasterExt(caster:TableModelCaster) extends Caster(caster.addTableModelListener, caster.removeTableModelListener, mkTableModelListener) {
		def onTableChanged(callback:TableModelEvent=>Unit):Disposer	= listen(on_tableChanged(callback))
	}

	type TableColumnModelCaster = {
		def addTableColumnModelListener(listener:TableColumnModelListener):Unit
		def removeTableColumnModelListener(listener:TableColumnModelListener):Unit
	}

	implicit def generifyTableColumnModelCaster(caster:TableColumnModelCaster):TableColumnModelCasterExt	= new TableColumnModelCasterExt(caster)
	// NOTE a generic mk*Listener does not work because we have multiple event types here
	final class TableColumnModelCasterExt(caster:TableColumnModelCaster) extends Caster(caster.addTableColumnModelListener, caster.removeTableColumnModelListener, sorryNotImplemented[TableColumnModelListener]) {
		def onColumnAdded(callback:TableColumnModelEvent=>Unit):Disposer			= listen(on_columnAdded(callback))
		def onColumnRemoved(callback:TableColumnModelEvent=>Unit):Disposer			= listen(on_columnRemoved(callback))
		def onColumnMoved(callback:TableColumnModelEvent=>Unit):Disposer			= listen(on_columnMoved(callback))
		def onColumnMarginChanged(callback:ChangeEvent=>Unit):Disposer				= listen(on_columnMarginChanged(callback))
		def onColumnSelectionChanged(callback:ListSelectionEvent=>Unit):Disposer	= listen(on_columnSelectionChanged(callback))
	}

	type TreeModelCaster = {
		def addTreeModelListener(listener:TreeModelListener):Unit
		def removeTreeModelListener(listener:TreeModelListener):Unit
	}
	implicit def generifyTreeModelCaster(caster:TreeModelCaster):TreeModelCasterExt	= new TreeModelCasterExt(caster)
	final class TreeModelCasterExt(caster:TreeModelCaster) extends Caster(caster.addTreeModelListener, caster.removeTreeModelListener, mkTreeModelListener) {
		def onTreeNodesChanged(callback:TreeModelEvent=>Unit):Disposer		= listen(on_treeNodesChanged(callback))
		def onTreeNodesInserted(callback:TreeModelEvent=>Unit):Disposer		= listen(on_treeNodesInserted(callback))
		def onTreeNodesRemoved(callback:TreeModelEvent=>Unit):Disposer		= listen(on_treeNodesRemoved(callback))
		def onTreeStructureChanged(callback:TreeModelEvent=>Unit):Disposer	= listen(on_treeStructureChanged(callback))
	}

	type TreeSelectionCaster = {
		def addTreeSelectionListener(listener:TreeSelectionListener):Unit
		def removeTreeSelectionListener(listener:TreeSelectionListener):Unit
	}
	implicit def generifyTreeSelectionCaster(caster:TreeSelectionCaster):TreeSelectionCasterExt	= new TreeSelectionCasterExt(caster)
	final class TreeSelectionCasterExt(caster:TreeSelectionCaster) extends Caster(caster.addTreeSelectionListener, caster.removeTreeSelectionListener, mkTreeSelectionListener) {
		def onValueChanged(callback:TreeSelectionEvent=>Unit):Disposer	= listen(on_valueChanged(callback))
	}

	type TreeWillExpandCaster = {
		def addTreeWillExpandListener(listener:TreeWillExpandListener):Unit
		def removeTreeWillExpandListener(listener:TreeWillExpandListener):Unit
	}
	implicit def generifyTreeWillExpandCaster(caster:TreeWillExpandCaster):TreeWillExpandCasterExt	= new TreeWillExpandCasterExt(caster)
	final class TreeWillExpandCasterExt(caster:TreeWillExpandCaster) extends Caster(caster.addTreeWillExpandListener, caster.removeTreeWillExpandListener, mkTreeWillExpandListener) {
		def onTreeWillExpand(callback:TreeExpansionEvent=>Unit):Disposer	= listen(on_treeWillExpand(callback))
		def onTreeWillCollapse(callback:TreeExpansionEvent=>Unit):Disposer	= listen(on_treeWillCollapse(callback))
	}

	// NOTE TreeExpansionEvent doesn't have an ID
	type TreeExpansionCaster = {
		def addTreeExpansionListener(listener:TreeExpansionListener):Unit
		def removeTreeExpansionListener(listener:TreeExpansionListener):Unit
	}
	implicit def generifyTreeExpansionCaster(caster:TreeExpansionCaster):TreeExpansionCasterExt	= new TreeExpansionCasterExt(caster)
	final class TreeExpansionCasterExt(caster:TreeExpansionCaster) extends Caster(caster.addTreeExpansionListener, caster.removeTreeExpansionListener, mkTreeExpansionListener) {
		def onTreeExpanded(callback:TreeExpansionEvent=>Unit):Disposer	= listen(on_treeExpanded(callback))
		def onTreeCollapsed(callback:TreeExpansionEvent=>Unit):Disposer	= listen(on_treeCollapsed(callback))
	}

	type UndoableEditCaster = {
		def addUndoableEditListener(listener:UndoableEditListener):Unit
		def removeUndoableEditListener(listener:UndoableEditListener):Unit
	}
	implicit def generifyUndoableEditCaster(caster:UndoableEditCaster):UndoableEditCasterExt	= new UndoableEditCasterExt(caster)
	final class UndoableEditCasterExt(caster:UndoableEditCaster) extends Caster(caster.addUndoableEditListener, caster.removeUndoableEditListener, mkUndoableEditListener) {
		def onUndoableEditHappened(callback:UndoableEditEvent=>Unit):Disposer	= listen(on_undoableEditHappened(callback))
	}

	//==============================================================================

	private def sorryNotImplemented[T](a:Any):T	= sys error "not implemented"
}
