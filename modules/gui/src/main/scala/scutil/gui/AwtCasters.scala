package scutil.gui

import scala.annotation.targetName
import scala.reflect.Selectable.reflectiveSelectable

import java.awt.event.*
import java.awt.dnd.*

import scutil.lang.*

import  scutil.gui.AwtAdapters.*

object AwtCasters {
	type DropTargetCaster = {
		def addDropTargetListener(listener:DropTargetListener):Unit
		def removeDropTargetListener(listener:DropTargetListener):Unit
	}

	extension (peer:DropTargetCaster) {
		def dropTargetCaster:Caster[DropTargetListener]	= new Caster(peer.addDropTargetListener, peer.removeDropTargetListener)

		// NOTE we do not have a common event here

		def onDragEnter(callback:DropTargetDragEvent=>Unit):Disposer	=
			dropTargetCaster.listen(new DropTargetAdapter2 {
				override def dragEnter(ev:DropTargetDragEvent):Unit = callback(ev)
			})

		def onDragExit(callback:DropTargetEvent=>Unit):Disposer	=
			dropTargetCaster.listen(new DropTargetAdapter2 {
				override def dragExit(ev:DropTargetEvent):Unit = callback(ev)
			})

		def onDragOver(callback:DropTargetDragEvent=>Unit):Disposer	=
			dropTargetCaster.listen(new DropTargetAdapter2 {
				override def dragOver(ev:DropTargetDragEvent):Unit = callback(ev)
			})

		def onDrop(callback:DropTargetDropEvent=>Unit):Disposer	=
			dropTargetCaster.listen(new DropTargetAdapter2 {
				override def drop(ev:DropTargetDropEvent):Unit = callback(ev)
			})

		def onDropActionChanged(callback:DropTargetDragEvent=>Unit):Disposer	=
			dropTargetCaster.listen(new DropTargetAdapter2 {
				override def dropActionChanged(ev:DropTargetDragEvent):Unit = callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type AdjustmentCaster = {
		def addAdjustmentListener(listener:AdjustmentListener):Unit
		def removeAdjustmentListener(listener:AdjustmentListener):Unit
	}

	extension (peer:AdjustmentCaster) {
		def adjustmentCaster:Caster[AdjustmentListener]	= new Caster(peer.addAdjustmentListener, peer.removeAdjustmentListener)

		// def onAdjustmentEvent(callback:AdjustmentEvent=>Unit):Disposer	= adjustmentCaster.listen(callback(_))

		def onAdjustmentValueChanged(callback:AdjustmentEvent=>Unit):Disposer	= adjustmentCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type WindowCaster = {
		def addWindowListener(listener:WindowListener):Unit
		def removeWindowListener(listener:WindowListener):Unit
	}

	extension (peer:WindowCaster) {
		def windowCaster:Caster[WindowListener]	= new Caster(peer.addWindowListener, peer.removeWindowListener)

		def onWindowEvent(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowListener {
				def windowOpened(ev:WindowEvent):Unit		= callback(ev)
				def windowClosing(ev:WindowEvent):Unit		= callback(ev)
				def windowClosed(ev:WindowEvent):Unit		= callback(ev)
				def windowIconified(ev:WindowEvent):Unit	= callback(ev)
				def windowDeiconified(ev:WindowEvent):Unit	= callback(ev)
				def windowActivated(ev:WindowEvent):Unit	= callback(ev)
				def windowDeactivated(ev:WindowEvent):Unit	= callback(ev)
			})

		def onWindowOpened(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowOpened(ev:WindowEvent):Unit		= callback(ev)
			})

		def onWindowClosing(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowClosing(ev:WindowEvent):Unit		= callback(ev)
			})

		def onWindowClosed(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowClosed(ev:WindowEvent):Unit		= callback(ev)
			})

		def onWindowIconified(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowIconified(ev:WindowEvent):Unit	= callback(ev)
			})

		def onWindowDeiconified(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowDeiconified(ev:WindowEvent):Unit	= callback(ev)
			})

		def onWindowActivated(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowActivated(ev:WindowEvent):Unit	= callback(ev)
			})

		def onWindowDeactivated(callback:WindowEvent=>Unit):Disposer	=
			windowCaster.listen(new WindowAdapter {
				override def windowDeactivated(ev:WindowEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type WindowStateCaster = {
		def addWindowStateListener(listener:WindowStateListener):Unit
		def removeWindowStateListener(listener:WindowStateListener):Unit
	}

	extension (peer:WindowStateCaster) {
		def windowStateCaster:Caster[WindowStateListener]	= new Caster(peer.addWindowStateListener, peer.removeWindowStateListener)

		// def onWindowEvent(callback:WindowEvent=>Unit):Disposer	= windowStateCaster.listen(callback(_))

		def onWindowStateChanged(callback:WindowEvent=>Unit):Disposer	= windowStateCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type WindowFocusCaster = {
		def addWindowFocusListener(listener:WindowFocusListener):Unit
		def removeWindowFocusListener(listener:WindowFocusListener):Unit
	}

	extension (peer:WindowFocusCaster) {
		def windowFocusCaster:Caster[WindowFocusListener]	= new Caster(peer.addWindowFocusListener, peer.removeWindowFocusListener)

		def onWindowFocusEvent(callback:WindowEvent=>Unit):Disposer	=
			windowFocusCaster.listen(new WindowFocusListener {
				def windowGainedFocus(ev:WindowEvent):Unit	= callback(ev)
				def windowLostFocus(ev:WindowEvent):Unit	= callback(ev)
			})

		def onWindowGainedFocus(callback:WindowEvent=>Unit):Disposer	=
			windowFocusCaster.listen(new WindowFocusAdapter {
				override def windowGainedFocus(ev:WindowEvent):Unit	= callback(ev)
			})

		def onWindowLostFocus(callback:WindowEvent=>Unit):Disposer	=
			windowFocusCaster.listen(new WindowFocusAdapter {
				override def windowLostFocus(ev:WindowEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type FocusCaster = {
		def addFocusListener(listener:FocusListener):Unit
		def removeFocusListener(listener:FocusListener):Unit
	}

	extension (peer:FocusCaster) {
		def focusCaster:Caster[FocusListener]	= new Caster(peer.addFocusListener, peer.removeFocusListener)

		def onFocusEvent(callback:FocusEvent=>Unit):Disposer	=
			focusCaster.listen(new FocusListener {
				def focusGained(ev:FocusEvent):Unit	= callback(ev)
				def focusLost(ev:FocusEvent):Unit	= callback(ev)
			})

		def onFocusGained(callback:FocusEvent=>Unit):Disposer	=
			focusCaster.listen(new FocusAdapter {
				override def focusGained(ev:FocusEvent):Unit	= callback(ev)
			})

		def onFocusLost(callback:FocusEvent=>Unit):Disposer	=
			focusCaster.listen(new FocusAdapter {
				override def focusLost(ev:FocusEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type ComponentCaster = {
		def addComponentListener(listener:ComponentListener):Unit
		def removeComponentListener(listener:ComponentListener):Unit
	}

	extension (peer:ComponentCaster) {
		def componentCaster:Caster[ComponentListener]	= new Caster(peer.addComponentListener, peer.removeComponentListener)

		def onComponentEvent(callback:ComponentEvent=>Unit):Disposer	=
			componentCaster.listen(new ComponentListener {
				def componentResized(ev:ComponentEvent):Unit	= callback(ev)
				def componentMoved(ev:ComponentEvent):Unit		= callback(ev)
				def componentShown(ev:ComponentEvent):Unit		= callback(ev)
				def componentHidden(ev:ComponentEvent):Unit		= callback(ev)
			})

		def onComponentResized(callback:ComponentEvent=>Unit):Disposer	=
			componentCaster.listen(new ComponentAdapter {
				override def componentResized(ev:ComponentEvent):Unit	= callback(ev)
			})

		def onComponentMoved(callback:ComponentEvent=>Unit):Disposer	=
			componentCaster.listen(new ComponentAdapter {
				override def componentMoved(ev:ComponentEvent):Unit		= callback(ev)
			})

		def onComponentShown(callback:ComponentEvent=>Unit):Disposer	=
			componentCaster.listen(new ComponentAdapter {
				override def componentShown(ev:ComponentEvent):Unit		= callback(ev)
			})

		def onComponentHidden(callback:ComponentEvent=>Unit):Disposer	=
			componentCaster.listen(new ComponentAdapter {
				override def componentHidden(ev:ComponentEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type ContainerCaster = {
		def addContainerListener(listener:ContainerListener):Unit
		def removeContainerListener(listener:ContainerListener):Unit
	}

	extension (peer:ContainerCaster) {
		def containerCaster:Caster[ContainerListener]	= new Caster(peer.addContainerListener, peer.removeContainerListener)

		def onContainerEvent(callback:ContainerEvent=>Unit):Disposer	=
			containerCaster.listen(new ContainerListener {
				def componentAdded(ev:ContainerEvent):Unit		= callback(ev)
				def componentRemoved(ev:ContainerEvent):Unit	= callback(ev)
			})

		def onComponentAdded(callback:ContainerEvent=>Unit):Disposer	=
			containerCaster.listen(new ContainerAdapter {
				override def componentAdded(ev:ContainerEvent):Unit	= callback(ev)
			})

		def onComponentRemoved(callback:ContainerEvent=>Unit):Disposer	=
			containerCaster.listen(new ContainerAdapter {
				override def componentRemoved(ev:ContainerEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type ItemCaster = {
		def addItemListener(listener:ItemListener):Unit
		def removeItemListener(listener:ItemListener):Unit
	}

	extension (peer:ItemCaster) {
		def itemCaster:Caster[ItemListener]	= new Caster(peer.addItemListener, peer.removeItemListener)

		// def onItemEvent(callback:ItemEvent=>Unit):Disposer	= itemCaster.listen(callback(_))

		def onItemStateChanged(callback:ItemEvent=>Unit):Disposer	= itemCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type ActionCaster = {
		def addActionListener(listener:ActionListener):Unit
		def removeActionListener(listener:ActionListener):Unit
	}

	extension (peer:ActionCaster) {
		def actionCaster:Caster[ActionListener]	= new Caster(peer.addActionListener, peer.removeActionListener)

		// def onActionEvent(callback:ActionEvent=>Unit):Disposer	= actionCaster.listen(callback(_))

		def onActionPerformed(callback:ActionEvent=>Unit):Disposer	= actionCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type KeyCaster = {
		def addKeyListener(listener:KeyListener):Unit
		def removeKeyListener(listener:KeyListener):Unit
	}

	extension (peer:KeyCaster) {
		def keyCaster:Caster[KeyListener]	= new Caster(peer.addKeyListener, peer.removeKeyListener)

		def onKeyEvent(callback:KeyEvent=>Unit):Disposer	=
			keyCaster.listen(new KeyListener {
				def keyTyped(ev:KeyEvent):Unit		= callback(ev)
				def keyPressed(ev:KeyEvent):Unit	= callback(ev)
				def keyReleased(ev:KeyEvent):Unit	= callback(ev)
			})

		def onKeyTyped(callback:KeyEvent=>Unit):Disposer	=
			keyCaster.listen(new KeyAdapter {
				override def keyTyped(ev:KeyEvent):Unit	= callback(ev)
			})

		def onKeyPressed(callback:KeyEvent=>Unit):Disposer	=
			keyCaster.listen(new KeyAdapter {
				override def keyPressed(ev:KeyEvent):Unit	= callback(ev)
			})

		def onKeyReleased(callback:KeyEvent=>Unit):Disposer	=
			keyCaster.listen(new KeyAdapter {
				override def keyReleased(ev:KeyEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type MouseCaster = {
		def addMouseListener(listener:MouseListener):Unit
		def removeMouseListener(listener:MouseListener):Unit
	}

	extension (peer:MouseCaster) {
		def mouseCaster:Caster[MouseListener]	= new Caster(peer.addMouseListener, peer.removeMouseListener)

		def onMouseEvent(callback:MouseEvent=>Unit):Disposer	=
			mouseCaster.listen(new MouseListener {
				def mouseClicked(ev:MouseEvent):Unit	= callback(ev)
				def mousePressed(ev:MouseEvent):Unit	= callback(ev)
				def mouseReleased(ev:MouseEvent):Unit	= callback(ev)
				def mouseEntered(ev:MouseEvent):Unit	= callback(ev)
				def mouseExited(ev:MouseEvent):Unit		= callback(ev)
			})

		def onMouseClicked(callback:MouseEvent=>Unit):Disposer	=
			mouseCaster.listen(new MouseAdapter {
				override def mouseClicked(ev:MouseEvent):Unit	= callback(ev)
			})

		def onMousePressed(callback:MouseEvent=>Unit):Disposer	=
			mouseCaster.listen(new MouseAdapter {
				override def mousePressed(ev:MouseEvent):Unit	= callback(ev)
			})

		def onMouseReleased(callback:MouseEvent=>Unit):Disposer	=
			mouseCaster.listen(new MouseAdapter {
				override def mouseReleased(ev:MouseEvent):Unit	= callback(ev)
			})

		def onMouseEntered(callback:MouseEvent=>Unit):Disposer	=
			mouseCaster.listen(new MouseAdapter {
				override def mouseEntered(ev:MouseEvent):Unit	= callback(ev)
			})


		def onMouseExited(callback:MouseEvent=>Unit):Disposer	=
			mouseCaster.listen(new MouseAdapter {
				override def mouseExited(ev:MouseEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type MouseMotionCaster = {
		def addMouseMotionListener(listener:MouseMotionListener):Unit
		def removeMouseMotionListener(listener:MouseMotionListener):Unit
	}

	extension (peer:MouseMotionCaster) {
		def mouseMotionCaster:Caster[MouseMotionListener]	= new Caster(peer.addMouseMotionListener, peer.removeMouseMotionListener)

		def onMouseMotionEvent(callback:MouseEvent=>Unit):Disposer	=
			mouseMotionCaster.listen(new MouseMotionListener {
				def mouseDragged(ev:MouseEvent):Unit	= callback(ev)
				def mouseMoved(ev:MouseEvent):Unit	= callback(ev)
			})

		def onMouseDragged(callback:MouseEvent=>Unit):Disposer	=
			mouseMotionCaster.listen(new MouseMotionAdapter {
				override def mouseDragged(ev:MouseEvent):Unit	= callback(ev)
			})

		def onMouseMoved(callback:MouseEvent=>Unit):Disposer	=
			mouseMotionCaster.listen(new MouseMotionAdapter {
				override def mouseMoved(ev:MouseEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type MouseWheelCaster = {
		def addMouseWheelListener(listener:MouseWheelListener):Unit
		def removeMouseWheelListener(listener:MouseWheelListener):Unit
	}

	extension (peer:MouseWheelCaster) {
		def mouseWheelCaster:Caster[MouseWheelListener]	= new Caster(peer.addMouseWheelListener, peer.removeMouseWheelListener)

		// def onMouseWheelEvent(callback:MouseWheelEvent=>Unit):Disposer	= mouseWheelCaster.listen(callback(_))

		def onMouseWheelMoved(callback:MouseWheelEvent=>Unit):Disposer	= mouseWheelCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type HierarchyBoundsCaster = {
		def addHierarchyBoundsListener(listener:HierarchyBoundsListener):Unit
		def removeHierarchyBoundsListener(listener:HierarchyBoundsListener):Unit
	}

	extension (peer:HierarchyBoundsCaster) {
		def hierarchyBoundsCaster:Caster[HierarchyBoundsListener]	= new Caster(peer.addHierarchyBoundsListener, peer.removeHierarchyBoundsListener)

		def onHierarchyEvent(callback:HierarchyEvent=>Unit):Disposer	=
			hierarchyBoundsCaster.listen(new HierarchyBoundsListener {
				def ancestorMoved(ev:HierarchyEvent):Unit	= callback(ev)
				def ancestorResized(ev:HierarchyEvent):Unit	= callback(ev)
			})

		@targetName("hierarchyBounds_onAncestorMoved")
		def onAncestorMoved(callback:HierarchyEvent=>Unit):Disposer	=
			hierarchyBoundsCaster.listen(new HierarchyBoundsAdapter {
				override def ancestorMoved(ev:HierarchyEvent):Unit	= callback(ev)
			})

		def onAncestorResized(callback:HierarchyEvent=>Unit):Disposer	=
			hierarchyBoundsCaster.listen(new HierarchyBoundsAdapter {
				override def ancestorResized(ev:HierarchyEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type HierarchyCaster = {
		def addHierarchyListener(listener:HierarchyListener):Unit
		def removeHierarchyListener(listener:HierarchyListener):Unit
	}

	extension (peer:HierarchyCaster) {
		def hierarchyCaster:Caster[HierarchyListener]	= new Caster(peer.addHierarchyListener, peer.removeHierarchyListener)

		// def onHierarchyEvent(callback:HierarchyEvent=>Unit):Disposer	= hierarchyCaster.listen(callback(_))

		def onHierarchyChanged(callback:HierarchyEvent=>Unit):Disposer	= hierarchyCaster.listen(callback(_))
	}

	//-----------------------------------------------------------------------------

	type InputMethodCaster = {
		def addInputMethodListener(listener:InputMethodListener):Unit
		def removeInputMethodListener(listener:InputMethodListener):Unit
	}

	extension (peer:InputMethodCaster) {
		def inputMethodCaster:Caster[InputMethodListener]	= new Caster(peer.addInputMethodListener, peer.removeInputMethodListener)

		def onInputMethodEvent(callback:InputMethodEvent=>Unit):Disposer	=
			inputMethodCaster.listen(new InputMethodListener {
				def inputMethodTextChanged(ev:InputMethodEvent):Unit	= callback(ev)
				def caretPositionChanged(ev:InputMethodEvent):Unit		= callback(ev)
			})

		def onInputMethodTextChanged(callback:InputMethodEvent=>Unit):Disposer	=
			inputMethodCaster.listen(new InputMethodAdapter {
				override def inputMethodTextChanged(ev:InputMethodEvent):Unit	= callback(ev)
			})

		def onCaretPositionChanged(callback:InputMethodEvent=>Unit):Disposer	=
			inputMethodCaster.listen(new InputMethodAdapter {
				override def caretPositionChanged(ev:InputMethodEvent):Unit	= callback(ev)
			})
	}

	//-----------------------------------------------------------------------------

	type TextCaster = {
		def addTextListener(listener:TextListener):Unit
		def removeTextListener(listener:TextListener):Unit
	}

	extension (peer:TextCaster) {
		def textCaster:Caster[TextListener]	= new Caster(peer.addTextListener, peer.removeTextListener)

		// def onTextEvent(callback:TextEvent=>Unit):Disposer	= textCaster.listen(callback(_))

		def onTextValueChanged(callback:TextEvent=>Unit):Disposer	= textCaster.listen(callback(_))
	}
}
