package scutil.gui

import java.awt.event._

trait AwtCasterInstances {
	/*
	import java.awt.dnd.DropTargetListener
	
	type DropTargetCaster = {
		def addDropTargetListener(listener:DropTargetListener):Unit
		def removeDropTargetListener(listener:DropTargetListener):Unit
	}
	*/
	
	type AdjustmentCaster = {
		def addAdjustmentListener(listener:AdjustmentListener):Unit
		def removeAdjustmentListener(listener:AdjustmentListener):Unit
	}
	def mkAdjustmentListener(callback:AdjustmentEvent=>Unit):AdjustmentListener = new AdjustmentListener {
		def adjustmentValueChanged(ev:AdjustmentEvent) { callback(ev) }
	}
	implicit def generifyAdjustmentCaster(caster:AdjustmentCaster):Caster[AdjustmentListener,AdjustmentEvent] = Caster(
			caster.addAdjustmentListener,
			caster.removeAdjustmentListener,
			mkAdjustmentListener)
	
	type WindowCaster = {
		def addWindowListener(listener:WindowListener):Unit
		def removeWindowListener(listener:WindowListener):Unit
	}
	def mkWindowListener(callback:WindowEvent=>Unit):WindowListener = new WindowListener {
		def windowOpened(ev:WindowEvent)		{ callback(ev) }
		def windowClosing(ev:WindowEvent)		{ callback(ev) }
		def windowClosed(ev:WindowEvent)		{ callback(ev) }
		def windowIconified(ev:WindowEvent)		{ callback(ev) }
		def windowDeiconified(ev:WindowEvent)	{ callback(ev) }
		def windowActivated(ev:WindowEvent)		{ callback(ev) }
		def windowDeactivated(ev:WindowEvent)	{ callback(ev) }
	}
	implicit def generifyWindowCaster(caster:WindowCaster):Caster[WindowListener,WindowEvent] = Caster(
			caster.addWindowListener,
			caster.removeWindowListener,
			mkWindowListener)

	type WindowStateCaster = {
		def addWindowStateListener(listener:WindowStateListener):Unit
		def removeWindowStateListener(listener:WindowStateListener):Unit
	}
	def mkWindowStateListener(callback:WindowEvent=>Unit):WindowStateListener = new WindowStateListener {
		def windowStateChanged(ev:WindowEvent) { callback(ev) }
	}
	implicit def generifyWindowStateCaster(caster:WindowStateCaster):Caster[WindowStateListener,WindowEvent] = Caster(
			caster.addWindowStateListener,
			caster.removeWindowStateListener,
			mkWindowStateListener)

	type WindowFocusCaster = {
		def addWindowFocusListener(listener:WindowFocusListener):Unit
		def removeWindowFocusListener(listener:WindowFocusListener):Unit
	}
	def mkWindowFocusListener(callback:WindowEvent=>Unit):WindowFocusListener = new WindowFocusListener {
		def windowGainedFocus(ev:WindowEvent)	{ callback(ev) }
		def windowLostFocus(ev:WindowEvent)		{ callback(ev) }
	}
	implicit def generifyWindowFocusCaster(caster:WindowFocusCaster):Caster[WindowFocusListener,WindowEvent] = Caster(
			caster.addWindowFocusListener,
			caster.removeWindowFocusListener,
			mkWindowFocusListener)
	
	type FocusCaster = {
		def addFocusListener(listener:FocusListener):Unit
		def removeFocusListener(listener:FocusListener):Unit
	}
	def mkFocusListener(callback:FocusEvent=>Unit):FocusListener = new FocusListener {
		def focusGained(ev:FocusEvent)	{ callback(ev) }
		def focusLost(ev:FocusEvent)	{ callback(ev) }
	}
	implicit def generifyFocusCaster(caster:FocusCaster):Caster[FocusListener,FocusEvent] = Caster(
			caster.addFocusListener,
			caster.removeFocusListener,
			mkFocusListener)
	
	type ComponentCaster = {
		def addComponentListener(listener:ComponentListener):Unit
		def removeComponentListener(listener:ComponentListener):Unit
	}
	def mkComponentListener(callback:ComponentEvent=>Unit):ComponentListener = new ComponentListener {
		def componentResized(ev:ComponentEvent)	{ callback(ev) }
		def componentMoved(ev:ComponentEvent)	{ callback(ev) }
		def componentShown(ev:ComponentEvent)	{ callback(ev) }
		def componentHidden(ev:ComponentEvent)	{ callback(ev) }
	}
	implicit def generifyComponentCaster(caster:ComponentCaster):Caster[ComponentListener,ComponentEvent] = Caster(
			caster.addComponentListener,
			caster.removeComponentListener,
			mkComponentListener)
	
	type ContainerCaster = {
		def addContainerListener(listener:ContainerListener):Unit
		def removeContainerListener(listener:ContainerListener):Unit
	}
	def mkContainerListener(callback:ContainerEvent=>Unit):ContainerListener = new ContainerListener {
		def componentAdded(ev:ContainerEvent)	{ callback(ev) }
		def componentRemoved(ev:ContainerEvent)	{ callback(ev) }
	}
	implicit def generifyContainerCaster(caster:ContainerCaster):Caster[ContainerListener,ContainerEvent] = Caster(
			caster.addContainerListener,
			caster.removeContainerListener,
			mkContainerListener)
	
	type ItemCaster = {
		def addItemListener(listener:ItemListener):Unit
		def removeItemListener(listener:ItemListener):Unit
	}
	def mkItemListener(callback:ItemEvent=>Unit):ItemListener = new ItemListener {
		def itemStateChanged(ev:ItemEvent) { callback(ev) }
	}
	implicit def generifyItemCaster(caster:ItemCaster):Caster[ItemListener,ItemEvent] = Caster(
			caster.addItemListener,
			caster.removeItemListener,
			mkItemListener)

	type ActionCaster = {
		def addActionListener(listener:ActionListener):Unit
		def removeActionListener(listener:ActionListener):Unit
	}
	def mkActionListener(callback:ActionEvent=>Unit):ActionListener = new ActionListener {
		def actionPerformed(ev:ActionEvent) { callback(ev) }
	}
	implicit def generifyActionCaster(caster:ActionCaster):Caster[ActionListener,ActionEvent] = Caster(
			caster.addActionListener,
			caster.removeActionListener,
			mkActionListener)
	
	type KeyCaster = {
		def addKeyListener(listener:KeyListener):Unit
		def removeKeyListener(listener:KeyListener):Unit
	}
	def mkKeyListener(callback:KeyEvent=>Unit):KeyListener = new KeyListener {
		def keyTyped(ev:KeyEvent)		{ callback(ev) }
		def keyPressed(ev:KeyEvent)		{ callback(ev) }
		def keyReleased(ev:KeyEvent)	{ callback(ev) }
	}
	implicit def generifyKeyCaster(caster:KeyCaster):Caster[KeyListener,KeyEvent] = Caster(
			caster.addKeyListener,
			caster.removeKeyListener,
			mkKeyListener)
	
	type MouseCaster = {
		def addMouseListener(listener:MouseListener):Unit
		def removeMouseListener(listener:MouseListener):Unit
	}
	def mkMouseListener(callback:MouseEvent=>Unit):MouseListener = new MouseListener {
		def mouseClicked(ev:MouseEvent)		{ callback(ev) }
		def mousePressed(ev:MouseEvent)		{ callback(ev) }
		def mouseReleased(ev:MouseEvent)	{ callback(ev) }
		def mouseEntered(ev:MouseEvent)		{ callback(ev) }
		def mouseExited(ev:MouseEvent)		{ callback(ev) }
	}
	implicit def generifyMouseCaster(caster:MouseCaster):Caster[MouseListener,MouseEvent] = Caster(
			caster.addMouseListener,
			caster.removeMouseListener,
			mkMouseListener)

	type MouseMotionCaster = {
		def addMouseMotionListener(listener:MouseMotionListener):Unit
		def removeMouseMotionListener(listener:MouseMotionListener):Unit
	}
	def mkMouseMotionListener(callback:MouseEvent=>Unit):MouseMotionListener = new MouseMotionListener {
		def mouseDragged(ev:MouseEvent)	{ callback(ev) }
		def mouseMoved(ev:MouseEvent)	{ callback(ev) }
	}
	implicit def generifyMouseMotionCaster(caster:MouseMotionCaster):Caster[MouseMotionListener,MouseEvent] = Caster(
			caster.addMouseMotionListener,
			caster.removeMouseMotionListener,
			mkMouseMotionListener)
	
	type MouseWheelCaster = {
		def addMouseWheelListener(listener:MouseWheelListener):Unit
		def removeMouseWheelListener(listener:MouseWheelListener):Unit
	}
	def mkMouseWheelListener(callback:MouseWheelEvent=>Unit):MouseWheelListener = new MouseWheelListener {
		def mouseWheelMoved(ev:MouseWheelEvent) { callback(ev) }
	}
	implicit def generifyMouseWheelCaster(caster:MouseWheelCaster):Caster[MouseWheelListener,MouseWheelEvent] = Caster(
			caster.addMouseWheelListener,
			caster.removeMouseWheelListener,
			mkMouseWheelListener)
	
	type HierarchyBoundsCaster = {
		def addHierarchyBoundsListener(listener:HierarchyBoundsListener):Unit
		def removeHierarchyBoundsListener(listener:HierarchyBoundsListener):Unit
	}
	def mkHierarchyBoundsListener(callback:HierarchyEvent=>Unit):HierarchyBoundsListener = new HierarchyBoundsListener {
		def ancestorMoved(ev:HierarchyEvent)	{ callback(ev) }
		def ancestorResized(ev:HierarchyEvent)	{ callback(ev) }
	}
	implicit def generifyHierarchyBoundsCaster(caster:HierarchyBoundsCaster):Caster[HierarchyBoundsListener,HierarchyEvent] = Caster(
			caster.addHierarchyBoundsListener,
			caster.removeHierarchyBoundsListener,
			mkHierarchyBoundsListener)
	
	type HierarchyCaster = {
		def addHierarchyListener(listener:HierarchyListener):Unit
		def removeHierarchyListener(listener:HierarchyListener):Unit
	}
	def mkHierarchyListener(callback:HierarchyEvent=>Unit):HierarchyListener = new HierarchyListener {
		def hierarchyChanged(ev:HierarchyEvent) { callback(ev) }
	}
	implicit def generifyHierarchyCaster(caster:HierarchyCaster):Caster[HierarchyListener,HierarchyEvent] = Caster(
			caster.addHierarchyListener,
			caster.removeHierarchyListener,
			mkHierarchyListener)
	
	type InputMethodCaster = {
		def addInputMethodListener(listener:InputMethodListener):Unit
		def removeInputMethodListener(listener:InputMethodListener):Unit
	}
	def mkInputMethodListener(callback:InputMethodEvent=>Unit):InputMethodListener = new InputMethodListener {
		def inputMethodTextChanged(ev:InputMethodEvent)	{ callback(ev) }
		def caretPositionChanged(ev:InputMethodEvent)	{ callback(ev) }
	}
	implicit def generifyInputMethodCaster(caster:InputMethodCaster):Caster[InputMethodListener,InputMethodEvent] = Caster(
			caster.addInputMethodListener,
			caster.removeInputMethodListener,
			mkInputMethodListener)
	
	type TextCaster = {
		def addTextListener(listener:TextListener):Unit
		def removeTextListener(listener:TextListener):Unit
	}
	def mkTextListener(callback:TextEvent=>Unit):TextListener = new TextListener {
		def textValueChanged(ev:TextEvent) { callback(ev) }
	}
	implicit def generifyTextCaster(caster:TextCaster):Caster[TextListener,TextEvent] = Caster(
			caster.addTextListener,
			caster.removeTextListener,
			mkTextListener)
}
