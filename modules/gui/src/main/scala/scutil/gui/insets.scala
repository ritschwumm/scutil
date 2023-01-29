package scutil.gui.insets

import java.awt.Insets

def TopLeft(hgap:Int, vgap:Int):Insets		= new Insets(0,		0,		vgap,	hgap)
def TopCenter(hgap:Int, vgap:Int):Insets	= new Insets(0,		hgap,	vgap,	hgap)
def TopRight(hgap:Int, vgap:Int):Insets		= new Insets(0,		hgap,	vgap,	0)
def CenterLeft(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		vgap,	hgap)
def CenterCenter(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	vgap,	hgap)
def CenterRight(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	vgap,	0)
def BottomLeft(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		0,		hgap)
def BottomCenter(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	0,		hgap)
def BottomRight(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	hgap,	0,		0)
def AloneLeft(hgap:Int, vgap:Int):Insets	= new Insets(0,		0,		0,		hgap)
def AloneCenter(hgap:Int, vgap:Int):Insets	= new Insets(0,		hgap,	0,		hgap)
def AloneRight(hgap:Int, vgap:Int):Insets	= new Insets(0,		hgap,	0,		0)
def TopAlone(hgap:Int, vgap:Int):Insets		= new Insets(0,		0,		vgap,	0)
def CenterAlone(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		vgap,	0)
def BottomAlone(hgap:Int, vgap:Int):Insets	= new Insets(vgap,	0,		0,		0)
def AloneAlone(hgap:Int, vgap:Int):Insets	= new Insets(0,		0,		0,		0)
