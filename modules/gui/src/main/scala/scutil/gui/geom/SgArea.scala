package scutil.gui.geom

import java.awt.Shape
import java.awt.geom.Area

object SgArea {
	//------------------------------------------------------------------------------
	//## awt conversion

	def fromAwtShape(shape:Shape):SgArea	= unsafeFromAwtArea(new Area(shape))

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def fromAwtArea(it:Area):SgArea	= unsafeFromAwtArea(it.clone.asInstanceOf[Area])
	def toAwtArea(it:SgArea):Area	= it.toAwtArea

	def unsafeFromAwtArea(delegate:Area):SgArea	= new SgArea(delegate)
}

final case class SgArea private (delegate:Area) {
	def | (that:SgArea):SgArea	= this.add(that)
	def |! (that:SgArea):SgArea	= this.subtract(that)
	def ^ (that:SgArea):SgArea	= this.exclusiveOr(that)
	def & (that:SgArea):SgArea	= this.intersect(that)

	def add(that:SgArea):SgArea			= modify { _.add(that.delegate)			}
	def subtract(that:SgArea):SgArea	= modify { _.subtract(that.delegate)	}
	def exclusiveOr(that:SgArea):SgArea	= modify { _.exclusiveOr(that.delegate)	}
	def intersect(that:SgArea):SgArea	= modify { _.intersect(that.delegate)	}

	//------------------------------------------------------------------------------
	//## awt conversion

	def toAwtArea:Area	= cloneDelegate

	//------------------------------------------------------------------------------
	//## internals

	private def modify(effect:Area=>Unit):SgArea = {
		val	out	= cloneDelegate
		effect(out)
		SgArea.unsafeFromAwtArea(out)
	}

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	private def cloneDelegate:Area	=
		delegate.clone.asInstanceOf[Area]
}
