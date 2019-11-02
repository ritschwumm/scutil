package scutil.gui

import java.awt.{ Shape }
import java.awt.geom.{ Point2D, AffineTransform, NoninvertibleTransformException }

import scutil.geom._

object SafeAffineTransform {
	//------------------------------------------------------------------------------
	//## factory

	val identity:SafeAffineTransform	= unsafeFromAwtAffineTransform(new AffineTransform)

	def translate(offset:DoublePoint):SafeAffineTransform	=
			unsafeFromAwtAffineTransform(AffineTransform getTranslateInstance	(offset.x,	offset.y))
	def scale(factor:DoublePoint):SafeAffineTransform		=
			unsafeFromAwtAffineTransform(AffineTransform getScaleInstance		(factor.x,	factor.y))
	def shear(factor:DoublePoint):SafeAffineTransform		=
			unsafeFromAwtAffineTransform(AffineTransform getShearInstance		(factor.x,	factor.y))
	def rotate(theta:Double):SafeAffineTransform		=
			unsafeFromAwtAffineTransform(AffineTransform getRotateInstance		theta)
	def rotateAround(theta:Double, center:DoublePoint):SafeAffineTransform	=
			unsafeFromAwtAffineTransform(AffineTransform getRotateInstance	(theta, center.x, center.y))

	def unsafeFromAwtAffineTransform(delegate:AffineTransform):SafeAffineTransform	= new SafeAffineTransform(delegate)

	//------------------------------------------------------------------------------
	// awt

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def fromAwtAffineTransform(it:AffineTransform):SafeAffineTransform	=
			unsafeFromAwtAffineTransform(it.clone.asInstanceOf[AffineTransform])

	def toAwtAffineTransform(it:SafeAffineTransform):AffineTransform	=
			it.toAwtAffineTransform
}

final case class SafeAffineTransform private (delegate:AffineTransform) {
	/** alias for transform */
	def apply(point:DoublePoint):DoublePoint	=
			transform(point)

	def transform(point:DoublePoint):DoublePoint	=
			geomConversion Point2D_DoublePoint (delegate transform (geomConversion DoublePoint_Point2D point, null))

	def transformAwtPoint2D(point:Point2D):Point2D	=
			delegate transform (point, null)

	def transformAwtShape(shape:Shape):Shape	=
			delegate createTransformedShape shape

	/** fast bounds calculation for a transformed rectangle, as long as the transform is orthogonal */
	def transformBounds(rect:DoubleRect):DoubleRect	= {
			 if (isIdentity)	rect
		else if (!isOrthogonal)	geomConversion Rectangle2D_DoubleRect (delegate createTransformedShape (geomConversion DoubleRect_Rectangle2D rect)).getBounds2D
		else {
			val coords:Array[Double]	=
					Array(
						rect.left,
						rect.top,
						rect.right,
						rect.bottom
					)
			delegate transform (coords, 0, coords, 0, 2)
			DoubleRect leftTopRightBottom (
				coords(0),
				coords(1),
				coords(2),
				coords(3)
			)
		}
	}

	def inverse:Option[SafeAffineTransform]	=
			try { Some(SafeAffineTransform.unsafeFromAwtAffineTransform(delegate.createInverse)) }
			catch { case e:NoninvertibleTransformException => None }

	/** rotate around a given center */
	def rotateAround(theta:Double, center:DoublePoint):SafeAffineTransform	=
			modify { _ rotate (theta, center.x, center.y) }

	def translate(offset:DoublePoint):SafeAffineTransform =
			modify { _ translate (offset.x, offset.y) }

	def scale(factor:DoublePoint):SafeAffineTransform =
			modify { _ scale (factor.x, factor.y) }

	def shear(factor:DoublePoint):SafeAffineTransform =
			modify { _ shear (factor.x, factor.y) }

	def rotate(theta:Double):SafeAffineTransform =
			modify { _ rotate theta }

	def andThen(that:SafeAffineTransform):SafeAffineTransform	=
			modify { _ concatenate that.delegate }

	def compose(that:SafeAffineTransform):SafeAffineTransform	=
			that andThen this

	private val orthogonalMask	= AffineTransform.TYPE_MASK_ROTATION | AffineTransform.TYPE_GENERAL_TRANSFORM

	def isOrthogonal:Boolean	= (delegate.getType & orthogonalMask) == 0

	def isIdentity:Boolean		= delegate.isIdentity

	def toAwtAffineTransform:AffineTransform	= cloneDelegate

	private def modify(effect:AffineTransform=>Unit):SafeAffineTransform = {
		val	out	= cloneDelegate
		effect(out)
		SafeAffineTransform.unsafeFromAwtAffineTransform(out)
	}

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	private def cloneDelegate:AffineTransform	=
			delegate.clone.asInstanceOf[AffineTransform]
}
