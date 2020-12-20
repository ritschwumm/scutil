package scutil.resource

import scutil.resource.extension._

object extensions extends extensions

trait extensions
	extends	ClassResourceImplicits
	with	ClassLoaderResourceImplicits
