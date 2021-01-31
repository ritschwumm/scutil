package scutil.classpath

import scutil.classpath.extension._

object extensions extends extensions

trait extensions
	extends	ClassClasspathImplicits
	with	ClassLoaderClasspathImplicits
