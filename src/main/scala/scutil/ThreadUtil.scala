package scutil

import scutil.time._

object ThreadUtil {
	def sleepFor(duration:Duration) {
		Thread sleep duration.millis
	}
}
