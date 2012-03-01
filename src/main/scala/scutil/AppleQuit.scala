package scutil

import java.lang.reflect.{Proxy,Method,InvocationHandler}

import scutil.log._

object AppleQuit extends Logging {
	/** tries to install an com.apple.mrj.MRJQuitHandler */
	def install(task: => Unit) {
		try {
			val handlerClass	= Class forName "com.apple.mrj.MRJQuitHandler"
			val adapterInstance	= Proxy newProxyInstance (
					this.getClass.getClassLoader,
					Array(handlerClass), 
					new InvocationHandler {
						override def invoke(proxy:Object, method:Method, args:Array[Object]):Object = {
							INFO("apple quit handler executing")
							task
							null
						}
					})
					
			val utilsClass		= Class forName "com.apple.mrj.MRJApplicationUtils"
			val registerMethod	= utilsClass getMethod ("registerQuitHandler", Array(handlerClass):_*)
			registerMethod invoke (null, Array[Object](adapterInstance):_*)
			INFO("apple quit handler installed")
		}
		catch {
			// not an apple? no problem ;)
			case e:ClassNotFoundException	=> INFO("apple quithandler not installed")
			case e:Exception				=> ERROR("cannot install apple quithandler", e)
		}
	}
}
