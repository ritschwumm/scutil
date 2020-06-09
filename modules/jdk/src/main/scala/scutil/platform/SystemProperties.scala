package scutil.platform

/** mirrors important system properties */
object SystemProperties {
	object file {
		// NOTE this one is not guaranteed to exist
		def encoding			= System getProperty "file.encoding"
		def separator			= System getProperty "file.separator"
	}

	object path {
		def separator			= System getProperty "path.separator"
	}

	object line {
		def separator			= System getProperty "line.separator"
	}

	object user {
		def dir					= System getProperty "user.dir"
		def home				= System getProperty "user.home"
		def name				= System getProperty "user.name"

		def country				= System getProperty "user.country"
		def language			= System getProperty "user.language"
		def variant				= System getProperty "user.variant"
		def timezone			= System getProperty "user.timezone"
		//def region			= System getProperty "user.region"
	}

	object java {
		def home				= System getProperty "java.home"
		def vendor				= System getProperty "java.vendor"
		def vendorUrl			= System getProperty "java.vendor.url"
		def version				= System getProperty "java.version"
		def compiler			= System getProperty "java.compiler"

		object io {
			def tmpdir			= System getProperty "java.io.tmpdir"
		}

		object library {
			def path			= System getProperty "java.library.path"
		}

		object clazz {
			def path			= System getProperty "java.class.path"
			def version			= System getProperty "java.class.version"
		}

		object ext {
			def dirs			= System getProperty "java.ext.dirs"
		}

		object specification {
			def version			= System getProperty "java.specification.version"
			def vendor			= System getProperty "java.specification.vendor"
			def name			= System getProperty "java.specification.name"
		}

		object vm {
			def version			= System getProperty "java.vm.version"
			def vendor			= System getProperty "java.vm.vendor"
			def name			= System getProperty "java.vm.name"

			object specification {
				def version		= System getProperty "java.vm.specification.version"
				def vendor		= System getProperty "java.vm.specification.vendor"
				def name		= System getProperty "java.vm.specification.name"
			}
		}
	}

	object os {
		def arch				= System getProperty "os.arch"
		def name				= System getProperty "os.name"
		def version				= System getProperty "os.version"
	}
}
