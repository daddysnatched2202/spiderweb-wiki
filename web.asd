(defsystem :web
  :depends-on (:clack
	       :ningle
	       :str
	       :cl-json
	       :alexandria
	       :bknr.datastore 
	       :ironclad
	       :babel
	       :arrow-macros
	       :parenscript
	       :closer-mop
	       :trivia)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "serial")
	       (:file "endpoints")
	       (:file "database")
	       (:file "main")))
