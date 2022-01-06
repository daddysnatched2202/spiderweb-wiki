;; Copyright 2021, 2022 Curtis Klassen
;; This file is part of Spiderweb Wiki.

;; Spiderweb Wiki is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Spiderweb Wiki is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Spiderweb Wiki.  If not, see <https://www.gnu.org/licenses/>.

(asdf:defsystem :web
  :depends-on (:clack
	       :ningle
	       :str
	       :jonathan
	       :alexandria
	       :bknr.datastore 
	       :ironclad
	       :babel
	       :arrow-macros
	       :parenscript
	       :closer-mop
	       :trivia
	       :uiop
	       :spinneret
	       :lass
	       :cl-ppcre)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "serial")
	       (:file "endpoints")
	       (:file "database")
	       (:file "main")))
