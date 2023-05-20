;; Copyright 2021-2023 Curtis Klassen
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
  :description "Wiki software for highly interconnected information"
  :license "GPL3.0"
  :depends-on (:clack
	       :ningle
	       :str
	       :jonathan
	       :alexandria
	       :anaphora
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
	       :cl-ppcre
	       :3bmd
	       :3bmd-ext-wiki-links
	       :zs3
	       :drakma
               :cl-cron)
  :serial t
  :components ((:file "package")
	       (:file "lambda")
	       (:file "conf")
               (:file "setup")
	       (:file "util")
               (:file "routing")
               (:file "serial")
	       (:file "database")
	       (:file "css")
	       (:file "html")
               (:file "script")
	       (:file "endpoints")
	       (:file "main")))
