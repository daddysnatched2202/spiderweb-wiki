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
;; along with Spiderweb Wiki. If not, see <https://www.gnu.org/licenses/>.

(in-package :web)

(defvar *handler*)

(defun load-jquery ()
  (setf *jquery-file*
	(cond ((eq *jquery-source* :web)
	       (multiple-value-bind (data code hash quri res)
		   (dex:get "https://code.jquery.com/jquery-3.6.0.min.js")
		 (declare (ignore code hash quri res))
		 data))
	      ((eq *jquery-source* :local)
	       (uiop:read-file-string *jquery-path*))
	      (t (error "Jquery source must be either `:local` or `:web`, it is in ~
                        fact `~a`"
			*jquery-source*)))
	*jquery-hash* (am:->> *jquery-file*
			(babel:string-to-octets)
			(ironclad:digest-sequence :sha256)
			(ironclad:byte-array-to-hex-string))))

(defun run ()
  (if (eq *storage-type* :local)
      (db/load-local (make-rel-path "datastore/"))
      (error "Only local storage is supported for now"))
  (setf *handler* (clack:clackup *app*))
  (load-jquery))

(defun stop ()
  (b.d:close-store)
  (clack:stop *handler*))
