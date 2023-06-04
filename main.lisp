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
;; along with Spiderweb Wiki. If not, see <https://www.gnu.org/licenses/>.

(in-package :web)

(defvar *handler*)

(defun save-db ()
  (print "Saving…")
  (am:-<> *storage/path*
    (uiop:subdirectories)
    (sort #'< :key #'file-write-date)
    (remove-if-not #λ(cl-ppcre:scan "\d*T\d*" (car (last (pathname-directory _0))))
                   am:<>)
    (loop for dir in am:<>
          for i from 1
          do (if (> i *storage/max-backups*)
                 (uiop:delete-directory-tree dir :validate t))))
  (b.d:snapshot))

(defun load-jquery ()
  (setf *jquery-file*
	(ccase *jquery/source-type*
          ((:web) (multiple-value-bind (data code hash quri res)
                      (drakma:http-request *jquery/source-url*)
                    (declare (ignore code hash quri res))
                    data))
	  ((:local) (uiop:read-file-string *jquery/local-path*))
	  ((:cdn) nil))
	*jquery-hash*
	(if (eq *jquery/source-type* :cdn)
            nil
	    (am:->> *jquery-file*
	      (ironclad:digest-sequence :sha256)
	      (ironclad:byte-array-to-hex-string)))))

(defun run ()
  (if (eq *storage/type* :local)
      (db/load-local *storage/path*)
      (error "Only local storage is supported for now"))
  (when *storage/save-interval*
    (cl-cron:make-cron-job #'save-db
                           :hash-key :save
                           :minute *storage/save-interval*)
    (cl-cron:start-cron))
  (setf *handler* (clack:clackup *app*))
  (load-jquery))

(defun stop ()
  (b.d:snapshot)
  (b.d:close-store)
  (cl-cron:stop-cron)
  (clack:stop *handler*))
