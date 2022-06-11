;; Copyright 2022 Curtis Klassen
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

;;; TODO: have a system where different themes can be slotted in and out

(in-package :web)

(defmacro css/with-nord-palette (&body body)
  `(let-bound ((nord0  "#2E3440")
	       (nord1  "#3B4252")
	       (nord2  "#434C5E")
	       (nord3  "#4C566A")

	       (nord4  "#D8DEE9")
	       (nord5  "#E5E9F0")
	       (nord6  "#ECEFF4")

	       (nord7  "#8FBCBB")
	       (nord8  "#88C0D0")
	       (nord9  "#81A1C1")
	       (nord10 "#5E81AC")

	       (nord11 "#BF616A")
	       (nord12 "#D08770")
	       (nord13 "#EBCB8B")
	       (nord14 "#A3BE8C")
	       (nord15 "#B48EAD"))
     ,@body))

(defmacro css/nord-list (&rest els)
  (let ((keyw (find-package "KEYWORD")))
    (labels ((nord-sym? (sym)
               (and (symbolp sym)
                    sym
                    (am:->> sym
                      (symbol-name)
                      (str:downcase)
                      (ppcre:scan "^nord[0-9]*$"))))
             (nord-list-dispatch (el)
               (cond ((nord-sym? el)
                      el)
                     ((null el)
                      nil)
                     ((and (listp el)
                           (string= "LS" (symbol-name (car el))))
                      (nord-make-list (cdr el)))
                     ((and (symbolp el)
                           (not (eq (symbol-package el)
                                    keyw)))
                      (list 'quote el))
                     (t el)))
             (nord-make-list (els)
               (cons 'list (mapcar #'nord-list-dispatch els))))
      (list 'css/with-nord-palette
            (nord-make-list els)))))

(defun css/std ()
  (lass:compile-and-write
   (css/nord-list footer :position absolute
			 :bottom 0px
			 :left 0px
			 :height 3em
			 :width 100%
			 :background-color nord1
			 (ls a :bottom -15px
			       :margin 5px
			       :position relative))
   (css/nord-list body :background-color nord0
		       :color nord5
                       :line-height 1.5em)
   (css/nord-list p :color nord5)
   (css/nord-list a :background-color nord2
		    :color nord5
		    :text-decoration none
		    :padding 4px)
   (css/nord-list "a:hover" :background-color nord3)
   (css/nord-list '(:or input textarea)
                  :background-color nord2
                  :color nord5
                  :border none
                  :padding 5px
                  :margin 20px)
   (css/nord-list "input[type=submit]:hover"
                  :background-color nord3)
   (css/nord-list ".note-edit-form"
                  :display flex
                  :flex-direction column
                  :width 60%)
   (css/nord-list '(:or ".note-preview-grid" ".note-preview")
                  :padding 1em
                  :background-color nord1
                  (ls p :margin-left 4px))
   (css/nord-list ".note-preview"
                  :margin-top 1em
                  :margin-bottom 1em)
   (css/nord-list '(:or ".note-button-edit" ".note-button-delete")
                  :margin 0.5em)
   (css/nord-list ".note-button-edit:hover"
                  :background-color nord14)
   (css/nord-list ".note-button-delete:hover"
                  :background-color nord11)
   '(.nodes-container
     :display "grid"
     :grid-template-columns auto auto auto auto
     :grid-gap 1em)
   '(.notes-container
     :display "grid"
     :grid-template-columns auto auto
     :grid-gap 1em)
   '(.note-title
     :font-size 2em)
   '(.note-button-bar
     :float "right"
     :position absolute
     :bottom 4em
     :right 0px)))
