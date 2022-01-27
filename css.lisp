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

(in-package :web)

;;; TODO: have a system where different themes can be slotted in and out
(defmacro css/with-nord-palette (&body body)
  `(let ((nord0  "#2E3440")
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

(defun css/std ()
  (css/with-nord-palette
   (lass:compile-and-write `(footer :position absolute
				    :bottom 0px
				    :left 0px
				    :height 3em
				    :width 100%
				    :background-color ,nord1
				    (a :bottom -15px
				       :margin 5px
				       :position relative))
			   `(body :background-color ,nord0
				  :color ,nord5)
			   `(p :color ,nord5)
			   `(a :background-color ,nord2
			       :color ,nord5
			       :text-decoration none
			       :padding 4px)
			   `("a:hover" :background-color ,nord3))))