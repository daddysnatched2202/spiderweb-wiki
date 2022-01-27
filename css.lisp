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
