(in-package :web)

(defmacro set-unless-bound (var val)
  `(if (boundp ',var)
       nil
       (setq ,var ,val)))

(defvar *jquery/source-type*)
(defvar *jquery/source-url*)
(defvar *jquery/local-path*)
(defvar *storage/type*)
(defvar *storage/path)
(defvar *storage/save-interval*)

(set-unless-bound *jquery/source-type* :web)
(set-unless-bound *jquery/source-url* "https://code.jquery.com/jquery-3.6.0.min.js")
(set-unless-bound *storage/type* :local)
(set-unless-bound *storage/save-interval* 5)
