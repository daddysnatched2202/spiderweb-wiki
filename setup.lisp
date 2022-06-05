(in-package :web)

(defmacro set-unless-bound (var val)
  `(if (boundp ',var)
       nil
       (setq ,var ,val)))

(defvar *jquery-source*)
(defvar *jquery-path*)
(defvar *storage-type*)

(set-unless-bound *jquery-source* :web)
(set-unless-bound *jquery-path* "https://code.jquery.com/jquery-3.6.0.min.js")
(set-unless-bound *storage-type* :local)
