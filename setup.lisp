;; Copyright 2023 Curtis Klassen
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
