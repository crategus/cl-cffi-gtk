;;; ----------------------------------------------------------------------------
;;; pango.init.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :pango)

(glib-init::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library pango
      ((:and :unix (:not :darwin)) "libpango-1.0.so.0")
      (:darwin (:or "libpango-1.0.0.dylib" "libpango-1.0.dylib"))
      (:windows "libpango-1.0-0.dll")
      (t (:default "libgpango-1.0"))))

  (use-foreign-library pango))

(glib-init::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library pangocairo
      ((:and :unix (:not :darwin)) "libpangocairo-1.0.so.0")
      (:darwin (:or "libpangocairo-1.0.0.dylib" "libpangocairo-1.0.dylib"))
      (:windows "libpangocairo-1.0-0.dll")
      (t (:default "libgpangocairo-1.0"))))

  (use-foreign-library pangocairo))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :pango *features*))

(defparameter +pango-version+ (foreign-funcall "pango_version" :int))
(defparameter +pango-major-version+
              (truncate (/ (foreign-funcall "pango_version" :int) 10000)))
(defparameter +pango-minor-version+
              (- (truncate (/ (foreign-funcall "pango_version" :int) 100))
                 (* (truncate (/ (foreign-funcall "pango_version" :int) 10000))
                    100)))

(glib-init::push-library-version-features
  pango
  +pango-major-version+ +pango-minor-version+
  1 38  ; Since 21.09.2015
  1 40  ;       22.03.2016
  1 42  ;       12.03.2018
  1 44  ;       27.07.2019
  1 46  ;       10.08.2020
  1 48  ;       08.11.2020
)

(glib-init::require-library-version
  "Pango"
  1 38  ; Since 21.09.2015
  +pango-major-version+
  +pango-minor-version+)

;;; --- End of file pango.init.lisp --------------------------------------------
