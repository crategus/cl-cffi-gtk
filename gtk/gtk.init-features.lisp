;;; ----------------------------------------------------------------------------
;;; gtk.init-features.lisp
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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

(defpackage :gtk-init
  (:use :glib-init :cl :cffi :iter))

(in-package :gtk-init)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :gtk *features*))

(glib-init::push-library-version-features gtk
    (cffi:foreign-funcall "gtk_get_major_version" :int)
    (cffi:foreign-funcall "gtk_get_minor_version" :int)
    3 10
    3 12
    3 14
    3 16   ; Since 23.03.2015
    3 18   ; Since 22.09.2015
    3 20   ; Since 21.03.2016
    3 22   ; Since 20.09.2016
    3 24   ; Since 03.09.2018
    )

(glib-init::require-library-version "GTK+" 3 16
    (cffi:foreign-funcall "gtk_get_major_version" :int)
    (cffi:foreign-funcall "gtk_get_minor_version" :int))

;;; --- End of file gtk.init-features.lisp -------------------------------------
