;;; ----------------------------------------------------------------------------
;;; gdk.init.lisp
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

(defpackage :gdk-init
  (:use :cl :cffi :iter))

(in-package :gdk-init)

(glib-init::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gdk
      ((:and :unix (:not :darwin))
       (:or "libgdk-3.so.0" "libgdk-3.so"))
      (:darwin (:or "libgdk-3.0.dylib"
                    "libgdk-3.dylib"
                    "libgdk-x11-3.0.0.dylib"
                    "libgdk-x11-3.0.dylib"))
      (:windows "libgdk-3-0.dll")
      (t "libgdk-3-0")))

  (use-foreign-library gdk))

;; Moved from gtk.package.lisp to this place because we need the version
;; of the GTK library when compiling GDK
(glib-init::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gtk
      ((:and :unix (:not :darwin))
       (:or "libgtk-3.so.0" "libgtk-3.so"))
      (:darwin (:or "libgtk-3.0.dylib"
                    "libgtk-3.dylib"
                    "libgtk-x11-3.0.0.dylib"
                    "libgtk-x11-3.0.dylib"))
      (:windows (:or "libgtk-3-0.dll" "libgtk-win32-2.0-0.dll"))
      (t "libgtk-3-0")))
  (use-foreign-library gtk))

(glib-init::push-library-version-features gdk
    ;; We can not call the Lisp implementations gtk-major-version and
    ;; gtk-minor-version because GTK is not compiled at this time.
    (cffi:foreign-funcall "gtk_get_major_version" :int)
    (cffi:foreign-funcall "gtk_get_minor_version" :int)
    3 4
    3 6
    3 8
    3 10
    3 12
    3 14
    3 16
    3 18
    3 20
    3 22
    3 24
    3 26
    3 28
    3 30)

;;; End of file gdk.init.lisp --------------------------------------------------
