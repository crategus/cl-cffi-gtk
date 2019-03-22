;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.init.lisp
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

(in-package :gdk-pixbuf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :gdk-pixbuf *features*))

(glib-init::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gdk-pixbuf
      ((:and :unix (:not :darwin))
       (:or "libgdk_pixbuf-2.0.so.0" "libgdk_pixbuf-2.0.so"))
      (:darwin (:or "libgdk_pixbuf-2.0.0.dylib" "libgdk_pixbuf-2.0.dylib"))
      (:windows (:or "libgdk_pixbuf-win32-2.0-0" "libgdk_pixbuf-2.0-0.dll"))
      (t "libgdk_pixbuf-2.0")))

  (use-foreign-library gdk-pixbuf))

;;; End of file gdk-pixbuf.init.lisp -------------------------------------------
