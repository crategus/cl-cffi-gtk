;;; ----------------------------------------------------------------------------
;;; gdk.init.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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

(in-package :gdk)

(glib::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (format t "~&Loading GDK ...~%")
    (define-foreign-library gdk
      ((:and :unix (:not :darwin))
       (:or "libgdk-3.so.0" "libgdk-3.so"))
      (:darwin (:or "libgdk-x11-3.0.0.dylib" "libgdk-x11-3.0.dylib"))
      (:windows "libgdk-3-0.dll")
      (t "libgdk-3-0"))

;    (define-foreign-library gtk
;      ((:and :unix (:not :darwin))
;       (:or "libgtk-3.so.0" "libgtk-3.so"))
;      (:darwin (:or "libgtk-x11-2.0.0.dylib" "libgtk-x11-2.0.dylib"))
;      (:windows (:or "libgtk-3-0.dll" "libgtk-win32-2.0-0.dll"))
;      (t "libgtk-3-0"))
)

  (use-foreign-library gdk))
;  (use-foreign-library gtk))

;;; End of file gdk.init.lisp --------------------------------------------------
