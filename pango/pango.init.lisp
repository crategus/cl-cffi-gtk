;;; ----------------------------------------------------------------------------
;;; pango.init.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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

(glib::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library pango
      ((:and :unix (:not :darwin)) "libpango-1.0.so.0")
      (:darwin (:or "libpango-1.0.0.dylib" "libpango-1.0.dylib"))
      (:windows "libpango-1.0-0.dll")
      (t (:default "libgpango-1.0"))))

  (use-foreign-library pango))

(glib::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library pangocairo
      ((:and :unix (:not :darwin)) "libpangocairo-1.0.so.0")
      (:darwin (:or "libpangocairo-1.0.0.dylib" "libpangocairo-1.0.dylib"))
      (:windows "libpangocairo-1.0-0.dll")
      (t (:default "libgpangocairo-1.0"))))

  (use-foreign-library pangocairo))

;;; --- End of file pango.init.lisp --------------------------------------------
