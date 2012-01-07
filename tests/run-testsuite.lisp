;;; ----------------------------------------------------------------------------
;;; run-testsuite.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; ----------------------------------------------------------------------------
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

(load "../lisp-utils/lisp-unit.lisp")

(asdf:operate 'asdf:load-op :cl-gtk-gtk)

(load "rtest-glib.lisp")
(in-package :glib-tests)
(run-all-tests :glib-tests)

(load "rtest-gobject.lisp")
(in-package :gobject-tests)
(run-all-tests :gobject-tests)

(load "rtest-gtk-window.lisp")
(load "rtest-gtk-box.lisp")
(in-package :gtk-tests)
(run-all-tests :gtk-tests)

;;; --- End of file run-testsuite.lisp -----------------------------------------
