;;; ----------------------------------------------------------------------------
;;; run-testsuite.lisp
;;;
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

(asdf:load-system :lisp-unit)
(asdf:load-system :cl-cffi-gtk)

;;; ----------------------------------------------------------------------------

(load "rtest-glib.lisp")
(load "rtest-gobject.lisp")

;;; ----------------------------------------------------------------------------

(defpackage :gio-tests
  (:use :gio :gobject :glib :cffi :common-lisp :lisp-unit))

(load "rtest-gio-action.lisp")
(load "rtest-gio-application.lisp")
(load "rtest-gio-simple-action.lisp")
(load "rtest-gio-simple-action-group.lisp")

(in-package :gio-tests)
(run-all-tests :gio-tests)

;;; ----------------------------------------------------------------------------

(load "rtest-gdk.lisp")

;;; ----------------------------------------------------------------------------

(load "rtest-gtk.lisp")

;;; ----------------------------------------------------------------------------

;;; --- End of file run-testsuite.lisp -----------------------------------------
