;;; ----------------------------------------------------------------------------
;;; rtest-gdk.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

#-lisp-unit
(progn
  (format t "~&Loading of package lisp-unit ...~%")
  (asdf:operate 'asdf:load-op :lisp-unit)
  (format t "Loading of package lisp-unit is finished.~%"))

#-gtk
(progn
  (format t "~&Loading of package cl-cffi-gtk ...~%")
  (asdf:operate 'asdf:load-op :cl-cffi-gtk)
  (format t "Loading is finished.~%"))

(defpackage :gdk-tests
  (:use :gdk :gdk-pixbuf :gobject :glib :cffi :common-lisp :lisp-unit))

(in-package :gdk-tests)

(load "rtest-gdk-color.lisp")
(load "rtest-gdk-cursor.lisp")
(load "rtest-gdk-device.lisp")
(load "rtest-gdk-device-manager.lisp")
(load "rtest-gdk-display.lisp")
(load "rtest-gdk-display-manager.lisp")
(load "rtest-gdk-keymap.lisp")
(load "rtest-gdk-region.lisp")
(load "rtest-gdk-rgba.lisp")
(load "rtest-gdk-screen.lisp")
(load "rtest-gdk-visual.lisp")
(load "rtest-gdk-window.lisp")

;;; ----------------------------------------------------------------------------

(format t "~&-----------------------------------------------------------------")
(run-all-tests :gdk-tests)

