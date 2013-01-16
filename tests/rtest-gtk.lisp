;;; ----------------------------------------------------------------------------
;;; rtest-gtk.lisp
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

(defpackage :gtk-tests
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo
   :cffi :common-lisp :lisp-unit))

(in-package :gtk-tests)

(load "rtest-gtk-adjustment.lisp")
(load "rtest-gtk-application.lisp")
(load "rtest-gtk-border.lisp")
(load "rtest-gtk-box.lisp")
(load "rtest-gtk-button.lisp")
(load "rtest-gtk-button-box.lisp")
(load "rtest-gtk-cell-renderer-progress.lisp")
(load "rtest-gtk-check-button.lisp")
(load "rtest-gtk-grid.lisp")
(load "rtest-gtk-label.lisp")
(load "rtest-gtk-progress-bar.lisp")
(load "rtest-gtk-radio-button.lisp")
(load "rtest-gtk-range.lisp")
(load "rtest-gtk-settings.lisp")
(load "rtest-gtk-scale.lisp")
(load "rtest-gtk-scrollbar.lisp")
(load "rtest-gtk-statusbar.lisp")
(load "rtest-gtk-table.lisp")
(load "rtest-gtk-toggle-button.lisp")
(load "rtest-gtk-tree-model.lisp")
(load "rtest-gtk-window.lisp")
(load "rtest-gtk-widget.lisp")

(format t "~&-----------------------------------------------------------------")
(run-all-tests :gtk-tests)

