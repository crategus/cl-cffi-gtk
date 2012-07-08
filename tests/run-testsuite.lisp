;;; ----------------------------------------------------------------------------
;;; run-testsuite.lisp
;;;
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

(asdf:operate 'asdf:load-op :lisp-unit)
(asdf:operate 'asdf:load-op :cl-cffi-gtk)

;;; ----------------------------------------------------------------------------

(defpackage :glib-tests
  (:use :glib :cffi :common-lisp :lisp-unit))

(load "rtest-glib.lisp")

(in-package :glib-tests)
(run-all-tests :glib-tests)

;;; ----------------------------------------------------------------------------

(defpackage :gobject-tests
  (:use :gtk :gdk :gobject :glib :cffi :common-lisp :lisp-unit))

(load "rtest-gobject.lisp")
(load "rtest-gobject-type-info.lisp")

(in-package :gobject-tests)
(run-all-tests :gobject-tests)

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

(defpackage :gdk-tests
  (:use :gdk :gobject :glib :cffi :common-lisp :lisp-unit))

(load "rtest-gdk-color.lisp")
(load "rtest-gdk-cursor.lisp")
(load "rtest-gdk-device.lisp")
(load "rtest-gdk-device-manager.lisp")
(load "rtest-gdk-display.lisp")
(load "rtest-gdk-display-manager.lisp")
(load "rtest-gdk-region.lisp")
(load "rtest-gdk-rgba.lisp")
(load "rtest-gdk-screen.lisp")
(load "rtest-gdk-visual.lisp")
(load "rtest-gdk-window.lisp")

(in-package :gdk-tests)
(run-all-tests :gdk-tests)

;;; ----------------------------------------------------------------------------

(defpackage :gtk-tests
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :common-lisp :lisp-unit))

(load "rtest-gtk-adjustment.lisp")
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
(load "rtest-gtk-scale.lisp")
(load "rtest-gtk-scrollbar.lisp")
(load "rtest-gtk-statusbar.lisp")
(load "rtest-gtk-table.lisp")
(load "rtest-gtk-toggle-button.lisp")
(load "rtest-gtk-window.lisp")
(load "rtest-gtk-widget.lisp")

(in-package :gtk-tests)
(run-all-tests :gtk-tests)

;;; --- End of file run-testsuite.lisp -----------------------------------------
