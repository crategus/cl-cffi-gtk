;;; ----------------------------------------------------------------------------
;;; cl-gtk-pango.asd
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.30.0. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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

(defpackage :pango
  (:use :cl :iter :cffi :gobject :glib))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;;; ----------------------------------------------------------------------------

(setf (documentation (find-package :pango) t)
 "Pango is a text layout and shaping library. Pango facilitates the
  layout and shaping of multi-language text. Full-function rendering of text and
  cross-platform support is had when Pango is used with platform APIs or
  3rd party libraries, such as Uniscribe and FreeType, as text rendering
  backends. Pango-processed text will appear similar under different operating
  systems.
  This is the API documentation of a Lisp binding to Pango. ")

;;; --- End of file pango.package.lisp -----------------------------------------
