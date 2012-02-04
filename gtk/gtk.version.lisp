;;; ----------------------------------------------------------------------------
;;; gtk.version.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(in-package :gtk)

(defcvar (*gtk-major-version* "gtk_major_version" :read-only t) :uint)
(defcvar (*gtk-minor-version* "gtk_minor_version" :read-only t) :uint)
(defcvar (*gtk-micro-version* "gtk_micro_version" :read-only t) :uint)
(defcvar (*gtk-binary-age* "gtk_binary_age" :read-only t) :uint)
(defcvar (*gtk-interface-age* "gtk_interface_age" :read-only t) :uint)

(defun get-major-version ()
  *gtk-major-version*)

(defun get-minor-version ()
  *gtk-minor-version*)

(defun get-micro-version ()
  *gtk-micro-version*)
  
(defun get-binary-age ()
  *gtk-binary-age*)

(defun get-interface-age ()
  *gtk-interface-age*)

(defcfun (check-version "gtk_check_version") :string
  (required_major :uint)
  (required_minor :uint)
  (required_micro :uint))

(glib:push-library-version-features gtk
                                    *gtk-major-version*
                                    *gtk-minor-version*
  2 2
  2 4
  2 6
  2 8
  2 10
  2 12
  2 14
  2 16
  2 18)

(glib:require-library-version "Gtk+"
                              2 16
                              *gtk-major-version*
                              *gtk-minor-version*)
