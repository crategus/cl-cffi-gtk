;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-gobject.asd
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

(defsystem :cl-cffi-gtk-gobject
  :name :cl-cffi-gtk-gobject
  :version "0.0.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "gobject.package")
               (:file "gobject.init")
               (:file "gobject.object-function")
               (:file "gobject.glib-defcallback")
               (:file "gobject.generating")
               
               (:file "gobject.type-info")   ; Type Information
               (:file "gobject.g-value")     ; Generic Values
               (:file "gobject.enumeration") ; Enumeration and flag types
               (:file "gobject.boxed-lisp")
               (:file "gobject.boxed")       ; Boxed Types
               (:file "glib.variant-type")   ; GVariant type system
               (:file "glib.variant")        ; Strongly typed value datatype
               (:file "gobject.param-spec")  ; GParamSpec
               (:file "gobject.param")       ; Parameters and Values
               (:file "gobject.gobject-class")
               (:file "gobject.base")        ; The Base Object Type
               (:file "gobject.closures")    ; Closures
               (:file "gobject.signals")     ; Signals
               
               (:file "gobject.foreign-gobject-subclassing")
               (:file "gobject.utils")
               )
  :depends-on (:cl-cffi-gtk-glib
               :cffi
               :trivial-garbage
               :iterate
               :bordeaux-threads
               :closer-mop))

;;; --- End of file cl-cffi-gtk-gobject.asd ------------------------------------
