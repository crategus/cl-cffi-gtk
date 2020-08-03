;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-pango.asd
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

(defsystem :cl-cffi-gtk-pango
  :name :cl-cffi-gtk-pango
  :version "1.32.5"                              ; Version of Pango Library
  :author "Dieter Kaiser"
  :license "LLGPL"
  :description "A Lisp binding to Pango"
  :serial t
  :components ((:file "pango.package")
               (:file "pango.init")
               (:file "pango.version")
               (:file "pango.attributes")
               (:file "pango.tab-array")
               (:file "pango.fonts")
               (:file "pango.layout")
               (:file "pango.script")
               (:file "pango.bidirectional")
               (:file "pango.renderer")
               (:file "pango.context")
               (:file "pango.cairo-render")
               (:file "pango.glyph")
              )
  :depends-on (:cl-cffi-gtk-glib
               :cl-cffi-gtk-gobject
               :cl-cffi-gtk-cairo))

;;; --- End of file cl-cffi-gtk-pango.asd --------------------------------------
