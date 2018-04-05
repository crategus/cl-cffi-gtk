;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-gdk-pixbuf.asd
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

(defsystem :cl-cffi-gtk-gdk-pixbuf
  :name :cl-cffi-gtk-gdk-pixbuf
  :version "2.28.0"                           ; Version of GdK Pixbuf Library
  :author "Dieter Kaiser"
  :license "LLGPL"
  :description "A Lisp binding to GDK Pixbuf 2"
  :serial t
  :components ((:file "gdk-pixbuf.package")
               (:file "gdk-pixbuf.init")
               (:file "gdk-pixbuf.version")   ; Library version numbers
               (:file "gdk-pixbuf.structure") ; Implementation of Pixbuf
               (:file "gdk-pixbuf.load")      ; Loading a Pixbuf
               (:file "gdk-pixbuf.save")      ; Saving a Pixbuf
               (:file "gdk-pixbuf.memory")    ; Image Data in Memory
               (:file "gdk-pixbuf.scaling")   ; Scaling pixbufs
               (:file "gdk-pixbuf.utilities") ; Utility functions
               (:file "gdk-pixbuf.animation") ; Animated images
               (:file "gdk-pixbuf.loader")    ; Progressive image loading
               (:file "gdk-pixbuf.interface") ; Extending GdkPixBuf
              )
  :depends-on (:cl-cffi-gtk-gobject
               :cl-cffi-gtk-glib
               :cffi))

;;; --- End of file cl-cffi-gtk-gdk-pixbuf.asd ---------------------------------
