;;; ----------------------------------------------------------------------------
;;; cl-gtk-gdk.asd
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

(defsystem :cl-gtk-gdk
  :name :cl-gtk-gdk
  :version "0.0.0"
  :author "Dr. Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "gdk.package")
               (:file "gdk.init")
               
               (:file "gdk.region")           ; Points, Rectangles and Regions
               (:file "gdk.threads")          ; Using GDK with threads
               (:file "gdk.properties")       ; Manipulate properties on windows
               (:file "gdk.event-structures") ; Data structures for events
               
               (:file "gdk.device")           ; Handling extended input devices
               (:file "gdk.screen")           ; Representing a physical screen
               (:file "gdk.visual")           ; Low-level display information
               (:file "gdk.color")            ; Colormaps and Colors
               (:file "gdk.cursor")           ; Standard and pixmap cursors
               
               (:file "gdk.font")             ; Loading and manipulating fonts
               (:file "gdk.gc")               ; Encapsulate drawing properties
               (:file "gdk.rgb")              ; Renders RGBto a GdkDrawable
               (:file "gdk.drawable")         ; Drawing Primitives
               (:file "gdk.pixmap")           ; Offscreen drawables
               (:file "gdk.display")          ; Controls the keyboard/mouse
               (:file "gdk.manager")          ; Maintains a list GdkDisplays
               (:file "gdk.pixbuf-structure") ; Implementation of PixBuf
               (:file "gdk.pixbuf-file")      ; Loading and saving PixBuf
               (:file "gdk.pixbuf")           ; Gdk functions for PixBuf
               
               (:file "gdk.window")           ; Onscreen display areas
               (:file "gdk.events")           ; Functions for handling events
               (:file "gdk.general")          ; Library initialization
               
               (:file "gdk.key-values")       ; Manipulating keyboard codes
               (:file "gdk.selections")       ; Transfering data
               (:file "gdk.drag-and-drop")    ; Drag and drop handling
               (:file "gdk.app")              ; Notification for applications
               (:file "gdk.pango")            ; Using Pango in GDK
               )
  :depends-on (:cl-gtk-gobject
               :cl-gtk-glib
               :cffi
               :cl-gtk-pango))
