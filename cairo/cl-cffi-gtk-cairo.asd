;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-cairo.asd
;;;
;;; Copyright (C) 2012, 2013, 2014 Dieter Kaiser
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

(defsystem :cl-cffi-gtk-cairo
  :name :cl-cffi-gtk-cairo
  :version "1.12.14"                           ; Version of the Cairo Library
  :author  "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "cairo.package")
               (:file "cairo.init")
               ;; Utilities
               (:file "cairo.version")
               (:file "cairo.matrix")          ; Generic matrix operations
               (:file "cairo.status")          ; Decoding cairo's status
               (:file "cairo.types")           ; Generic data types

               (:file "cairo.regions")
               (:file "cairo.device")          ; interface to rendering system
               (:file "cairo.surface")
               (:file "cairo.image-surface")   ; Rendering to memory buffers
               (:file "cairo.png-surface")     ; Reading and writing PNG images
               (:file "cairo.pattern")         ; Sources for drawing
               (:file "cairo.context")
               (:file "cairo.transformations") ; Transformations
               (:file "cairo.paths")
               (:file "cairo.scaled-font")     ; Font face at particular size
               (:file "cairo.font-options")    ; How a font should be rendered
               (:file "cairo.text")            ; Rendering text and glyphs
              )
  :depends-on (:cffi
               :cl-cffi-gtk-glib
               :iterate))

;;; --- End of file cl-cffi-gtk-cairo.asd --------------------------------------
