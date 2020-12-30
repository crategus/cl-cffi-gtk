;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-cairo.asd
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
  :version "1.16"                              ; Version of the Cairo Library
  :author  "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "cairo.package")
               (:file "cairo.init")

               ;; Utilities
               (:file "cairo.version")         ; Version checks
               (:file "cairo.status")          ; Decoding cairo's status
               (:file "cairo.matrix")          ; Generic matrix operations
               (:file "cairo.types")           ; Generic data types

               ;; Surfaces
               (:file "cairo.device")          ; interface to rendering system
               (:file "cairo.surface")
               (:file "cairo.image-surface")   ; Rendering to memory buffers
               (:file "cairo.png-surface")     ; Reading and writing PNG images
               (:file "cairo.pdf-surface")     ; Rendering PDF documents
               (:file "cairo.ps-surface")      ; Rendering PostScript documents
               (:file "cairo.recording-surface"); Records all drawing operations
               (:file "cairo.win32-surface")   ; Windows surface support
               (:file "cairo.svg-surface")     ; Rendering SVG documents
               (:file "cairo.quartz-surface")  ; Rendering to Quartz surfaces
               (:file "cairo.xcb-surface")     ; Rendering using the XCB library
               (:file "cairo.xlib-surface")    ; Rendering using XLib
               (:file "cairo.xlib-xrender-surface"); using XLib and X Render
               (:file "cairo.script-surface")  ; Rendering to replayable scripts

               ;; Drawing
               (:file "cairo.pattern")         ; Sources for drawing
               (:file "cairo.context")         ; Cairo drawing context
               (:file "cairo.region")          ; Representing pixel area
               (:file "cairo.transformation")  ; Transformations
               (:file "cairo.raster-source")   ; Supplying arbitrary image data
               (:file "cairo.tag")             ; Hyperlinks, document structure

               ;; Fonts
               (:file "cairo.font-option")     ; How a font should be rendered
               (:file "cairo.font-face")       ; Base class for font faces
               (:file "cairo.scaled-font")     ; Font face at particular size

               (:file "cairo.freetype-font")   ; Font support for FreeType
               (:file "cairo.win32-font")      ; Font support for Windows
               (:file "cairo.quartz-font")     ; Font support on OS X
               (:file "cairo.user-font")       ; Font support for user font

               ;; More Drawing
               (:file "cairo.text")            ; Rendering text and glyphs
               (:file "cairo.path")            ; Creating paths


              )
  :depends-on (:cffi
               :cl-cffi-gtk-glib
               :iterate))

;;; --- End of file cl-cffi-gtk-cairo.asd --------------------------------------
