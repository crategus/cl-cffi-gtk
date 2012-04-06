;;; ----------------------------------------------------------------------------
;;; pango.lisp
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

(in-package :pango)

(define-g-enum "PangoWrapMode" pango-wrap-mode
  (:export t
   :type-initializer "pango_wrap_mode_get_type")
  (:word 0)
  (:char 1)
  (:word-char 2))

(export 'pango-wrap-mode)

(define-g-enum "PangoEllipsizeMode" pango-ellipsize-mode
  (:export t
   :type-initializer "pango_ellipsize_mode_get_type")
  (:none 0)
  (:start 1)
  (:middle 2)
  (:end 3))

(export 'pango-ellipsize-mode)

(define-g-object-class "PangoLayout" pango-layout
  (:type-initializer "pango_layout_get_type")
  ())

(export 'pango-layout)

(define-g-enum "PangoUnderline" pango-underline
  (:export t
   :type-initializer "pango_underline_get_type")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4))

(export 'pango-underline)

(define-g-enum "PangoDirection" pango-direction
  (:export t
   :type-initializer "pango_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb-ltr 2)
  (:ttb-rtl 3)
  (:weak-ltr 4)
  (:weak-rtl 5)
  (:neutral 6))

(define-g-object-class "PangoRenderer" pango-renderer
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_renderer_get_type")
  nil)

(define-g-object-class "PangoContext" pango-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_context_get_type")
  nil)

(define-g-enum "PangoRenderPart" pango-render-part
  (:export t
   :type-initializer "pango_render_part_get_type")
  (:foreground 0)
  (:background 1)
  (:underline 2)
  (:strikethrough 3))

(define-g-boxed-opaque pango-layout-line "PangoLayoutLine"
  :alloc (error "Use Pango to create PANGO-LAYOUT-LINEs"))

(export (boxed-related-symbols 'pango-layout-line))

(define-g-enum "PangoRenderPart" pango-render-part
  (:export t
   :type-initializer "pango_render_part_get_type")
  (:foreground 0)
  (:background 1)
  (:underline 2)
  (:strikethrough 3))

(defcfun pango_glyph_string_new :pointer)

(define-g-boxed-opaque pango-glyph-string "PangoGlyphString"
  :alloc (pango_glyph_string_new))

(export (boxed-related-symbols 'pango-glyph-string))

(define-g-object-class "PangoFont" pango-font
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_font_get_type")
  nil)

(define-g-boxed-cstruct pango-matrix "PangoMatrix"
  (xx :double :initform 0.0)
  (xy :double :initform 0.0)
  (yx :double :initform 0.0)
  (yy :double :initform 0.0)
  (x0 :double :initform 0.0)
  (y0 :double :initform 0.0))

(export (boxed-related-symbols 'pango-matrix))

(define-g-boxed-opaque pango-layout-line "PangoLayoutLine"
  :alloc (error "You do not create PangoLayoutLine yourself"))

(export (boxed-related-symbols 'pango-layout-line))

;;; --- End of file pango.lisp -------------------------------------------------
