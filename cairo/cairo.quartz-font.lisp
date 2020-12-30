;;; ----------------------------------------------------------------------------
;;; cairo.quartz-font.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;;
;;; Quartz (CGFont) Fonts
;;;
;;;     Font support via CGFont on OS X
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_QUARTZ_FONT
;;;
;;; Functions
;;;
;;;     cairo_quartz_font_face_create_for_cgfont
;;;     cairo_quartz_font_face_create_for_atsu_font_id
;;;
;;; Description
;;;
;;; The Quartz font backend is primarily used to render text on Apple MacOS X
;;; systems. The CGFont API is used for the internal implementation of the font
;;; backend methods.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_QUARTZ_FONT
;;;
;;; #define CAIRO_HAS_QUARTZ_FONT 1
;;;
;;; Defined if the Quartz font backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_font_face_create_for_cgfont ()
;;;
;;; cairo_font_face_t *
;;; cairo_quartz_font_face_create_for_cgfont (CGFontRef font);
;;;
;;; Creates a new font for the Quartz font backend based on a CGFontRef. This
;;; font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create().
;;;
;;; font :
;;;     a CGFontRef obtained through a method external to cairo.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_quartz_font_face_create_for_atsu_font_id ()
;;;
;;; cairo_font_face_t *
;;; cairo_quartz_font_face_create_for_atsu_font_id (ATSUFontID font_id);
;;;
;;; Creates a new font for the Quartz font backend based on an ATSUFontID. This
;;; font can then be used with cairo_set_font_face() or
;;; cairo_scaled_font_create().
;;;
;;; font_id :
;;;     an ATSUFontID for the font.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.quartz-font.lisp -------------------------------------
