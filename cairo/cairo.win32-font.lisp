;;; ----------------------------------------------------------------------------
;;; cairo.win32-font.lisp
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
;;; Win32 Fonts
;;;
;;;     Font support for Microsoft Windows
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_WIN32_FONT
;;;
;;; Functions
;;;
;;;     cairo_win32_font_face_create_for_logfontw
;;;     cairo_win32_font_face_create_for_hfont
;;;     cairo_win32_font_face_create_for_logfontw_hfont
;;;     cairo_win32_scaled_font_select_font
;;;     cairo_win32_scaled_font_done_font
;;;     cairo_win32_scaled_font_get_metrics_factor
;;;     cairo_win32_scaled_font_get_logical_to_device
;;;     cairo_win32_scaled_font_get_device_to_logical
;;;
;;; Description
;;;
;;; The Microsoft Windows font backend is primarily used to render text on
;;; Microsoft Windows systems.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_WIN32_FONT
;;;
;;; #define CAIRO_HAS_WIN32_FONT 1
;;;
;;; Defined if the Microsoft Windows font backend is available. This macro can
;;; be used to conditionally compile backend-specific code.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_font_face_create_for_logfontw ()
;;;
;;; cairo_font_face_t *
;;; cairo_win32_font_face_create_for_logfontw (LOGFONTW *logfont);
;;;
;;; Creates a new font for the Win32 font backend based on a LOGFONT. This font
;;; can then be used with cairo_set_font_face() or cairo_scaled_font_create().
;;; The cairo_scaled_font_t returned from cairo_scaled_font_create() is also for
;;; the Win32 backend and can be used with functions such as
;;; cairo_win32_scaled_font_select_font().
;;;
;;; logfont :
;;;     A LOGFONTW structure specifying the font to use. The lfHeight, lfWidth,
;;;     lfOrientation and lfEscapement fields of this structure are ignored.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;cairo_win32_font_face_create_for_hfont ()
;;;
;;; cairo_font_face_t *
;;; cairo_win32_font_face_create_for_hfont (HFONT font);
;;;
;;; Creates a new font for the Win32 font backend based on a HFONT. This font
;;; can then be used with cairo_set_font_face() or cairo_scaled_font_create().
;;; The cairo_scaled_font_t returned from cairo_scaled_font_create() is also
;;; for the Win32 backend and can be used with functions such as
;;; cairo_win32_scaled_font_select_font().
;;;
;;; font :
;;;     An HFONT structure specifying the font to use.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_font_face_create_for_logfontw_hfont ()
;;;
;;; cairo_font_face_t *
;;; cairo_win32_font_face_create_for_logfontw_hfont (LOGFONTW *logfont,
;;;                                                  HFONT font);
;;;
;;; Creates a new font for the Win32 font backend based on a LOGFONT. This font
;;; can then be used with cairo_set_font_face() or cairo_scaled_font_create().
;;; The cairo_scaled_font_t returned from cairo_scaled_font_create() is also
;;; for the Win32 backend and can be used with functions such as
;;; cairo_win32_scaled_font_select_font().
;;;
;;; logfont :
;;;     A LOGFONTW structure specifying the font to use. If font is NULL then
;;;     the lfHeight, lfWidth, lfOrientation and lfEscapement fields of this
;;;     structure are ignored. Otherwise lfWidth, lfOrientation and lfEscapement
;;;     must be zero.
;;;
;;; font :
;;;     An HFONT that can be used when the font matrix is a scale by -lfHeight
;;;     and the CTM is identity.
;;;
;;; Returns :
;;;     a newly created cairo_font_face_t. Free with cairo_font_face_destroy()
;;;     when you are done using it.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_scaled_font_select_font ()
;;;
;;; cairo_status_t
;;; cairo_win32_scaled_font_select_font (cairo_scaled_font_t *scaled_font,
;;;                                      HDC hdc);
;;;
;;; Selects the font into the given device context and changes the map mode and
;;; world transformation of the device context to match that of the font. This
;;; function is intended for use when using layout APIs such as Uniscribe to do
;;; text layout with the cairo font. After finishing using the device context,
;;; you must call cairo_win32_scaled_font_done_font() to release any resources
;;; allocated by this function.
;;;
;;; See cairo_win32_scaled_font_get_metrics_factor() for converting logical
;;; coordinates from the device context to font space.
;;;
;;; Normally, calls to SaveDC() and RestoreDC() would be made around the use of
;;; this function to preserve the original graphics state.
;;;
;;; scaled_font :
;;;     A cairo_scaled_font_t from the Win32 font backend. Such an object can
;;;     be created with cairo_win32_font_face_create_for_logfontw().
;;;
;;; hdc :
;;;     a device context
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS if the operation succeeded. otherwise an error
;;;     such as CAIRO_STATUS_NO_MEMORY and the device context is unchanged.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_scaled_font_done_font ()
;;;
;;; void
;;; cairo_win32_scaled_font_done_font (cairo_scaled_font_t *scaled_font);
;;;
;;; Releases any resources allocated by cairo_win32_scaled_font_select_font()
;;;
;;; scaled_font :
;;;     A scaled font from the Win32 font backend.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_scaled_font_get_metrics_factor ()
;;;
;;; double
;;; cairo_win32_scaled_font_get_metrics_factor
;;;                                (cairo_scaled_font_t *scaled_font);
;;;
;;; Gets a scale factor between logical coordinates in the coordinate space used
;;; by cairo_win32_scaled_font_select_font() (that is, the coordinate system
;;; used by the Windows functions to return metrics) and font space coordinates.
;;;
;;; scaled_font :
;;;     a scaled font from the Win32 font backend
;;;
;;; Returns :
;;;     factor to multiply logical units by to get font space coordinates.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_scaled_font_get_logical_to_device ()
;;;
;;; void
;;; cairo_win32_scaled_font_get_logical_to_device
;;;                                (cairo_scaled_font_t *scaled_font,
;;;                                 cairo_matrix_t *logical_to_device);
;;;
;;; Gets the transformation mapping the logical space used by scaled_font to
;;; device space.
;;;
;;; scaled_font :
;;;     a scaled font from the Win32 font backend
;;;
;;; logical_to_device :
;;;     matrix to return
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_scaled_font_get_device_to_logical ()
;;;
;;; void
;;; cairo_win32_scaled_font_get_device_to_logical
;;;                                (cairo_scaled_font_t *scaled_font,
;;;                                 cairo_matrix_t *device_to_logical);
;;;
;;; Gets the transformation mapping device space to the logical space used by
;;; scaled_font .
;;;
;;; scaled_font :
;;;     a scaled font from the Win32 font backend
;;;
;;; device_to_logical :
;;;     matrix to return
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.win32-font.lisp --------------------------------------
