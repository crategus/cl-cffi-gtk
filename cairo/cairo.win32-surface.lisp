;;; ----------------------------------------------------------------------------
;;; cairo.win32-surface.lisp
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
;;; Win32 Surfaces
;;;
;;;     Microsoft Windows surface support
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_WIN32_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_win32_surface_create
;;;     cairo_win32_surface_create_with_dib
;;;     cairo_win32_surface_create_with_ddb
;;;     cairo_win32_surface_create_with_format
;;;     cairo_win32_printing_surface_create
;;;     cairo_win32_surface_get_dc
;;;     cairo_win32_surface_get_image
;;;
;;; Description
;;;
;;; The Microsoft Windows surface is used to render cairo graphics to Microsoft
;;; Windows windows, bitmaps, and printing device contexts.
;;;
;;; The surface returned by cairo_win32_printing_surface_create() is of surface
;;; type CAIRO_SURFACE_TYPE_WIN32_PRINTING and is a multi-page vector surface
;;; type.
;;;
;;; The surface returned by the other win32 constructors is of surface type
;;; CAIRO_SURFACE_TYPE_WIN32 and is a raster surface type.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_WIN32_SURFACE
;;;
;;; #define CAIRO_HAS_WIN32_SURFACE 1
;;;
;;; Defined if the Microsoft Windows surface backend is available. This macro
;;; can be used to conditionally compile backend-specific code.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_win32_surface_create (HDC hdc);
;;;
;;; Creates a cairo surface that targets the given DC. The DC will be queried
;;; for its initial clip extents, and this will be used as the size of the cairo
;;; surface. The resulting surface will always be of format CAIRO_FORMAT_RGB24;
;;; should you need another surface format, you will need to create one through
;;; cairo_win32_surface_create_with_format() or
;;; cairo_win32_surface_create_with_dib().
;;;
;;; hdc :
;;;     the DC to create a surface for
;;;
;;; Returns :
;;;     the newly created surface, NULL on failure
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_surface_create_with_dib ()
;;;
;;; cairo_surface_t *
;;; cairo_win32_surface_create_with_dib (cairo_format_t format,
;;;                                      int width,
;;;                                      int height);
;;;
;;; Creates a device-independent-bitmap surface not associated with any
;;; particular existing surface or device context. The created bitmap will be
;;; uninitialized.
;;;
;;; format :
;;;     format of pixels in the surface to create
;;;
;;; width :
;;;     width of the surface, in pixels
;;;
;;; height :
;;;     height of the surface, in pixels
;;;
;;; Returns :
;;;     the newly created surface
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_surface_create_with_ddb ()
;;;
;;; cairo_surface_t *
;;; cairo_win32_surface_create_with_ddb (HDC hdc,
;;;                                      cairo_format_t format,
;;;                                      int width,
;;;                                      int height);
;;;
;;; Creates a device-dependent-bitmap surface not associated with any
;;; particular existing surface or device context. The created bitmap will be
;;; uninitialized.
;;;
;;; hdc :
;;;     a DC compatible with the surface to create
;;;
;;; format :
;;;     format of pixels in the surface to create
;;;
;;; width :
;;;     width of the surface, in pixels
;;;
;;; height :
;;;     height of the surface, in pixels
;;;
;;; Returns :
;;;     the newly created surface
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_surface_create_with_format ()
;;;
;;; cairo_surface_t *
;;; cairo_win32_surface_create_with_format
;;;                                (HDC hdc,
;;;                                 cairo_format_t format);
;;;
;;; Creates a cairo surface that targets the given DC. The DC will be queried
;;; for its initial clip extents, and this will be used as the size of the
;;; cairo surface.
;;;
;;; Supported formats are: CAIRO_FORMAT_ARGB32 CAIRO_FORMAT_RGB24
;;;
;;; Note: format only tells cairo how to draw on the surface, not what the
;;; format of the surface is. Namely, cairo does not (and cannot) check that
;;; hdc actually supports alpha-transparency.
;;;
;;; hdc :
;;;     the DC to create a surface for
;;;
;;; format :
;;;     format of pixels in the surface to create
;;;
;;; Returns :
;;;     the newly created surface, NULL on failure
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_printing_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_win32_printing_surface_create (HDC hdc);
;;;
;;; Creates a cairo surface that targets the given DC. The DC will be queried
;;; for its initial clip extents, and this will be used as the size of the
;;; cairo surface. The DC should be a printing DC; antialiasing will be ignored,
;;; and GDI will be used as much as possible to draw to the surface.
;;;
;;; The returned surface will be wrapped using the paginated surface to provide
;;; correct complex rendering behaviour; cairo_surface_show_page() and
;;; associated methods must be used for correct output.
;;;
;;; hdc :
;;;     the DC to create a surface for
;;;
;;; Returns :
;;;     the newly created surface
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_surface_get_dc ()
;;;
;;; HDC
;;; cairo_win32_surface_get_dc (cairo_surface_t *surface);
;;;
;;; Returns the HDC associated with this surface, or NULL if none. Also returns
;;; NULL if the surface is not a win32 surface.
;;;
;;; A call to cairo_surface_flush() is required before using the HDC to ensure
;;; that all pending drawing operations are finished and to restore any
;;; temporary modification cairo has made to its state. A call to
;;; cairo_surface_mark_dirty() is required after the state or the content of
;;; the HDC has been modified.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Returns :
;;;     HDC or NULL if no HDC available.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_win32_surface_get_image ()
;;;
;;; cairo_surface_t *
;;; cairo_win32_surface_get_image (cairo_surface_t *surface);
;;;
;;; Returns a cairo_surface_t image surface that refers to the same bits as the
;;; DIB of the Win32 surface. If the passed-in win32 surface is not a DIB
;;; surface, NULL is returned.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Returns :
;;;     a cairo_surface_t (owned by the win32 cairo_surface_t), or NULL if the
;;;     win32 surface is not a DIB.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.win32-surface.lisp -----------------------------------
