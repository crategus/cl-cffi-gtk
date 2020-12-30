;;; ----------------------------------------------------------------------------
;;; cairo.xlib-xrender-surface.lisp
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
;;; XLib-XRender Backend
;;;
;;;     X Window System rendering using XLib and the X Render extension
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_XLIB_XRENDER_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_xlib_surface_create_with_xrender_format
;;;     cairo_xlib_surface_get_xrender_format
;;;
;;; Description
;;;
;;;     The XLib surface is used to render cairo graphics to X Window System
;;;     windows and pixmaps using the XLib and Xrender libraries.
;;;
;;;     Note that the XLib surface automatically takes advantage of X Render
;;;     extension if it is available.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_XLIB_XRENDER_SURFACE
;;;
;;; #define CAIRO_HAS_XLIB_XRENDER_SURFACE 1
;;;
;;; Defined if the XLib/XRender surface functions are available. This macro can
;;; be used to conditionally compile backend-specific code.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_create_with_xrender_format ()
;;;
;;; cairo_surface_t *
;;; cairo_xlib_surface_create_with_xrender_format
;;;                                (Display *dpy,
;;;                                 Drawable drawable,
;;;                                 Screen *screen,
;;;                                 XRenderPictFormat *format,
;;;                                 int width,
;;;                                 int height);
;;;
;;; Creates an Xlib surface that draws to the given drawable. The way that
;;; colors are represented in the drawable is specified by the provided picture
;;; format.
;;;
;;; Note: If drawable is a Window, then the function
;;; cairo_xlib_surface_set_size() must be called whenever the size of the
;;; window changes.
;;;
;;; dpy :
;;;     an X Display
;;;
;;; drawable :
;;;     an X Drawable, (a Pixmap or a Window)
;;;
;;; screen :
;;;     the X Screen associated with drawable
;;;
;;; format :
;;;     the picture format to use for drawing to drawable . The depth of format
;;;     must match the depth of the drawable.
;;;
;;; width :
;;;     the current width of drawable .
;;;
;;; height :
;;;     the current height of drawable .
;;;
;;; Returns :
;;;     the newly created surface
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_xrender_format ()
;;;
;;; XRenderPictFormat *
;;; cairo_xlib_surface_get_xrender_format (cairo_surface_t *surface);
;;;
;;; Gets the X Render picture format that surface uses for rendering with the X
;;; Render extension. If the surface was created by
;;; cairo_xlib_surface_create_with_xrender_format() originally, the return
;;; value is the format passed to that constructor.
;;;
;;; surface :
;;;     an xlib surface
;;;
;;; Returns :
;;;     the XRenderPictFormat* associated with surface , or NULL if the surface
;;;     is not an xlib surface or if the X Render extension is not available.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.xlib-xrender-surface.lisp ----------------------------
