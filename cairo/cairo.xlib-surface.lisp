;;; ----------------------------------------------------------------------------
;;; cairo.xlib-surface.lisp
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
;;; XLib Surfaces
;;;
;;;     X Window System rendering using XLib
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_XLIB_SURFACE
;;;
;;; Functions
;;;
;;;     cairo_xlib_surface_create
;;;     cairo_xlib_surface_create_for_bitmap
;;;     cairo_xlib_surface_set_size
;;;     cairo_xlib_surface_get_display
;;;     cairo_xlib_surface_get_screen
;;;     cairo_xlib_surface_set_drawable
;;;     cairo_xlib_surface_get_drawable
;;;     cairo_xlib_surface_get_visual
;;;     cairo_xlib_surface_get_width
;;;     cairo_xlib_surface_get_height
;;;     cairo_xlib_surface_get_depth
;;;     cairo_xlib_device_debug_cap_xrender_version
;;;     cairo_xlib_device_debug_get_precision
;;;     cairo_xlib_device_debug_set_precision
;;;
;;; Description
;;;
;;;     The XLib surface is used to render cairo graphics to X Window System
;;;     windows and pixmaps using the XLib library.
;;;
;;;     Note that the XLib surface automatically takes advantage of X render
;;;     extension if it is available.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_XLIB_SURFACE
;;;
;;; #define CAIRO_HAS_XLIB_SURFACE 1
;;;
;;; Defined if the Xlib surface backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_xlib_surface_create (Display *dpy,
;;;                            Drawable drawable,
;;;                            Visual *visual,
;;;                            int width,
;;;                            int height);
;;;
;;; Creates an Xlib surface that draws to the given drawable. The way that
;;; colors are represented in the drawable is specified by the provided visual.
;;;
;;; Note: If drawable is a Window, then the function
;;; cairo_xlib_surface_set_size() must be called whenever the size of the
;;; window changes.
;;;
;;; When drawable is a Window containing child windows then drawing to the
;;; created surface will be clipped by those child windows. When the created
;;; surface is used as a source, the contents of the children will be included.
;;;
;;; dpy :
;;;     an X Display
;;;
;;; drawable :
;;;     an X Drawable, (a Pixmap or a Window)
;;;
;;; visual :
;;;     the visual to use for drawing to drawable . The depth of the visual must
;;;     match the depth of the drawable. Currently, only TrueColor visuals are
;;;     fully supported.
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
;;; cairo_xlib_surface_create_for_bitmap ()
;;;
;;; cairo_surface_t *
;;; cairo_xlib_surface_create_for_bitmap (Display *dpy,
;;;                                       Pixmap bitmap,
;;;                                       Screen *screen,
;;;                                       int width,
;;;                                       int height);
;;;
;;; Creates an Xlib surface that draws to the given bitmap. This will be drawn
;;; to as a CAIRO_FORMAT_A1 object.
;;;
;;; dpy :
;;;     an X Display
;;;
;;; bitmap :
;;;     an X Drawable, (a depth-1 Pixmap)
;;;
;;; screen :
;;;     the X Screen associated with bitmap
;;;
;;; width :
;;;     the current width of bitmap .
;;;
;;; height :
;;;     the current height of bitmap .
;;;
;;; Returns :
;;;     the newly created surface
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_set_size ()
;;;
;;; void
;;; cairo_xlib_surface_set_size (cairo_surface_t *surface,
;;;                              int width,
;;;                              int height);
;;;
;;; Informs cairo of the new size of the X Drawable underlying the surface. For
;;; a surface created for a Window (rather than a Pixmap), this function must
;;; be called each time the size of the window changes. (For a subwindow, you
;;; are normally resizing the window yourself, but for a toplevel window, it is
;;; necessary to listen for ConfigureNotify events.)
;;;
;;; A Pixmap can never change size, so it is never necessary to call this
;;; function on a surface created for a Pixmap.
;;;
;;; surface :
;;;     a cairo_surface_t for the XLib backend
;;;
;;; width :
;;;     the new width of the surface
;;;
;;; height :
;;;     the new height of the surface
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_display ()
;;;
;;; Display *
;;; cairo_xlib_surface_get_display (cairo_surface_t *surface);
;;;
;;; Get the X Display for the underlying X Drawable.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the display.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_screen ()
;;;
;;; Screen *
;;; cairo_xlib_surface_get_screen (cairo_surface_t *surface);
;;;
;;; Get the X Screen for the underlying X Drawable.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the screen.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_set_drawable ()
;;;
;;; void
;;; cairo_xlib_surface_set_drawable (cairo_surface_t *surface,
;;;                                  Drawable drawable,
;;;                                  int width,
;;;                                  int height);
;;;
;;; Informs cairo of a new X Drawable underlying the surface. The drawable must
;;; match the display, screen and format of the existing drawable or the
;;; application will get X protocol errors and will probably terminate. No
;;; checks are done by this function to ensure this compatibility.
;;;
;;; surface :
;;;     a cairo_surface_t for the XLib backend
;;;
;;; drawable :
;;;     the new drawable for the surface
;;;
;;; width :
;;;     the width of the new drawable
;;;
;;; height :
;;;     the height of the new drawable
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_drawable ()
;;;
;;; Drawable
;;; cairo_xlib_surface_get_drawable (cairo_surface_t *surface);
;;;
;;; Get the underlying X Drawable used for the surface.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the drawable.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_visual ()
;;;
;;; Visual *
;;; cairo_xlib_surface_get_visual (cairo_surface_t *surface);
;;;
;;; Gets the X Visual associated with surface , suitable for use with the
;;; underlying X Drawable. If surface was created by
;;; cairo_xlib_surface_create(), the return value is the Visual passed to that
;;; constructor.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the Visual or NULL if there is no appropriate Visual for surface .
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_width ()
;;;
;;; int
;;; cairo_xlib_surface_get_width (cairo_surface_t *surface);
;;;
;;; Get the width of the X Drawable underlying the surface in pixels.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the width of the surface in pixels.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_height ()
;;;
;;; int
;;; cairo_xlib_surface_get_height (cairo_surface_t *surface);
;;;
;;; Get the height of the X Drawable underlying the surface in pixels.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the height of the surface in pixels.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_surface_get_depth ()
;;;
;;; int
;;; cairo_xlib_surface_get_depth (cairo_surface_t *surface);
;;;
;;; Get the number of bits used to represent each pixel value.
;;;
;;; surface :
;;;     a cairo_xlib_surface_t
;;;
;;; Returns :
;;;     the depth of the surface in bits.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_device_debug_cap_xrender_version ()
;;;
;;; void
;;; cairo_xlib_device_debug_cap_xrender_version
;;;                                (cairo_device_t *device,
;;;                                 int major_version,
;;;                                 int minor_version);
;;;
;;; Restricts all future Xlib surfaces for this devices to the specified version
;;; of the RENDER extension. This function exists solely for debugging purpose.
;;; It lets you find out how cairo would behave with an older version of the
;;; RENDER extension.
;;;
;;; Use the special values -1 and -1 for disabling the RENDER extension.
;;;
;;; device :
;;;     a cairo_device_t for the Xlib backend
;;;
;;; major_version :
;;;     major version to restrict to
;;;
;;; minor_version :
;;;     minor version to restrict to
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_device_debug_get_precision ()
;;;
;;; int
;;; cairo_xlib_device_debug_get_precision (cairo_device_t *device);
;;;
;;; Get the Xrender precision mode.
;;;
;;; device :
;;;     a cairo_device_t for the Xlib backend
;;;
;;; Returns :
;;;     the render precision mode
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xlib_device_debug_set_precision ()
;;;
;;; void
;;; cairo_xlib_device_debug_set_precision (cairo_device_t *device,
;;;                                        int precision);
;;;
;;; Render supports two modes of precision when rendering trapezoids. Set the
;;; precision to the desired mode.
;;;
;;; device :
;;;     a cairo_device_t for the Xlib backend
;;;
;;; precision :
;;;     the precision to use
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.xlib-surface.lisp ------------------------------------
