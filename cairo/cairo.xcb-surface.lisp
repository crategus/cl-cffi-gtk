;;; ----------------------------------------------------------------------------
;;; cairo.xcb-surface.lisp
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
;;; XCB Surfaces
;;;
;;;     X Window System rendering using the XCB library
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_XCB_SURFACE
;;;     CAIRO_HAS_XCB_SHM_FUNCTIONS
;;;
;;; Functions
;;;
;;;     cairo_xcb_surface_create
;;;     cairo_xcb_surface_create_for_bitmap
;;;     cairo_xcb_surface_create_with_xrender_format
;;;     cairo_xcb_surface_set_size
;;;     cairo_xcb_surface_set_drawable
;;;     cairo_xcb_device_get_connection
;;;     cairo_xcb_device_debug_cap_xrender_version
;;;     cairo_xcb_device_debug_cap_xshm_version
;;;     cairo_xcb_device_debug_get_precision
;;;     cairo_xcb_device_debug_set_precision
;;;
;;; Description
;;;
;;;     The XCB surface is used to render cairo graphics to X Window System
;;;     windows and pixmaps using the XCB library.
;;;
;;;     Note that the XCB surface automatically takes advantage of the X render
;;;     extension if it is available.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_XCB_SURFACE
;;;
;;; #define CAIRO_HAS_XCB_SURFACE 1
;;;
;;; Defined if the xcb surface backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_XCB_SHM_FUNCTIONS
;;;
;;; #define CAIRO_HAS_XCB_SHM_FUNCTIONS 1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_surface_create ()
;;;
;;; cairo_surface_t *
;;; cairo_xcb_surface_create (xcb_connection_t *connection,
;;;                           xcb_drawable_t drawable,
;;;                           xcb_visualtype_t *visual,
;;;                           int width,
;;;                           int height);
;;;
;;; Creates an XCB surface that draws to the given drawable. The way that colors
;;; are represented in the drawable is specified by the provided visual.
;;;
;;; Note: If drawable is a Window, then the function
;;; cairo_xcb_surface_set_size() must be called whenever the size of the window
;;; changes.
;;;
;;; When drawable is a Window containing child windows then drawing to the
;;; created surface will be clipped by those child windows. When the created
;;; surface is used as a source, the contents of the children will be included.
;;;
;;; connection :
;;;     an XCB connection
;;;
;;; drawable :
;;;     an XCB drawable
;;;
;;; visual :
;;;     the visual to use for drawing to drawable . The depth of the visual must
;;;     match the depth of the drawable. Currently, only TrueColor visuals are
;;;     fully supported.
;;;
;;; width :
;;;     the current width of drawable
;;;
;;; height :
;;;     the current height of drawable
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_surface_create_for_bitmap ()
;;;
;;; cairo_surface_t *
;;; cairo_xcb_surface_create_for_bitmap (xcb_connection_t *connection,
;;;                                      xcb_screen_t *screen,
;;;                                      xcb_pixmap_t bitmap,
;;;                                      int width,
;;;                                      int height);
;;;
;;; Creates an XCB surface that draws to the given bitmap. This will be drawn
;;; to as a CAIRO_FORMAT_A1 object.
;;;
;;; connection :
;;;     an XCB connection
;;;
;;; screen :
;;;     the XCB screen associated with bitmap
;;;
;;; bitmap :
;;;     an XCB drawable (a Pixmap with depth 1)
;;;
;;; width :
;;;     the current width of bitmap
;;;
;;; height :
;;;     the current height of bitmap
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_surface_create_with_xrender_format ()
;;;
;;; cairo_surface_t *
;;; cairo_xcb_surface_create_with_xrender_format
;;;                                (xcb_connection_t *connection,
;;;                                 xcb_screen_t *screen,
;;;                                 xcb_drawable_t drawable,
;;;                                 xcb_render_pictforminfo_t *format,
;;;                                 int width,
;;;                                 int height);
;;;
;;; Creates an XCB surface that draws to the given drawable. The way that colors
;;; are represented in the drawable is specified by the provided picture format.
;;;
;;; Note: If drawable is a Window, then the function
;;; cairo_xcb_surface_set_size() must be called whenever the size of the window
;;; changes.
;;;
;;; When drawable is a Window containing child windows then drawing to the
;;; created surface will be clipped by those child windows. When the created
;;; surface is used as a source, the contents of the children will be included.
;;;
;;; connection :
;;;     an XCB connection
;;;
;;; drawable :
;;;     an XCB drawable
;;;
;;; screen :
;;;     the XCB screen associated with drawable
;;;
;;; format :
;;;     the picture format to use for drawing to drawable . The depth of format
;;;     mush match the depth of the drawable.
;;;
;;; width :
;;;     the current width of drawable
;;;
;;; height :
;;;     the current height of drawable
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_surface_set_size ()
;;;
;;; void
;;; cairo_xcb_surface_set_size (cairo_surface_t *surface,
;;;                             int width,
;;;                             int height);
;;;
;;; Informs cairo of the new size of the XCB drawable underlying the surface.
;;; For a surface created for a window (rather than a pixmap), this function
;;; must be called each time the size of the window changes. (For a subwindow,
;;; you are normally resizing the window yourself, but for a toplevel window,
;;; it is necessary to listen for ConfigureNotify events.)
;;;
;;; A pixmap can never change size, so it is never necessary to call this
;;; function on a surface created for a pixmap.
;;;
;;; If cairo_surface_flush() wasn't called, some pending operations might be
;;; discarded.
;;;
;;; surface :
;;;     a cairo_surface_t for the XCB backend
;;;
;;; width :
;;;     the new width of the surface
;;;
;;; height :
;;;     the new height of the surface
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_surface_set_drawable ()
;;;
;;; void
;;; cairo_xcb_surface_set_drawable (cairo_surface_t *surface,
;;;                                 xcb_drawable_t drawable,
;;;                                 int width,
;;;                                 int height);
;;;
;;; Informs cairo of the new drawable and size of the XCB drawable underlying
;;; the surface.
;;;
;;; If cairo_surface_flush() wasn't called, some pending operations might be
;;; discarded.
;;;
;;; surface :
;;;     a cairo_surface_t for the XCB backend
;;;
;;; drawable :
;;;     the new drawable of the surface
;;;
;;; width :
;;;     the new width of the surface
;;;
;;; height :
;;;     the new height of the surface
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_device_get_connection ()
;;;
;;; xcb_connection_t *
;;; cairo_xcb_device_get_connection (cairo_device_t *device);
;;;
;;; Get the connection for the XCB device.
;;;
;;; device :
;;;     a cairo_device_t for the XCB backend
;;;
;;; Returns :
;;;     the xcb_connection_t for the connection
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_device_debug_cap_xrender_version ()
;;;
;;; void
;;; cairo_xcb_device_debug_cap_xrender_version
;;;                                (cairo_device_t *device,
;;;                                 int major_version,
;;;                                 int minor_version);
;;;
;;; Restricts all future XCB surfaces for this devices to the specified version
;;; of the RENDER extension. This function exists solely for debugging purpose.
;;; It let's you find out how cairo would behave with an older version of the
;;; RENDER extension.
;;;
;;; Use the special values -1 and -1 for disabling the RENDER extension.
;;;
;;; device :
;;;     a cairo_device_t for the XCB backend
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
;;; cairo_xcb_device_debug_cap_xshm_version ()
;;;
;;; void
;;; cairo_xcb_device_debug_cap_xshm_version
;;;                                (cairo_device_t *device,
;;;                                 int major_version,
;;;                                 int minor_version);
;;;
;;; Restricts all future XCB surfaces for this devices to the specified version
;;; of the SHM extension. This function exists solely for debugging purpose. It
;;; let's you find out how cairo would behave with an older version of the SHM
;;; extension.
;;;
;;; Use the special values -1 and -1 for disabling the SHM extension.
;;;
;;; device :
;;;     a cairo_device_t for the XCB backend
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
;;; cairo_xcb_device_debug_get_precision ()
;;;
;;; int
;;; cairo_xcb_device_debug_get_precision (cairo_device_t *device);
;;;
;;; Get the Xrender precision mode.
;;;
;;; device :
;;;     a cairo_device_t for the XCB backend
;;;
;;; Returns :
;;;     the render precision mode
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_xcb_device_debug_set_precision ()
;;;
;;; void
;;; cairo_xcb_device_debug_set_precision (cairo_device_t *device,
;;;                                       int precision);
;;;
;;; Render supports two modes of precision when rendering trapezoids. Set the
;;; precision to the desired mode.
;;;
;;; device :
;;;     a cairo_device_t for the XCB backend
;;;
;;; precision :
;;;     the precision to use
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.xcb-surface.lisp -------------------------------------
