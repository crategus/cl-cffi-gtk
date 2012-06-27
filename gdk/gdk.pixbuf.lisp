;;; ----------------------------------------------------------------------------
;;; gdk.pixbuf.lisp
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; Pixbufs
;;;
;;; Functions for obtaining pixbufs
;;;
;;; Synopsis
;;;
;;;     gdk_pixbuf_get_from_window
;;;     gdk_pixbuf_get_from_surface
;;;
;;; Description
;;;
;;; Pixbufs are client-side images. For details on how to create and manipulate
;;; pixbufs, see the GdkPixbuf API documentation.
;;;
;;; The functions described here allow to obtain pixbufs from GdkWindows and
;;; cairo surfaces.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_window ()
;;;
;;; GdkPixbuf * gdk_pixbuf_get_from_window (GdkWindow *window,
;;;                                         gint src_x,
;;;                                         gint src_y,
;;;                                         gint width,
;;;                                         gint height);
;;;
;;; Transfers image data from a GdkWindow and converts it to an RGB(A)
;;; representation inside a GdkPixbuf. In other words, copies image data from a
;;; server-side drawable to a client-side RGB(A) buffer. This allows you to
;;; efficiently read individual pixels on the client side.
;;;
;;; This function will create an RGB pixbuf with 8 bits per channel with the
;;; same size specified by the width and height arguments. The pixbuf will
;;; contain an alpha channel if the window contains one.
;;;
;;; If the window is off the screen, then there is no image data in the
;;; obscured/offscreen regions to be placed in the pixbuf. The contents of
;;; portions of the pixbuf corresponding to the offscreen region are undefined.
;;;
;;; If the window you're obtaining data from is partially obscured by other
;;; windows, then the contents of the pixbuf areas corresponding to the obscured
;;; regions are undefined.
;;;
;;; If the window is not mapped (typically because it's iconified/minimized or
;;; not on the current workspace), then NULL will be returned.
;;;
;;; If memory can't be allocated for the return value, NULL will be returned
;;; instead.
;;;
;;; (In short, there are several ways this function can fail, and if it fails
;;; it returns NULL; so check the return value.)
;;;
;;; window :
;;;     Source window
;;;
;;; src_x :
;;;     Source X coordinate within window
;;;
;;; src_y :
;;;     Source Y coordinate within window
;;;
;;; width :
;;;     Width in pixels of region to get
;;;
;;; height :
;;;     Height in pixels of region to get
;;;
;;; Returns :
;;;     A newly-created pixbuf with a reference count of 1, or NULL on error.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_from_window" gdk-pixbuf-get-from-window)
    (g-object gdk-pixbuf)
  (window (g-object gdk-window))
  (src-x :int)
  (src-y :int)
  (width :int)
  (height :int))

(export 'gdk-pixbuf-get-from-window)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_surface ()
;;;
;;; GdkPixbuf * gdk_pixbuf_get_from_surface (cairo_surface_t *surface,
;;;                                          gint src_x,
;;;                                          gint src_y,
;;;                                          gint width,
;;;                                          gint height);
;;;
;;; Transfers image data from a cairo_surface_t and converts it to an RGB(A)
;;; representation inside a GdkPixbuf. This allows you to efficiently read
;;; individual pixels from cairo surfaces. For GdkWindows, use
;;; gdk_pixbuf_get_from_window() instead.
;;;
;;; This function will create an RGB pixbuf with 8 bits per channel. The pixbuf
;;; will contain an alpha channel if the surface contains one.
;;;
;;; surface :
;;;     surface to copy from
;;;
;;; src_x :
;;;     Source X coordinate within surface
;;;
;;; src_y :
;;;     Source Y coordinate within surface
;;;
;;; width :
;;;     Width in pixels of region to get
;;;
;;; height :
;;;     Height in pixels of region to get
;;;
;;; Returns :
;;;     A newly-created pixbuf with a reference count of 1, or NULL on error.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_from_surface" gdk-pixbuf-get-from-surface)
    (g-object gdk-pixbuf)
  (surface cairo-surface-t)
  (src-x :int)
  (src-y :int)
  (width :int)
  (height :int))

(export 'gdk-pixbuf-get-from-surface)

;;; --- End of file gdk.pixbuf.lisp --------------------------------------------
