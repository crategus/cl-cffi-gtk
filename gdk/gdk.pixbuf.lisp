;;; ----------------------------------------------------------------------------
;;; gdk.pixbuf.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; Pixbufs
;;;
;;;     Functions for obtaining pixbufs
;;;
;;; Functions
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
;;; gdk_pixbuf_get_from_window () -> gdk-pixbuf-from-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_from_window" gdk-pixbuf-from-window)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @argument[window]{a @class{gdk-window} source window}
  @argument[src-x]{an integer with the source x coordinate within window}
  @argument[src-y]{an integer with the source y coordinate within window}
  @argument[width]{an integer with the width in pixels of region to get}
  @argument[height]{an integer with the height in pixels of region to get}
  @return{A newly-created @class{gdk-pixbuf} object, or @code{nil} on error.}
  @begin{short}
    Transfers image data from a @class{gdk-window} object and converts it to an
    RGB(A) representation inside a @class{gdk-pixbuf} object.
  @end{short}
  In other words, copies image data from a server-side drawable to a client-side
  RGB(A) buffer. This allows you to efficiently read individual pixels on the
  client side.

  This function will create an RGB pixbuf with 8 bits per channel with the
  same size specified by the width and height arguments. The pixbuf will
  contain an alpha channel if the window contains one.

  If the window is off the screen, then there is no image data in the
  obscured/offscreen regions to be placed in the pixbuf. The contents of
  portions of the pixbuf corresponding to the offscreen region are undefined.

  If the window you are obtaining data from is partially obscured by other
  windows, then the contents of the pixbuf areas corresponding to the obscured
  regions are undefined.

  If the window is not mapped (typically because it is iconified/minimized or
  not on the current workspace), then @code{nil} will be returned.

  If memory cannot be allocated for the return value, @code{nil} will be
  returned instead.

  In short, there are several ways this function can fail, and if it fails
  it returns @code{nil}; so check the return value.
  @see-class{gdk-window}
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-from-surface}"
  (window (g-object gdk-window))
  (src-x :int)
  (src-y :int)
  (width :int)
  (height :int))

(export 'gdk-pixbuf-from-window)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_from_surface () -> gdk-pixbuf-from-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_get_from_surface" gdk-pixbuf-from-surface)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @argument[surface]{a @symbol{cairo-surface-t} instance to copy from}
  @argument[src-x]{an integer with the source x coordinate within surface}
  @argument[src-y]{an integer with the source y coordinate within surface}
  @argument[width]{an integer with width in pixels of region to get}
  @argument[height]{an integer with the height in pixels of region to get}
  @return{A newly-created @class{gdk-pixbuf} object, or @code{nil} on error.}
  @begin{short}
    Transfers image data from a @symbol{cairo-surface-t} instance and converts
    it to an RGB(A) representation inside a @class{gdk-pixbuf} object.
  @end{short}
  This allows you to efficiently read individual pixels from cairo surfaces.
  For @class{gdk-window} objects, use the function
  @fun{gdk-pixbuf-from-window} instead.

  This function will create an RGB pixbuf with 8 bits per channel. The pixbuf
  will contain an alpha channel if the surface contains one.
  @see-symbol{cairo-surface-t}
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-from-window}"
  (surface (:pointer (:struct cairo-surface-t)))
  (src-x :int)
  (src-y :int)
  (width :int)
  (height :int))

(export 'gdk-pixbuf-from-surface)

;;; --- End of file gdk.pixbuf.lisp --------------------------------------------
