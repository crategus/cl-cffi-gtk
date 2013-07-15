;;; ----------------------------------------------------------------------------
;;; gdk.cairo.lisp
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org. See <http://www.gtk.org>.
;;; The API  documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; Cairo Interaction
;;;
;;; Functions to support using cairo
;;;
;;; Synopsis
;;;
;;;     gdk_window_create_similar_surface
;;;     gdk_cairo_create
;;;     gdk_cairo_get_clip_rectangle
;;;     gdk_cairo_set_source_color
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf
;;;     gdk_cairo_set_source_window
;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;
;;; Description
;;;
;;; Cairo is a graphics library that supports vector graphics and image
;;; compositing that can be used with GDK. GTK+ does all of its drawing using
;;; cairo.
;;;
;;; GDK does not wrap the cairo API, instead it allows to create cairo contexts
;;; which can be used to draw on GdkWindows. Additional functions allow use
;;; GdkRectangles with cairo and to use GdkColors, GdkRGBAs, GdkPixbufs and
;;; GdkWindows as sources for drawing operations.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_similar_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_create_similar_surface"
           gdk-window-create-similar-surface)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{window to make new surface similar to}
  @argument[content]{the content for the new surface}
  @argument[width]{width of the new surface}
  @argument[height]{height of the new surface}
  @begin{return}
    A pointer to the newly allocated surface. The caller owns the surface
    and should call @fun{cairo-surface-destroy} when done with it. This function
    always returns a valid pointer, but it will return a pointer to a \"nil\"
    surface if other is already in an error state or any other error occurs.
  @end{return}
  @begin{short}
    Create a new surface that is as compatible as possible with the given
    window. For example the new surface will have the same fallback resolution
    and font options as window. Generally, the new surface will also use the
    same backend as window, unless that is not possible for some reason. The
    type of the returned surface may be examined with cairo_surface_get_type().
  @end{short}

  Initially the surface contents are all 0 (transparent if contents have
  transparency, black otherwise.)

  Since 2.22"
  (window (g-object gdk-window))
  (content cairo-content-t)
  (width :int)
  (height :int))

(export 'gdk-window-create-similar-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_create ()
;;;
;;; cairo_t * gdk_cairo_create (GdkWindow *window);
;;;
;;; Creates a Cairo context for drawing to window.
;;;
;;; Warning
;;;
;;; Note that calling cairo_reset_clip() on the resulting cairo_t will produce
;;; undefined results, so avoid it at all costs.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; Returns :
;;;     A newly created Cairo context. Free with cairo_destroy() when you are
;;;     done drawing.
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_get_clip_rectangle ()
;;;
;;; gboolean gdk_cairo_get_clip_rectangle (cairo_t *cr, GdkRectangle *rect);
;;;
;;; This is a convenience function around cairo_clip_extents(). It rounds the
;;; clip extents to integer coordinates and returns a boolean indicating if a
;;; clip area exists.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; rect :
;;;     return location for the clip, or NULL
;;;
;;; Returns :
;;;     TRUE if a clip rectangle exists, FALSE if all of cr is clipped and all
;;;     drawing can be skipped
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_color ()
;;;
;;; void gdk_cairo_set_source_color (cairo_t *cr, const GdkColor *color);
;;;
;;; Warning
;;;
;;; gdk_cairo_set_source_color has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gdk_cairo_set_source_rgba() instead
;;;
;;; Sets the specified GdkColor as the source color of cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; color :
;;;     a GdkColor
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_rgba ()
;;;
;;; void gdk_cairo_set_source_rgba (cairo_t *cr, const GdkRGBA *rgba);
;;;
;;; Sets the specified GdkRGBA as the source color of cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; rgba :
;;;     a GdkRGBA
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_pixbuf" gdk-cairo-set-source-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[cr]{a cairo context}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[pixbuf-x]{x coordinate of location to place upper left corner
    of @arg{pixbuf}}
  @argument[pixbuf-y]{y coordinate of location to place upper left corner
    of @arg{pixbuf}}
  @begin{short}
    Sets the given @arg{pixbuf} as the source pattern for @arg{cr}.
  @end{short}

  The pattern has an extend mode of @code{CAIRO_EXTEND_NONE} and is aligned so
  that the origin of pixbuf is @arg{pixbuf-x}, @arg{pixbuf-y}.

  Since 2.8"
  (cr (:pointer (:struct cairo-t)))
  (pixbuf (g-object gdk-pixbuf))
  (pixbuf-x :double)
  (pxibuf-y :double))

(export 'gdk-cairo-set-source-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_window ()
;;;
;;; void gdk_cairo_set_source_window (cairo_t *cr,
;;;                                   GdkWindow *window,
;;;                                   gdouble x,
;;;                                   gdouble y);
;;;
;;; Sets the given window as the source pattern for cr.
;;;
;;; The pattern has an extend mode of CAIRO_EXTEND_NONE and is aligned so that
;;; the origin of window is x, y. The window contains all its subwindows when
;;; rendering.
;;;
;;; Note that the contents of window are undefined outside of the visible part
;;; of window, so use this function with care.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; x :
;;;     X coordinate of location to place upper left corner of window
;;;
;;; y :
;;;     Y coordinate of location to place upper left corner of window
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_rectangle ()
;;;
;;; void gdk_cairo_rectangle (cairo_t *cr, const GdkRectangle *rectangle);
;;;
;;; Adds the given rectangle to the current path of cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; rectangle :
;;;     a GdkRectangle
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region ()
;;;
;;; void gdk_cairo_region (cairo_t *cr, const cairo_region_t *region);
;;;
;;; Adds the given region to the current path of cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; region :
;;;     a cairo_region_t
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region_create_from_surface ()
;;;
;;; cairo_region_t * gdk_cairo_region_create_from_surface
;;;                                                  (cairo_surface_t *surface);
;;;
;;; Creates region that describes covers the area where the given surface is
;;; more than 50% opaque.
;;;
;;; This function takes into account device offsets that might be set with
;;; cairo_surface_set_device_offset().
;;;
;;; surface :
;;;     a cairo surface
;;;
;;; Returns :
;;;     A cairo_region_t; must be freed with cairo_region_destroy()
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.cairo.lisp ---------------------------------------------
