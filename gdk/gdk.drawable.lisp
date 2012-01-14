;;; ----------------------------------------------------------------------------
;;; gdk.drawable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;; Drawing Primitives
;;; 
;;; Functions for drawing points, lines, arcs, and text
;;; 	
;;; Synopsis
;;; 
;;;     GdkDrawable
;;;     gdk_drawable_ref
;;;     gdk_drawable_unref
;;;     gdk_drawable_set_data
;;;     gdk_drawable_get_data
;;;     gdk_drawable_get_display
;;;     gdk_drawable_get_screen
;;;     gdk_drawable_get_visual
;;;     gdk_drawable_set_colormap
;;;     gdk_drawable_get_colormap
;;;     gdk_drawable_get_depth
;;;     gdk_drawable_get_size
;;;     gdk_drawable_get_clip_region
;;;     gdk_drawable_get_visible_region
;;;     gdk_draw_point
;;;     gdk_draw_points
;;;     gdk_draw_line
;;;     gdk_draw_lines
;;;     gdk_draw_pixbuf
;;;     gdk_draw_segments
;;;     GdkSegment
;;;     gdk_draw_rectangle
;;;     gdk_draw_arc
;;;     gdk_draw_polygon
;;;     gdk_draw_trapezoids
;;;     GdkTrapezoid
;;;     gdk_draw_glyphs
;;;     gdk_draw_glyphs_transformed
;;;     gdk_draw_layout_line
;;;     gdk_draw_layout_line_with_colors
;;;     gdk_draw_layout
;;;     gdk_draw_layout_with_colors     
;;;     gdk_draw_string
;;;     gdk_draw_text
;;;     gdk_draw_text_wc
;;;     gdk_draw_pixmap
;;;     gdk_draw_drawable
;;;     gdk_draw_image
;;;     gdk_drawable_get_image
;;;     gdk_drawable_copy_to_image
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkDrawable
;;;          +----GdkWindow
;;;          +----GdkPixmap
;;; 
;;; Description
;;; 
;;; These functions provide support for drawing points, lines, arcs and text
;;; onto what are called 'drawables'. Drawables, as the name suggests, are
;;; things which support drawing onto them, and are either GdkWindow or
;;; GdkPixmap objects.
;;; 
;;; Many of the drawing operations take a GdkGC argument, which represents a
;;; graphics context. This GdkGC contains a number of drawing attributes such
;;; as foreground color, background color and line width, and is used to reduce
;;; the number of arguments needed for each drawing operation. See the Graphics
;;; Contexts section for more information.
;;; 
;;; Some of the drawing operations take Pango data structures like PangoContext,
;;; PangoLayout or PangoLayoutLine as arguments. If you're using GTK+, the
;;; ususal way to obtain these structures is via
;;; gtk_widget_create_pango_context() or gtk_widget_create_pango_layout().
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDrawable
;;; 
;;; typedef struct _GdkDrawable GdkDrawable;
;;; 
;;; An opaque structure representing an object that can be drawn onto. This can
;;; be a GdkPixmap, a GdkBitmap, or a GdkWindow.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDrawable" gdk-drawable
  (:type-initializer "gdk_drawable_get_type")
  ((:cffi display gdk-drawable-display (g-object gdk-display)
          "gdk_drawable_get_display" nil)
   (:cffi screen gdk-drawable-screen (g-object gdk-screen)
          "gdk_drawable_get_screen" nil)
   (:cffi visual gdk-drawable-visual (g-object gdk-visual)
          "gdk_drawable_get_visual" nil)
   (:cffi colormap gdk-drawable-colormap (g-object gdk-colormap)
          "gdk_drawable_get_colormap" "gdk_drawable_set_colormap")
   (:cffi depth gdk-drawable-depth :int
          "gdk_drawable_get_depth" nil)
   (:cffi clip-region gdk-drawable-clip-region (g-boxed-foreign gdk-region :return)
          "gdk_drawable_get_clip_region" nil)
   (:cffi visible-region gdk-drawable-visible-region
          (g-boxed-foreign gdk-region :return)
          "gdk_drawable_get_visible_region" nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_ref ()
;;; 
;;; GdkDrawable * gdk_drawable_ref (GdkDrawable *drawable);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_ref has been deprecated since version 2.0 and should not be
;;; used in newly-written code. Use g_object_ref() instead.
;;; 
;;; Deprecated equivalent of calling g_object_ref() on drawable. (Drawables
;;; were not objects in previous versions of GDK.)
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	the same drawable passed in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_unref ()
;;; 
;;; void gdk_drawable_unref (GdkDrawable *drawable);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_unref has been deprecated since version 2.0 and should not be
;;; used in newly-written code. Use g_object_unref() instead.
;;; 
;;; Deprecated equivalent of calling g_object_unref() on drawable.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_set_data ()
;;; 
;;; void gdk_drawable_set_data (GdkDrawable *drawable,
;;;                             const gchar *key,
;;;                             gpointer data,
;;;                             GDestroyNotify destroy_func);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_set_data is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; This function is equivalent to g_object_set_data(), the GObject variant
;;; should be used instead.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; key :
;;; 	name to store the data under
;;; 
;;; data :
;;; 	arbitrary data
;;; 
;;; destroy_func :
;;; 	function to free data, or NULL. [allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_data ()
;;; 
;;; gpointer gdk_drawable_get_data (GdkDrawable *drawable, const gchar *key);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_get_data is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Equivalent to g_object_get_data(); the GObject variant should be used
;;; instead.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; key :
;;; 	name the data was stored under
;;; 
;;; Returns :
;;; 	the data stored at key
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_display ()
;;; 
;;; GdkDisplay * gdk_drawable_get_display (GdkDrawable *drawable);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_get_display has been deprecated since version 2.24 and should
;;; not be used in newly-written code. Use gdk_window_get_display() instead
;;; 
;;; Gets the GdkDisplay associated with a GdkDrawable.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	the GdkDisplay associated with drawable
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_screen ()
;;; 
;;; GdkScreen * gdk_drawable_get_screen (GdkDrawable *drawable);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_get_screen has been deprecated since version 2.24 and should
;;; not be used in newly-written code. Use gdk_window_get_screen() instead
;;; 
;;; Gets the GdkScreen associated with a GdkDrawable.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	the GdkScreen associated with drawable
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_visual ()
;;; 
;;; GdkVisual * gdk_drawable_get_visual (GdkDrawable *drawable);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_get_visual has been deprecated since version 2.24 and should
;;; not be used in newly-written code. Use gdk_window_get_visual()
;;; 
;;; Gets the GdkVisual describing the pixel format of drawable.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	a GdkVisual
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_set_colormap ()
;;; 
;;; void gdk_drawable_set_colormap (GdkDrawable *drawable,
;;;                                 GdkColormap *colormap);
;;; 
;;; Sets the colormap associated with drawable. Normally this will happen
;;; automatically when the drawable is created; you only need to use this
;;; function if the drawable-creating function did not have a way to determine
;;; the colormap, and you then use drawable operations that require a colormap.
;;; The colormap for all drawables and graphics contexts you intend to use
;;; together should match. i.e. when using a GdkGC to draw to a drawable, or
;;; copying one drawable to another, the colormaps should match.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; colormap :
;;; 	a GdkColormap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_colormap ()
;;; 
;;; GdkColormap * gdk_drawable_get_colormap (GdkDrawable *drawable);
;;; 
;;; Gets the colormap for drawable, if one is set; returns NULL otherwise.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	the colormap, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_depth ()
;;; 
;;; gint gdk_drawable_get_depth (GdkDrawable *drawable);
;;; 
;;; Obtains the bit depth of the drawable, that is, the number of bits that
;;; make up a pixel in the drawable's visual. Examples are 8 bits per pixel,
;;; 24 bits per pixel, etc.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	number of bits per pixel
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_size ()
;;; 
;;; void gdk_drawable_get_size (GdkDrawable *drawable,
;;;                             gint *width,
;;;                             gint *height);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_get_size has been deprecated since version 2.24 and should not
;;; be used in newly-written code. Use gdk_window_get_width() and
;;; gdk_window_get_height() for GdkWindows. Use gdk_pixmap_get_size() for
;;; GdkPixmaps.
;;; 
;;; Fills *width and *height with the size of drawable. width or height can be
;;; NULL if you only want the other one.
;;; 
;;; On the X11 platform, if drawable is a GdkWindow, the returned size is the
;;; size reported in the most-recently-processed configure event, rather than
;;; the current size on the X server.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; width :
;;; 	location to store drawable's width, or NULL. [out][allow-none]
;;; 
;;; height :
;;; 	location to store drawable's height, or NULL. [out][allow-none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drawable_get_size" %gdk-drawable-get-size) :void
  (drawable (g-object gdk-drawable))
  (width (:pointer :int))
  (height (:pointer :int)))

;; The Lisp implementation returns the values width and height.

(defun gdk-drawable-get-size (drawable)
  (with-foreign-objects ((x :int)
                         (y :int))
    (%gdk-drawable-get-size drawable x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gdk-drawable-get-size)

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_clip_region ()
;;; 
;;; GdkRegion * gdk_drawable_get_clip_region (GdkDrawable *drawable);
;;; 
;;; Computes the region of a drawable that potentially can be written to by
;;; drawing primitives. This region will not take into account the clip region
;;; for the GC, and may also not take into account other factors such as if the
;;; window is obscured by other windows, but no area outside of this region
;;; will be affected by drawing primitives.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	a GdkRegion. This must be freed with gdk_region_destroy() when you are
;;;     done.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_visible_region ()
;;; 
;;; GdkRegion * gdk_drawable_get_visible_region (GdkDrawable *drawable);
;;; 
;;; Computes the region of a drawable that is potentially visible. This does
;;; not necessarily take into account if the window is obscured by other
;;; windows, but no area outside of this region is visible.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; Returns :
;;; 	a GdkRegion. This must be freed with gdk_region_destroy() when you
;;;     are done.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_point ()
;;; 
;;; void gdk_draw_point (GdkDrawable *drawable, GdkGC *gc, gint x, gint y);
;;; 
;;; Warning
;;; 
;;; gdk_draw_point has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use cairo_rectangle() and cairo_fill() or
;;; cairo_move_to() and cairo_stroke() instead.
;;; 
;;; Draws a point, using the foreground color and other attributes of the GdkGC.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x :
;;; 	the x coordinate of the point.
;;; 
;;; y :
;;; 	the y coordinate of the point.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_point" gdk-draw-point) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int))

(export 'gdk-draw-point)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_points ()
;;; 
;;; void gdk_draw_points (GdkDrawable *drawable,
;;;                       GdkGC *gc,
;;;                       const GdkPoint *points,
;;;                       gint n_points);
;;; 
;;; Warning
;;; 
;;; gdk_draw_points has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use n_points calls to cairo_rectangle() and
;;; cairo_fill() instead.
;;; 
;;; Draws a number of points, using the foreground color and other attributes
;;; of the GdkGC.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; points :
;;; 	an array of GdkPoint structures.
;;; 
;;; n_points :
;;; 	the number of points to be drawn.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_points" %gdk-draw-points) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (points :pointer)
  (n :int))

(defun gdk-draw-points (drawable gc points)
  (with-foreign-boxed-array (n points-ptr gdk-point points)
    (%gdk-draw-points drawable gc points-ptr n)))

(export 'gdk-draw-points)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_line ()
;;; 
;;; void gdk_draw_line (GdkDrawable *drawable,
;;;                     GdkGC *gc,
;;;                     gint x1_,
;;;                     gint y1_,
;;;                     gint x2_,
;;;                     gint y2_);
;;; 
;;; Warning
;;; 
;;; gdk_draw_line has been deprecated since version 2.22 and should not be used
;;; in newly-written code. Use cairo_line_to() and cairo_stroke() instead. Be
;;; aware that the default line width in Cairo is 2 pixels and that your
;;; coordinates need to describe the center of the line. To draw a single pixel
;;; wide pixel-aligned line, you would use:
;;; 
;;;  1 cairo_set_line_width (cr, 1.0);
;;;  2 cairo_set_line_cap (cr, CAIRO_LINE_CAP_SQUARE);
;;;  3 cairo_move_to (cr, 0.5, 0.5);
;;;  4 cairo_line_to (cr, 9.5, 0.5);
;;;  5 cairo_stroke (cr);
;;; 
;;; See also the Cairo FAQ on this topic.
;;; 
;;; Draws a line, using the foreground color and other attributes of the GdkGC.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x1_ :
;;; 	the x coordinate of the start point.
;;; 
;;; y1_ :
;;; 	the y coordinate of the start point.
;;; 
;;; x2_ :
;;; 	the x coordinate of the end point.
;;; 
;;; y2_ :
;;; 	the y coordinate of the end point.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_line" gdk-draw-line) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(export 'gdk-draw-line)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_lines ()
;;; 
;;; void gdk_draw_lines (GdkDrawable *drawable,
;;;                      GdkGC *gc,
;;;                      const GdkPoint *points,
;;;                      gint n_points);
;;; 
;;; Warning
;;; 
;;; gdk_draw_lines has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use cairo_line_to() and cairo_stroke() instead.
;;; See the documentation of gdk_draw_line() for notes on line drawing with
;;; Cairo.
;;; 
;;; Draws a series of lines connecting the given points. The way in which joins
;;; between lines are draw is determined by the GdkCapStyle value in the GdkGC.
;;; This can be set with gdk_gc_set_line_attributes().
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; points :
;;; 	an array of GdkPoint structures specifying the endpoints of the
;;; 
;;; n_points :
;;; 	the size of the points array.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_lines" %gdk-draw-lines) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (points :pointer)
  (n :int))

(defun gdk-draw-lines (drawable gc points)
  (with-foreign-boxed-array (n points-ptr gdk-point points)
    (%gdk-draw-lines drawable gc points-ptr n)))

(export 'gdk-draw-lines)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_pixbuf ()
;;; 
;;; void gdk_draw_pixbuf (GdkDrawable *drawable,
;;;                       GdkGC *gc,
;;;                       const GdkPixbuf *pixbuf,
;;;                       gint src_x,
;;;                       gint src_y,
;;;                       gint dest_x,
;;;                       gint dest_y,
;;;                       gint width,
;;;                       gint height,
;;;                       GdkRgbDither dither,
;;;                       gint x_dither,
;;;                       gint y_dither);
;;; 
;;; Warning
;;; 
;;; gdk_draw_pixbuf has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use gdk_cairo_set_source_pixbuf() and
;;; cairo_paint() or cairo_rectangle() and cairo_fill() instead.
;;; 
;;; Renders a rectangular portion of a pixbuf to a drawable. The destination
;;; drawable must have a colormap. All windows have a colormap, however, pixmaps
;;; only have colormap by default if they were created with a non-NULL window
;;; argument. Otherwise a colormap must be set on them with
;;; gdk_drawable_set_colormap().
;;; 
;;; On older X servers, rendering pixbufs with an alpha channel involves round
;;; trips to the X server, and may be somewhat slow.
;;; 
;;; If GDK is built with the Sun mediaLib library, the gdk_draw_pixbuf function
;;; is accelerated using mediaLib, which provides hardware acceleration on
;;; Intel, AMD, and Sparc chipsets. If desired, mediaLib support can be turned
;;; off by setting the GDK_DISABLE_MEDIALIB environment variable.
;;; 
;;; drawable :
;;; 	Destination drawable.
;;; 
;;; gc :
;;; 	a GdkGC, used for clipping, or NULL.
;;; 
;;; pixbuf :
;;; 	a GdkPixbuf
;;; 
;;; src_x :
;;; 	Source X coordinate within pixbuf.
;;; 
;;; src_y :
;;; 	Source Y coordinates within pixbuf.
;;; 
;;; dest_x :
;;; 	Destination X coordinate within drawable.
;;; 
;;; dest_y :
;;; 	Destination Y coordinate within drawable.
;;; 
;;; width :
;;; 	Width of region to render, in pixels, or -1 to use pixbuf width.
;;; 
;;; height :
;;; 	Height of region to render, in pixels, or -1 to use pixbuf height.
;;; 
;;; dither :
;;; 	Dithering mode for GdkRGB.
;;; 
;;; x_dither :
;;; 	X offset for dither.
;;; 
;;; y_dither :
;;; 	Y offset for dither.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_pixbuf" gdk-draw-pixbuf) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (pixbuf (g-object gdk-pixbuf))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (dither gdk-rgb-dither)
  (x-dither :int)
  (y-dither :int))

(export 'gdk-draw-pixbuf)

;;; ----------------------------------------------------------------------------
;;; struct GdkSegment
;;; 
;;; struct GdkSegment {
;;;   gint x1;
;;;   gint y1;
;;;   gint x2;
;;;   gint y2;
;;; };
;;; 
;;; Specifies the start and end point of a line for use by the
;;; gdk_draw_segments() function.
;;; 
;;; gint x1;
;;; 	the x coordinate of the start point.
;;; 
;;; gint y1;
;;; 	the y coordinate of the start point.
;;; 
;;; gint x2;
;;; 	the x coordinate of the end point.
;;; 
;;; gint y2;
;;; 	the y coordinate of the end point.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-segment nil
  (x1 :int :initform 0)
  (y1 :int :initform 0)
  (x2 :int :initform 0)
  (y2 :int :initform 0))

(export (boxed-related-symbols 'gdk-segment))

;;; ----------------------------------------------------------------------------
;;; gdk_draw_segments ()
;;; 
;;; void gdk_draw_segments (GdkDrawable *drawable,
;;;                         GdkGC *gc,
;;;                         const GdkSegment *segs,
;;;                         gint n_segs);
;;; 
;;; Warning
;;; 
;;; gdk_draw_segments has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use cairo_move_to(), cairo_line_to() and
;;; cairo_stroke() instead. See the documentation of gdk_draw_line() for notes
;;; on line drawing with Cairo.
;;; 
;;; Draws a number of unconnected lines.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; segs :
;;; 	an array of GdkSegment structures specifying the start and end points
;;;     of the lines to be drawn.
;;; 
;;; n_segs :
;;; 	the number of line segments to draw, i.e. the size of the segs array.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_segments" %gdk-draw-segments) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (segments :pointer)
  (n-segments :int))

(defun gdk-draw-segments (drawable gc segments)
  (with-foreign-boxed-array (n segments-ptr gdk-segment segments)
    (%gdk-draw-segments drawable gc segments-ptr n)))

(export 'gdk-draw-segments)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_rectangle ()
;;; 
;;; void gdk_draw_rectangle (GdkDrawable *drawable,
;;;                          GdkGC *gc,
;;;                          gboolean filled,
;;;                          gint x,
;;;                          gint y,
;;;                          gint width,
;;;                          gint height);
;;; 
;;; Warning
;;; 
;;; gdk_draw_rectangle has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use cairo_rectangle() and cairo_fill() or
;;; cairo_stroke() instead. For stroking, the same caveats for converting code
;;; apply as for gdk_draw_line().
;;; 
;;; Draws a rectangular outline or filled rectangle, using the foreground color
;;; and other attributes of the GdkGC.
;;; 
;;; A rectangle drawn filled is 1 pixel smaller in both dimensions than a
;;; rectangle outlined. Calling gdk_draw_rectangle
;;; (window, gc, TRUE, 0, 0, 20, 20) results in a filled rectangle 20 pixels
;;; wide and 20 pixels high. Calling gdk_draw_rectangle
;;; (window, gc, FALSE, 0, 0, 20, 20) results in an outlined rectangle with
;;; corners at (0, 0), (0, 20), (20, 20), and (20, 0), which makes it 21 pixels
;;; wide and 21 pixels high.
;;; 
;;; Note
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; filled :
;;; 	TRUE if the rectangle should be filled.
;;; 
;;; x :
;;; 	the x coordinate of the left edge of the rectangle.
;;; 
;;; y :
;;; 	the y coordinate of the top edge of the rectangle.
;;; 
;;; width :
;;; 	the width of the rectangle.
;;; 
;;; height :
;;; 	the height of the rectangle.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_rectangle" gdk-draw-rectangle) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (filled :boolean)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-draw-rectangle)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_arc ()
;;; 
;;; void gdk_draw_arc (GdkDrawable *drawable,
;;;                    GdkGC *gc,
;;;                    gboolean filled,
;;;                    gint x,
;;;                    gint y,
;;;                    gint width,
;;;                    gint height,
;;;                    gint angle1,
;;;                    gint angle2);
;;; 
;;; Warning
;;; 
;;; gdk_draw_arc has been deprecated since version 2.22 and should not be used
;;; in newly-written code. Use cairo_arc() and cairo_fill() or cairo_stroke()
;;; instead. Note that arcs just like any drawing operation in Cairo are
;;; antialiased unless you call cairo_set_antialias().
;;; 
;;; Draws an arc or a filled 'pie slice'. The arc is defined by the bounding
;;; rectangle of the entire ellipse, and the start and end angles of the part
;;; of the ellipse to be drawn.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; filled :
;;; 	TRUE if the arc should be filled, producing a 'pie slice'.
;;; 
;;; x :
;;; 	the x coordinate of the left edge of the bounding rectangle.
;;; 
;;; y :
;;; 	the y coordinate of the top edge of the bounding rectangle.
;;; 
;;; width :
;;; 	the width of the bounding rectangle.
;;; 
;;; height :
;;; 	the height of the bounding rectangle.
;;; 
;;; angle1 :
;;; 	the start angle of the arc, relative to the 3 o'clock position,
;;;     counter-clockwise, in 1/64ths of a degree.
;;; 
;;; angle2 :
;;; 	the end angle of the arc, relative to angle1, in 1/64ths of a degree.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_arc" gdk-draw-arc) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (filled :boolean)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (angle1 :int)
  (angle2 :int))

(export 'gdk-draw-arc)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_polygon ()
;;; 
;;; void gdk_draw_polygon (GdkDrawable *drawable,
;;;                        GdkGC *gc,
;;;                        gboolean filled,
;;;                        const GdkPoint *points,
;;;                        gint n_points);
;;; 
;;; Warning
;;; 
;;; gdk_draw_polygon has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use cairo_line_to() or cairo_append_path() and
;;; cairo_fill() or cairo_stroke() instead.
;;; 
;;; Draws an outlined or filled polygon.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; filled :
;;; 	TRUE if the polygon should be filled. The polygon is closed
;;;     automatically, connecting the last point to the first point if
;;;     necessary.
;;; 
;;; points :
;;; 	an array of GdkPoint structures specifying the points making up the
;;;     polygon.
;;; 
;;; n_points :
;;; 	the number of points.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_polygon" %gdk-draw-polygon) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (filled :boolean)
  (points :pointer)
  (n-points :int))

(defun gdk-draw-polygon (drawable gc filled points)
  (with-foreign-boxed-array (n points-ptr gdk-point points)
    (%gdk-draw-polygon drawable gc filled points-ptr n)))

(export 'gdk-draw-polygon)

;;; ----------------------------------------------------------------------------
;;; struct GdkTrapezoid
;;; 
;;; struct GdkTrapezoid {
;;;   double y1, x11, x21, y2, x12, x22;
;;; };
;;; 
;;; Specifies a trapezpoid for use by the gdk_draw_trapezoids(). The trapezoids
;;; used here have parallel, horizontal top and bottom edges.
;;; 
;;; double y1;
;;; 	the y coordinate of the start point.
;;; 
;;; double x11;
;;; 	the x coordinate of the top left corner
;;; 
;;; double x21;
;;; 	the x coordinate of the top right corner
;;; 
;;; double y2;
;;; 	the y coordinate of the end point.
;;; 
;;; double x12;
;;; 	the x coordinate of the bottom left corner
;;; 
;;; double x22;
;;; 	the x coordinate of the bottom right corner
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct trapezoid nil
  (y1 :double :initform 0d0)
  (x11 :double :initform 0d0)
  (x21 :double :initform 0d0)
  (y2 :double :initform 0d0)
  (x12 :double :initform 0d0)
  (x22 :double :initform 0d0))

(export (boxed-related-symbols 'trapezoid))

;;; ----------------------------------------------------------------------------
;;; gdk_draw_trapezoids ()
;;; 
;;; void gdk_draw_trapezoids (GdkDrawable *drawable,
;;;                           GdkGC *gc,
;;;                           const GdkTrapezoid *trapezoids,
;;;                           gint n_trapezoids);
;;; 
;;; Warning
;;; 
;;; gdk_draw_trapezoids has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use Cairo path contruction functions and
;;; cairo_fill() instead.
;;; 
;;; Draws a set of anti-aliased trapezoids. The trapezoids are combined using
;;; saturation addition, then drawn over the background as a set. This is low
;;; level functionality used internally to implement rotated underlines and
;;; backgrouds when rendering a PangoLayout and is likely not useful for
;;; applications.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; trapezoids :
;;; 	an array of GdkTrapezoid structures
;;; 
;;; n_trapezoids :
;;; 	the number of trapezoids to draw
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_trapezoids" %gdk-draw-trapezoids) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (trapezoids :pointer)
  (n :int))

(defun gdk-draw-trapezoids (drawable gc trapezoids)
  (with-foreign-boxed-array (n trapezoids-ptr trapezoid trapezoids)
    (%gdk-draw-trapezoids drawable gc trapezoids-ptr n)))

(export 'gdk-draw-trapezoids)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_glyphs ()
;;; 
;;; void gdk_draw_glyphs (GdkDrawable *drawable,
;;;                       GdkGC *gc,
;;;                       PangoFont *font,
;;;                       gint x,
;;;                       gint y,
;;;                       PangoGlyphString *glyphs);
;;; 
;;; Warning
;;; 
;;; gdk_draw_glyphs has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use pango_cairo_show_glyphs() instead.
;;; 
;;; This is a low-level function; 99% of text rendering should be done using
;;; gdk_draw_layout() instead.
;;; 
;;; A glyph is a single image in a font. This function draws a sequence of
;;; glyphs. To obtain a sequence of glyphs you have to understand a lot about
;;; internationalized text handling, which you don't want to understand; thus,
;;; use gdk_draw_layout() instead of this function, gdk_draw_layout() handles
;;; the details.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; font :
;;; 	font to be used
;;; 
;;; x :
;;; 	X coordinate of baseline origin
;;; 
;;; y :
;;; 	Y coordinate of baseline origin
;;; 
;;; glyphs :
;;; 	the glyph string to draw
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_glyphs" gdk-draw-glyphs) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (font (g-object pango-font))
  (x :int)
  (y :int)
  (glyphs (g-boxed-foreign pango-glyph-string)))

(export 'gdk-draw-glyphs)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_glyphs_transformed ()
;;; 
;;; void gdk_draw_glyphs_transformed (GdkDrawable *drawable,
;;;                                   GdkGC *gc,
;;;                                   const PangoMatrix *matrix,
;;;                                   PangoFont *font,
;;;                                   gint x,
;;;                                   gint y,
;;;                                   PangoGlyphString *glyphs);
;;; 
;;; Warning
;;; 
;;; gdk_draw_glyphs_transformed has been deprecated since version 2.22 and
;;; should not be used in newly-written code. Use pango_cairo_show_glyphs()
;;; instead.
;;; 
;;; Renders a PangoGlyphString onto a drawable, possibly transforming the
;;; layed-out coordinates through a transformation matrix. Note that the
;;; transformation matrix for font is not changed, so to produce correct
;;; rendering results, the font must have been loaded using a PangoContext with
;;; an identical transformation matrix to that passed in to this function.
;;; 
;;; See also gdk_draw_glyphs(), gdk_draw_layout().
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; matrix :
;;; 	a PangoMatrix, or NULL to use an identity transformation.
;;; 
;;; font :
;;; 	the font in which to draw the string
;;; 
;;; x :
;;; 	the x position of the start of the string (in Pango units in user space
;;;     coordinates)
;;; 
;;; y :
;;; 	the y position of the baseline (in Pango units in user space
;;;     coordinates)
;;; 
;;; glyphs :
;;; 	the glyph string to draw
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_glyphs_transformed" gdk-draw-glyphs-transformed) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (matrix (g-boxed-foreign pango-matrix))
  (font (g-object pango-font))
  (x :int)
  (y :int))

(export 'gdk-draw-glyphs-transformed)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_layout_line ()
;;; 
;;; void gdk_draw_layout_line (GdkDrawable *drawable,
;;;                            GdkGC *gc,
;;;                            gint x,
;;;                            gint y,
;;;                            PangoLayoutLine *line);
;;; 
;;; Warning
;;; 
;;; gdk_draw_layout_line is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Render a PangoLayoutLine onto an GDK drawable
;;; 
;;; If the layout's PangoContext has a transformation matrix set, then x and y
;;; specify the position of the left edge of the baseline (left is in
;;; before-tranform user coordinates) in after-transform device coordinates.
;;; 
;;; drawable :
;;; 	the drawable on which to draw the line
;;; 
;;; gc :
;;; 	base graphics to use
;;; 
;;; x :
;;; 	the x position of start of string (in pixels)
;;; 
;;; y :
;;; 	the y position of baseline (in pixels)
;;; 
;;; line :
;;; 	a PangoLayoutLine
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_layout_line" gdk-draw-layout-line) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (line (g-boxed-foreign pango-layout-line)))

(export 'gdk-draw-layout-line)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_layout_line_with_colors ()
;;; 
;;; void gdk_draw_layout_line_with_colors (GdkDrawable *drawable,
;;;                                        GdkGC *gc,
;;;                                        gint x,
;;;                                        gint y,
;;;                                        PangoLayoutLine *line,
;;;                                        const GdkColor *foreground,
;;;                                        const GdkColor *background);
;;; 
;;; Warning
;;; 
;;; gdk_draw_layout_line_with_colors is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Render a PangoLayoutLine onto a GdkDrawable, overriding the layout's normal
;;; colors with foreground and/or background. foreground and background need
;;; not be allocated.
;;; 
;;; If the layout's PangoContext has a transformation matrix set, then x and y
;;; specify the position of the left edge of the baseline (left is in
;;; before-tranform user coordinates) in after-transform device coordinates.
;;; 
;;; drawable :
;;; 	the drawable on which to draw the line
;;; 
;;; gc :
;;; 	base graphics to use
;;; 
;;; x :
;;; 	the x position of start of string (in pixels)
;;; 
;;; y :
;;; 	the y position of baseline (in pixels)
;;; 
;;; line :
;;; 	a PangoLayoutLine
;;; 
;;; foreground :
;;; 	foreground override color, or NULL for none. [allow-none]
;;; 
;;; background :
;;; 	background override color, or NULL for none. [allow-none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_layout_line_with_colors" gdk-draw-layout-line-with-colors)
    :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (line (g-boxed-foreign pango-layout-line))
  (foreground (g-boxed-foreign gdk-color))
  (background (g-boxed-foreign gdk-color)))

(export 'gdk-draw-layout-line-with-colors)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_layout ()
;;; 
;;; void gdk_draw_layout (GdkDrawable *drawable,
;;;                       GdkGC *gc,
;;;                       gint x,
;;;                       gint y,
;;;                       PangoLayout *layout);
;;; 
;;; Warning
;;; 
;;; gdk_draw_layout is deprecated and should not be used in newly-written code.
;;; 
;;; Render a PangoLayout onto a GDK drawable
;;; 
;;; If the layout's PangoContext has a transformation matrix set, then x and y
;;; specify the position of the top left corner of the bounding box (in device
;;; space) of the transformed layout.
;;; 
;;; If you're using GTK+, the usual way to obtain a PangoLayout is
;;; gtk_widget_create_pango_layout().
;;; 
;;; drawable :
;;; 	the drawable on which to draw string
;;; 
;;; gc :
;;; 	base graphics context to use
;;; 
;;; x :
;;; 	the X position of the left of the layout (in pixels)
;;; 
;;; y :
;;; 	the Y position of the top of the layout (in pixels)
;;; 
;;; layout :
;;; 	a PangoLayout
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_layout" gdk-draw-layout) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (layout (g-object pango-layout)))

(export 'gdk-draw-layout)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_layout_with_colors ()
;;; 
;;; void gdk_draw_layout_with_colors (GdkDrawable *drawable,
;;;                                   GdkGC *gc,
;;;                                   gint x,
;;;                                   gint y,
;;;                                   PangoLayout *layout,
;;;                                   const GdkColor *foreground,
;;;                                   const GdkColor *background);
;;; 
;;; Warning
;;; 
;;; gdk_draw_layout_with_colors is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Render a PangoLayout onto a GdkDrawable, overriding the layout's normal
;;; colors with foreground and/or background. foreground and background need
;;; not be allocated.
;;; 
;;; If the layout's PangoContext has a transformation matrix set, then x and y
;;; specify the position of the top left corner of the bounding box (in device
;;; space) of the transformed layout.
;;; 
;;; If you're using GTK+, the ususal way to obtain a PangoLayout is
;;; gtk_widget_create_pango_layout().
;;; 
;;; drawable :
;;; 	the drawable on which to draw string
;;; 
;;; gc :
;;; 	base graphics context to use
;;; 
;;; x :
;;; 	the X position of the left of the layout (in pixels)
;;; 
;;; y :
;;; 	the Y position of the top of the layout (in pixels)
;;; 
;;; layout :
;;; 	a PangoLayout
;;; 
;;; foreground :
;;; 	foreground override color, or NULL for none. [allow-none]
;;; 
;;; background :
;;; 	background override color, or NULL for none. [allow-none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_layout_with_colors" gdk-draw-layout-with-colors) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (x :int)
  (y :int)
  (layout (g-object pango-layout))
  (foreground (g-boxed-foreign gdk-color))
  (background (g-boxed-foreign gdk-color)))

(export 'gdk-draw-layout-with-colors)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_string ()
;;; 
;;; void gdk_draw_string (GdkDrawable *drawable,
;;;                       GdkFont *font,
;;;                       GdkGC *gc,
;;;                       gint x,
;;;                       gint y,
;;;                       const gchar *string);
;;; 
;;; Warning
;;; 
;;; gdk_draw_string has been deprecated since version 2.4 and should not be
;;; used in newly-written code. Use gdk_draw_layout() instead.
;;; 
;;; Draws a string of characters in the given font or fontset.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; font :
;;; 	a GdkFont.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x :
;;; 	the x coordinate of the left edge of the text.
;;; 
;;; y :
;;; 	the y coordinate of the baseline of the text.
;;; 
;;; string :
;;; 	the string of characters to draw.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_text ()
;;; 
;;; void gdk_draw_text (GdkDrawable *drawable,
;;;                     GdkFont *font,
;;;                     GdkGC *gc,
;;;                     gint x,
;;;                     gint y,
;;;                     const gchar *text,
;;;                     gint text_length);
;;; 
;;; Warning
;;; 
;;; gdk_draw_text has been deprecated since version 2.4 and should not be used
;;; in newly-written code. Use gdk_draw_layout() instead.
;;; 
;;; Draws a number of characters in the given font or fontset.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; font :
;;; 	a GdkFont.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x :
;;; 	the x coordinate of the left edge of the text.
;;; 
;;; y :
;;; 	the y coordinate of the baseline of the text.
;;; 
;;; text :
;;; 	the characters to draw.
;;; 
;;; text_length :
;;; 	the number of characters of text to draw.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_text_wc ()
;;; 
;;; void gdk_draw_text_wc (GdkDrawable *drawable,
;;;                        GdkFont *font,
;;;                        GdkGC *gc,
;;;                        gint x,
;;;                        gint y,
;;;                        const GdkWChar *text,
;;;                        gint text_length);
;;; 
;;; Warning
;;; 
;;; gdk_draw_text_wc has been deprecated since version 2.4 and should not be
;;; used in newly-written code. Use gdk_draw_layout() instead.
;;; 
;;; Draws a number of wide characters using the given font of fontset. If the
;;; font is a 1-byte font, the string is converted into 1-byte characters
;;; (discarding the high bytes) before output.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; font :
;;; 	a GdkFont.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x :
;;; 	the x coordinate of the left edge of the text.
;;; 
;;; y :
;;; 	the y coordinate of the baseline of the text.
;;; 
;;; text :
;;; 	the wide characters to draw.
;;; 
;;; text_length :
;;; 	the number of characters to draw.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_pixmap
;;; 
;;; #define gdk_draw_pixmap gdk_draw_drawable
;;; 
;;; Warning
;;; 
;;; gdk_draw_pixmap is deprecated and should not be used in newly-written code.
;;; Use gdk_draw_drawable() instead.
;;; 
;;; Draws a pixmap, or a part of a pixmap, onto another drawable.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_draw_drawable ()
;;; 
;;; void gdk_draw_drawable (GdkDrawable *drawable,
;;;                         GdkGC *gc,
;;;                         GdkDrawable *src,
;;;                         gint xsrc,
;;;                         gint ysrc,
;;;                         gint xdest,
;;;                         gint ydest,
;;;                         gint width,
;;;                         gint height);
;;; 
;;; Warning
;;; 
;;; gdk_draw_drawable has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use gdk_cairo_set_source_pixmap(),
;;; cairo_rectangle() and cairo_fill() to draw pixmap on top of other drawables.
;;; Also keep in mind that the limitations on allowed sources do not apply to
;;; Cairo.
;;; 
;;; Copies the width x height region of src at coordinates (xsrc, ysrc) to
;;; coordinates (xdest, ydest) in drawable. width and/or height may be given
;;; as -1, in which case the entire src drawable will be copied.
;;; 
;;; Most fields in gc are not used for this operation, but notably the clip
;;; mask or clip region will be honored.
;;; 
;;; The source and destination drawables must have the same visual and colormap,
;;; or errors will result. (On X11, failure to match visual/colormap results in
;;; a BadMatch error from the X server.) A common cause of this problem is an
;;; attempt to draw a bitmap to a color drawable. The way to draw a bitmap is
;;; to set the bitmap as the stipple on the GdkGC, set the fill mode to
;;; GDK_STIPPLED, and then draw the rectangle.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; gc :
;;; 	a GdkGC sharing the drawable's visual and colormap
;;; 
;;; src :
;;; 	the source GdkDrawable, which may be the same as drawable
;;; 
;;; xsrc :
;;; 	X position in src of rectangle to draw
;;; 
;;; ysrc :
;;; 	Y position in src of rectangle to draw
;;; 
;;; xdest :
;;; 	X position in drawable where the rectangle should be drawn
;;; 
;;; ydest :
;;; 	Y position in drawable where the rectangle should be drawn
;;; 
;;; width :
;;; 	width of rectangle to draw, or -1 for entire src width
;;; 
;;; height :
;;; 	height of rectangle to draw, or -1 for entire src height
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_drawable" gdk-draw-drawable) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (src (g-object gdk-drawable))
  (x-src :int)
  (y-src :int)
  (x-dest :int)
  (y-dest :int)
  (width :int)
  (height :int))

(export 'gdk-draw-drawable)

;;; ----------------------------------------------------------------------------
;;; gdk_draw_image ()
;;; 
;;; void gdk_draw_image (GdkDrawable *drawable,
;;;                      GdkGC *gc,
;;;                      GdkImage *image,
;;;                      gint xsrc,
;;;                      gint ysrc,
;;;                      gint xdest,
;;;                      gint ydest,
;;;                      gint width,
;;;                      gint height);
;;; 
;;; Warning
;;; 
;;; gdk_draw_image has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Do not use GdkImage anymore, instead use Cairo
;;; image surfaces.
;;; 
;;; Draws a GdkImage onto a drawable. The depth of the GdkImage must match the
;;; depth of the GdkDrawable.
;;; 
;;; drawable :
;;; 	a GdkDrawable (a GdkWindow or a GdkPixmap).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; image :
;;; 	the GdkImage to draw.
;;; 
;;; xsrc :
;;; 	the left edge of the source rectangle within image.
;;; 
;;; ysrc :
;;; 	the top of the source rectangle within image.
;;; 
;;; xdest :
;;; 	the x coordinate of the destination within drawable.
;;; 
;;; ydest :
;;; 	the y coordinate of the destination within drawable.
;;; 
;;; width :
;;; 	the width of the area to be copied, or -1 to make the area extend to
;;;     the right edge of image.
;;; 
;;; height :
;;; 	the height of the area to be copied, or -1 to make the area extend to
;;;     the bottom edge of image.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_draw_image" gdk-draw-image) :void
  (drawable (g-object gdk-drawable))
  (gc (g-object gdk-gc))
  (image (g-object gdk-image))
  (x-src :int)
  (y-src :int)
  (x-dest :int)
  (y-dest :int)
  (width :int)
  (height :int))

(export 'gdk-draw-image)

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_get_image ()
;;; 
;;; GdkImage * gdk_drawable_get_image (GdkDrawable *drawable,
;;;                                    gint x,
;;;                                    gint y,
;;;                                    gint width,
;;;                                    gint height);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_get_image has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use drawable as the source and draw to a
;;; Cairo image surface if you want to download contents to the client.
;;; 
;;; A GdkImage stores client-side image data (pixels). In contrast, GdkPixmap
;;; and GdkWindow are server-side objects. gdk_drawable_get_image() obtains the
;;; pixels from a server-side drawable as a client-side GdkImage. The format of
;;; a GdkImage depends on the GdkVisual of the current display, which makes
;;; manipulating GdkImage extremely difficult; therefore, in most cases you
;;; should use gdk_pixbuf_get_from_drawable() instead of this lower-level
;;; function. A GdkPixbuf contains image data in a canonicalized RGB format,
;;; rather than a display-dependent format. Of course, there's a convenience vs.
;;; speed tradeoff here, so you'll want to think about what makes sense for your
;;; application.
;;; 
;;; x, y, width, and height define the region of drawable to obtain as an image.
;;; 
;;; You would usually copy image data to the client side if you intend to
;;; examine the values of individual pixels, for example to darken an image or
;;; add a red tint. It would be prohibitively slow to make a round-trip request
;;; to the windowing system for each pixel, so instead you get all of them at
;;; once, modify them, then copy them all back at once.
;;; 
;;; If the X server or other windowing system backend is on the local machine,
;;; this function may use shared memory to avoid copying the image data.
;;; 
;;; If the source drawable is a GdkWindow and partially offscreen or obscured,
;;; then the obscured portions of the returned image will contain undefined
;;; data.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; x :
;;; 	x coordinate on drawable
;;; 
;;; y :
;;; 	y coordinate on drawable
;;; 
;;; width :
;;; 	width of region to get
;;; 
;;; height :
;;; 	height or region to get
;;; 
;;; Returns :
;;; 	a GdkImage containing the contents of drawable
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drawable_get_image" gdk-drawable-get-image) (g-object gdk-image)
  (drawable (g-object gdk-drawable))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-drawable-get-image)

;;; ----------------------------------------------------------------------------
;;; gdk_drawable_copy_to_image ()
;;; 
;;; GdkImage * gdk_drawable_copy_to_image (GdkDrawable *drawable,
;;;                                        GdkImage *image,
;;;                                        gint src_x,
;;;                                        gint src_y,
;;;                                        gint dest_x,
;;;                                        gint dest_y,
;;;                                        gint width,
;;;                                        gint height);
;;; 
;;; Warning
;;; 
;;; gdk_drawable_copy_to_image has been deprecated since version 2.22 and
;;; should not be used in newly-written code. Use drawable as the source and
;;; draw to a Cairo image surface if you want to download contents to the
;;; client.
;;; 
;;; Copies a portion of drawable into the client side image structure image.
;;; If image is NULL, creates a new image of size width x height and copies
;;; into that. See gdk_drawable_get_image() for further details.
;;; 
;;; drawable :
;;; 	a GdkDrawable
;;; 
;;; image :
;;; 	a GdkDrawable, or NULL if a new image should be created.
;;; 
;;; src_x :
;;; 	x coordinate on drawable
;;; 
;;; src_y :
;;; 	y coordinate on drawable
;;; 
;;; dest_x :
;;; 	x coordinate within image. Must be 0 if image is NULL
;;; 
;;; dest_y :
;;; 	y coordinate within image. Must be 0 if image is NULL
;;; 
;;; width :
;;; 	width of region to get
;;; 
;;; height :
;;; 	height or region to get
;;; 
;;; Returns :
;;; 	image, or a new a GdkImage containing the contents of drawable
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drawable_copy_to_image" gdk-drawable-copy-to-image)
    (g-object gdk-image)
  (drawable (g-object gdk-drawable))
  (image (g-object gdk-image))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(export 'gdk-drawable-copy-to-image)

;;; --- End of file gdk.drawable.lisp ------------------------------------------
