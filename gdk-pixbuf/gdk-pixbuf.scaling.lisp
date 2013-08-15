;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.scaling.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.28.1 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; Scaling
;;;
;;; Scaling pixbufs and scaling and compositing pixbufs
;;;
;;; Synopsis
;;;
;;;     GdkInterpType;
;;;
;;;     gdk_pixbuf_scale_simple
;;;     gdk_pixbuf_scale
;;;     gdk_pixbuf_composite_color_simple
;;;     gdk_pixbuf_composite
;;;     gdk_pixbuf_composite_color
;;;
;;;     GdkPixbufRotation
;;;
;;;     gdk_pixbuf_rotate_simple
;;;     gdk_pixbuf_flip
;;;
;;; Description
;;;
;;; The GdkPixBuf contains functions to scale pixbufs, to scale pixbufs and
;;; composite against an existing image, and to scale pixbufs and composite
;;; against a solid color or checkerboard. Compositing a checkerboard is a
;;; common way to show an image with an alpha channel in image-viewing and
;;; editing software.
;;;
;;; Since the full-featured functions (gdk_pixbuf_scale(),
;;; gdk_pixbuf_composite(), and gdk_pixbuf_composite_color()) are rather complex
;;; to use and have many arguments, two simple convenience functions are
;;; provided, gdk_pixbuf_scale_simple() and gdk_pixbuf_composite_color_simple()
;;; which create a new pixbuf of a given size, scale an original image to fit,
;;; and then return the new pixbuf.
;;;
;;; Scaling and compositing functions take advantage of MMX hardware
;;; acceleration on systems where MMX is supported. If gdk-pixbuf is built with
;;; the Sun mediaLib library, these functions are instead accelerated using
;;; mediaLib, which provides hardware acceleration on Intel, AMD, and Sparc
;;; chipsets. If desired, mediaLib support can be turned off by setting the
;;; GDK_DISABLE_MEDIALIB environment variable.
;;;
;;; The following example demonstrates handling an expose event by rendering the
;;; appropriate area of a source image (which is scaled to fit the widget) onto
;;; the widget's window. The source image is rendered against a checkerboard,
;;; which provides a visual representation of the alpha channel if the image has
;;; one. If the image doesn't have an alpha channel, calling
;;; gdk_pixbuf_composite_color() function has exactly the same effect as calling
;;; gdk_pixbuf_scale().
;;;
;;; Example 2. Handling an expose event.
;;;
;;; gboolean
;;; expose_cb (GtkWidget *widget, GdkEventExpose *event, gpointer data)
;;; {
;;;   GdkPixbuf *dest;
;;;
;;;   dest = gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8,
;;;                          event->area.width, event->area.height);
;;;
;;;   gdk_pixbuf_composite_color (pixbuf, dest,
;;;         0, 0, event->area.width, event->area.height,
;;;         -event->area.x, -event->area.y,
;;;         (double) widget->allocation.width / gdk_pixbuf_get_width (pixbuf),
;;;         (double) widget->allocation.height / gdk_pixbuf_get_height (pixbuf),
;;;         GDK_INTERP_BILINEAR, 255,
;;;         event->area.x, event->area.y, 16, 0xaaaaaa, 0x555555);
;;;
;;;   gdk_draw_pixbuf (widget->window, widget->style->fg_gc[GTK_STATE_NORMAL],
;;;                    dest, 0, 0,
;;;                    event->area.x, event->area.y,
;;;                    event->area.width, event->area.height,
;;;                    GDK_RGB_DITHER_NORMAL, event->area.x, event->area.y);
;;;
;;;   gdk_pixbuf_unref (dest);
;;;
;;;   return TRUE;
;;; }
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; enum GdkInterpType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInterpType" gdk-interp-type
  (:export t
   :type-initializer "gdk_interp_type_get_type")
  (:nearest 0)
  (:tiles 0)
  (:bilinear 0)
  (:hyper 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-interp-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-interp-type atdoc:*external-symbols*)
 "@version{2013-6-23}
  @begin{short}
    This enumeration describes the different interpolation modes that can be
    used with the scaling functions. @code{:nearest} is the fastest scaling
    method, but has horrible quality when scaling down. @code{:bilinear} is the
    best choice if you are not sure what to choose, it has a good speed/quality
    balance.
  @end{short}

  @subheading{Note}
    Cubic filtering is missing from the list; hyperbolic interpolation is just
    as fast and results in higher quality.
  @begin{pre}
(define-g-enum \"GdkInterpType\" gdk-interp-type
  (:export t
   :type-initializer \"gdk_interp_type_get_type\")
  (:nearest 0)
  (:tiles 0)
  (:bilinear 0)
  (:hyper 0))
  @end{pre}
  @begin[code]{table}
    @entry[:nearest]{Nearest neighbor sampling; this is the fastest and lowest
      quality mode. Quality is normally unacceptable when scaling down, but may
      be OK when scaling up.}
    @entry[:tiles]{This is an accurate simulation of the PostScript image
      operator without any interpolation enabled. Each pixel is rendered as a
      tiny parallelogram of solid color, the edges of which are implemented with
      antialiasing. It resembles nearest neighbor for enlargement, and bilinear
      for reduction.}
    @entry[:bilinear]{Best quality/speed balance; use this mode by default.
      Bilinear interpolation. For enlargement, it is equivalent to
      point-sampling the ideal bilinear-interpolated image. For reduction, it is
      equivalent to laying down small tiles and integrating over the coverage
      area.}
    @entry[:hyper]{This is the slowest and highest quality reconstruction
      function. It is derived from the hyperbolic filters in Wolberg's \"Digital
      Image Warping\", and is formally defined as the hyperbolic-filter sampling
      the ideal hyperbolic-filter interpolated image (the filter is designed to
      be idempotent for 1:1 pixel mapping).}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale_simple ()
;;;
;;; GdkPixbuf * gdk_pixbuf_scale_simple (const GdkPixbuf *src,
;;;                                      int dest_width,
;;;                                      int dest_height,
;;;                                      GdkInterpType interp_type);
;;;
;;; Create a new GdkPixbuf containing a copy of src scaled to dest_width x
;;; dest_height. Leaves src unaffected. interp_type should be GDK_INTERP_NEAREST
;;; if you want maximum speed (but when scaling down GDK_INTERP_NEAREST is
;;; usually unusably ugly). The default interp_type should be
;;; GDK_INTERP_BILINEAR which offers reasonable quality and speed.
;;;
;;; You can scale a sub-portion of src by creating a sub-pixbuf pointing into
;;; src; see gdk_pixbuf_new_subpixbuf().
;;;
;;; For more complicated scaling/compositing see gdk_pixbuf_scale() and
;;; gdk_pixbuf_composite().
;;;
;;; src :
;;;     a GdkPixbuf
;;;
;;; dest_width :
;;;     the width of destination image
;;;
;;; dest_height :
;;;     the height of destination image
;;;
;;; interp_type :
;;;     the interpolation type for the transformation.
;;;
;;; Returns :
;;;     The new GdkPixbuf, or NULL if not enough memory could be allocated
;;;     for it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_scale" gdk-pixbuf-scale) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest]{the @class{gdk-pixbuf} into which to render the results}
  @argument[dest-x]{the left coordinate for region to render}
  @argument[dest-y]{the top coordinate for region to render}
  @argument[dest-width]{the width of the region to render}
  @argument[dest-height]{the height of the region to render}
  @argument[offset-x]{the offset in the x direction (currently rounded to an
    integer)}
  @argument[offset-y]{the offset in the y direction (currently rounded to an
    integer)}
  @argument[scale-x]{the scale factor in the x direction}
  @argument[scale-y]{the scale factor in the y direction}
  @argument[interp-type]{the interpolation type for the transformation}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{scale-x} and @arg{scale-y} then translating by @arg{offset-x} and
    @arg{offset-y}, then renders the rectangle (@arg{dest-x}, @arg{dest-y},
    @arg{dest-width}, @arg{dest-height}) of the resulting image onto the
    destination image replacing the previous contents.
  @end{short}

  Try to use the function @fun{gdk-pixbuf-scale-simple} first, this function is
  the industrial-strength power tool you can fall back to if the function
  @fun{gdk-pixbuf-scale-simple} is not powerful enough.

  If the source rectangle overlaps the destination rectangle on the same
  pixbuf, it will be overwritten during the scaling which results in rendering
  artifacts.
  @see-function{gdk-pixbuf-scale-simple}"
  (src (g-object gdk-pixbuf))
  (dest (g-object gdk-pixbuf))
  (dest-x :int)
  (dest-y :int)
  (dest-width :int)
  (dest-height :int)
  (offset-x :double)
  (offset-y :double)
  (scale-x :double)
  (scale-y :double)
  (interp-type gdk-interp-type))

(export 'gdk-pixbuf-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite_color_simple ()
;;;
;;; GdkPixbuf * gdk_pixbuf_composite_color_simple (const GdkPixbuf *src,
;;;                                                int dest_width,
;;;                                                int dest_height,
;;;                                                GdkInterpType interp_type,
;;;                                                int overall_alpha,
;;;                                                int check_size,
;;;                                                guint32 color1,
;;;                                                guint32 color2);
;;;
;;; Creates a new GdkPixbuf by scaling src to dest_width x dest_height and
;;; compositing the result with a checkboard of colors color1 and color2.
;;;
;;; src :
;;;     a GdkPixbuf
;;;
;;; dest_width :
;;;     the width of destination image
;;;
;;; dest_height :
;;;     the height of destination image
;;;
;;; interp_type :
;;;     the interpolation type for the transformation.
;;;
;;; overall_alpha :
;;;     overall alpha for source image (0..255)
;;;
;;; check_size :
;;;     the size of checks in the checkboard (must be a power of two)
;;;
;;; color1 :
;;;     the color of check at upper left
;;;
;;; color2 :
;;;     the color of the other check
;;;
;;; Returns :
;;;     The new GdkPixbuf, or NULL if not enough memory could be allocated for
;;;     it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_composite" gdk-pixbuf-composite) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-23}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest]{the @class{gdk-pixbuf} into which to render the results}
  @argument[dest-x]{the left coordinate for region to render}
  @argument[dest-y]{the top coordinate for region to render}
  @argument[dest-width]{the width of the region to render}
  @argument[dest-height]{the height of the region to render}
  @argument[offset-x]{the offset in the x direction (currently rounded to an
    integer)}
  @argument[offset-y]{the offset in the y direction (currently rounded to an
    integer)}
  @argument[scale-x]{the scale factor in the x direction}
  @argument[scale-y]{the scale factor in the y direction}
  @argument[interp-type]{the interpolation type for the transformation}
  @argument[overall-alpha]{overall alpha for source image (0 .. 255)}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{scale-x} and @arg{scale-y} then translating by @arg{offset-x} and
    @arg{offset-y}. This gives an image in the coordinates of the destination
    pixbuf. The rectangle (@arg{dest-x}, @arg{dest-y}, @arg{dest-width},
    @arg{dest-height}) is then composited onto the corresponding rectangle of
    the original destination image.
  @end{short}

  When the destination rectangle contains parts not in the source image, the
  data at the edges of the source image is replicated to infinity."
  (src (g-object gdk-pixbuf))
  (dest (g-object gdk-pixbuf))
  (dest-x :int)
  (dest-y :int)
  (dest-width :int)
  (dest-height :int)
  (offset-x :double)
  (offset-y :double)
  (scale-x :double)
  (scale-y :double)
  (interp-type gdk-interp-type)
  (overall-alpha :int))

(export 'gdk-pixbuf-composite)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite_color ()
;;;
;;; void gdk_pixbuf_composite_color (const GdkPixbuf *src,
;;;                                  GdkPixbuf *dest,
;;;                                  int dest_x,
;;;                                  int dest_y,
;;;                                  int dest_width,
;;;                                  int dest_height,
;;;                                  double offset_x,
;;;                                  double offset_y,
;;;                                  double scale_x,
;;;                                  double scale_y,
;;;                                  GdkInterpType interp_type,
;;;                                  int overall_alpha,
;;;                                  int check_x,
;;;                                  int check_y,
;;;                                  int check_size,
;;;                                  guint32 color1,
;;;                                  guint32 color2);
;;;
;;; Creates a transformation of the source image src by scaling by scale_x and
;;; scale_y then translating by offset_x and offset_y, then composites the
;;; rectangle (dest_x ,dest_y, dest_width, dest_height) of the resulting image
;;; with a checkboard of the colors color1 and color2 and renders it onto the
;;; destination image.
;;;
;;; See gdk_pixbuf_composite_color_simple() for a simpler variant of this
;;; function suitable for many tasks.
;;;
;;; src :
;;;     a GdkPixbuf
;;;
;;; dest :
;;;     the GdkPixbuf into which to render the results
;;;
;;; dest_x :
;;;     the left coordinate for region to render
;;;
;;; dest_y :
;;;     the top coordinate for region to render
;;;
;;; dest_width :
;;;     the width of the region to render
;;;
;;; dest_height :
;;;     the height of the region to render
;;;
;;; offset_x :
;;;     the offset in the X direction (currently rounded to an integer)
;;;
;;; offset_y :
;;;     the offset in the Y direction (currently rounded to an integer)
;;;
;;; scale_x :
;;;     the scale factor in the X direction
;;;
;;; scale_y :
;;;     the scale factor in the Y direction
;;;
;;; interp_type :
;;;     the interpolation type for the transformation.
;;;
;;; overall_alpha :
;;;     overall alpha for source image (0..255)
;;;
;;; check_x :
;;;     the X offset for the checkboard (origin of checkboard is at
;;;     -check_x, -check_y)
;;;
;;; check_y :
;;;     the Y offset for the checkboard
;;;
;;; check_size :
;;;     the size of checks in the checkboard (must be a power of two)
;;;
;;; color1 :
;;;     the color of check at upper left
;;;
;;; color2 :
;;;     the color of the other check
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufRotation
;;;
;;; typedef enum {
;;;     GDK_PIXBUF_ROTATE_NONE             =   0,
;;;     GDK_PIXBUF_ROTATE_COUNTERCLOCKWISE =  90,
;;;     GDK_PIXBUF_ROTATE_UPSIDEDOWN       = 180,
;;;     GDK_PIXBUF_ROTATE_CLOCKWISE        = 270
;;; } GdkPixbufRotation;
;;;
;;; The possible rotations which can be passed to gdk_pixbuf_rotate_simple().
;;; To make them easier to use, their numerical values are the actual degrees.
;;;
;;; GDK_PIXBUF_ROTATE_NONE
;;;     No rotation.
;;;
;;; GDK_PIXBUF_ROTATE_COUNTERCLOCKWISE
;;;     Rotate by 90 degrees.
;;;
;;; GDK_PIXBUF_ROTATE_UPSIDEDOWN
;;;     Rotate by 180 degrees.
;;;
;;; GDK_PIXBUF_ROTATE_CLOCKWISE
;;;     Rotate by 270 degrees.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_rotate_simple ()
;;;
;;; GdkPixbuf * gdk_pixbuf_rotate_simple (const GdkPixbuf *src,
;;;                                       GdkPixbufRotation angle);
;;;
;;; Rotates a pixbuf by a multiple of 90 degrees, and returns the result in a
;;; new pixbuf.
;;;
;;; src :
;;;     a GdkPixbuf
;;;
;;; angle :
;;;     the angle to rotate by
;;;
;;; Returns :
;;;     the new GdkPixbuf, or NULL if not enough memory could be allocated
;;;     for it.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_flip ()
;;;
;;; GdkPixbuf * gdk_pixbuf_flip (const GdkPixbuf *src, gboolean horizontal);
;;;
;;; Flips a pixbuf horizontally or vertically and returns the result in a
;;; new pixbuf.
;;;
;;; src :
;;;     a GdkPixbuf
;;;
;;; horizontal :
;;;     TRUE to flip horizontally, FALSE to flip vertically
;;;
;;; Returns :
;;;     The new GdkPixbuf, or NULL if not enough memory could be allocated
;;;     for it.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.scaling.lisp ------------------------------------
