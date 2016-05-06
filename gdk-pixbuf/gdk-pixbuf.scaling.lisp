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
;;;     GdkInterpType
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
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; enum GdkInterpType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkInterpType" gdk-interp-type
  (:export t
   :type-initializer "gdk_interp_type_get_type")
  (:nearest 0)
  (:tiles 1)
  (:bilinear 2)
  (:hyper 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-interp-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-interp-type atdoc:*external-symbols*)
 "@version{2013-9-1}
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
  (:tiles 1)
  (:bilinear 2)
  (:hyper 3))
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
      the ideal hyperbolic-filter interpolated image. The filter is designed to
      be idempotent for 1:1 pixel mapping.}
  @end{table}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale_simple ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_scale_simple" gdk-pixbuf-scale-simple)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-2}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest-width]{the width of destination image}
  @argument[dest-height]{the height of destination image}
  @argument[interp-type]{the interpolation type for the transformation}
  @begin{return}
    The new @class{gdk-pixbuf} object, or @code{nil} if not enough memory could
    be allocated for it.
  @end{return}
  @begin{short}
    Create a new @class{gdk-pixbuf} object containing a copy of @arg{src} scaled
    to @arg{dest-width} @code{x} @arg{dest-height}.
  @end{short}
  Leaves @arg{src} unaffected. @arg{interp-type} should be @code{:nearest} if
  you want maximum speed, but when scaling down @code{:nearest} is usually
  unusably ugly. The default @arg{interp-type} should be @code{:bilinear}  which
  offers reasonable quality and speed.

  You can scale a sub-portion of @arg{src} by creating a sub-pixbuf pointing
  into @arg{src}; see the function @fun{gdk-pixbuf-new-subpixbuf}.

  For more complicated scaling/compositing see the functions
  @fun{gdk-pixbuf-scale} and @fun{gdk-pixbuf-composite}.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-new-subpixbuf}
  @see-function{gdk-pixbuf-scale}
  @see-function{gdk-pixbuf-composite}"
  (src (g-object gdk-pixbuf))
  (dest-width :int)
  (dest-height :int)
  (interp-type gdk-interp-type))

(export 'gdk-pixbuf-scale-simple)

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
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_composite_color_simple" gdk-pixbuf-composite-color-simple)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest-width]{the width of destination image}
  @argument[dest-height]{the height of destination image}
  @argument[interp-type]{the interpolation type for the transformation}
  @argument[overall-alpha]{overall alpha for source image (0..255)}
  @argument[check-size]{the size of checks in the checkboard (must be a power
    of two)}
  @argument[color1]{the color of check at upper left}
  @argument[color2]{the color of the other check}
  @begin{return}
    The new @class{gdk-pixbuf}, or @code{nil} if not enough memory could be
    allocated for it.
  @end{return}
  Creates a new @class{gdk-pixbuf} by scaling @arg{src} to @arg{dest-width}
  @code{x} @arg{dest-height} and compositing the result with a checkboard of
  colors @arg{color1} and @arg{color2}.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-composite-color}"
  (src (g-object gdk-pixbuf))
  (dest-width :int)
  (dest-height :int)
  (interp-type gdk-interp-type)
  (overall-alpha :int)
  (check-size :int)
  (color1 :uint32)
  (color2 :uint32))

(export 'gdk-pixbuf-composite-color-simple)

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
  data at the edges of the source image is replicated to infinity.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-composite-color}
  @see-function{gdk-pixbuf-composite-color-simple}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_composite_color" gdk-pixbuf-composite-color)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest]{the @class{gdk-pixbuf} object into which to render the results}
  @argument[dest-x]{the left coordinate for region to render}
  @argument[dest-y]{the top coordinate for region to render}
  @argument[dest-width]{the width of the region to render}
  @argument[dest-height]{the height of the region to render}
  @argument[offset-x]{the offset in the x direction, currently rounded to an
    integer}
  @argument[offset-y]{the offset in the y direction, currently rounded to an
    integer}
  @argument[scale-x]{the scale factor in the x direction}
  @argument[scale-y]{the scale factor in the y direction}
  @argument[interp-type]{the interpolation type for the transformation}
  @argument[overall-alpha]{overall alpha for source image (0..255)}
  @argument[check-x]{the x offset for the checkboard, origin of checkboard is at
    @code{(-@arg{check-x}, -@arg{check-y})}}
  @argument[check-y]{the y offset for the checkboard}
  @argument[check-size]{the size of checks in the checkboard, must be a power of
    two}
  @argument[color1]{the color of check at upper left}
  @argument[color2]{the color of the other check}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{scale-x} and @arg{scale-y} then translating by @arg{offset-x} and
    @arg{offset-y}, then composites the rectangle @code{(@arg{dest-x},
    @arg{dest-y}, @arg{dest-width}, @arg{dest-height})} of the resulting image
    with a checkboard of the colors @arg{color1} and @arg{color2} and renders it
    onto the destination image.
  @end{short}

  See the function @fun{gdk-pixbuf-composite-color-simple} for a simpler variant
  of this function suitable for many tasks.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-composite-color-simple}"
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
  (overall-alpha :int)
  (check-x :int)
  (check-y :int)
  (check-size :int)
  (color1 :uint32)
  (color2 :uint32))

(export 'gdk-pixbuf-composite-color)

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

(define-g-enum "GdkPixbufRotation" gdk-pixbuf-rotation
  (:export t
   :type-initializer "gdk_pixbuf_rotation_get_type")
  (:none 0)
  (:counterclockwise 90)
  (:upsidedown 180)
  (:clockwise 270))

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

(defcfun ("gdk_pixbuf_rotate_simple" gdk-pixbuf-rotate-simple)
    (g-object gdk-pixbuf)
  (src (g-object gdk-pixbuf))
  (angle gdk-pixbuf-rotation))

(export 'gdk-pixbuf-rotate-simple)

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

(defcfun ("gdk_pixbuf_flip" gdk-pixbuf-flip)
    (g-object gdk-pixbuf)
  (src (g-object gdk-pixbuf))
  (horizontal :boolean))

(export 'gdk-pixbuf-flip)

;;; --- End of file gdk-pixbuf.scaling.lisp ------------------------------------
