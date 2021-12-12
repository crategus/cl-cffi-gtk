;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.scaling.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     Scaling pixbufs and scaling and compositing pixbufs
;;;
;;; Types and Values
;;;
;;;     GdkInterpType
;;;     GdkPixbufRotation
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_scale_simple
;;;     gdk_pixbuf_scale
;;;     gdk_pixbuf_composite_color_simple
;;;     gdk_pixbuf_composite
;;;     gdk_pixbuf_composite_color
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
(setf (gethash 'gdk-interp-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gdk-interp-type atdoc:*external-symbols*)
 "@version{2021-12-12}
  @begin{short}
    This enumeration describes the different interpolation modes that can be
    used with the scaling functions.
  @end{short}
  The @code{:nearest} mode is the fastest scaling method, but has horrible
  quality when scaling down. The @code{:bilinear} mode is the best choice if
  you are not sure what to choose, it has a good speed/quality balance.
  @begin[Note]{dictionary}
    Cubic filtering is missing from the list. Hyperbolic interpolation is just
    as fast and results in higher quality.
  @end{dictionary}
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
    @entry[:nearest]{Nearest neighbor sampling: This is the fastest and lowest
      quality mode. Quality is normally unacceptable when scaling down, but may
      be fine when scaling up.}
    @entry[:tiles]{This is an accurate simulation of the PostScript image
      operator without any interpolation enabled. Each pixel is rendered as a
      tiny parallelogram of solid color, the edges of which are implemented with
      antialiasing. It resembles nearest neighbor for enlargement, and bilinear
      for reduction.}
    @entry[:bilinear]{Bilinear interpolation: Best quality/speed balance. Use
      this mode by default. For enlargement, it is equivalent to point-sampling
      the ideal bilinear-interpolated image. For reduction, it is equivalent to
      laying down small tiles and integrating over the coverage area.}
    @entry[:hyper]{This is the slowest and highest quality reconstruction
      function. It is derived from the hyperbolic filters in Wolberg's \"Digital
      Image Warping\", and is formally defined as the hyperbolic-filter sampling
      the ideal hyperbolic-filter interpolated image. The filter is designed to
      be idempotent for 1:1 pixel mapping.
      @em{Deprecated:} This interpolation filter is deprecated since 2.38, as in
      reality it has a lower quality than the @code{:bilinear} filter.}
  @end{table}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufRotation
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkPixbufRotation" gdk-pixbuf-rotation
  (:export t
   :type-initializer "gdk_pixbuf_rotation_get_type")
  (:none 0)
  (:counterclockwise 90)
  (:upsidedown 180)
  (:clockwise 270))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-rotation atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gdk-pixbuf-rotation atdoc:*external-symbols*)
 "@version{2021-12-12}
  @begin{short}
    The possible rotations which can be passed to the
    @fun{gdk-pixbuf-rotate-simple} function.
  @end{short}
  To make them easier to use, their numerical values are the actual degrees.
  @begin{pre}
(define-g-enum \"GdkPixbufRotation\" gdk-pixbuf-rotation
  (:export t
   :type-initializer \"gdk_pixbuf_rotation_get_type\")
  (:none 0)
  (:counterclockwise 90)
  (:upsidedown 180)
  (:clockwise 270))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No rotation.}
    @entry[:counterclockwise]{Rotate by 90 degrees.}
    @entry[:upsidedown]{Rotate by 180 degrees.}
    @entry[:clockwise]{Rotate by 270 degrees.}
  @end{table}
  @see-class{gdk-pixbuf}
  @see-function{gdk-pixbuf-rotate-simple}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale_simple ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_scale_simple" gdk-pixbuf-scale-simple)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[width]{an integer with the width of destination image}
  @argument[height]{an integer with the height of destination image}
  @argument[interp]{a @symbol{gdk-interp-type} interpolation type for the
    transformation}
  @begin{return}
    The new @class{gdk-pixbuf} object, or @code{nil} if not enough memory could
    be allocated for it.
  @end{return}
  @begin{short}
    Create a new @class{gdk-pixbuf} object containing a copy of @arg{src} scaled
    to @arg{width} @code{x} @arg{height}.
  @end{short}
  Leaves @arg{src} unaffected. The @arg{interp} mode should be @code{:nearest}
  if you want maximum speed, but when scaling down the @code{:nearest} mode is
  usually unusably ugly. The default @arg{interp} mode should be
  @code{:bilinear} which offers reasonable quality and speed.

  You can scale a sub-portion of @arg{src} by creating a sub-pixbuf pointing
  into @arg{src}, see the @fun{gdk-pixbuf-new-subpixbuf} function.

  For more complicated scaling/compositing see the @fun{gdk-pixbuf-scale} and
  @fun{gdk-pixbuf-composite} functions.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-new-subpixbuf}
  @see-function{gdk-pixbuf-scale}
  @see-function{gdk-pixbuf-composite}"
  (src (g-object gdk-pixbuf))
  (width :int)
  (height :int)
  (interp gdk-interp-type))

(export 'gdk-pixbuf-scale-simple)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_scale" %gdk-pixbuf-scale) :void
  (src (g-object gdk-pixbuf))
  (dest (g-object gdk-pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (xoffset :double)
  (yoffset :double)
  (xscale :double)
  (yscale :double)
  (interp gdk-interp-type))

(defun gdk-pixbuf-scale (src dest
                         x y width height
                         xoffset yoffset
                         xscale yscale
                         interp)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest]{a @class{gdk-pixbuf} object into which to render the
    results}
  @argument[x]{an integer with the left coordinate for region to render}
  @argument[y]{an integer with the top coordinate for region to render}
  @argument[width]{an integer with the width of the region to render}
  @argument[height]{an integer with the height of the region to render}
  @argument[xoffset]{a double float with the offset in the x direction,
    currently rounded to an integer}
  @argument[yoffset]{a double float with the offset in the y direction,
    currently rounded to an integer}
  @argument[xscale]{a double float with the scale factor in the x direction}
  @argument[yscale]{a double float with the scale factor in the y direction}
  @argument[interp]{a @symbol{gdk-interp-type} interpolation type for the
    transformation}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{xscale} and @arg{yscale} then translating by @arg{xoffset} and
    @arg{yoffset}, then renders the rectangle (@arg{x}, @arg{y}, @arg{width},
    @arg{height}) of the resulting image onto the destination image replacing
    the previous contents.
  @end{short}

  Try to use the @fun{gdk-pixbuf-scale-simple} function first, this function is
  the industrial-strength power tool you can fall back to if the
  @fun{gdk-pixbuf-scale-simple} function is not powerful enough.

  If the source rectangle overlaps the destination rectangle on the same
  pixbuf, it will be overwritten during the scaling which results in rendering
  artifacts.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-scale-simple}"
  (%gdk-pixbuf-scale src dest x y width height
                     (coerce xoffset 'double-float)
                     (coerce yoffset 'double-float)
                     (coerce xscale 'double-float)
                     (coerce yscale 'double-float)
                     interp))

(export 'gdk-pixbuf-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite_color_simple ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_composite_color_simple" gdk-pixbuf-composite-color-simple)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[width]{an integer with the width of destination image}
  @argument[height]{an integer with the height of destination image}
  @argument[interp]{a @symbol{gdk-interp-type} interpolation type for the
    transformation}
  @argument[alpha]{an integer with the overall alpha for source image (0..255)}
  @argument[size]{an integer with the size of checks in the checkboard, must be
    a power of two}
  @argument[color1]{an unsigned integer with the color of check at upper left}
  @argument[color2]{an unsigned integer with the color of the other check}
  @begin{return}
    The new @class{gdk-pixbuf} object, or @code{nil} if not enough memory could
    be allocated for it.
  @end{return}
  @begin{short}
    Creates a new @class{gdk-pixbuf} object by scaling @arg{src} to @arg{width}
    @code{x} @arg{height} and compositing the result with a checkboard of colors
    @arg{color1} and @arg{color2}.
  @end{short}
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-composite-color}"
  (src (g-object gdk-pixbuf))
  (width :int)
  (height :int)
  (interp gdk-interp-type)
  (alpha :int)
  (size :int)
  (color1 :uint32)
  (color2 :uint32))

(export 'gdk-pixbuf-composite-color-simple)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_composite" %gdk-pixbuf-composite) :void
  (src (g-object gdk-pixbuf))
  (dest (g-object gdk-pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (xoffset :double)
  (yoffset :double)
  (xscale :double)
  (yscale :double)
  (interp gdk-interp-type)
  (alpha :int))

(defun gdk-pixbuf-composite (src dest x y width height
                             xoffset yoffset
                             xscale yscale
                             interp
                             alpha)
#+cl-cffi-gtk-documentation
 "@version{*2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest]{a @class{gdk-pixbuf} object into which to render the
    results}
  @argument[x]{an integer with the left coordinate for region to render}
  @argument[y]{an integer with the top coordinate for region to render}
  @argument[width]{an integer with the width of the region to render}
  @argument[height]{an integer with the height of the region to render}
  @argument[xoffset]{a double float with the offset in the x direction,
    currently rounded to an integer}
  @argument[yoffset]{a double float with the offset in the y direction,
    currently rounded to an integer}
  @argument[xscale]{a double float with the scale factor in the x direction}
  @argument[yscale]{a double float with the scale factor in the y direction}
  @argument[interp]{a @symbol{gdk-interp-type} interpolation type for the
    transformation}
  @argument[alpha]{an integer with the overall alpha for source image (0..255)}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{xscale} and @arg{yscale} then translating by @arg{xoffset} and
    @arg{yoffset}.
  @end{short}
  This gives an image in the coordinates of the destination pixbuf. The
  rectangle (@arg{x}, @arg{y}, @arg{width}, @arg{height}) is then composited
  onto the corresponding rectangle of the original destination image.

  When the destination rectangle contains parts not in the source image, the
  data at the edges of the source image is replicated to infinity.

  @image[composite]{}

  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-composite-color}
  @see-function{gdk-pixbuf-composite-color-simple}"
  (%gdk-pixbuf-composite src dest x y width height
                         (coerce xoffset 'double-float)
                         (coerce yoffset 'double-float)
                         (coerce xscale 'double-float)
                         (coerce yscale 'double-float)
                         interp
                         alpha))

(export 'gdk-pixbuf-composite)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_composite_color" %gdk-pixbuf-composite-color)
    (g-object gdk-pixbuf)
  (src (g-object gdk-pixbuf))
  (dest (g-object gdk-pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (xoffset :double)
  (yoffset :double)
  (xscale :double)
  (yscale :double)
  (interp gdk-interp-type)
  (alpha :int)
  (xcheck :int)
  (ycheck :int)
  (size :int)
  (color1 :uint32)
  (color2 :uint32))

(defun gdk-pixbuf-composite-color (src dest x y width height
                                   xoffset yoffset
                                   xscale yscale
                                   interp
                                   alpha
                                   xcheck ycheck
                                   size
                                   color1 color2)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[dest]{a @class{gdk-pixbuf} object into which to render the
    results}
  @argument[x]{an integer with the left coordinate for region to render}
  @argument[y]{an integer with the top coordinate for region to render}
  @argument[width]{an integer with the width of the region to render}
  @argument[height]{an integer with the height of the region to render}
  @argument[xoffset]{a double float with the offset in the x direction,
    currently rounded to an integer}
  @argument[yoffset]{a double float with the offset in the y direction,
    currently rounded to an integer}
  @argument[xscale]{a double float with the scale factor in the x direction}
  @argument[yscale]{a double float with the scale factor in the y direction}
  @argument[interp]{a @symbol{gdk-interp-type} interpolation type for the
    transformation}
  @argument[alpha]{an integer with the overall alpha for source image (0..255)}
  @argument[xcheck]{an integer with the x offset for the checkboard, origin of
    checkboard is at @code{(-@arg{xcheck}, -@arg{ycheck})}}
  @argument[ycheck]{an integer with the y offset for the checkboard}
  @argument[size]{an integer with the size of checks in the checkboard, must be
    a power of two}
  @argument[color1]{an unsigned integer with the color of check at upper left}
  @argument[color2]{an unsigned integer with the color of the other check}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{xscale} and @arg{yscale} then translating by @arg{xoffset} and
    @arg{yoffset}, then composites the rectangle @code{(@arg{x}, @arg{y},
    @arg{width}, @arg{height})} of the resulting image with a checkboard of the
    colors @arg{color1} and @arg{color2} and renders it onto the destination
    image.
  @end{short}

  See the @fun{gdk-pixbuf-composite-color-simple} function for a simpler variant
  of this function suitable for many tasks.
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-interp-type}
  @see-function{gdk-pixbuf-composite-color-simple}"
  (%gdk-pixbuf-composite-color src dest x y width height
                                   (coerce xoffset 'double-float)
                                   (coerce yoffset 'double-float)
                                   (coerce xscale 'double-float)
                                   (coerce yscale 'double-float)
                                   interp
                                   alpha
                                   xcheck ycheck
                                   size
                                   color1 color2))

(export 'gdk-pixbuf-composite-color)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_rotate_simple ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_rotate_simple" gdk-pixbuf-rotate-simple)
    (g-object gdk-pixbuf)
#+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[angle]{a @symbol{gdk-pixbuf-rotation} value}
  @return{The new @class{gdk-pixbuf} object, or @code{nil} if not enough memory
    could be allocated for it.}
  @begin{short}
    Rotates a pixbuf by a multiple of 90 degrees, and returns the result in a
    new pixbuf.
  @end{short}
  @see-class{gdk-pixbuf}
  @see-symbol{gdk-pixbuf-rotation}"
  (src (g-object gdk-pixbuf))
  (angle gdk-pixbuf-rotation))

(export 'gdk-pixbuf-rotate-simple)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_flip ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_flip" gdk-pixbuf-flip) (g-object gdk-pixbuf)
#+cl-cffi-gtk-documentation
 "@version{2021-12-12}
  @argument[src]{a @class{gdk-pixbuf} object}
  @argument[horizontal]{@em{true} to flip horizontally, @em{false} to flip
    vertically}
  @begin{short}
    Flips a pixbuf horizontally or vertically and returns the result in a new
    pixbuf.
  @end{short}
  @see-class{gdk-pixbuf}"
  (src (g-object gdk-pixbuf))
  (horizontal :boolean))

(export 'gdk-pixbuf-flip)

;;; --- End of file gdk-pixbuf.scaling.lisp ------------------------------------
