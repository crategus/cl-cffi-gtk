;;; ----------------------------------------------------------------------------
;;; gdk.visual.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Visuals
;;;
;;; Low-level display hardware information
;;;
;;; Synopsis
;;;
;;;     GdkVisual
;;;     GdkVisualType
;;;     GdkByteOrder
;;;
;;;     gdk_query_depths
;;;     gdk_query_visual_types
;;;     gdk_list_visuals
;;;     gdk_visual_get_bits_per_rgb
;;;     gdk_visual_get_blue_pixel_details
;;;     gdk_visual_get_byte_order
;;;     gdk_visual_get_colormap_size
;;;     gdk_visual_get_depth
;;;     gdk_visual_get_green_pixel_details
;;;     gdk_visual_get_red_pixel_details
;;;     gdk_visual_get_visual_type
;;;     gdk_visual_get_best_depth
;;;     gdk_visual_get_best_type
;;;     gdk_visual_get_system
;;;     gdk_visual_get_best
;;;     gdk_visual_get_best_with_depth
;;;     gdk_visual_get_best_with_type
;;;     gdk_visual_get_best_with_both
;;;     gdk_visual_get_screen
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkVisual
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkVisual
;;; ----------------------------------------------------------------------------


(define-g-object-class "GdkVisual" gdk-visual
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_visual_get_type")
  nil)

(setf (documentation 'gdk-visual 'type)
 "@version{2013-7-29}
  @begin{short}
    A @sym{gdk-visual} describes a particular video hardware display format. It
    includes information about the number of bits used for each color, the way
    the bits are translated into an RGB value for display, and the way the bits
    are stored in memory. For example, a piece of display hardware might support
    24-bit color, 16-bit color, or 8-bit color; meaning 24/16/8-bit pixel sizes.
    For a given pixel size, pixels can be in different formats; for example the
    \"red\" element of an RGB pixel may be in the top 8 bits of the pixel, or
    may be in the lower 4 bits.
  @end{short}

  There are several standard visuals. The visual returned by
  @fun{gdk-screen-get-system-visual} is the system's default visual.

  A number of functions are provided for determining the \"best\" available
  visual. For the purposes of making this determination, higher bit depths are
  considered better, and for visuals of the same bit depth,
  @code{:pseudo-color} is preferred at 8bpp, otherwise, the visual types
  are ranked in the order of highest to lowest @code{:direct-color},
  @code{:true-color}, @code{:pseudo-color}, @code{:static-color},
  @code{:grayscale}, then @code{:static-gray}.
  @see-function{gdk-screen-get-system-visual}")

;;; ----------------------------------------------------------------------------

(defmethod print-object ((visual gdk-visual) stream)
  (print-unreadable-object (visual stream :type t :identity t)
    (format stream "~S at ~S bpp"
                   (gdk-visual-get-visual-type visual)
                   (gdk-visual-get-depth visual))))

;;; ----------------------------------------------------------------------------
;;; enum GdkVisualType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkVisualType" gdk-visual-type
  (:export t
   :type-initializer "gdk_visual_type_get_type")
  (:static-gray 0)
  (:grayscale 1)
  (:static-color 2)
  (:pseudo-color 3)
  (:true-color 4)
  (:direct-color 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-visual-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-visual-type atdoc:*external-symbols*)
 "@version{2013-7-29}
  @begin{short}
    A set of values that describe the manner in which the pixel values for a
    visual are converted into RGB values for display.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkVisualType\" gdk-visual-type
  (:export t
   :type-initializer \"gdk_visual_type_get_type\")
  (:static-gray 0)
  (:grayscale 1)
  (:static-color 2)
  (:pseudo-color 3)
  (:true-color 4)
  (:direct-color 5))
  @end{pre}
  @begin[code]{table}
    @entry[:static-gray]{Each pixel value indexes a grayscale value directly.}
    @entry[:grayscale]{Each pixel is an index into a color map that maps pixel
      values into grayscale values. The color map can be changed by an
      application.}
    @entry[:static-color]{Each pixel value is an index into a predefined,
      unmodifiable color map that maps pixel values into RGB values.}
    @entry[:pseudo-color]{Each pixel is an index into a color map that maps
      pixel values into RGB values. The color map can be changed by an
      application.}
    @entry[:true-color]{Each pixel value directly contains red, green, and blue
      components. Use @fun{gdk-visual-get-red-pixel-details}, etc, to obtain
      information about how the components are assembled into a pixel value.}
    @entry[:direct-color]{Each pixel value contains red, green, and blue
      components as for @code{:true-color}, but the components are mapped via a
      color table into the final output table instead of being converted
      directly.}
  @end{table}
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-red-pixel-details}
  @see-function{gdk-visual-get-blue-pixel-details}
  @see-function{gdk-visual-get-green-pixel-details}")

;;; ----------------------------------------------------------------------------
;;; enum GdkByteOrder
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkByteOrder" gdk-byte-order
  (:export t
   :type-initializer "gdk_byte_order_get_type")
  (:lsb-first 0)
  (:msb-first 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-byte-order atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-byte-order atdoc:*external-symbols*)
 "@version{2013-7-29}
  @begin{short}
    A set of values describing the possible byte-orders for storing pixel values
    in memory.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkByteOrder\" gdk-byte-order
  (:export t
   :type-initializer \"gdk_byte_order_get_type\")
  (:lsb-first 0)
  (:msb-first 1))
  @end{pre}
  @begin[code]{table}
    @entry[:lsb-first]{The values are stored with the least-significant byte
      first. For instance, the 32-bit value 0xffeecc would be stored in memory
      as 0xcc, 0xee, 0xff, 0x00.}
    @entry[:msb-first]{The values are stored with the most-significant byte
      first. For instance, the 32-bit value 0xffeecc would be stored in memory
      as 0x00, 0xcc, 0xee, 0xff.}
  @end{table}
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-byte-order}")

;;; ----------------------------------------------------------------------------
;;; gdk_query_depths ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_query_depths" %gdk-query-depths) :void
  (depths (:pointer (:pointer :int)))
  (count (:pointer :int)))

(defun gdk-query-depths ()
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{A list of the available depths.}
  This function returns the available bit depths for the default screen. It is
  equivalent to listing the visuals with the function @fun{gdk-list-visuals}
  and then looking at the depth field in each visual, removing duplicates.
  @see-class{gdk-visual}
  @see-function{gdk-list-visuals}"
  (with-foreign-objects ((count-r :int) (depths-r :pointer))
    (%gdk-query-depths depths-r count-r)
    (iter (with count = (mem-ref count-r :int))
          (with depths = (mem-ref depths-r :pointer))
          (for i from 0 below count)
          (collect (mem-aref depths :int i)))))

(export 'gdk-query-depths)

;;; ----------------------------------------------------------------------------
;;; gdk_query_visual_types ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_query_visual_types" %gdk-query-visual-types) :void
  (depths (:pointer (:pointer gdk-visual-type)))
  (count (:pointer :int)))

(defun gdk-query-visual-types ()
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{A list of the available visual types.}
  This function returns the available visual types for the default screen.
  It is equivalent to listing the visuals with the function
  @fun{gdk-list-visuals} and then looking at the type field in each visual,
  removing duplicates.
  @see-class{gdk-visual}
  @see-function{gdk-list-visuals}"
  (with-foreign-objects ((count-r :int) (types-r 'gdk-visual-type))
    (%gdk-query-visual-types types-r count-r)
    (iter (with count = (mem-ref count-r :int))
          (with types = (mem-ref types-r :pointer))
          (for i from 0 below count)
          (collect (mem-aref types 'gdk-visual-type i)))))

(export 'gdk-query-visual-types)

;;; ----------------------------------------------------------------------------
;;; gdk_list_visuals ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_list_visuals" gdk-list-visuals)
    (g-list (g-object gdk-visual) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{A list of visuals.}
  Lists the available visuals for the default screen. See the function
  @fun{gdk-screen-list-visuals}. A visual describes a hardware image data
  format. For example, a visual might support 24-bit color, or 8-bit color, and
  might expect pixels to be in a certain format.
  @see-class{gdk-visual}
  @see-function{gdk-screen-list-visuals}")

(export 'gdk-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_bits_per_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_bits_per_rgb" gdk-visual-get-bits-per-rgb) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @return{The number of significant bits per color value for visual.}
  @begin{short}
    Returns the number of significant bits per red, green and blue value.
  @end{short}

  Since 2.22
  @see-class{gdk-visual}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-get-bits-per-rgb)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_blue_pixel_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_blue_pixel_details"
          %gdk-visual-get-blue-pixel-details) :void
  (visual (g-object gdk-visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun gdk-visual-get-blue-pixel-details (visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @begin{return}
    @code{mask} -- a @code{guint32}, or @code{nil} @br{}
    @code{shift} -- a @code{gint}, or @code{nil} @br{}
    @code{precision} -- a @code{gint}, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate blue pixel values in TrueColor
    and DirectColor. The @arg{mask} is the significant bits within the pixel.
    The @arg{shift} is the number of bits left we must shift a primary for it to
    be in position according to the @arg{mask}. Finally, @arg{precision} refers
    to how much precision the pixel value contains for a particular primary.
  @end{short}

  Since 2.22
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-red-pixel-details}
  @see-function{gdk-visual-get-green-pixel-details}"
  (with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%gdk-visual-get-blue-pixel-details visual mask shift precision)
    (values (mem-ref mask :uint32)
            (mem-ref shift :int)
            (mem-ref precision :int))))

(export 'gdk-visual-get-blue-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_byte_order ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_byte_order" gdk-visual-get-byte-order) gdk-byte-order
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @return{A @symbol{gdk-byte-order} stating the byte order of visual.}
  @begin{return}
    Returns the byte order of this visual.
  @end{return}

  Since 2.22
  @see-class{gdk-visual}
  @see-symbol{gdk-byte-order}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-get-byte-order)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_colormap_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_colormap_size" gdk-visual-get-colormap-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @return{The size of a colormap that is suitable for visual.}
  @begin{short}
    Returns the size of a colormap for this @arg{visual}.
  @end{short}

  Since 2.22
  @see-class{gdk-visual}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-get-colormap-size)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_depth" gdk-visual-get-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @return{The bit depth of this @arg{visual}.}
  @begin{short}
    Returns the bit depth of this @arg{visual}.
  @end{short}

  Since 2.22
  @see-class{gdk-visual}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-get-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_green_pixel_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_green_pixel_details"
          %gdk-visual-get-green-pixel-details) :void
  (visual (g-object gdk-visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun gdk-visual-get-green-pixel-details (visual)
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @begin{return}
    @code{mask} -- a @code{guint32}, or @code{nil} @br{}
    @code{shift} -- a @code{gint}, or @code{nil} @br{}
    @code{precision} -- a @code{gint}, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate green pixel values in TrueColor
    and DirectColor. The @arg{mask} is the significant bits within the pixel.
    The @arg{shift} is the number of bits left we must shift a primary for it to
    be in position according to the @arg{mask}. Finally, @arg{precision} refers
    to how much precision the pixel value contains for a particular primary.
  @end{short}

  Since 2.22
  @see-class{gdk-visual}
  @see-function{gdk-visuel-get-red-pixel-details}
  @see-function{gdk-visual-get-blue-pixel-details}"
  (with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%gdk-visual-get-green-pixel-details visual mask shift precision)
    (values (mem-ref mask :uint32)
            (mem-ref shift :int)
            (mem-ref precision :int))))

(export 'gdk-visual-get-green-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_red_pixel_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_red_pixel_details"
          %gdk-visual-get-red-pixel-details) :void
  (visual (g-object gdk-visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun gdk-visual-get-red-pixel-details (visual)
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @begin{return}
    @code{mask} -- a @code{guint32}, or @code{nil} @br{}
    @code{shift} -- a @code{gint}, or @code{nil} @br{}
    @code{precision} -- a @code{gint}, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate red pixel values in TrueColor
    and DirectColor. The @arg{mask} is the significant bits within the pixel.
    The @arg{shift} is the number of bits left we must shift a primary for it to
    be in position according to the @arg{mask}. Finally, @arg{precision} refers
    to how much precision the pixel value contains for a particular primary.
  @end{short}

  Since 2.22
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-blue-pixel-details}
  @see-function{gdk-visual-get-green-pixel-details}"
  (with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%gdk-visual-get-red-pixel-details visual mask shift precision)
    (values (mem-ref mask :uint32)
            (mem-ref shift :int)
            (mem-ref precision :int))))

(export 'gdk-visual-get-red-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_visual_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_visual_type" gdk-visual-get-visual-type)
    gdk-visual-type
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @return{A @symbol{gdk-visual-type} stating the type of visual.}
  @begin{return}
    Returns the type of visual this is (PseudoColor, TrueColor, etc).
  @end{return}

  Since 2.22
  @see-class{gdk-visual}
  @see-symbol{gdk-visual-type}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-get-visual-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_depth" gdk-visual-get-best-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{Best available depth.}
  Get the best available depth for the default GDK screen. \"Best\" means
  \"largest\", i. e. 32 preferred over 24 preferred over 8 bits per pixel.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-best-type}")

(export 'gdk-visual-get-best-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_type" gdk-visual-get-best-type) gdk-visual-type
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{Best visual type.}
  Return the best available visual type for the default GDK screen.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-best-depth}")

(export 'gdk-visual-get-best-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_system ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_system" gdk-visual-get-system) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{The system visual.}
  Get the system's default visual for the default GDK screen. This is the
  visual for the root window of the display. The return value should not be
  freed.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-best}")

(export 'gdk-visual-get-system)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best" gdk-visual-get-best) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @return{The best visual.}
  Get the visual with the most available colors for the default GDK screen.
  The return value should not be freed.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-system}
  @see-function{gdk-visual-get-best-with-depth}
  @see-function{gdk-visual-get-best-with-type}
  @see-function{gdk-visual-get-best-with-both}")

(export 'gdk-visual-get-best)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_depth" gdk-visual-get-best-with-depth)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[depth]{a bit depth}
  @return{The best visual for the given @arg{depth}.}
  Get the best visual with depth @arg{depth} for the default GDK screen. Color
  visuals and visuals with mutable colormaps are preferred over grayscale or
  fixed-colormap visuals. The return value should not be freed. @code{Nil} may
  be returned if no visual supports @arg{depth}.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-system}
  @see-function{gdk-visual-get-best}
  @see-function{gdk-visual-get-best-with-type}
  @see-function{gdk-visual-get-best-with-both}"
  (depth :int))

(export 'gdk-visual-get-best-with-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_type" gdk-visual-get-best-with-type)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual-type]{a visual type}
  @return{The best visual of the given type.}
  Get the best visual of the given @arg{visual-type} for the default GDK screen.
  Visuals with higher color depths are considered better. The return value
  should not be freed. @code{Nil} may be returned if no visual has type
  @arg{visual-type}.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-system}
  @see-function{gdk-visual-get-best}
  @see-function{gdk-visual-get-best-with-depth}
  @see-function{gdk-visual-get-best-with-both}"
  (visual-type gdk-visual-type))

(export 'gdk-visual-get-best-with-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_both ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_both" gdk-visual-get-best-with-both)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[depth]{a bit depth}
  @argument[visual-type]{a visual type}
  @return{The best visual with both @arg{depth} and @arg{visual-type},
    or @code{nil} if none.}
  Combines the functions @fun{gdk-visual-get-best-with-depth} and
  @fun{gdk-visual-get-best-with-type}.
  @see-class{gdk-visual}
  @see-function{gdk-visual-get-system}
  @see-function{gdk-visual-get-best}
  @see-function{gdk-visual-get-best-with-depth}
  @see-function{gdk-visual-get-best-with-type}"
  (depth :int)
  (visual-type gdk-visual-type))

(export 'gdk-visual-get-best-with-both)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_screen" gdk-visual-get-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-29}
  @argument[visual]{a @class{gdk-visual} object}
  @return{The screen to which this @arg{visual} belongs.}
  @begin{short}
    Gets the screen to which this @arg{visual} belongs.
  @end{short}

  Since 2.2
  @see-class{gdk-visual}
  @see-class{gdk-screen}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-get-screen)

;;; --- End of file gdk.visual.lisp --------------------------------------------
