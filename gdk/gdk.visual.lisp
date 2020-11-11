;;; ----------------------------------------------------------------------------
;;; gdk.visual.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Low-level display hardware information
;;;
;;; Types and Values
;;;
;;;     GdkVisual
;;;     GdkVisualType
;;;     GdkByteOrder
;;;
;;; Functions
;;;
;;;     gdk_query_depths                                   deprecated
;;;     gdk_query_visual_types                             deprecated
;;;     gdk_list_visuals                                   deprecated
;;;     gdk_visual_get_bits_per_rgb                        deprecated
;;;     gdk_visual_get_blue_pixel_details
;;;     gdk_visual_get_byte_order                          deprecated
;;;     gdk_visual_get_colormap_size                       deprecated
;;;     gdk_visual_get_depth
;;;     gdk_visual_get_green_pixel_details
;;;     gdk_visual_get_red_pixel_details
;;;     gdk_visual_get_visual_type
;;;     gdk_visual_get_best_depth                          deprecated
;;;     gdk_visual_get_best_type                           deprecated
;;;     gdk_visual_get_system                              deprecated
;;;     gdk_visual_get_best                                deprecated
;;;     gdk_visual_get_best_with_depth                     deprecated
;;;     gdk_visual_get_best_with_type                      deprecated
;;;     gdk_visual_get_best_with_both                      deprecated
;;;     gdk_visual_get_screen
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkVisual
;;; ----------------------------------------------------------------------------

(in-package :gdk)

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
 "@version{2020-9-25}
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
      components. Use the function @fun{gdk-visual-red-pixel-details}, etc, to
      obtain information about how the components are assembled into a pixel
      value.}
    @entry[:direct-color]{Each pixel value contains red, green, and blue
      components as for @code{:true-color}, but the components are mapped via a
      color table into the final output table instead of being converted
      directly.}
  @end{table}
  @see-class{gdk-visual}
  @see-function{gdk-visual-red-pixel-details}
  @see-function{gdk-visual-blue-pixel-details}
  @see-function{gdk-visual-green-pixel-details}")

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
 "@version{2020-9-25}
  @begin{short}
    A set of values describing the possible byte-orders for storing pixel
    values in memory.
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
  @see-function{gdk-visual-byte-order}")

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
 "@version{2020-9-25}
  @begin{short}
    A @sym{gdk-visual} describes a particular video hardware display format.
  @end{short}
  It includes information about the number of bits used for each color, the way
  the bits are translated into an RGB value for display, and the way the bits
  are stored in memory. For example, a piece of display hardware might support
  24-bit color, 16-bit color, or 8-bit color; meaning 24/16/8-bit pixel sizes.
  For a given pixel size, pixels can be in different formats; for example the
  \"red\" element of an RGB pixel may be in the top 8 bits of the pixel, or
  may be in the lower 4 bits.

  There are several standard visuals. The visual returned by the function
  @fun{gdk-screen-system-visual} is the system's default visual.

  A number of functions are provided for determining the \"best\" available
  visual. For the purposes of making this determination, higher bit depths are
  considered better, and for visuals of the same bit depth,
  @code{:pseudo-color} is preferred at 8bpp, otherwise, the visual types
  are ranked in the order of highest to lowest @code{:direct-color},
  @code{:true-color}, @code{:pseudo-color}, @code{:static-color},
  @code{:grayscale}, then @code{:static-gray}.
  @see-function{gdk-screen-system-visual}")

;;; ----------------------------------------------------------------------------

(defmethod print-object ((visual gdk-visual) stream)
  (print-unreadable-object (visual stream :type t :identity t)
    (format stream "~S at ~S bpp"
                   (gdk-visual-visual-type visual)
                   (gdk-visual-depth visual))))

;;; ----------------------------------------------------------------------------
;;; gdk_query_depths ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_query_depths" %gdk-query-depths) :void
  (depths (:pointer (:pointer :int)))
  (count (:pointer :int)))

(defun gdk-query-depths ()
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @return{A list of integers of the available depths.}
  @begin{short}
    This function returns the available bit depths for the default screen.
  @end{short}
  It is equivalent to listing the visuals with the function
  @fun{gdk-list-visuals} and then looking at the depth field in each visual,
  removing duplicates.
  @begin[Warning]{dictionary}
    The function @sym{gdk-query-depths} has been deprecated since version 3.22
    and should not be used in newly-written code. Visual selection should be
    done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @begin[Example]{dictionary}
    @begin{pre}
 (gdk-query-depths)
=> (32 24)
    @end{pre}
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-list-visuals}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}"
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
 "@version{2020-9-25}
  @return{A list of the available visual types of type
    @symbol{gdk-visual-type}.}
  @begin{short}
    This function returns the available visual types for the default screen.
  @end{short}
  It is equivalent to listing the visuals with the function
  @fun{gdk-list-visuals} and then looking at the type field in each visual,
  removing duplicates.
  @begin[Warning]{dictionary}
    The function @sym{gdk-query-visual-types} has been deprecated since version
    3.22 and should not be used in newly-written code. Visual selection should
    be done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-symbol{gdk-visual-type}
  @see-function{gdk-list-visuals}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}"
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
 "@version{2020-9-25}
  @return{A list of @class{gdk-visual} objects.}
  @begin{short}
    Lists the available visuals for the default screen.
  @end{short}
  A visual describes a hardware image data format. See the function
  @fun{gdk-screen-list-visuals}.

  For example, a visual might support 24-bit color, or 8-bit color, and might
  expect pixels to be in a certain format.
  @begin[Warning]{dictionary}
    The function @sym{gdk-list-visuals} has been deprecated since version 3.22
    and should not be used in newly-written code. Use the call
    @code{(gdk-screen-list-visuals (gdk-screen-default))}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-screen-list-visuals}")

(export 'gdk-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_bits_per_rgb () -> gdk-visual-bits-per-rgb
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_bits_per_rgb" gdk-visual-bits-per-rgb) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-10-11}
  @argument[visual]{a @class{gdk-visual} object}
  @return{An integer with the number of significant bits per color value for
    visual.}
  @begin{short}
    Returns the number of significant bits per red, green and blue value.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-bits-per-rgb} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-visual-red-pixel-details} and its variants to learn about the
    pixel layout of TrueColor and DirectColor visuals.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-red-pixel-details}
  @see-function{gdk-visual-blue-pixel-details}
  @see-function{gdk-visual-green-pixel-details}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-bits-per-rgb)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_blue_pixel_details () -> gdk-visual-blue-pixel-details
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_blue_pixel_details" %gdk-visual-blue-pixel-details)
    :void
  (visual (g-object gdk-visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun gdk-visual-blue-pixel-details (visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-11}
  @argument[visual]{a @class{gdk-visual} object}
  @begin{return}
    @code{mask} -- an unsigned integer, or @code{nil} @br{}
    @code{shift} -- an integer, or @code{nil} @br{}
    @code{precision} -- an integer, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate blue pixel values in TrueColor
    and DirectColor.
  @end{short}
  @arg{mask} is the significant bits within the pixel. @arg{shift} is the
  number of bits left we must shift a primary for it to be in position
  according to @arg{mask}. Finally, @arg{precision} refers to how much
  precision the pixel value contains for a particular primary.
  @see-class{gdk-visual}
  @see-function{gdk-visual-red-pixel-details}
  @see-function{gdk-visual-green-pixel-details}"
  (with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%gdk-visual-blue-pixel-details visual mask shift precision)
    (values (mem-ref mask :uint32)
            (mem-ref shift :int)
            (mem-ref precision :int))))

(export 'gdk-visual-blue-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_byte_order () -> gdk-visual-byte-order
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_byte_order" gdk-visual-byte-order) gdk-byte-order
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @argument[visual]{a @class{gdk-visual} object}
  @return{A @symbol{gdk-byte-order} value stating the byte order of
    @arg{visual}.}
  @begin{short}
    Returns the byte order of the visual.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-byte-order} has been deprecated since version
    3.22 and should not be used in newly-written code. This information is not
    useful.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-symbol{gdk-byte-order}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-byte-order)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_colormap_size () -> gdk-visual-colormap-size
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_colormap_size" gdk-visual-colormap-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-10-11}
  @argument[visual]{a @class{gdk-visual} object}
  @return{An integer with the size of a colormap that is suitable for
    @arg{visual}.}
  @begin{short}
    Returns the size of a colormap for the visual.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-colormap-size} has been deprecated since
    version 3.22 and should not be used in newly-written code. This information
    is not useful, since GDK does not provide APIs to operate on colormaps.
  @end{dictionary}
  @see-class{gdk-visual}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-colormap-size)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_depth () -> gdk-visual-depth
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_depth" gdk-visual-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-10-11}
  @argument[visual]{a @class{gdk-visual} object}
  @return{An integer with the bit depth of @arg{visual}.}
  @begin{short}
    Returns the bit depth of the visual.
  @end{short}
  @see-class{gdk-visual}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_green_pixel_details ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_green_pixel_details" %gdk-visual-green-pixel-details)
    :void
  (visual (g-object gdk-visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun gdk-visual-green-pixel-details (visual)
 "@version{2020-10-11}
  @argument[visual]{a @class{gdk-visual} object}
  @begin{return}
    @code{mask} -- an unsigned integer, or @code{nil} @br{}
    @code{shift} -- an integer, or @code{nil} @br{}
    @code{precision} -- an integer, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate green pixel values in TrueColor
    and DirectColor.
  @end{short}
  @arg{mask} is the significant bits within the pixel. @arg{shift} is the
  number of bits left we must shift a primary for it to be in position
  according to @arg{mask}. Finally, @arg{precision} refers to how much
  precision the pixel value contains for a particular primary.
  @see-class{gdk-visual}
  @see-function{gdk-visual-red-pixel-details}
  @see-function{gdk-visual-blue-pixel-details}"
  (with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%gdk-visual-green-pixel-details visual mask shift precision)
    (values (mem-ref mask :uint32)
            (mem-ref shift :int)
            (mem-ref precision :int))))

(export 'gdk-visual-green-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_red_pixel_details () -> gdk-visual-red-pixel-details
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_red_pixel_details" %gdk-visual-red-pixel-details)
    :void
  (visual (g-object gdk-visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun gdk-visual-red-pixel-details (visual)
 "@version{2020-11-10}
  @argument[visual]{a @class{gdk-visual} object}
  @begin{return}
    @code{mask} -- an unsigned integer, or @code{nil} @br{}
    @code{shift} -- an integer, or @code{nil} @br{}
    @code{precision} -- an integer, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate red pixel values in TrueColor
    and DirectColor.
  @end{short}
  @arg{mask} is the significant bits within the pixel. @arg{shift} is the
  number of bits left we must shift a primary for it to be in position
  according to @arg{mask}. Finally, @arg{precision} refers to how much
  precision the pixel value contains for a particular primary.
  @see-class{gdk-visual}
  @see-function{gdk-visual-blue-pixel-details}
  @see-function{gdk-visual-green-pixel-details}"
  (with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%gdk-visual-red-pixel-details visual mask shift precision)
    (values (mem-ref mask :uint32)
            (mem-ref shift :int)
            (mem-ref precision :int))))

(export 'gdk-visual-red-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_visual_type () -> gdk-visual-visual-type
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_visual_type" gdk-visual-visual-type) gdk-visual-type
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @argument[visual]{a @class{gdk-visual} object}
  @return{A @symbol{gdk-visual-type} value stating the type of @arg{visual}.}
  @begin{short}
    Returns the type of visual this is (PseudoColor, TrueColor, etc).
  @end{short}
  @see-class{gdk-visual}
  @see-symbol{gdk-visual-type}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-visual-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_depth () -> gdk-visual-best-depth
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_depth" gdk-visual-best-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @return{An integer with the best available depth.}
  @begin{short}
    Get the best available depth for the default GDK screen.
  @end{short}
  \"Best\" means \"largest\", i.e. 32 preferred over 24 preferred over 8 bits
  per pixel.
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-best-depth} has been deprecated since version
    3.22 and should not be used in newly-written code. Visual selection should
    be done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-best-type}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}")

(export 'gdk-visual-best-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_type () -> gdk-visual-best-type
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_type" gdk-visual-best-type) gdk-visual-type
 #+cl-cffi-gtk-documentation
 "@version{2020-9-26}
  @return{Best visual type of type @symbol{gdk-visual-type}.}
  @begin{short}
    Return the best available visual type for the default GDK screen.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-best-type} has been deprecated since version
    3.22 and should not be used in newly-written code. Visual selection should
    be done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-best-depth}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}")

(export 'gdk-visual-best-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_system () -> gdk-visual-system
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_system" gdk-visual-system) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-26}
  @return{The system @class{gdk-visual} object.}
  @begin{short}
    Get the system's default visual for the default GDK screen.
  @end{short}
  This is the visual for the root window of the display.
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-system} has been deprecated since version 3.22
    and should not be used in newly-written code. Use the call
    @code{(gdk-screen-system-visual (gdk-screen-default))}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-best}
  @see-function{gdk-screen-default}
  @see-function{gdk-screen-system-visual}")

(export 'gdk-visual-system)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best () -> gdk-visual-best
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best" gdk-visual-best) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-26}
  @return{The best @class{gdk-visual} object.}
  @begin{short}
    Get the visual with the most available colors for the default GDK screen.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-best} has been deprecated since version 3.22
    and should not be used in newly-written code. Visual selection should be
    done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-system}
  @see-function{gdk-visual-best-with-depth}
  @see-function{gdk-visual-best-with-type}
  @see-function{gdk-visual-best-with-both}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}")

(export 'gdk-visual-best)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_depth () -> gdk-visual-best-with-depth
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_depth" gdk-visual-best-with-depth)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @argument[depth]{an integer with the bit depth}
  @return{The best @class{gdk-visual} object for the given @arg{depth}.}
  @begin{short}
    Get the best visual with depth for the default GDK screen.
  @end{short}
  Color visuals and visuals with mutable colormaps are preferred over grayscale
  or fixed-colormap visuals. @code{Nil} may be returned if no visual supports
  @arg{depth}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-best-with-depth} has been deprecated since
    version 3.22 and should not be used in newly-written code. Visual selection
    should be done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-system}
  @see-function{gdk-visual-best}
  @see-function{gdk-visual-best-with-type}
  @see-function{gdk-visual-best-with-both}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}"
  (depth :int))

(export 'gdk-visual-best-with-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_type () -> gdk-visual-best-with-type
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_type" gdk-visual-best-with-type)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-26}
  @argument[visual-type]{a value of the @symbol{gdk-visual-type} enumeration}
  @return{The best @class{gdk-visual} object of the given @arg{visual-type}.}
  @begin{short}
    Get the best visual of the given visual type for the default GDK screen.
  @end{short}
  Visuals with higher color depths are considered better. @code{Nil} may be
  returned if no visual has type @arg{visual-type}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-best-with-type} has been deprecated since
    version 3.22 and should not be used in newly-written code. Visual selection
    should be done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-system}
  @see-function{gdk-visual-best}
  @see-function{gdk-visual-best-with-depth}
  @see-function{gdk-visual-best-with-both}
  @see-function{gdk-screen-system-visual}
  @see-function{gdk-screen-rgba-visual}"
  (visual-type gdk-visual-type))

(export 'gdk-visual-best-with-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_both () -> gdk-visual-best-with-both
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_both" gdk-visual-best-with-both)
    (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-10}
  @argument[depth]{an integer with the bit depth}
  @argument[visual-type]{a value of the @symbol{gdk-visual-type} enumeration}
  @return{The best @class{gdk-visual} object with both @arg{depth} and
    @arg{visual-type}, or @code{nil} if none.}
  @begin{short}
    Combines the functions @fun{gdk-visual-best-with-depth} and
    @fun{gdk-visual-best-with-type}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-visual-best-with-both} has been deprecated since
    version 3.22 and should not be used in newly-written code. Visual selection
    should be done using the functions @fun{gdk-screen-system-visual} and
    @fun{gdk-screen-rgba-visual}.
  @end{dictionary}
  @see-class{gdk-visual}
  @see-function{gdk-visual-system}
  @see-function{gdk-visual-best}
  @see-function{gdk-visual-best-with-depth}
  @see-function{gdk-visual-best-with-type}
  @see-function{gdk-screen-system-visual}"
  (depth :int)
  (visual-type gdk-visual-type))

(export 'gdk-visual-best-with-both)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_screen () -> gdk-visual-screen
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_screen" gdk-visual-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-26}
  @argument[visual]{a @class{gdk-visual} object}
  @return{The @class{gdk-screen} object to which @arg{visual} belongs.}
  @begin{short}
    Gets the screen to which the visual belongs.
  @end{short}
  @see-class{gdk-visual}
  @see-class{gdk-screen}"
  (visual (g-object gdk-visual)))

(export 'gdk-visual-screen)

;;; --- End of file gdk.visual.lisp --------------------------------------------
