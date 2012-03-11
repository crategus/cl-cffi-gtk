;;; ----------------------------------------------------------------------------
;;; gdk.visual.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;     GdkVisualType;
;;;     GdkVisual;
;;;     GdkByteOrder;
;;;     gdk_query_depths
;;;     gdk_query_visual_types
;;;     gdk_list_visuals
;;;     gdk_visual_get_best_depth
;;;     gdk_visual_get_best_type
;;;     gdk_visual_get_system
;;;     gdk_visual_get_best
;;;     gdk_visual_get_best_with_depth
;;;     gdk_visual_get_best_with_type
;;;     gdk_visual_get_best_with_both
;;;     gdk_visual_ref                      
;;;     gdk_visual_unref                    
;;;     gdk_visual_get_screen
;;;     gdk_visual_get_bits_per_rgb
;;;     gdk_visual_get_blue_pixel_details
;;;     gdk_visual_get_byte_order
;;;     gdk_visual_get_colormap_size
;;;     gdk_visual_get_depth
;;;     gdk_visual_get_green_pixel_details
;;;     gdk_visual_get_red_pixel_details
;;;     gdk_visual_get_visual_type
;;; 
;;; Description
;;; 
;;; A GdkVisual describes a particular video hardware display format. It
;;; includes information about the number of bits used for each color, the way
;;; the bits are translated into an RGB value for display, and the way the bits
;;; are stored in memory. For example, a piece of display hardware might support
;;; 24-bit color, 16-bit color, or 8-bit color; meaning 24/16/8-bit pixel sizes.
;;; For a given pixel size, pixels can be in different formats; for example the
;;; "red" element of an RGB pixel may be in the top 8 bits of the pixel, or may
;;; be in the lower 4 bits.
;;; 
;;; Usually you can avoid thinking about visuals in GTK+. Visuals are useful to
;;; interpret the contents of a GdkImage, but you should avoid GdkImage
;;; precisely because its contents depend on the display hardware; use GdkPixbuf
;;; instead, for all but the most low-level purposes. Also, anytime you provide
;;; a GdkColormap, the visual is implied as part of the colormap
;;; (gdk_colormap_get_visual()), so you won't have to provide a visual in
;;; addition.
;;; 
;;; There are several standard visuals. The visual returned by
;;; gdk_visual_get_system() is the system's default visual. gdk_rgb_get_visual()
;;; return the visual most suited to displaying full-color image data. If you
;;; use the calls in GdkRGB, you should create your windows using this visual
;;; (and the colormap returned by gdk_rgb_get_colormap()).
;;; 
;;; A number of functions are provided for determining the "best" available
;;; visual. For the purposes of making this determination, higher bit depths
;;; are considered better, and for visuals of the same bit depth,
;;; GDK_VISUAL_PSEUDO_COLOR is preferred at 8bpp, otherwise, the visual types
;;; are ranked in the order of (highest to lowest) GDK_VISUAL_DIRECT_COLOR,
;;; GDK_VISUAL_TRUE_COLOR, GDK_VISUAL_PSEUDO_COLOR, GDK_VISUAL_STATIC_COLOR,
;;; GDK_VISUAL_GRAYSCALE, then GDK_VISUAL_STATIC_GRAY.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkByteOrder
;;; 
;;; typedef enum
;;; {
;;;   GDK_LSB_FIRST,
;;;   GDK_MSB_FIRST
;;; } GdkByteOrder;
;;; 
;;; A set of values describing the possible byte-orders for storing pixel
;;; values in memory.
;;; 
;;; GDK_LSB_FIRST
;;;     The values are stored with the least-significant byte first. For
;;;     instance, the 32-bit value 0xffeecc would be stored in memory as 0xcc,
;;;     0xee, 0xff, 0x00.
;;; 
;;; GDK_MSB_FIRST
;;;     The values are stored with the most-significant byte first. For
;;;     instance, the 32-bit value 0xffeecc would be stored in memory as 0x00,
;;;     0xcc, 0xee, 0xff.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkByteOrder" gdk-byte-order
  (:export t
   :type-initializer "gdk_byte_order_get_type")
  (:lsb-first 0)
  (:msb-first 1))

;;; ----------------------------------------------------------------------------
;;; enum GdkVisualType
;;; 
;;; typedef enum
;;; {
;;;   GDK_VISUAL_STATIC_GRAY,
;;;   GDK_VISUAL_GRAYSCALE,
;;;   GDK_VISUAL_STATIC_COLOR,
;;;   GDK_VISUAL_PSEUDO_COLOR,
;;;   GDK_VISUAL_TRUE_COLOR,
;;;   GDK_VISUAL_DIRECT_COLOR
;;; } GdkVisualType;
;;; 
;;; A set of values that describe the manner in which the pixel values for a
;;; visual are converted into RGB values for display.
;;; 
;;; GDK_VISUAL_STATIC_GRAY
;;;     Each pixel value indexes a grayscale value directly.
;;; 
;;; GDK_VISUAL_GRAYSCALE
;;;     Each pixel is an index into a color map that maps pixel values into
;;;     grayscale values. The color map can be changed by an application.
;;; 
;;; GDK_VISUAL_STATIC_COLOR
;;;     Each pixel value is an index into a predefined, unmodifiable color map
;;;     that maps pixel values into RGB values.
;;; 
;;; GDK_VISUAL_PSEUDO_COLOR
;;;     Each pixel is an index into a color map that maps pixel values into
;;;     rgb values. The color map can be changed by an application.
;;; 
;;; GDK_VISUAL_TRUE_COLOR
;;;     Each pixel value directly contains red, green, and blue components. The
;;;     red_mask, green_mask, and blue_mask fields of the GdkVisual structure
;;;     describe how the components are assembled into a pixel value.
;;; 
;;; GDK_VISUAL_DIRECT_COLOR
;;;     Each pixel value contains red, green, and blue components as for
;;;     GDK_VISUAL_TRUE_COLOR, but the components are mapped via a color table
;;;     into the final output table instead of being converted directly.
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

;;; ----------------------------------------------------------------------------
;;; GdkVisual
;;; 
;;; typedef struct {
;;;   GObject parent_instance;
;;;   
;;;   GdkVisualType GSEAL (type);
;;;   gint GSEAL (depth);
;;;   GdkByteOrder GSEAL (byte_order);
;;;   gint GSEAL (colormap_size);
;;;   gint GSEAL (bits_per_rgb);
;;; 
;;;   guint32 GSEAL (red_mask);
;;;   gint GSEAL (red_shift);
;;;   gint GSEAL (red_prec);
;;; 
;;;   guint32 GSEAL (green_mask);
;;;   gint GSEAL (green_shift);
;;;   gint GSEAL (green_prec);
;;; 
;;;   guint32 GSEAL (blue_mask);
;;;   gint GSEAL (blue_shift);
;;;   gint GSEAL (blue_prec);
;;; } GdkVisual;
;;; 
;;; The GdkVisual structure contains information about a particular visual.
;;; 
;;; Example 5. Constructing a pixel value from components
;;; 
;;; guint
;;; pixel_from_rgb (GdkVisual *visual,
;;;                 guchar r, guchar b, guchar g)
;;; {
;;;   return ((r >> (16 - visual->red_prec))   << visual->red_shift) |
;;;          ((g >> (16 - visual->green_prec)) << visual->green_shift) |
;;;          ((r >> (16 - visual->blue_prec))  << visual->blue_shift);
;;; }
;;; 
;;; 
;;; GObject parent_instance;
;;;     inherited portion from GObject
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkVisual" gdk-visual
  (:type-initializer "gdk_visual_get_type")
  ((:cffi screen gdk-visual-screen (g-object gdk-screen)
          "gdk_visual_get_screen" nil)
   (:cffi visual-type gdk-visual-visual-type gdk-visual-type
          gdk-visual-get-visual-type nil)
   (:cffi depth gdk-visual-depth :int gdk-visual-get-depth nil)
   (:cffi byte-order gdk-visual-byte-order
          gdk-byte-order gdk-visual-get-byte-order nil)
   (:cffi colormap-size gdk-visual-colormap-size :int
          gdk-visual-get-colormap-size nil)
   (:cffi bits-per-rgb gdk-visual-bits-per-rgb :int
          gdk-visual-get-bits-per-rgb nil)
   (:cffi red-mask gdk-visual-red-mask :uint32 gdk-visual-get-red-mask nil)
   (:cffi red-shift gdk-visual-red-shift :int gdk-visual-get-red-shift nil)
   (:cffi red-prec gdk-visual-red-prec :int gdk-visual-get-red-prec nil)
   (:cffi green-mask gdk-visual-green-mask :uint32
          gdk-visual-get-green-mask nil)
   (:cffi green-shift gdk-visual-green-shift :int
          gdk-visual-get-green-shift nil)
   (:cffi green-prec gdk-visual-green-prec :int gdk-visual-get-green-prec nil)
   (:cffi blue-mask gdk-visual-blue-mask :uint32 gdk-visual-get-blue-mask nil)
   (:cffi blue-shift gdk-visual-blue-shift :int gdk-visual-get-blue-shift nil)
   (:cffi blue-prec gdk-visual-blue-prec :int gdk-visual-get-blue-prec nil)))

(defmethod print-object ((visual gdk-visual) stream)
  (print-unreadable-object (visual stream :type t :identity t)
    (format stream "~S at ~S bpp"
                   (gdk-visual-visual-type visual)
            (gdk-visual-depth visual))))

;;; ----------------------------------------------------------------------------

(defcstruct gdk-visual-cstruct
  (parent-instance gobject::%g-object)
  (visual-type gdk-visual-type)
  (depth :int)
  (byte-order gdk-byte-order)
  (colormap-size :int)
  (bits-per-rgb :int)
  (red-mask :uint32)
  (red-shift :int)
  (red-prec :int)
  (green-mask :uint32)
  (green-shift :int)
  (green-prec :int)
  (blue-mask :uint32)
  (blue-shift :int)
  (blue-prec :int))

;;; ----------------------------------------------------------------------------

(defmacro def-visual-accessor (slot)
  `(defun ,(intern (format nil "~A-GET-~A"
                           (symbol-name 'gdk-visual)
                           (symbol-name slot))) (visual)
     (foreign-slot-value (pointer visual) 'gdk-visual-cstruct ',slot)))

(def-visual-accessor visual-type)
(def-visual-accessor depth)
(def-visual-accessor byte-order)
(def-visual-accessor colormap-size)
(def-visual-accessor bits-per-rgb)
(def-visual-accessor red-mask)
(def-visual-accessor red-shift)
(def-visual-accessor red-prec)
(def-visual-accessor green-mask)
(def-visual-accessor green-shift)
(def-visual-accessor green-prec)
(def-visual-accessor blue-mask)
(def-visual-accessor blue-shift)
(def-visual-accessor blue-prec)

;;; ----------------------------------------------------------------------------
;;; gdk_query_depths ()
;;; 
;;; void gdk_query_depths (gint **depths, gint *count);
;;; 
;;; This function returns the available bit depths for the default screen.
;;; It's equivalent to listing the visuals (gdk_list_visuals()) and then looking
;;; at the depth field in each visual, removing duplicates.
;;; 
;;; The array returned by this function should not be freed.
;;; 
;;; depths :
;;;     return location for available depths. [out][array]
;;; 
;;; count :
;;;     return location for number of available depths. [out]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_query_depths" %gdk-query-depths) :void
  (depths (:pointer (:pointer :int)))
  (count (:pointer :int)))

(defun gdk-query-depths ()
  (with-foreign-objects ((count-r :int) (depths-r :pointer))
    (%gdk-query-depths depths-r count-r)
    (iter (with count = (mem-ref count-r :int))
          (with depths = (mem-ref depths-r :pointer))
          (for i from 0 below count)
          (collect (mem-aref depths :int i)))))

(export 'gdk-query-depths)

;;; ----------------------------------------------------------------------------
;;; gdk_query_visual_types ()
;;; 
;;; void gdk_query_visual_types (GdkVisualType **visual_types, gint *count);
;;; 
;;; This function returns the available visual types for the default screen.
;;; It's equivalent to listing the visuals (gdk_list_visuals()) and then looking
;;; at the type field in each visual, removing duplicates.
;;; 
;;; The array returned by this function should not be freed.
;;; 
;;; visual_types :
;;;     return location for the available visual types
;;; 
;;; count :
;;;     return location for the number of available visual types
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_query_visual_types" %gdk-query-visual-types) :void
  (depths (:pointer (:pointer gdk-visual-type)))
  (count (:pointer :int)))

(defun gdk-query-visual-types ()
  (with-foreign-objects ((count-r :int) (types-r 'gdk-visual-type))
    (%gdk-query-visual-types types-r count-r)
    (iter (with count = (mem-ref count-r :int))
          (with types = (mem-ref types-r :pointer))
          (for i from 0 below count)
          (collect (mem-aref types 'gdk-visual-type i)))))

(export 'gdk-query-visual-types)

;;; ----------------------------------------------------------------------------
;;; gdk_list_visuals ()
;;; 
;;; GList * gdk_list_visuals (void);
;;; 
;;; Lists the available visuals for the default screen. (See
;;; gdk_screen_list_visuals()) A visual describes a hardware image data format.
;;; For example, a visual might support 24-bit color, or 8-bit color, and might
;;; expect pixels to be in a certain format.
;;; 
;;; Call g_list_free() on the return value when you're finished with it.
;;; 
;;; Returns :
;;;     a list of visuals; the list must be freed, but not its contents.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_list_visuals" gdk-list-visuals)
    (g-list (g-object gdk-visual) :free-from-foreign t))

(export 'gdk-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_depth ()
;;; 
;;; gint gdk_visual_get_best_depth (void);
;;; 
;;; Get the best available depth for the default GDK screen. "Best" means
;;; "largest," i.e. 32 preferred over 24 preferred over 8 bits per pixel.
;;; 
;;; Returns :
;;;     best available depth
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_depth" gdk-visual-get-best-depth) :int)

(export 'gdk-visual-get-best-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_type ()
;;; 
;;; GdkVisualType gdk_visual_get_best_type (void);
;;; 
;;; Return the best available visual type for the default GDK screen.
;;; 
;;; Returns :
;;;     best visual type
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_type" gdk-visual-get-best-type) gdk-visual-type)

(export 'gdk-visual-get-best-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_system ()
;;; 
;;; GdkVisual * gdk_visual_get_system (void);
;;; 
;;; Get the system's default visual for the default GDK screen. This is the
;;; visual for the root window of the display. The return value should not be
;;; freed.
;;; 
;;; Returns :
;;;     system visual.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_system" gdk-visual-get-system) (g-object gdk-visual))

(export 'gdk-visual-get-system)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best ()
;;; 
;;; GdkVisual * gdk_visual_get_best (void);
;;; 
;;; Get the visual with the most available colors for the default GDK screen.
;;; The return value should not be freed.
;;; 
;;; Returns :
;;;     best visual. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best" gdk-visual-get-best) (g-object gdk-visual))

(export 'gdk-visual-get-best)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_depth ()
;;; 
;;; GdkVisual * gdk_visual_get_best_with_depth (gint depth);
;;; 
;;; Get the best visual with depth depth for the default GDK screen. Color
;;; visuals and visuals with mutable colormaps are preferred over grayscale or
;;; fixed-colormap visuals. The return value should not be freed. NULL may be
;;; returned if no visual supports depth.
;;; 
;;; depth :
;;;     a bit depth
;;; 
;;; Returns :
;;;     best visual for the given depth. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_depth" gdk-visual-get-best-with-depth)
    (g-object gdk-visual)
  (depth :int))

(export 'gdk-visual-get-best-with-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_type ()
;;; 
;;; GdkVisual * gdk_visual_get_best_with_type (GdkVisualType visual_type);
;;; 
;;; Get the best visual of the given visual_type for the default GDK screen.
;;; Visuals with higher color depths are considered better. The return value
;;; should not be freed. NULL may be returned if no visual has type visual_type.
;;; 
;;; visual_type :
;;;     a visual type
;;; 
;;; Returns :
;;;     best visual of the given type. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_both ()
;;; 
;;; GdkVisual * gdk_visual_get_best_with_both (gint depth,
;;;                                            GdkVisualType visual_type);
;;; 
;;; Combines gdk_visual_get_best_with_depth() and
;;; gdk_visual_get_best_with_type().
;;; 
;;; depth :
;;;     a bit depth
;;; 
;;; visual_type :
;;;     a visual type
;;; 
;;; Returns :
;;;     best visual with both depth and visual_type, or NULL if none.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_visual_get_best_with_both" gdk-visual-get-best-with-both)
    (g-object gdk-visual)
  (depth :int)
  (visual-type gdk-visual-type))

(export 'gdk-visual-get-best-with-both)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_ref()
;;; 
;;; #define gdk_visual_ref(v) g_object_ref(v)
;;; 
;;; Warning
;;; 
;;; gdk_visual_ref is deprecated and should not be used in newly-written code.
;;; 
;;; Deprecated equivalent of g_object_ref().
;;; 
;;; v :
;;;     a GdkVisual
;;; 
;;; Returns :
;;;     the same visual
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_unref()
;;; 
;;; #define gdk_visual_unref(v) g_object_unref(v)
;;; 
;;; Warning
;;; 
;;; gdk_visual_unref is deprecated and should not be used in newly-written code.
;;; 
;;; Deprecated equivalent of g_object_unref().
;;; 
;;; v :
;;;     a GdkVisual
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_screen ()
;;; 
;;; GdkScreen * gdk_visual_get_screen (GdkVisual *visual);
;;; 
;;; Gets the screen to which this visual belongs
;;; 
;;; visual :
;;;     a GdkVisual
;;; 
;;; Returns :
;;;     the screen to which this visual belongs. [transfer none]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_bits_per_rgb ()
;;; 
;;; gint gdk_visual_get_bits_per_rgb (GdkVisual *visual);
;;; 
;;; Returns the number of significant bits per red, green and blue value.
;;; 
;;; visual :
;;;     a GdkVisual
;;; 
;;; Returns :
;;;     The number of significant bits per color value for visual.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_blue_pixel_details ()
;;; 
;;; void gdk_visual_get_blue_pixel_details (GdkVisual *visual,
;;;                                         guint32 *mask,
;;;                                         gint *shift,
;;;                                         gint *precision);
;;; 
;;; Obtains values that are needed to calculate blue pixel values in TrueColor
;;; and DirectColor. The "mask" is the significant bits within the pixel. The
;;; "shift" is the number of bits left we must shift a primary for it to be in
;;; position (according to the "mask"). Finally, "precision" refers to how much
;;; precision the pixel value contains for a particular primary.
;;; 
;;; visual :
;;;     a GdkVisual
;;; 
;;; mask :
;;;     A pointer to a guint32 to be filled in, or NULL. [out][allow-none]
;;; 
;;; shift :
;;;     A pointer to a gint to be filled in, or NULL. [out][allow-none]
;;; 
;;; precision :
;;;     A pointer to a gint to be filled in, or NULL. [out][allow-none]
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_byte_order ()
;;; 
;;; GdkByteOrder gdk_visual_get_byte_order (GdkVisual *visual);
;;; 
;;; Returns the byte order of this visual.
;;; 
;;; visual :
;;;     A GdkVisual.
;;; 
;;; Returns :
;;;     A GdkByteOrder stating the byte order of visual.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_colormap_size ()
;;; 
;;; gint gdk_visual_get_colormap_size (GdkVisual *visual);
;;; 
;;; Returns the size of a colormap for this visual.
;;; 
;;; visual :
;;;     A GdkVisual.
;;; 
;;; Returns :
;;;     The size of a colormap that is suitable for visual.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_depth ()
;;; 
;;; gint gdk_visual_get_depth (GdkVisual *visual);
;;; 
;;; Returns the bit depth of this visual.
;;; 
;;; visual :
;;;     A GdkVisual.
;;; 
;;; Returns :
;;;     The bit depth of this visual.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_green_pixel_details ()
;;; 
;;; void gdk_visual_get_green_pixel_details  (GdkVisual *visual,
;;;                                           guint32 *mask,
;;;                                           gint *shift,
;;;                                           gint *precision);
;;; 
;;; Obtains values that are needed to calculate green pixel values in TrueColor
;;; and DirectColor. The "mask" is the significant bits within the pixel. The
;;; "shift" is the number of bits left we must shift a primary for it to be in
;;; position (according to the "mask"). Finally, "precision" refers to how much
;;; precision the pixel value contains for a particular primary.
;;; 
;;; visual :
;;;     a GdkVisual
;;; 
;;; mask :
;;;     A pointer to a guint32 to be filled in, or NULL. [out][allow-none]
;;; 
;;; shift :
;;;     A pointer to a gint to be filled in, or NULL. [out][allow-none]
;;; 
;;; precision :
;;;     A pointer to a gint to be filled in, or NULL. [out][allow-none]
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_red_pixel_details ()
;;; 
;;; void gdk_visual_get_red_pixel_details (GdkVisual *visual,
;;;                                        guint32 *mask,
;;;                                        gint *shift,
;;;                                        gint *precision);
;;; 
;;; Obtains values that are needed to calculate red pixel values in TrueColor
;;; and DirectColor. The "mask" is the significant bits within the pixel. The
;;; "shift" is the number of bits left we must shift a primary for it to be in
;;; position (according to the "mask"). Finally, "precision" refers to how much
;;; precision the pixel value contains for a particular primary.
;;; 
;;; visual :
;;;     A GdkVisual.
;;; 
;;; mask :
;;;     A pointer to a guint32 to be filled in, or NULL. [out][allow-none]
;;; 
;;; shift :
;;;     A pointer to a gint to be filled in, or NULL. [out][allow-none]
;;; 
;;; precision :
;;;     A pointer to a gint to be filled in, or NULL. [out][allow-none]
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_visual_type ()
;;; 
;;; GdkVisualType gdk_visual_get_visual_type (GdkVisual *visual);
;;; 
;;; Returns the type of visual this is (PseudoColor, TrueColor, etc).
;;; 
;;; visual :
;;;     A GdkVisual.
;;; 
;;; Returns :
;;;     A GdkVisualType stating the type of visual.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.visual.lisp --------------------------------------------
