;;; ----------------------------------------------------------------------------
;;; gdk.color.lisp
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
;;; Colormaps and Colors
;;; 
;;; Manipulation of colors and colormaps
;;; 
;;; Synopsis
;;; 
;;;     GdkColor;
;;;     GdkColormap;
;;;     gdk_colormap_new
;;;     gdk_colormap_ref
;;;     gdk_colormap_unref
;;;     gdk_colormap_get_system
;;;     gdk_colormap_get_system_size
;;;     gdk_colormap_change
;;;     gdk_colormap_alloc_colors
;;;     gdk_colormap_alloc_color
;;;     gdk_colormap_free_colors
;;;     gdk_colormap_query_color
;;;     gdk_colormap_get_visual
;;;     gdk_colormap_get_screen
;;;     gdk_colors_store
;;;     gdk_color_copy
;;;     gdk_color_free
;;;     gdk_colors_alloc
;;;     gdk_colors_free
;;;     gdk_color_white
;;;     gdk_color_black
;;;     gdk_color_parse
;;;     gdk_color_alloc
;;;     gdk_color_change
;;;     gdk_color_equal
;;;     gdk_color_hash
;;;     gdk_color_to_string
;;; 
;;; Description
;;; 
;;; These functions are used to modify colormaps. A colormap is an object that
;;; contains the mapping between the color values stored in memory and the RGB
;;; values that are used to display color values. In general, colormaps only
;;; contain significant information for pseudo-color visuals, but even for
;;; other visual types, a colormap object is required in some circumstances.
;;; 
;;; There are a couple of special colormaps that can be retrieved. The system
;;; colormap (retrieved with gdk_colormap_get_system()) is the default colormap
;;; of the system. If you are using GdkRGB, there is another colormap that is
;;; important - the colormap in which GdkRGB works, retrieved with
;;; gdk_rgb_get_colormap(). However, when using GdkRGB, it is not generally
;;; necessary to allocate colors directly.
;;; 
;;; In previous revisions of this interface, a number of functions that take a
;;; GdkColormap parameter were replaced with functions whose names began with
;;; "gdk_colormap_". This process will probably be extended somewhat in the
;;; future - gdk_color_white(), gdk_color_black(), and gdk_color_change() will
;;; probably become aliases.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkColor
;;; 
;;; typedef struct {
;;;   guint32 pixel;
;;;   guint16 red;
;;;   guint16 green;
;;;   guint16 blue;
;;; } GdkColor;
;;; 
;;; The GdkColor structure is used to describe an allocated or unallocated
;;; color.
;;; 
;;; guint32 pixel;
;;; 	For allocated colors, the value used to draw this color on the screen.
;;; 
;;; guint16 red;
;;; 	The red component of the color. This is a value between 0 and 65535,
;;;     with 65535 indicating full intensitiy.
;;; 
;;; guint16 green;
;;; 	The green component of the color.
;;; 
;;; guint16 blue;
;;; 	The blue component of the color.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-color "GdkColor"
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

(export (boxed-related-symbols 'gdk-color))

;;; ----------------------------------------------------------------------------
;;; GdkColormap
;;; 
;;; typedef struct {
;;;   gint      GSEAL (size);
;;;   GdkColor *GSEAL (colors);
;;; } GdkColormap;
;;; 
;;; The colormap structure contains the following public fields.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkColormap" gdk-colormap
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_colormap_get_type")
  ((:cffi visual
          gdk-colormap-visual
          (g-object gdk-visual)
          "gdk_colormap_get_visual"
          nil)
   (:cffi screen
          gdk-colormap-screen
          (g-object gdk-screen)
          "gdk_colormap_get_screen"
          nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_new ()
;;; 
;;; GdkColormap * gdk_colormap_new (GdkVisual *visual, gboolean allocate);
;;; 
;;; Creates a new colormap for the given visual.
;;; 
;;; visual :
;;;     a GdkVisual.
;;; 
;;; allocate :
;;;     if TRUE, the newly created colormap will be a private colormap, and all
;;;     colors in it will be allocated for the applications use.
;;; 
;;; Returns :
;;;     the new GdkColormap.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_colormap_new" gdk-colormap-new)
    (g-object gdk-colormap :already-referenced)
  (visual (g-object gdk-visual))
  (allocate :boolean))

(export 'gdk-colormap-new)

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_ref ()
;;; 
;;; GdkColormap * gdk_colormap_ref (GdkColormap *cmap);
;;; 
;;; Warning
;;; 
;;; gdk_colormap_ref has been deprecated since version 2.0 and should not be
;;; used in newly-written code. Use g_object_ref() instead.
;;; 
;;; Deprecated function; use g_object_ref() instead.
;;; 
;;; cmap :
;;;     a GdkColormap
;;; 
;;; Returns :
;;;     the colormap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_unref ()
;;; 
;;; void gdk_colormap_unref (GdkColormap *cmap);
;;; 
;;; Warning
;;; 
;;; gdk_colormap_unref has been deprecated since version 2.0 and should not be
;;; used in newly-written code. Use g_object_unref() instead.
;;; 
;;; Deprecated function; use g_object_unref() instead.
;;; 
;;; cmap :
;;;     a GdkColormap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_get_system ()
;;; 
;;; GdkColormap * gdk_colormap_get_system (void);
;;; 
;;; Gets the system's default colormap for the default screen. (See
;;; gdk_colormap_get_system_for_screen())
;;; 
;;; Returns :
;;;     the default colormap.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_colormap_get_system" gdk-colormap-get-system)
    (g-object gdk-colormap))

(export 'gdk-colormap-get-system)

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_get_system_size ()
;;; 
;;; gint gdk_colormap_get_system_size (void);
;;; 
;;; Warning
;;; 
;;; gdk_colormap_get_system_size is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Returns the size of the system's default colormap. (See the description of
;;; struct GdkColormap for an explanation of the size of a colormap.)
;;; 
;;; Returns :
;;;     the size of the system's default colormap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_change ()
;;; 
;;; void gdk_colormap_change (GdkColormap *colormap, gint ncolors);
;;; 
;;; Warning
;;; 
;;; gdk_colormap_change is deprecated and should not be used in newly-written 
;;; code.
;;; 
;;; Changes the value of the first ncolors in a private colormap to match the
;;; values in the colors array in the colormap. This function is obsolete and
;;; should not be used. See gdk_color_change().
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; ncolors :
;;;     the number of colors to change.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_alloc_colors ()
;;; 
;;; gint gdk_colormap_alloc_colors (GdkColormap *colormap,
;;;                                 GdkColor *colors,
;;;                                 gint n_colors,
;;;                                 gboolean writeable,
;;;                                 gboolean best_match,
;;;                                 gboolean *success);
;;; 
;;; Allocates colors from a colormap.
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; colors :
;;;     The color values to allocate. On return, the pixel values for 
;;;     allocated colors will be filled in.
;;; 
;;; n_colors :
;;;     The number of colors in colors.
;;; 
;;; writeable :
;;;     If TRUE, the colors are allocated writeable (their values can later be
;;;     changed using gdk_color_change()). Writeable colors cannot be shared
;;;     between applications.
;;; 
;;; best_match :
;;;     If TRUE, GDK will attempt to do matching against existing colors if
;;;     the colors cannot be allocated as requested.
;;; 
;;; success :
;;;     An array of length ncolors. On return, this indicates whether the
;;;     corresponding color in colors was successfully allocated or not.
;;; 
;;; Returns :
;;;     The number of colors that were not successfully allocated.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_colormap_alloc_colors" %gdk-colormap-alloc-colors) :int
  (colormap (g-object gdk-colormap))
  (colors :pointer)
  (n-colors :int)
  (writeable :boolean)
  (best-match :boolean)
  (success (:pointer :boolean)))

(defun gdk-colormap-alloc-colors (colormap colors writeable best-match)
  (with-foreign-boxed-array (n colors-ar gdk-color colors)
    (with-foreign-object (success :boolean)
      (%gdk-colormap-alloc-colors colormap
                                  colors-ar
                                  n
                                  writeable
                                  best-match
                                  success)
      (mem-ref success :boolean))))

(export 'gdk-colormap-alloc-colors)

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_alloc_color ()
;;; 
;;; gboolean gdk_colormap_alloc_color (GdkColormap *colormap,
;;;                                    GdkColor *color,
;;;                                    gboolean writeable,
;;;                                    gboolean best_match);
;;; 
;;; Allocates a single color from a colormap.
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; color :
;;;     the color to allocate. On return the pixel field will be filled in if
;;;     allocation succeeds.
;;; 
;;; writeable :
;;;     If TRUE, the color is allocated writeable (their values can later be
;;;     changed using gdk_color_change()). Writeable colors cannot be shared
;;;     between applications.
;;; 
;;; best_match :
;;;     If TRUE, GDK will attempt to do matching against existing colors if 
;;;     the color cannot be allocated as requested.
;;; 
;;; Returns :
;;;     TRUE if the allocation succeeded.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_colormap_alloc_color" gdk-colormap-alloc-color) :boolean
  (colormap (g-object gdk-colormap))
  (color (g-boxed-foreign gdk-color))
  (writeable :boolean)
  (best-match :boolean))

(export 'gdk-colormap-alloc-color)

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_free_colors ()
;;; 
;;; void gdk_colormap_free_colors (GdkColormap *colormap,
;;;                                const GdkColor *colors,
;;;                                gint n_colors);
;;; 
;;; Frees previously allocated colors.
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; colors :
;;;     the colors to free.
;;; 
;;; n_colors :
;;;     the number of colors in colors.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_colormap_free_colors" %gdk-colormap-free-colors) :void
  (colormap (g-object gdk-colormap))
  (colors :pointer)
  (n-colors :int))

(defun gdk-colormap-free-colors (colormap colors)
  (with-foreign-boxed-array (n colors-ptr gdk-color colors)
    (%gdk-colormap-free-colors colormap colors-ptr n)))

(export 'gdk-colormap-free-colors)

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_query_color ()
;;; 
;;; void gdk_colormap_query_color (GdkColormap *colormap,
;;;                                gulong pixel,
;;;                                GdkColor *result);
;;; 
;;; Locates the RGB color in colormap corresponding to the given hardware 
;;; pixel pixel. pixel must be a valid pixel in the colormap; it's a programmer
;;; error to call this function with a pixel which is not in the colormap. 
;;; Hardware pixels are normally obtained from gdk_colormap_alloc_colors(), or 
;;; from a GdkImage. (A GdkImage contains image data in hardware format, a 
;;; GdkPixbuf contains image data in a canonical 24-bit RGB format.)
;;; 
;;; This function is rarely useful; it's used for example to implement the 
;;; eyedropper feature in GtkColorSelection.
;;; 
;;; colormap :
;;;     a GdkColormap
;;; 
;;; pixel :
;;;     pixel value in hardware display format
;;; 
;;; result :
;;;     GdkColor with red, green, blue fields initialized
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_colormap_query_color" %gdk-colormap-query-color) :void
  (colormap (g-object gdk-colormap))
  (pixel :ulong)
  (result (g-boxed-foreign gdk-color)))

(defun gdk-colormap-query-color (colormap pixel)
  (let ((color (make-gdk-color)))
    (%gdk-colormap-query-color colormap pixel color)
    color))

(export 'gdk-colormap-query-color)

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_get_visual ()
;;; 
;;; GdkVisual * gdk_colormap_get_visual (GdkColormap *colormap);
;;; 
;;; Returns the visual for which a given colormap was created.
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; Returns :
;;;     the visual of the colormap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colormap_get_screen ()
;;; 
;;; GdkScreen * gdk_colormap_get_screen (GdkColormap *cmap);
;;; 
;;; Gets the screen for which this colormap was created.
;;; 
;;; cmap :
;;;     a GdkColormap
;;; 
;;; Returns :
;;;     the screen for which this colormap was created.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colors_store ()
;;; 
;;; void gdk_colors_store (GdkColormap *colormap,
;;;                        GdkColor *colors,
;;;                        gint ncolors);
;;; 
;;; Warning
;;; 
;;; gdk_colors_store is deprecated and should not be used in newly-written code.
;;; 
;;; Changes the value of the first ncolors colors in a private colormap. This 
;;; function is obsolete and should not be used. See gdk_color_change().
;;; 
;;; colormap :
;;; 	a GdkColormap.
;;; 
;;; colors :
;;; 	the new color values.
;;; 
;;; ncolors :
;;; 	the number of colors to change.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_copy ()
;;; 
;;; GdkColor * gdk_color_copy (const GdkColor *color);
;;; 
;;; Makes a copy of a color structure. The result must be freed using 
;;; gdk_color_free().
;;; 
;;; color :
;;; 	a GdkColor.
;;; 
;;; Returns :
;;; 	a copy of color.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_free ()
;;; 
;;; void gdk_color_free (GdkColor *color);
;;; 
;;; Frees a color structure created with gdk_color_copy().
;;; 
;;; color :
;;; 	a GdkColor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colors_alloc ()
;;; 
;;; gint gdk_colors_alloc (GdkColormap *colormap,
;;;                        gboolean contiguous,
;;;                        gulong *planes,
;;;                        gint nplanes,
;;;                        gulong *pixels,
;;;                        gint npixels);
;;; 
;;; Warning
;;; 
;;; gdk_colors_alloc is deprecated and should not be used in newly-written code.
;;; 
;;; Allocates colors from a colormap. This function is obsolete. See 
;;; gdk_colormap_alloc_colors(). For full documentation of the fields, see the 
;;; Xlib documentation for XAllocColorCells().
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; contiguous :
;;;     if TRUE, the colors should be allocated in contiguous color cells.
;;; 
;;; planes :
;;;     an array in which to store the plane masks.
;;; 
;;; nplanes :
;;;     the number of planes to allocate. (Or zero, to indicate that the color 
;;;     allocation should not be planar.)
;;; 
;;; pixels :
;;;     an array into which to store allocated pixel values.
;;; 
;;; npixels :
;;;     the number of pixels in each plane to allocate.
;;; 
;;; Returns :
;;;     TRUE if the allocation was successful
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_colors_free ()
;;; 
;;; void gdk_colors_free (GdkColormap *colormap,
;;;                       gulong *pixels,
;;;                       gint npixels,
;;;                       gulong planes);
;;; 
;;; Warning
;;; 
;;; gdk_colors_free is deprecated and should not be used in newly-written code.
;;; 
;;; Frees colors allocated with gdk_colors_alloc(). This function is obsolete. 
;;; See gdk_colormap_free_colors().
;;; 
;;; colormap :
;;; 	a GdkColormap.
;;; 
;;; pixels :
;;; 	the pixel values of the colors to free.
;;; 
;;; npixels :
;;; 	the number of values in pixels.
;;; 
;;; planes :
;;; 	the plane masks for all planes to free, OR'd together.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_white ()
;;; 
;;; gint gdk_color_white (GdkColormap *colormap, GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_color_white is deprecated and should not be used in newly-written code.
;;; 
;;; Returns the white color for a given colormap. The resulting value has 
;;; already allocated been allocated.
;;; 
;;; colormap :
;;; 	a GdkColormap.
;;; 
;;; color :
;;; 	the location to store the color.
;;; 
;;; Returns :
;;; 	TRUE if the allocation succeeded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_black ()
;;; 
;;; gint gdk_color_black (GdkColormap *colormap, GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_color_black is deprecated and should not be used in newly-written code.
;;; 
;;; Returns the black color for a given colormap. The resulting value has 
;;; already been allocated.
;;; 
;;; colormap :
;;; 	a GdkColormap.
;;; 
;;; color :
;;; 	the location to store the color.
;;; 
;;; Returns :
;;; 	TRUE if the allocation succeeded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_parse ()
;;; 
;;; gboolean gdk_color_parse (const gchar *spec, GdkColor *color);
;;; 
;;; Parses a textual specification of a color and fill in the red, green, and 
;;; blue fields of a GdkColor structure. The color is not allocated, you must
;;; call gdk_colormap_alloc_color() yourself. The string can either one of a
;;; large set of standard names. (Taken from the X11 rgb.txt file), or it can
;;; be a hex value in the form '#rgb' '#rrggbb' '#rrrgggbbb' or '#rrrrggggbbbb'
;;; where 'r', 'g' and 'b' are hex digits of the red, green, and blue components
;;; of the color, respectively. (White in the four forms is '#fff' '#ffffff'
;;; '#fffffffff' and '#ffffffffffff')
;;; 
;;; spec :
;;;     the string specifying the color.
;;; 
;;; color :
;;;     the GdkColor to fill in. [out]
;;; 
;;; Returns :
;;;     TRUE if the parsing succeeded.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_parse" %gdk-color-parse) :boolean
  (spec :string)
  (color (g-boxed-foreign gdk-color)))

(defun gdk-color-parse (color-spec)
  (let ((color (make-gdk-color)))
    (when (%gdk-color-parse color-spec color)
      color)))

(export 'gdk-color-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_color_alloc ()
;;; 
;;; gint gdk_color_alloc (GdkColormap *colormap, GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_color_alloc has been deprecated since version 2.2 and should not be 
;;; used in newly-written code. Use gdk_colormap_alloc_color() instead.
;;; 
;;; Allocates a single color from a colormap.
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; color :
;;;     The color to allocate. On return, the pixel field will be filled in.
;;; 
;;; Returns :
;;;     TRUE if the allocation succeeded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_change ()
;;; 
;;; gint gdk_color_change (GdkColormap *colormap, GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_color_change is deprecated and should not be used in newly-written code.
;;; 
;;; Changes the value of a color that has already been allocated. If colormap 
;;; is not a private colormap, then the color must have been allocated using 
;;; gdk_colormap_alloc_colors() with the writeable set to TRUE.
;;; 
;;; colormap :
;;;     a GdkColormap.
;;; 
;;; color :
;;;     a GdkColor, with the color to change in the pixel field, and the new 
;;;     value in the remaining fields.
;;; 
;;; Returns :
;;;     TRUE if the color was successfully changed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_equal ()
;;; 
;;; gboolean gdk_color_equal (const GdkColor *colora, const GdkColor *colorb);
;;; 
;;; Compares two colors.
;;; 
;;; colora :
;;;     a GdkColor.
;;; 
;;; colorb :
;;;     another GdkColor.
;;; 
;;; Returns :
;;;     TRUE if the two colors compare equal
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_equal" gdk-color-equal) :boolean
  (color-a (g-boxed-foreign gdk-color))
  (color-b (g-boxed-foreign gdk-color)))

(export 'gdk-color-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_color_hash ()
;;; 
;;; guint gdk_color_hash (const GdkColor *colora)
;;; 
;;; A hash function suitable for using for a hash table that stores GdkColor's.
;;; 
;;; colora :
;;;     a GdkColor.
;;; 
;;; Returns :
;;;     The hash function applied to colora
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_hash" gdk-color-hash) :uint
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-color-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_color_to_string ()
;;; 
;;; gchar * gdk_color_to_string (const GdkColor *color);
;;; 
;;; Returns a textual specification of color in the hexadecimal form 
;;; #rrrrggggbbbb, where r, g and b are hex digits representing the red, green
;;; and blue components respectively.
;;; 
;;; color :
;;;     a GdkColor
;;; 
;;; Returns :
;;;     a newly-allocated text string
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_color_to_string" gdk-color-to-string)
    (g-string :free-from-foreign t)
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-color-to-string)

;;; --- End of file gdk.color.lisp ---------------------------------------------
