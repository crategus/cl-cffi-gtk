;;; ----------------------------------------------------------------------------
;;; gdk.gc.lisp
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
;;; Graphics Contexts
;;; 
;;; Objects to encapsulate drawing properties
;;; 	
;;; Synopsis
;;; 
;;;     GdkGC;
;;;     GdkGCValues;
;;;     GdkGCValuesMask;
;;;     GdkFunction;
;;;     gdk_gc_new
;;;     gdk_gc_new_with_values
;;;     gdk_gc_get_screen
;;;     gdk_gc_ref
;;;     gdk_gc_unref
;;;     gdk_gc_destroy               
;;;     gdk_gc_set_values
;;;     gdk_gc_get_values
;;;     gdk_gc_set_foreground
;;;     gdk_gc_set_background
;;;     gdk_gc_set_rgb_fg_color
;;;     gdk_gc_set_rgb_bg_color
;;;     gdk_gc_set_font
;;;     gdk_gc_set_function
;;;     gdk_gc_set_fill
;;;     GdkFill                     
;;;     gdk_gc_set_tile
;;;     gdk_gc_set_stipple
;;;     gdk_gc_set_ts_origin
;;;     gdk_gc_set_clip_origin
;;;     gdk_gc_set_clip_mask
;;;     gdk_gc_set_clip_rectangle
;;;     gdk_gc_set_clip_region
;;;     gdk_gc_set_subwindow
;;;     GdkSubwindowMode;            
;;;     gdk_gc_set_exposures
;;;     gdk_gc_set_line_attributes
;;;     GdkLineStyle;                 
;;;     GdkCapStyle;                  
;;;     GdkJoinStyle;                 
;;;     gdk_gc_set_dashes
;;;     gdk_gc_copy
;;;     gdk_gc_set_colormap
;;;     gdk_gc_get_colormap
;;;     gdk_gc_offset
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkGC
;;; 
;;; Description
;;; 
;;; All drawing operations in GDK take a graphics context (GC) argument. A
;;; graphics context encapsulates information about the way things are drawn,
;;; such as the foreground color or line width. By using graphics contexts, the
;;; number of arguments to each drawing call is greatly reduced, and
;;; communication overhead is minimized, since identical arguments do not need
;;; to be passed repeatedly.
;;; 
;;; Most values of a graphics context can be set at creation time by using
;;; gdk_gc_new_with_values(), or can be set one-by-one using functions such as
;;; gdk_gc_set_foreground(). A few of the values in the GC, such as the dash
;;; pattern, can only be set by the latter method.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkFunction
;;; 
;;; typedef enum
;;; {
;;;   GDK_COPY,
;;;   GDK_INVERT,
;;;   GDK_XOR,
;;;   GDK_CLEAR,
;;;   GDK_AND,
;;;   GDK_AND_REVERSE,
;;;   GDK_AND_INVERT,
;;;   GDK_NOOP,
;;;   GDK_OR,
;;;   GDK_EQUIV,
;;;   GDK_OR_REVERSE,
;;;   GDK_COPY_INVERT,
;;;   GDK_OR_INVERT,
;;;   GDK_NAND,
;;;   GDK_NOR,
;;;   GDK_SET
;;; } GdkFunction;
;;; 
;;; Determines how the bit values for the source pixels are combined with the
;;; bit values for destination pixels to produce the final result. The sixteen
;;; values here correspond to the 16 different possible 2x2 truth tables. Only
;;; a couple of these values are usually useful; for colored images, only
;;; GDK_COPY, GDK_XOR and GDK_INVERT are generally useful. For bitmaps, GDK_AND
;;; and GDK_OR are also useful.
;;; 
;;; GDK_COPY
;;; 	dst = src
;;; 
;;; GDK_INVERT
;;; 	dst = NOT dst
;;; 
;;; GDK_XOR
;;; 	dst = src XOR dst
;;; 
;;; GDK_CLEAR
;;; 	dst = 0
;;; 
;;; GDK_AND
;;; 	dst = dst AND src
;;; 
;;; GDK_AND_REVERSE
;;; 	dst = src AND (NOT dst)
;;; 
;;; GDK_AND_INVERT
;;; 	dst = (NOT src) AND dst
;;; 
;;; GDK_NOOP
;;; 	dst = dst
;;; 
;;; GDK_OR
;;; 	dst = src OR dst
;;; 
;;; GDK_EQUIV
;;; 	dst = (NOT src) XOR dst
;;; 
;;; GDK_OR_REVERSE
;;; 	dst = src OR (NOT dst)
;;; 
;;; GDK_COPY_INVERT
;;; 	dst = NOT src
;;; 
;;; GDK_OR_INVERT
;;; 	dst = (NOT src) OR dst
;;; 
;;; GDK_NAND
;;; 	dst = (NOT src) OR (NOT dst)
;;; 
;;; GDK_NOR
;;; 	dst = (NOT src) AND (NOT dst)
;;; 
;;; GDK_SET
;;; 	dst = 1
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFunction" gdk-function
  (:export t :type-initializer "gdk_function_get_type")
  (:copy 0)
  (:invert 1)
  (:xor 2)
  (:clear 3)
  (:and 4)
  (:and-reverse 5)
  (:and-invert 6)
  (:noop 7)
  (:or 8)
  (:equiv 9)
  (:or-reverse 10)
  (:copy-invert 11)
  (:or-invert 12)
  (:nand 13)
  (:nor 14)
  (:set 15))

;;; ----------------------------------------------------------------------------
;;; enum GdkFill
;;; 
;;; typedef enum
;;; {
;;;   GDK_SOLID,
;;;   GDK_TILED,
;;;   GDK_STIPPLED,
;;;   GDK_OPAQUE_STIPPLED
;;; } GdkFill;
;;; 
;;; Determines how primitives are drawn.
;;; 
;;; GDK_SOLID
;;; 	draw with the foreground color.
;;; 
;;; GDK_TILED
;;; 	draw with a tiled pixmap.
;;; 
;;; GDK_STIPPLED
;;; 	draw using the stipple bitmap. Pixels corresponding to bits in the
;;;     stipple bitmap that are set will be drawn in the foreground color;
;;;     pixels corresponding to bits that are not set will be left untouched.
;;; 
;;; GDK_OPAQUE_STIPPLED
;;; 	draw using the stipple bitmap. Pixels corresponding to bits in the
;;;     stipple bitmap that are set will be drawn in the foreground color;
;;;     pixels corresponding to bits that are not set will be drawn with the
;;;     background color.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFill" gdk-fill
  (:export t
   :type-initializer "gdk_fill_get_type")
  (:solid 0)
  (:tiled 1)
  (:stippled 2)
  (:opaque-stippled 3))

;;; ----------------------------------------------------------------------------
;;; enum GdkSubwindowMode
;;; 
;;; typedef enum
;;; {
;;;   GDK_CLIP_BY_CHILDREN = 0,
;;;   GDK_INCLUDE_INFERIORS = 1
;;; } GdkSubwindowMode;
;;; 
;;; Determines how drawing onto a window will affect child windows of that
;;; window.
;;; 
;;; GDK_CLIP_BY_CHILDREN
;;; 	only draw onto the window itself.
;;; 
;;; GDK_INCLUDE_INFERIORS
;;; 	draw onto the window and child windows.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkSubwindowMode" gdk-subwindow-mode
  (:export t
   :type-initializer "gdk_subwindow_mode_get_type")
  (:clip-by-children 0)
  (:include-inferiors 1))

;;; ----------------------------------------------------------------------------
;;; GdkGC
;;; 
;;; typedef struct _GdkGC GdkGC;
;;; 
;;; The GdkGC structure represents a graphics context. It is an opaque
;;; structure with no user-visible elements.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkGC" gdk-gc
  (:type-initializer "gdk_gc_get_type")
  ((:cffi screen graphics-context-screen (g-object gdk-screen)
          "gdk_gc_get_screen" nil)
   (:cffi foreground graphics-context-foreground (g-boxed-foreign gdk-color)
          nil "gdk_gc_set_foreground")
   (:cffi background graphics-context-background (g-boxed-foreign gdk-color)
          nil "gdk_gc_set_background")
   (:cffi rgb-fg-color graphics-context-rgb-fg-color (g-boxed-foreign gdk-color)
          nil "gdk_gc_set_rgb_fg_color")
   (:cffi rgb-bg-color graphics-context-rgb-bg-color (g-boxed-foreign gdk-color)
          nil "gdk_gc_set_rgb_bg_color")
   (:cffi font graphics-context-font (g-boxed-foreign font)
          nil "gdk_gc_set_font")
   (:cffi function graphics-context-function gdk-function
          nil "gdk_gc_set_function")
   (:cffi fill graphics-context-fill gdk-fill
          nil "gdk_gc_set_fill")
   (:cffi tile graphics-context-tile (g-object gdk-pixmap)
          nil "gdk_gc_set_tile")
   (:cffi stipple graphics-context-stipple (g-object gdk-pixmap)
          nil "gdk_gc_set_stipple")
   (:cffi clip-mask graphics-context-clip-mask (g-object gdk-pixmap)
          nil "gdk_gc_set_clip_mask")
   (:cffi clip-rectangle graphics-context-clip-rectangle
          (g-boxed-foreign gdk-rectangle)
          nil "gdk_gc_set_clip_rectangle")
   (:cffi clip-region graphics-context-clip-region (g-boxed-foreign gdk-region)
          nil "gdk_gc_set_clip_region")
   (:cffi subwindow graphics-context-subwindow subwindow-mode
          nil "gdk_gc_set_subwindow")
   (:cffi exposures graphics-context-exposures :boolean
          nil "gdk_gc_set_exposures")
   (:cffi colormap graphics-context-colormap (g-object gdk-colormap)
          "gdk_gc_get_colormap" "gdk_gc_set_colormap")))

;;; ----------------------------------------------------------------------------
;;; enum GdkJoinStyle
;;; 
;;; typedef enum
;;; {
;;;   GDK_JOIN_MITER,
;;;   GDK_JOIN_ROUND,
;;;   GDK_JOIN_BEVEL
;;; } GdkJoinStyle;
;;; 
;;; Determines how the joins between segments of a polygon are drawn.
;;; 
;;; GDK_JOIN_MITER
;;; 	the sides of each line are extended to meet at an angle.
;;; 
;;; GDK_JOIN_ROUND
;;; 	the sides of the two lines are joined by a circular arc.
;;; 
;;; GDK_JOIN_BEVEL
;;; 	the sides of the two lines are joined by a straight line which makes
;;;     an equal angle with each line.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkJoinStyle" gdk-join-style
  (:export t
   :type-initializer "gdk_join_style_get_type")
  (:miter 0)
  (:round 1)
  (:bevel 2))

;;; ----------------------------------------------------------------------------
;;; enum GdkLineStyle
;;; 
;;; typedef enum
;;; {
;;;   GDK_LINE_SOLID,
;;;   GDK_LINE_ON_OFF_DASH,
;;;   GDK_LINE_DOUBLE_DASH
;;; } GdkLineStyle;
;;; 
;;; Determines how lines are drawn.
;;; 
;;; GDK_LINE_SOLID
;;; 	lines are drawn solid.
;;; 
;;; GDK_LINE_ON_OFF_DASH
;;; 	even segments are drawn; odd segments are not drawn.
;;; 
;;; GDK_LINE_DOUBLE_DASH
;;; 	even segments are normally. Odd segments are drawn in the background
;;;     color if the fill style is GDK_SOLID, or in the background color masked
;;;     by the stipple if the fill style is GDK_STIPPLED.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkLineStyle" gdk-line-style
  (:export t
   :type-initializer "gdk_line_style_get_type")
  (:solid 0)
  (:on-off-dash 1)
  (:double-dash 2))

;;; ----------------------------------------------------------------------------
;;; enum GdkCapStyle
;;; 
;;; typedef enum
;;; {
;;;   GDK_CAP_NOT_LAST,
;;;   GDK_CAP_BUTT,
;;;   GDK_CAP_ROUND,
;;;   GDK_CAP_PROJECTING
;;; } GdkCapStyle;
;;; 
;;; Determines how the end of lines are drawn.
;;; 
;;; GDK_CAP_NOT_LAST
;;; 	the same as GDK_CAP_BUTT for lines of non-zero width. for zero width
;;;     lines, the final point on the line will not be drawn.
;;; 
;;; GDK_CAP_BUTT
;;; 	the ends of the lines are drawn squared off and extending to the
;;;     coordinates of the end point.
;;; 
;;; GDK_CAP_ROUND
;;; 	the ends of the lines are drawn as semicircles with the diameter equal
;;;     to the line width and centered at the end point.
;;; 
;;; GDK_CAP_PROJECTING
;;; 	the ends of the lines are drawn squared off and extending half the
;;;     width of the line beyond the end point.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkCapStyle" gdk-cap-style
  (:export t
   :type-initializer "gdk_cap_style_get_type")
  (:not-last 0)
  (:butt 1)
  (:round 2)
  (:projecting 3))

;;; ----------------------------------------------------------------------------
;;; struct GdkGCValues
;;; 
;;; struct GdkGCValues {
;;;   GdkColor	    foreground;
;;;   GdkColor	    background;
;;;   GdkFont	   *font;
;;;   GdkFunction	    function;
;;;   GdkFill	    fill;
;;;   GdkPixmap	   *tile;
;;;   GdkPixmap	   *stipple;
;;;   GdkPixmap	   *clip_mask;
;;;   GdkSubwindowMode  subwindow_mode;
;;;   gint		    ts_x_origin;
;;;   gint		    ts_y_origin;
;;;   gint		    clip_x_origin;
;;;   gint		    clip_y_origin;
;;;   gint		    graphics_exposures;
;;;   gint		    line_width;
;;;   GdkLineStyle	    line_style;
;;;   GdkCapStyle	    cap_style;
;;;   GdkJoinStyle	    join_style;
;;; };
;;; 
;;; The GdkGCValues structure holds a set of values used to create or modify a
;;; graphics context.
;;; 
;;; GdkColor foreground;
;;; 	the foreground color. Note that gdk_gc_get_values() only sets the pixel
;;;     value.
;;; 
;;; GdkColor background;
;;; 	the background color. Note that gdk_gc_get_values() only sets the pixel
;;;     value.
;;; 
;;; GdkFont *font;
;;; 	the default font.
;;; 
;;; GdkFunction function;
;;; 	the bitwise operation used when drawing.
;;; 
;;; GdkFill fill;
;;; 	the fill style.
;;; 
;;; GdkPixmap *tile;
;;; 	the tile pixmap.
;;; 
;;; GdkPixmap *stipple;
;;; 	the stipple bitmap.
;;; 
;;; GdkPixmap *clip_mask;
;;; 	the clip mask bitmap.
;;; 
;;; GdkSubwindowMode subwindow_mode;
;;; 	the subwindow mode.
;;; 
;;; gint ts_x_origin;
;;; 	the x origin of the tile or stipple.
;;; 
;;; gint ts_y_origin;
;;; 	the y origin of the tile or stipple.
;;; 
;;; gint clip_x_origin;
;;; 	the x origin of the clip mask.
;;; 
;;; gint clip_y_origin;
;;; 	the y origin of the clip mask.
;;; 
;;; gint graphics_exposures;
;;; 	whether graphics exposures are enabled.
;;; 
;;; gint line_width;
;;; 	the line width.
;;; 
;;; GdkLineStyle line_style;
;;; 	the way dashed lines are drawn.
;;; 
;;; GdkCapStyle cap_style;
;;; 	the way the ends of lines are drawn.
;;; 
;;; GdkJoinStyle join_style;
;;; 	the way joins between lines are drawn.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-gc-values nil
  (foregound gdk-color :initform (make-gdk-color) :inline t)
  (background gdk-color :initform (make-gdk-color) :inline t)
  (font (g-boxed-foreign gdk-font) :initform nil)
  (function gdk-function :initform :copy)
  (fill gdk-fill :initform :solid)
  (tile (g-object gdk-pixmap) :initform nil)
  (stipple (g-object gdk-pixmap) :initform nil)
  (clip-mask (g-object gdk-pixmap) :initform nil)
  (subwindow-mode gdk-subwindow-mode :initform :clip-by-children)
  (ts-x-origin :int :initform 0)
  (ts-y-origin :int :initform 0)
  (clip-x-origin :int :initform 0)
  (clip-y-origin :int :initform 0)
  (graphics-exposures :boolean :initform t)
  (line-width :int :initform 0)
  (line-style gdk-line-style :initform :solid)
  (cap-style gdk-cap-style :initform :butt)
  (join-style gdk-join-style :initform :miter))

(export (boxed-related-symbols 'gdk-gc-values))

;;; ----------------------------------------------------------------------------
;;; enum GdkGCValuesMask
;;; 
;;; typedef enum
;;; {
;;;   GDK_GC_FOREGROUND    = 1 << 0,
;;;   GDK_GC_BACKGROUND    = 1 << 1,
;;;   GDK_GC_FONT	       = 1 << 2,
;;;   GDK_GC_FUNCTION      = 1 << 3,
;;;   GDK_GC_FILL	       = 1 << 4,
;;;   GDK_GC_TILE	       = 1 << 5,
;;;   GDK_GC_STIPPLE       = 1 << 6,
;;;   GDK_GC_CLIP_MASK     = 1 << 7,
;;;   GDK_GC_SUBWINDOW     = 1 << 8,
;;;   GDK_GC_TS_X_ORIGIN   = 1 << 9,
;;;   GDK_GC_TS_Y_ORIGIN   = 1 << 10,
;;;   GDK_GC_CLIP_X_ORIGIN = 1 << 11,
;;;   GDK_GC_CLIP_Y_ORIGIN = 1 << 12,
;;;   GDK_GC_EXPOSURES     = 1 << 13,
;;;   GDK_GC_LINE_WIDTH    = 1 << 14,
;;;   GDK_GC_LINE_STYLE    = 1 << 15,
;;;   GDK_GC_CAP_STYLE     = 1 << 16,
;;;   GDK_GC_JOIN_STYLE    = 1 << 17
;;; } GdkGCValuesMask;
;;; 
;;; A set of bit flags used to indicate which fields GdkGCValues structure are
;;; set.
;;; 
;;; GDK_GC_FOREGROUND
;;; 	the foreground is set.
;;; 
;;; GDK_GC_BACKGROUND
;;; 	the background is set.
;;; 
;;; GDK_GC_FONT
;;; 	the font is set.
;;; 
;;; GDK_GC_FUNCTION
;;; 	the function is set.
;;; 
;;; GDK_GC_FILL
;;; 	the fill is set.
;;; 
;;; GDK_GC_TILE
;;; 	the tile is set.
;;; 
;;; GDK_GC_STIPPLE
;;; 	the stipple is set.
;;; 
;;; GDK_GC_CLIP_MASK
;;; 	the clip_mask is set.
;;; 
;;; GDK_GC_SUBWINDOW
;;; 	the subwindow_mode is set.
;;; 
;;; GDK_GC_TS_X_ORIGIN
;;; 	the ts_x_origin is set.
;;; 
;;; GDK_GC_TS_Y_ORIGIN
;;; 	the ts_y_origin is set.
;;; 
;;; GDK_GC_CLIP_X_ORIGIN
;;; 	the clip_x_origin is set.
;;; 
;;; GDK_GC_CLIP_Y_ORIGIN
;;; 	the clip_y_origin is set.
;;; 
;;; GDK_GC_EXPOSURES
;;; 	the graphics_exposures is set.
;;; 
;;; GDK_GC_LINE_WIDTH
;;; 	the line_width is set.
;;; 
;;; GDK_GC_LINE_STYLE
;;; 	the line_style is set.
;;; 
;;; GDK_GC_CAP_STYLE
;;; 	the cap_style is set.
;;; 
;;; GDK_GC_JOIN_STYLE
;;; 	the join_style is set.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkGCValuesMask" gdk-gc-values-mask
  (:export t
   :type-initializer "gdk_gc_values_mask_get_type")
  (:foreground 1)
  (:background 2)
  (:font 4)
  (:function 8)
  (:fill 16)
  (:tile 32)
  (:stipple 64)
  (:clip-mask 128)
  (:subwindow 256)
  (:ts-x-origin 512)
  (:ts-y-origin 1024)
  (:clip-x-origin 2048)
  (:clip-y-origin 4096)
  (:exposures 8192)
  (:line-width 16384)
  (:line-style 32768)
  (:cap-style 65536)
  (:join-style 131072))

(export 'gdk-gc-values-mask)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_new ()
;;; 
;;; GdkGC * gdk_gc_new (GdkDrawable *drawable);
;;; 
;;; Warning
;;; 
;;; gdk_gc_new has been deprecated since version 2.22 and should not be used
;;; in newly-written code. Use Cairo for rendering.
;;; 
;;; Create a new graphics context with default values.
;;; 
;;; drawable :
;;; 	a GdkDrawable. The created GC must always be used with drawables of the
;;;     same depth as this one.
;;; 
;;; Returns :
;;; 	the new graphics context.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_new" gdk-gc-new)
    (g-object gdk-gc :already-referenced)
  (drawable (g-object gdk-drawable)))

(export 'gdk-gc-new)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_new_with_values ()
;;; 
;;; GdkGC * gdk_gc_new_with_values (GdkDrawable *drawable,
;;;                                 GdkGCValues *values,
;;;                                 GdkGCValuesMask values_mask);
;;; 
;;; Warning
;;; 
;;; gdk_gc_new_with_values has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use Cairo for rendering.
;;; 
;;; Create a new GC with the given initial values.
;;; 
;;; drawable :
;;; 	a GdkDrawable. The created GC must always be used with drawables of
;;;     the same depth as this one.
;;; 
;;; values :
;;; 	a structure containing initial values for the GC.
;;; 
;;; values_mask :
;;; 	a bit mask indicating which fields in values are set.
;;; 
;;; Returns :
;;; 	the new graphics context.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_new_with_values" gdk-gc-new-with-values)
    (g-object gdk-gc :already-referenced)
  (drawable (g-object gdk-drawable))
  (values (g-boxed-foreign gdk-gc-values))
  (values-mask gdk-gc-values-mask))

(export 'gdk-gc-new-with-values)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_get_screen ()
;;; 
;;; GdkScreen * gdk_gc_get_screen (GdkGC *gc);
;;; 
;;; Warning
;;; 
;;; gdk_gc_get_screen is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Gets the GdkScreen for which gc was created
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; Returns :
;;; 	the GdkScreen for gc.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_ref ()
;;; 
;;; GdkGC * gdk_gc_ref (GdkGC *gc);
;;; 
;;; Warning
;;; 
;;; gdk_gc_ref has been deprecated since version 2.0 and should not be used in
;;; newly-written code. Use g_object_ref() instead.
;;; 
;;; Deprecated function; use g_object_ref() instead.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; Returns :
;;; 	the gc.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_unref ()
;;; 
;;; void gdk_gc_unref(GdkGC *gc);
;;; 
;;; Warning
;;; 
;;; gdk_gc_unref has been deprecated since version 2.0 and should not be used
;;; in newly-written code. Use g_object_unref() instead.
;;; 
;;; Decrement the reference count of gc.
;;; 
;;; gc :
;;; 	a GdkGC
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_destroy
;;; 
;;; #define gdk_gc_destroy g_object_unref
;;; 
;;; Warning
;;; 
;;; gdk_gc_destroy is deprecated and should not be used in newly-written code.
;;; Use g_object_unref() instead
;;; 
;;; This function is obsolete and should not be used.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_values ()
;;; 
;;; void gdk_gc_set_values (GdkGC *gc,
;;;                         GdkGCValues *values,
;;;                         GdkGCValuesMask values_mask);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_values has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use Cairo for rendering.
;;; 
;;; Sets attributes of a graphics context in bulk. For each flag set in
;;; values_mask, the corresponding field will be read from values and set as
;;; the new value for gc. If you're only setting a few values on gc, calling
;;; individual "setter" functions is likely more convenient.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; values :
;;; 	struct containing the new values
;;; 
;;; values_mask :
;;; 	mask indicating which struct fields are to be used
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_set_values" gdk-gc-set-values) :void
  (gc (g-object gdk-gc))
  (values (g-boxed-foreign gdk-gc-values))
  (values-mask gdk-gc-values-mask))

(export 'gdk-gc-set-values)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_get_values ()
;;; 
;;; void gdk_gc_get_values (GdkGC *gc, GdkGCValues *values);
;;; 
;;; Warning
;;; 
;;; gdk_gc_get_values has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use Cairo for rendering.
;;; 
;;; Retrieves the current values from a graphics context. Note that only the
;;; pixel values of the values->foreground and values->background are filled,
;;; use gdk_colormap_query_color() to obtain the rgb values if you need them.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; values :
;;; 	the GdkGCValues structure in which to store the results.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_get_values" %gdk-gc-get-values) :void
  (gc (g-object gdk-gc))
  (values (g-boxed-foreign gdk-gc-values)))

(defun gdk-gc-get-values (graphics-context)
  (let ((values (make-gdk-gc-values)))
    (%gdk-gc-get-values graphics-context values)
    values))

(export 'gdk-gc-get-values)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_foreground ()
;;; 
;;; void gdk_gc_set_foreground (GdkGC *gc, const GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_foreground has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use gdk_cairo_set_source_color() to use
;;; a GdkColor as the source in Cairo.
;;; 
;;; Sets the foreground color for a graphics context. Note that this function
;;; uses color->pixel, use gdk_gc_set_rgb_fg_color() to specify the foreground
;;; color as red, green, blue components.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; color :
;;; 	the new foreground color.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_background ()
;;; 
;;; void gdk_gc_set_background (GdkGC *gc, const GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_background has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use gdk_cairo_set_source_color() to use a
;;; GdkColor as the source in Cairo. Note that if you want to draw a background
;;; and a foreground in Cairo, you need to call drawing functions (like
;;; cairo_fill()) twice.
;;; 
;;; Sets the background color for a graphics context. Note that this function
;;; uses color->pixel, use gdk_gc_set_rgb_bg_color() to specify the background
;;; color as red, green, blue components.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; color :
;;; 	the new background color.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_rgb_fg_color ()
;;; 
;;; void gdk_gc_set_rgb_fg_color (GdkGC *gc, const GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_rgb_fg_color has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use gdk_cairo_set_source_color() instead.
;;; 
;;; Set the foreground color of a GC using an unallocated color. The pixel
;;; value for the color will be determined using GdkRGB. If the colormap for
;;; the GC has not previously been initialized for GdkRGB, then for pseudo-color
;;; colormaps (colormaps with a small modifiable number of colors), a colorcube
;;; will be allocated in the colormap.
;;; 
;;; Calling this function for a GC without a colormap is an error.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; color :
;;; 	an unallocated GdkColor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_rgb_bg_color ()
;;; 
;;; void gdk_gc_set_rgb_bg_color (GdkGC *gc, const GdkColor *color);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_rgb_bg_color has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use gdk_cairo_set_source_color() instead.
;;; 
;;; Set the background color of a GC using an unallocated color. The pixel value
;;; for the color will be determined using GdkRGB. If the colormap for the GC
;;; has not previously been initialized for GdkRGB, then for pseudo-color
;;; colormaps (colormaps with a small modifiable number of colors), a colorcube
;;; will be allocated in the colormap.
;;; 
;;; Calling this function for a GC without a colormap is an error.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; color :
;;; 	an unallocated GdkColor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_font ()
;;; 
;;; void gdk_gc_set_font (GdkGC *gc, GdkFont *font);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_font is deprecated and should not be used in newly-written code.
;;; 
;;; Sets the font for a graphics context. (Note that all text-drawing functions
;;; in GDK take a font argument; the value set here is used when that argument
;;; is NULL.)
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; font :
;;; 	the new font.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_function ()
;;; 
;;; void gdk_gc_set_function (GdkGC *gc, GdkFunction function);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_function has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use cairo_set_operator() with Cairo.
;;; 
;;; Determines how the current pixel values and the pixel values being drawn
;;; are combined to produce the final pixel values.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; function :
;;; 	the GdkFunction to use
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_fill ()
;;; 
;;; void gdk_gc_set_fill (GdkGC *gc, GdkFill fill);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_fill has been deprecated since version 2.22 and should not be
;;; used in newly-written code. You can achieve tiling in Cairo by using
;;; cairo_pattern_set_extend() on the source. For stippling, see the deprecation
;;; comments on gdk_gc_set_stipple().
;;; 
;;; Set the fill mode for a graphics context.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; fill :
;;; 	the new fill mode.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_tile ()
;;; 
;;; void gdk_gc_set_tile (GdkGC *gc, GdkPixmap *tile);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_tile has been deprecated since version 2.22 and should not be
;;; used in newly-written code. The following code snippet sets a tiling
;;; GdkPixmap as the source in Cairo:
;;; 
;;; gdk_cairo_set_source_pixmap (cr, tile, ts_origin_x, ts_origin_y);
;;; cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
;;; 
;;; Set a tile pixmap for a graphics context. This will only be used if the
;;; fill mode is GDK_TILED.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; tile :
;;; 	the new tile pixmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_stipple ()
;;; 
;;; void gdk_gc_set_stipple (GdkGC *gc, GdkPixmap *stipple);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_stipple has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Stippling has no direct replacement in Cairo.
;;; If you want to achieve an identical look, you can use the stipple bitmap as
;;; a mask. Most likely, this involves rendering the source to an intermediate
;;; surface using cairo_push_group() first, so that you can then use
;;; cairo_mask() to achieve the stippled look.
;;; 
;;; Set the stipple bitmap for a graphics context. The stipple will only be
;;; used if the fill mode is GDK_STIPPLED or GDK_OPAQUE_STIPPLED.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; stipple :
;;; 	the new stipple bitmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_ts_origin ()
;;; 
;;; void gdk_gc_set_ts_origin (GdkGC *gc, gint x, gint y);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_ts_origin has been deprecated since version 2.22 and should not
;;; be used in newly-written code. You can set the origin for tiles and stipples
;;; in Cairo by changing the source's matrix using cairo_pattern_set_matrix().
;;; Or you can specify it with gdk_cairo_set_source_pixmap() as shown in the
;;; example for gdk_gc_set_tile().
;;; 
;;; Set the origin when using tiles or stipples with the GC. The tile or stipple
;;; will be aligned such that the upper left corner of the tile or stipple will
;;; coincide with this point.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x :
;;; 	the x-coordinate of the origin.
;;; 
;;; y :
;;; 	the y-coordinate of the origin.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_set_ts_origin" gdk-gc-set-ts-origin) :void
  (gc (g-object gdk-gc))
  (x :int)
  (y :int))

(export 'gdk-gc-set-ts-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_clip_origin ()
;;; 
;;; void gdk_gc_set_clip_origin (GdkGC *gc, gint x, gint y);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_clip_origin has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use cairo_translate() before applying
;;; the clip path in Cairo.
;;; 
;;; Sets the origin of the clip mask. The coordinates are interpreted relative
;;; to the upper-left corner of the destination drawable of the current
;;; operation.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; x :
;;; 	the x-coordinate of the origin.
;;; 
;;; y :
;;; 	the y-coordinate of the origin.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_set_clip_origin" gdk-gc-set-clip-origin) :void
  (gc (g-object gdk-gc))
  (x :int)
  (y :int))

(export 'gdk-gc-set-clip-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_clip_mask ()
;;; 
;;; void gdk_gc_set_clip_mask (GdkGC *gc, GdkBitmap *mask);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_clip_mask has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use cairo_mask() instead.
;;; 
;;; Sets the clip mask for a graphics context from a bitmap. The clip mask is
;;; interpreted relative to the clip origin. (See gdk_gc_set_clip_origin()).
;;; 
;;; gc :
;;; 	the GdkGC.
;;; 
;;; mask :
;;; 	a bitmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_clip_rectangle ()
;;; 
;;; void gdk_gc_set_clip_rectangle (GdkGC *gc, const GdkRectangle *rectangle);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_clip_rectangle has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use cairo_rectangle() and cairo_clip()
;;; in Cairo.
;;; 
;;; Sets the clip mask for a graphics context from a rectangle. The clip mask
;;; is interpreted relative to the clip origin. (See gdk_gc_set_clip_origin()).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; rectangle :
;;; 	the rectangle to clip to.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_clip_region ()
;;; 
;;; void gdk_gc_set_clip_region (GdkGC *gc, const GdkRegion *region);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_clip_region has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use gdk_cairo_region() and cairo_clip()
;;; in Cairo.
;;; 
;;; Sets the clip mask for a graphics context from a region structure. The clip
;;; mask is interpreted relative to the clip origin.
;;; (See gdk_gc_set_clip_origin()).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; region :
;;; 	the GdkRegion.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_subwindow ()
;;; 
;;; void gdk_gc_set_subwindow (GdkGC *gc, GdkSubwindowMode mode);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_subwindow has been deprecated since version 2.22 and should not
;;; be used in newly-written code. There is no replacement. If you need to
;;; control subwindows, you must use drawing operations of the underlying window
;;; system manually. Cairo will always use GDK_INCLUDE_INFERIORS on sources and
;;; masks and GDK_CLIP_BY_CHILDREN on targets.
;;; 
;;; Sets how drawing with this GC on a window will affect child windows of that
;;; window.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; mode :
;;; 	the subwindow mode.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_exposures ()
;;; 
;;; void gdk_gc_set_exposures (GdkGC *gc, gboolean exposures);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_exposures has been deprecated since version 2.22 and should not
;;; be used in newly-written code. There is no replacement. If you need to
;;; control exposures, you must use drawing operations of the underlying window
;;; system or use gdk_window_invalidate_rect(). Cairo will never generate
;;; exposures.
;;; 
;;; Sets whether copying non-visible portions of a drawable using this graphics
;;; context generate exposure events for the corresponding regions of the
;;; destination drawable. (See gdk_draw_drawable()).
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; exposures :
;;; 	if TRUE, exposure events will be generated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_line_attributes ()
;;; 
;;; void gdk_gc_set_line_attributes (GdkGC *gc,
;;;                                  gint line_width,
;;;                                  GdkLineStyle line_style,
;;;                                  GdkCapStyle cap_style,
;;;                                  GdkJoinStyle join_style);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_line_attributes has been deprecated since version 2.22 and should
;;; not be used in newly-written code. Use the Cairo functions
;;; cairo_set_line_width(), cairo_set_line_join(), cairo_set_line_cap() and
;;; cairo_set_dash() to affect the stroking behavior in Cairo. Keep in mind that
;;; the default attributes of a cairo_t are different from the default
;;; attributes of a GdkGC.
;;; 
;;; Sets various attributes of how lines are drawn. See the corresponding
;;; members of GdkGCValues for full explanations of the arguments.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; line_width :
;;; 	the width of lines.
;;; 
;;; line_style :
;;; 	the dash-style for lines.
;;; 
;;; cap_style :
;;; 	the manner in which the ends of lines are drawn.
;;; 
;;; join_style :
;;; 	the in which lines are joined together.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_set_line_attributes" gdk-gc-set-line-attributes) :void
  (gc (g-object gdk-gc))
  (line-width :int)
  (line-style gdk-line-style)
  (cap-style gdk-cap-style)
  (join-style gdk-join-style))

(export 'gdk-gc-set-line-attributes)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_dashes ()
;;; 
;;; void gdk_gc_set_dashes (GdkGC *gc,
;;;                         gint dash_offset,
;;;                         gint8 dash_list[],
;;;                         gint n);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_dashes has been deprecated since version 2.22 and should not be
;;; used in newly-written code. Use cairo_set_dash() to set the dash in Cairo.
;;; 
;;; Sets the way dashed-lines are drawn. Lines will be drawn with alternating
;;; on and off segments of the lengths specified in dash_list. The manner in
;;; which the on and off segments are drawn is determined by the line_style
;;; value of the GC. (This can be changed with gdk_gc_set_line_attributes().)
;;; 
;;; The dash_offset defines the phase of the pattern, specifying how many
;;; pixels into the dash-list the pattern should actually begin.
;;; 
;;; gc :
;;; 	a GdkGC.
;;; 
;;; dash_offset :
;;; 	the phase of the dash pattern.
;;; 
;;; dash_list :
;;; 	an array of dash lengths.
;;; 
;;; n :
;;; 	the number of elements in dash_list.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_set_dashes" %gdk-gc-set-dashes) :void
  (gc (g-object gdk-gc))
  (dash-offset :int)
  (dash-list :pointer)
  (n :int))

(defun gdk-gc-set-dashes (graphics-context dash-offset dash-list)
  (let ((n (length dash-list)))
    (with-foreign-object (dash-list-ptr :int8 n)
      (let ((i 0))
        (map nil
             (lambda (dash)
               (setf (mem-aref dash-list-ptr :int8 i) dash)
               (incf i))
             dash-list))
      (%gdk-gc-set-dashes graphics-context dash-offset dash-list n))))

(export 'gdk-gc-set-dashes)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_copy ()
;;; 
;;; void gdk_gc_copy (GdkGC *dst_gc, GdkGC *src_gc);
;;; 
;;; Warning
;;; 
;;; gdk_gc_copy has been deprecated since version 2.22 and should not be used
;;; in newly-written code. Use Cairo for drawing. cairo_save() and
;;; cairo_restore() can be helpful in cases where you'd have copied a GdkGC.
;;; 
;;; Copy the set of values from one graphics context onto another graphics
;;; context.
;;; 
;;; dst_gc :
;;; 	the destination graphics context.
;;; 
;;; src_gc :
;;; 	the source graphics context.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_copy" gdk-gc-copy) :void
  (dst-gc (g-object gdk-gc))
  (src-gc (g-object gdk-gc)))

(export 'gdk-gc-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_gc_set_colormap ()
;;; 
;;; void gdk_gc_set_colormap (GdkGC *gc, GdkColormap *colormap);
;;; 
;;; Warning
;;; 
;;; gdk_gc_set_colormap has been deprecated since version 2.22 and should not
;;; be used in newly-written code. There is no replacement. Cairo handles
;;; colormaps automatically, so there is no need to care about them.
;;; 
;;; Sets the colormap for the GC to the given colormap. The depth of the
;;; colormap's visual must match the depth of the drawable for which the GC was
;;; created.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; colormap :
;;; 	a GdkColormap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_get_colormap ()
;;; 
;;; GdkColormap * gdk_gc_get_colormap (GdkGC *gc);
;;; 
;;; Warning
;;; 
;;; gdk_gc_get_colormap has been deprecated since version 2.22 and should not
;;; be used in newly-written code. There is no replacement. Cairo handles
;;; colormaps automatically, so there is no need to care about them.
;;; 
;;; Retrieves the colormap for a given GC, if it exists. A GC will have a
;;; colormap if the drawable for which it was created has a colormap, or if a
;;; colormap was set explicitely with gdk_gc_set_colormap.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; Returns :
;;; 	the colormap of gc, or NULL if gc doesn't have one.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gc_offset ()
;;; 
;;; void gdk_gc_offset (GdkGC *gc, gint x_offset, gint y_offset);
;;; 
;;; Warning
;;; 
;;; gdk_gc_offset has been deprecated since version 2.22 and should not be used
;;; in newly-written code. There is no direct replacement, as this is just a
;;; convenience function for gdk_gc_set_ts_origin and gdk_gc_set_clip_origin().
;;; 
;;; Offset attributes such as the clip and tile-stipple origins of the GC so
;;; that drawing at x - x_offset, y - y_offset with the offset GC has the same
;;; effect as drawing at x, y with the original GC.
;;; 
;;; gc :
;;; 	a GdkGC
;;; 
;;; x_offset :
;;; 	amount by which to offset the GC in the X direction
;;; 
;;; y_offset :
;;; 	amount by which to offset the GC in the Y direction
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_gc_offset" gdk-gc-offset) :void
  (gc (g-object gdk-gc))
  (x-offset :int)
  (y-offset :int))

(export 'gdk-gc-offset)

;;; --- End of file gdk.gc.lisp ------------------------------------------------
