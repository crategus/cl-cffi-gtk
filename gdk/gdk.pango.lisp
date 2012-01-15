;;; ----------------------------------------------------------------------------
;;; gdk.pango.lisp
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
;;; Pango Interaction
;;; 
;;; Using Pango in GDK
;;; 	
;;; Synopsis
;;; 
;;;     GdkPangoRenderer
;;;     GdkPangoRendererClass
;;;     gdk_pango_renderer_new
;;;     gdk_pango_renderer_get_default
;;;     gdk_pango_renderer_set_drawable
;;;     gdk_pango_renderer_set_gc
;;;     gdk_pango_renderer_set_stipple
;;;     gdk_pango_renderer_set_override_color
;;;     gdk_pango_context_get
;;;     gdk_pango_context_get_for_screen
;;;     gdk_pango_context_set_colormap
;;;     GdkPangoAttrEmbossed
;;;     GdkPangoAttrEmbossColor
;;;     GdkPangoAttrStipple
;;;     gdk_pango_attr_emboss_color_new
;;;     gdk_pango_attr_embossed_new
;;;     gdk_pango_attr_stipple_new
;;;     gdk_pango_layout_get_clip_region
;;;     gdk_pango_layout_line_get_clip_region
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----PangoRenderer
;;;          +----GdkPangoRenderer
;;; 
;;; Properties
;;; 
;;;   "screen" GdkScreen*            : Read / Write / Construct Only
;;; 
;;; Description
;;; 
;;; Pango is the text layout system used by GDK and GTK+. The functions and
;;; types in this section are used to render Pango objects to GDK. drawables,
;;; and also extend the set of Pango attributes to include stippling and
;;; embossing.
;;; 
;;; Creating a PangoLayout object is the first step in rendering text, and
;;; requires getting a handle to a PangoContext. For GTK+ programs, you'll
;;; usually want to use gtk_widget_get_pango_context(), or
;;; gtk_widget_create_pango_layout(), rather than using the lowlevel
;;; gdk_pango_context_get_for_screen(). Once you have a PangoLayout, you can
;;; set the text and attributes of it with Pango functions like
;;; pango_layout_set_text() and get its size with pango_layout_get_size().
;;; (Note that Pango uses a fixed point system internally, so converting between
;;; Pango units and pixels using PANGO_SCALE or the PANGO_PIXELS() macro.)
;;; 
;;; Rendering a Pango layout is done most simply with gdk_draw_layout(); you
;;; can also draw pieces of the layout with gdk_draw_layout() or
;;; gdk_draw_glyphs(). GdkPangoRenderer is a subclass of PangoRenderer that is
;;; used internally to implement these functions. Using it directly or
;;; subclassing it can be useful in some cases. See the GdkPangoRenderer
;;; documentation for details.
;;; 
;;; Example 8. Using GdkPangoRenderer to draw transformed text
;;; 
;;; #define RADIUS 100
;;; #define N_WORDS 10
;;; #define FONT "Sans Bold 18"
;;; GdkScreen *screen = gdk_drawable_get_screen (drawable);
;;; PangoRenderer *renderer;
;;; GdkGC *gc;
;;; PangoMatrix matrix = PANGO_MATRIX_INIT;
;;; PangoContext *context;
;;; PangoLayout *layout;
;;; PangoFontDescription *desc;
;;; double device_radius;
;;; int width, height;
;;; int i;
;;; /* Get the default renderer for the screen, and set it up for drawing  */
;;; renderer = gdk_pango_renderer_get_default (screen);
;;; gdk_pango_renderer_set_drawable (GDK_PANGO_RENDERER (renderer), drawable);
;;; gc = gdk_gc_new (drawable);
;;; gdk_pango_renderer_set_gc (GDK_PANGO_RENDERER (renderer), gc);
;;; /* Set up a transformation matrix so that the user space coordinates for
;;;  * where we are drawing are [-RADIUS, RADIUS], [-RADIUS, RADIUS]
;;;  * We first center, then change the scale */
;;; gdk_drawable_get_size (drawable, &width, &height);
;;; device_radius = MIN (width, height) / 2.;
;;; pango_matrix_translate (&matrix,
;;;                         device_radius + (width - 2 * device_radius) / 2,
;;;                         device_radius + (height - 2 * device_radius) / 2);
;;; pango_matrix_scale (&matrix,device_radius / RADIUS, device_radius / RADIUS);
;;; /* Create a PangoLayout, set the font and text */
;;; context = gdk_pango_context_get_for_screen (screen);
;;; layout = pango_layout_new (context);
;;; pango_layout_set_text (layout, "Text", -1);
;;; desc = pango_font_description_from_string (FONT);
;;; pango_layout_set_font_description (layout, desc);
;;; pango_font_description_free (desc);
;;; /* Draw the layout N_WORDS times in a circle */
;;; for (i = 0; i < N_WORDS; i++)
;;;   {
;;;     GdkColor color;
;;;     PangoMatrix rotated_matrix = matrix;
;;;     int width, height;
;;;     double angle = (360. * i) / N_WORDS;
;;;     /* Gradient from red at angle == 60 to blue at angle == 300 */
;;;     color.red   = 65535 * (1 + cos ((angle - 60) * M_PI / 180.)) / 2;
;;;     color.green = 0;
;;;     color.blue  = 65535  - color.red;
;;;     gdk_pango_renderer_set_override_color (GDK_PANGO_RENDERER (renderer),
;;;                                            PANGO_RENDER_PART_FOREGROUND,
;;;                                            &color);
;;;     pango_matrix_rotate (&rotated_matrix, angle);
;;;     pango_context_set_matrix (context, &rotated_matrix);
;;;     /* Inform Pango to re-layout the text with the new
;;;      * transformation matrix */
;;;     pango_layout_context_changed (layout);
;;;     pango_layout_get_size (layout, &width, &height);
;;;     pango_renderer_draw_layout (renderer, layout,
;;;                                 - width / 2, - RADIUS * PANGO_SCALE);
;;;   }
;;; /* Clean up default renderer, since it is shared */
;;; gdk_pango_renderer_set_override_color (GDK_PANGO_RENDERER (renderer),
;;;                                        PANGO_RENDER_PART_FOREGROUND, NULL);
;;; gdk_pango_renderer_set_drawable (GDK_PANGO_RENDERER (renderer), NULL);
;;; gdk_pango_renderer_set_gc (GDK_PANGO_RENDERER (renderer), NULL);
;;; /* free the objects we created */
;;; g_object_unref (layout);
;;; g_object_unref (context);
;;; g_object_unref (gc);
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen" property
;;; 
;;;   "screen"             GdkScreen*            : Read / Write / Construct Only
;;; 
;;; the GdkScreen for the renderer.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkPangoRenderer
;;; 
;;; struct GdkPangoRenderer;
;;; 
;;; GdkPangoRenderer is a subclass of PangoRenderer used for rendering Pango
;;; objects into GDK drawables. The default renderer for a particular screen is
;;; obtained with gdk_pango_renderer_get_default(); Pango functions like
;;; pango_renderer_draw_layout() and pango_renderer_draw_layout_line() are then
;;; used to draw objects with the renderer.
;;; 
;;; In most simple cases, applications can just use gdk_draw_layout(), and
;;; don't need to directly use GdkPangoRenderer at all. Using the
;;; GdkPangoRenderer directly is most useful when working with a transformation
;;; such as a rotation, because the Pango drawing functions take user space
;;; coordinates (coordinates before the transformation) instead of device
;;; coordinates.
;;; 
;;; In certain cases it can be useful to subclass GdkPangoRenderer. Examples of
;;; reasons to do this are to add handling of custom attributes by overriding
;;; 'prepare_run' or to do custom drawing of embedded objects by overriding
;;; 'draw_shape'.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPangoRendererClass
;;; 
;;; struct GdkPangoRendererClass {
;;; };
;;; 
;;; GdkPangoRenderer is the class structure for GdkPangoRenderer.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPangoRenderer" gdk-pango-renderer
  (:superclass pango-renderer :export t
   :interfaces nil
   :type-initializer "gdk_pango_renderer_get_type")
  ((screen gdk-pango-renderer-screen
           "screen" "GdkScreen" t nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_pango_renderer_new ()
;;; 
;;; PangoRenderer * gdk_pango_renderer_new (GdkScreen *screen);
;;; 
;;; Creates a new PangoRenderer for screen. Normally you can use the results
;;; of gdk_pango_renderer_get_default() rather than creating a new renderer.
;;; 
;;; screen :
;;; 	a GdkScreen
;;; 
;;; Returns :
;;; 	a newly created PangoRenderer. Free with g_object_unref().
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_renderer_new" gdk-pango-renderer-new)
    (g-object gdk-pango-renderer :already-referenced)
  (screen (g-object gdk-screen)))

(export 'gdk-pango-renderer-new)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_renderer_get_default ()
;;; 
;;; PangoRenderer * gdk_pango_renderer_get_default (GdkScreen *screen);
;;; 
;;; Gets the default PangoRenderer for a screen. This default renderer is
;;; shared by all users of the display, so properties such as the color or
;;; transformation matrix set for the renderer may be overwritten by functions
;;; such as gdk_draw_layout().
;;; 
;;; Before using the renderer, you need to call
;;; gdk_pango_renderer_set_drawable() and gdk_pango_renderer_set_gc() to set
;;; the drawable and graphics context to use for drawing.
;;; 
;;; screen :
;;; 	a GdkScreen
;;; 
;;; Returns :
;;; 	the default PangoRenderer for screen. The renderer is owned by GTK+
;;;     and will be kept around until the screen is closed.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_renderer_get_default" gdk-pango-renderer-get-default)
    (g-object gdk-pango-renderer)
  (screen (g-object gdk-screen)))

(export 'gdk-pango-renderer-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_renderer_set_drawable ()
;;; 
;;; void gdk_pango_renderer_set_drawable (GdkPangoRenderer *gdk_renderer,
;;;                                       GdkDrawable *drawable);
;;; 
;;; Sets the drawable the renderer draws to.
;;; 
;;; gdk_renderer :
;;; 	a GdkPangoRenderer
;;; 
;;; drawable :
;;; 	the new target drawable, or NULL.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_renderer_set_drawable" gdk-pango-renderer-set-drawable)
    :void
  (renderer (g-object gdk-pango-renderer))
  (drawable (g-object gdk-drawable)))

(export 'gdk-pango-renderer-set-drawable)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_renderer_set_gc ()
;;; 
;;; void gdk_pango_renderer_set_gc (GdkPangoRenderer *gdk_renderer, GdkGC *gc)
;;; 
;;; Sets the GC the renderer draws with. Note that the GC must not be modified
;;; until it is unset by calling the function again with NULL for the gc
;;; parameter, since GDK may make internal copies of the GC which won't be
;;; updated to follow changes to the original GC.
;;; 
;;; gdk_renderer :
;;; 	a GdkPangoRenderer
;;; 
;;; gc :
;;; 	the new GC to use for drawing, or NULL. [allow-none]
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_renderer_set_gc" gdk-pango-renderer-set-gc) :void
  (renderer (g-object gdk-pango-renderer))
  (gc (g-object gdk-gc)))

(export 'gdk-pango-renderer-set-gc)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_renderer_set_stipple ()
;;; 
;;; void gdk_pango_renderer_set_stipple (GdkPangoRenderer *gdk_renderer,
;;;                                      PangoRenderPart part,
;;;                                      GdkBitmap *stipple);
;;; 
;;; Sets the stipple for one render part (foreground, background, underline,
;;; etc.) Note that this is overwritten when iterating through the individual
;;; styled runs of a PangoLayout or PangoLayoutLine. This function is thus only
;;; useful when you call low level functions like pango_renderer_draw_glyphs()
;;; directly, or in the 'prepare_run' virtual function of a subclass of
;;; GdkPangoRenderer.
;;; 
;;; gdk_renderer :
;;; 	a GdkPangoRenderer
;;; 
;;; part :
;;; 	the part to render with the stipple
;;; 
;;; stipple :
;;; 	the new stipple value.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_renderer_set_stipple" gdk-pango-renderer-set-stipple) :void
  (renderer (g-object gdk-pango-renderer))
  (part pango-render-part)
  (stipple (g-object gdk-pixmap)))

(export 'gdk-pango-renderer-set-stipple)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_renderer_set_override_color ()
;;; 
;;; void gdk_pango_renderer_set_override_color (GdkPangoRenderer *gdk_renderer,
;;;                                             PangoRenderPart part,
;;;                                             const GdkColor *color);
;;; 
;;; Sets the color for a particular render part (foreground, background,
;;; underline, etc.), overriding any attributes on the layouts renderered with
;;; this renderer.
;;; 
;;; gdk_renderer :
;;; 	a GdkPangoRenderer
;;; 
;;; part :
;;; 	the part to render to set the color of
;;; 
;;; color :
;;; 	the color to use, or NULL to unset a previously set override color.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun gdk-pango-renderer-set-override-color :void
  (renderer (g-object gdk-pango-renderer))
  (part pango-render-part)
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-pango-renderer-set-override-color)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get ()
;;; 
;;; PangoContext * gdk_pango_context_get (void);
;;; 
;;; Creates a PangoContext for the default GDK screen.
;;; 
;;; The context must be freed when you're finished with it.
;;; 
;;; When using GTK+, normally you should use gtk_widget_get_pango_context()
;;; instead of this function, to get the appropriate context for the widget you
;;; intend to render text onto.
;;; 
;;; The newly created context will have the default font options (see
;;; cairo_font_options_t) for the default screen; if these options change it
;;; will not be updated. Using gtk_widget_get_pango_context() is more convenient
;;; if you want to keep a context around and track changes to the screen's font
;;; rendering settings.
;;; 
;;; Returns :
;;; 	a new PangoContext for the default display
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get" gdk-pango-context-get)
    (g-object pango-context :already-referenced))

(export 'gdk-pango-context-get)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_screen ()
;;; 
;;; PangoContext * gdk_pango_context_get_for_screen (GdkScreen *screen);
;;; 
;;; Creates a PangoContext for screen.
;;; 
;;; The context must be freed when you're finished with it.
;;; 
;;; When using GTK+, normally you should use gtk_widget_get_pango_context()
;;; instead of this function, to get the appropriate context for the widget you
;;; intend to render text onto.
;;; 
;;; The newly created context will have the default font options (see
;;; cairo_font_options_t) for the screen; if these options change it will not
;;; be updated. Using gtk_widget_get_pango_context() is more convenient if you
;;; want to keep a context around and track changes to the screen's font
;;; rendering settings.
;;; 
;;; screen :
;;; 	the GdkScreen for which the context is to be created.
;;; 
;;; Returns :
;;; 	a new PangoContext for screen
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get_for_screen" gdk-pango-context-get-for-screen)
    (g-object pango-context :already-referenced)
  (screen (g-object gdk-screen)))

(export 'gdk-pango-context-get-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_set_colormap ()
;;; 
;;; void gdk_pango_context_set_colormap (PangoContext *context,
;;;                                      GdkColormap *colormap);
;;; 
;;; Warning
;;; 
;;; gdk_pango_context_set_colormap is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; This function used to set the colormap to be used for drawing with context.
;;; The colormap is now always derived from the graphics context used for
;;; drawing, so calling this function is no longer necessary.
;;; 
;;; context :
;;; 	a PangoContext
;;; 
;;; colormap :
;;; 	a GdkColormap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPangoAttrEmbossed
;;; 
;;; struct GdkPangoAttrEmbossed {
;;;   PangoAttribute attr;
;;;   gboolean embossed;
;;; };
;;; 
;;; A Pango text attribute containing a embossed bitmap to be used when
;;; rendering the text.
;;; 
;;; PangoAttribute attr;
;;; 	the PangoAttribute.
;;; 
;;; gboolean embossed;
;;; 	the embossed bitmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPangoAttrEmbossColor
;;; 
;;; struct GdkPangoAttrEmbossColor {
;;;   PangoAttribute attr;
;;;   PangoColor color;
;;; };
;;; 
;;; A Pango text attribute specifying the color to emboss text with.
;;; 
;;; PangoAttribute attr;
;;; 	the PangoAttribute
;;; 
;;; PangoColor color;
;;; 	the color
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPangoAttrStipple
;;; 
;;; struct GdkPangoAttrStipple {
;;;   PangoAttribute attr;
;;;   GdkBitmap *stipple;
;;; };
;;; 
;;; A Pango text attribute containing a stipple bitmap to be used when
;;; rendering the text.
;;; 
;;; PangoAttribute attr;
;;; 	the PangoAttribute.
;;; 
;;; GdkBitmap *stipple;
;;; 	the stipple bitmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pango_attr_emboss_color_new ()
;;; 
;;; PangoAttribute * gdk_pango_attr_emboss_color_new (const GdkColor *color);
;;; 
;;; Creates a new attribute specifying the color to emboss text with.
;;; 
;;; color :
;;; 	a GdkColor representing the color to emboss with
;;; 
;;; Returns :
;;; 	new PangoAttribute
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pango_attr_embossed_new ()
;;; 
;;; PangoAttribute * gdk_pango_attr_embossed_new (gboolean embossed);
;;; 
;;; Creates a new attribute flagging a region as embossed or not.
;;; 
;;; embossed :
;;; 	if the region should be embossed
;;; 
;;; Returns :
;;; 	new PangoAttribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pango_attr_stipple_new ()
;;; 
;;; PangoAttribute * gdk_pango_attr_stipple_new (GdkBitmap *stipple);
;;; 
;;; Creates a new attribute containing a stipple bitmap to be used when
;;; rendering the text.
;;; 
;;; stipple :
;;; 	a bitmap to be set as stipple
;;; 
;;; Returns :
;;; 	new PangoAttribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_get_clip_region ()
;;; 
;;; GdkRegion * gdk_pango_layout_get_clip_region (PangoLayout *layout,
;;;                                               gint x_origin,
;;;                                               gint y_origin,
;;;                                               const gint *index_ranges,
;;;                                               gint n_ranges);
;;; 
;;; Obtains a clip region which contains the areas where the given ranges of
;;; text would be drawn. x_origin and y_origin are the same position you would
;;; pass to gdk_draw_layout_line(). index_ranges should contain ranges of bytes
;;; in the layout's text.
;;; 
;;; Note that the regions returned correspond to logical extents of the text
;;; ranges, not ink extents. So the drawn layout may in fact touch areas out of
;;; the clip region. The clip region is mainly useful for highlightling parts
;;; of text, such as when text is selected.
;;; 
;;; layout :
;;; 	a PangoLayout
;;; 
;;; x_origin :
;;; 	X pixel where you intend to draw the layout with this clip
;;; 
;;; y_origin :
;;; 	Y pixel where you intend to draw the layout with this clip
;;; 
;;; index_ranges :
;;; 	array of byte indexes into the layout, where even members of array are
;;;     start indexes and odd elements are end indexes
;;; 
;;; n_ranges :
;;; 	number of ranges in index_ranges, i.e. half the size of index_ranges
;;; 
;;; Returns :
;;; 	a clip region containing the given ranges
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_get_clip_region" %gdk-pango-layout-get-clip-region)
    (g-boxed-foreign gdk-region :return)
  (layout (g-object pango-layout))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-get-clip-region (layout x-origin y-origin index-ranges)
  (let ((n (length index-ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (with-foreign-object (ranges :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (mem-aref ranges :int i) x)
                 (incf i))
               index-ranges))
        (%gdk-pango-layout-get-clip-region layout
                                           x-origin
                                           y-origin
                                           index-ranges
                                           n-ranges)))))

(export 'gdk-pango-layout-get-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_line_get_clip_region ()
;;; 
;;; GdkRegion * gdk_pango_layout_line_get_clip_region (PangoLayoutLine *line,
;;;                                                    gint x_origin,
;;;                                                    gint y_origin,
;;;                                                    const gint *index_ranges,
;;;                                                    gint n_ranges);
;;; 
;;; Obtains a clip region which contains the areas where the given ranges of
;;; text would be drawn. x_origin and y_origin are the same position you would
;;; pass to gdk_draw_layout_line(). index_ranges should contain ranges of bytes
;;; in the layout's text. The clip region will include space to the left or
;;; right of the line (to the layout bounding box) if you have indexes above or
;;; below the indexes contained inside the line. This is to draw the selection
;;; all the way to the side of the layout. However, the clip region is in line
;;; coordinates, not layout coordinates.
;;; 
;;; Note that the regions returned correspond to logical extents of the text
;;; ranges, not ink extents. So the drawn line may in fact touch areas out of
;;; the clip region. The clip region is mainly useful for highlightling parts
;;; of text, such as when text is selected.
;;; 
;;; line :
;;; 	a PangoLayoutLine
;;; 
;;; x_origin :
;;; 	X pixel where you intend to draw the layout line with this clip
;;; 
;;; y_origin :
;;; 	baseline pixel where you intend to draw the layout line with this clip
;;; 
;;; index_ranges :
;;; 	array of byte indexes into the layout, where even members of array are
;;;     start indexes and odd elements are end indexes
;;; 
;;; n_ranges :
;;; 	number of ranges in index_ranges, i.e. half the size of index_ranges
;;; 
;;; Returns :
;;; 	a clip region containing the given ranges
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_line_get_clip_region"
          %gdk-pango-layout-line-get-clip-region)
    (g-boxed-foreign gdk-region :return)
  (layout-line (g-boxed-foreign pango-layout-line))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-line-get-clip-region (layout-line x-origin y-origin
                                                          index-ranges)
  (let ((n (length index-ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (with-foreign-object (ranges :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (mem-aref ranges :int i) x)
                 (incf i))
               index-ranges))
        (%gdk-pango-layout-line-get-clip-region layout-line
                                                x-origin
                                                y-origin
                                                index-ranges
                                                n-ranges)))))

(export 'gdk-pango-layout-line-get-clip-region)

;;; --- End of file gdk.pango.lisp ---------------------------------------------
