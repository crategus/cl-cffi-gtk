;;; ----------------------------------------------------------------------------
;;; pango.cairo-render.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.32.6. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Cairo Rendering
;;;
;;; Rendering with the Cairo backend
;;;
;;; Synopsis
;;;
;;;     PangoCairoFont
;;;     PangoCairoFontMap
;;;
;;;     pango_cairo_font_map_get_default
;;;     pango_cairo_font_map_set_default
;;;     pango_cairo_font_map_new
;;;     pango_cairo_font_map_new_for_font_type
;;;     pango_cairo_font_map_get_font_type
;;;     pango_cairo_font_map_set_resolution
;;;     pango_cairo_font_map_get_resolution
;;;     pango_cairo_font_map_create_context
;;;     pango_cairo_font_get_scaled_font
;;;     pango_cairo_context_set_resolution
;;;     pango_cairo_context_get_resolution
;;;     pango_cairo_context_set_font_options
;;;     pango_cairo_context_get_font_options
;;;     pango_cairo_context_set_shape_renderer
;;;     pango_cairo_context_get_shape_renderer
;;;     pango_cairo_create_context
;;;     pango_cairo_update_context
;;;     pango_cairo_create_layout
;;;     pango_cairo_update_layout
;;;     pango_cairo_show_glyph_string
;;;     pango_cairo_show_glyph_item
;;;     pango_cairo_show_layout_line
;;;     pango_cairo_show_layout
;;;     pango_cairo_show_error_underline
;;;     pango_cairo_glyph_string_path
;;;     pango_cairo_layout_line_path
;;;     pango_cairo_layout_path
;;;     pango_cairo_error_underline_path
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----PangoCairoFont
;;;
;;;   GInterface
;;;    +----PangoCairoFontMap
;;;
;;; Prerequisites
;;;
;;; PangoCairoFont requires PangoFont.
;;;
;;; PangoCairoFontMap requires PangoFontMap.
;;;
;;; Description
;;;
;;; The Cairo library is a vector graphics library with a powerful rendering
;;; model. It has such features as anti-aliased primitives, alpha-compositing,
;;; and gradients. Multiple backends for Cairo are available, to allow rendering
;;; to images, to PDF files, and to the screen on X and on other windowing
;;; systems. The functions in this section allow using Pango to render to Cairo
;;; surfaces.
;;;
;;; Using Pango with Cairo is straightforward. A PangoContext created with
;;; pango_cairo_font_map_create_context() can be used on any Cairo context
;;; (cairo_t), but needs to be updated to match the current transformation
;;; matrix and target surface of the Cairo context using
;;; pango_cairo_update_context(). The convenience functions
;;; pango_cairo_create_layout() and pango_cairo_update_layout() handle the
;;; common case where the program does not need to manipulate the properties
;;; of the PangoContext.
;;;
;;; When you get the metrics of a layout or of a piece of a layout using
;;; functions such as pango_layout_get_extents(), the reported metrics are in
;;; user-space coordinates. If a piece of text is 10 units long, and you call
;;; cairo_scale (cr, 2.0), it still is more-or-less 10 units long. However, the
;;; results will be affected by hinting (that is, the process of adjusting the
;;; text to look good on the pixel grid), so you shouldn't assume they are
;;; completely independent of the current transformation matrix. Note that the
;;; basic metrics functions in Pango report results in integer Pango units. To
;;; get to the floating point units used in Cairo divide by PANGO_SCALE.
;;;
;;; Example 1. Using Pango with Cairo
;;;
;;; #include <math.h>
;;; #include <pango/pangocairo.h>
;;; static void
;;; draw_text (cairo_t *cr)
;;; {
;;; #define RADIUS 150
;;; #define N_WORDS 10
;;; #define FONT "Sans Bold 27"
;;;   PangoLayout *layout;
;;;   PangoFontDescription *desc;
;;;   int i;
;;;   /* Center coordinates on the middle of the region we are drawing
;;;    */
;;;   cairo_translate (cr, RADIUS, RADIUS);
;;;   /* Create a PangoLayout, set the font and text */
;;;   layout = pango_cairo_create_layout (cr);
;;;   pango_layout_set_text (layout, "Text", -1);
;;;   desc = pango_font_description_from_string (FONT);
;;;   pango_layout_set_font_description (layout, desc);
;;;   pango_font_description_free (desc);
;;;   /* Draw the layout N_WORDS times in a circle */
;;;   for (i = 0; i < N_WORDS; i++)
;;;     {
;;;       int width, height;
;;;       double angle = (360. * i) / N_WORDS;
;;;       double red;
;;;       cairo_save (cr);
;;;       /* Gradient from red at angle == 60 to blue at angle == 240 */
;;;       red   = (1 + cos ((angle - 60) * G_PI / 180.)) / 2;
;;;       cairo_set_source_rgb (cr, red, 0, 1.0 - red);
;;;       cairo_rotate (cr, angle * G_PI / 180.);
;;;       /* Inform Pango to re-layout the text with the new transformation */
;;;       pango_cairo_update_layout (cr, layout);
;;;       pango_layout_get_size (layout, &width, &height);
;;;       cairo_move_to (cr, - ((double)width / PANGO_SCALE) / 2, - RADIUS);
;;;       pango_cairo_show_layout (cr, layout);
;;;       cairo_restore (cr);
;;;     }
;;;   /* free the layout object */
;;;   g_object_unref (layout);
;;; }
;;; int main (int argc, char **argv)
;;; {
;;;   cairo_t *cr;
;;;   char *filename;
;;;   cairo_status_t status;
;;;   cairo_surface_t *surface;
;;;   if (argc != 2)
;;;     {
;;;       g_printerr ("Usage: cairosimple OUTPUT_FILENAME\n");
;;;       return 1;
;;;     }
;;;   filename = argv[1];
;;;   surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
;;;                     2 * RADIUS, 2 * RADIUS);
;;;   cr = cairo_create (surface);
;;;   cairo_set_source_rgb (cr, 1.0, 1.0, 1.0);
;;;   cairo_paint (cr);
;;;   draw_text (cr);
;;;   cairo_destroy (cr);
;;;   status = cairo_surface_write_to_png (surface, filename);
;;;   cairo_surface_destroy (surface);
;;;   if (status != CAIRO_STATUS_SUCCESS)
;;;     {
;;;       g_printerr ("Could not save png to '%s'\n", filename);
;;;       return 1;
;;;     }
;;;   return 0;
;;; }
;;;
;;; Figure 2. Output of Example 1, “Using Pango with Cairo”
;;; Output of Example 1, “Using Pango with Cairo”
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoCairoFont
;;;
;;; typedef struct _PangoCairoFont PangoCairoFont;
;;;
;;; PangoCairoFont is an interface exported by fonts for use with Cairo. The
;;; actual type of the font will depend on the particular font technology Cairo
;;; was compiled to use.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoCairoFontMap
;;;
;;; typedef struct _PangoCairoFontMap PangoCairoFontMap;
;;;
;;; PangoCairoFontMap is an interface exported by font maps for use with Cairo.
;;; The actual type of the font map will depend on the particular font
;;; technology Cairo was compiled to use.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_get_default ()
;;;
;;; PangoFontMap * pango_cairo_font_map_get_default (void);
;;;
;;; Gets a default PangoCairoFontMap to use with Cairo.
;;;
;;; Note that the type of the returned object will depend on the particular font
;;; backend Cairo was compiled to use; You generally should only use the
;;; PangoFontMap and PangoCairoFontMap interfaces on the returned object.
;;;
;;; The default Cairo fontmap can be changed by using
;;; pango_cairo_font_map_set_default(). This can be used to change the Cairo
;;; font backend that the default fontmap uses for example.
;;;
;;; Note that since Pango 1.32.6, the default fontmap is per-thread. Each thread
;;; gets its own default fontmap. In this way, PangoCairo can be used safely
;;; from multiple threads.
;;;
;;; Returns :
;;;     The default PangoCairo fontmap for the current thread. This object is
;;;     owned by Pango and must not be freed.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_set_default ()
;;;
;;; void pango_cairo_font_map_set_default (PangoCairoFontMap *fontmap);
;;;
;;; Sets a default PangoCairoFontMap to use with Cairo.
;;;
;;; This can be used to change the Cairo font backend that the default fontmap
;;; uses for example. The old default font map is unreffed and the new font map
;;; referenced.
;;;
;;; Note that since Pango 1.32.6, the default fontmap is per-thread. This
;;; function only changes the default fontmap for the current thread. Default
;;; fontmaps of exisiting threads are not changed. Default fontmaps of any new
;;; threads will still be created using pango_cairo_font_map_new().
;;;
;;; A value of NULL for fontmap will cause the current default font map to be
;;; released and a new default font map to be created on demand, using
;;; pango_cairo_font_map_new().
;;;
;;; fontmap :
;;;     The new default font map, or NULL
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_new ()
;;;
;;; PangoFontMap * pango_cairo_font_map_new (void);
;;;
;;; Creates a new PangoCairoFontMap object; a fontmap is used to cache
;;; information about available fonts, and holds certain global parameters such
;;; as the resolution. In most cases, you can use
;;; pango_cairo_font_map_get_default() instead.
;;;
;;; Note that the type of the returned object will depend on the particular font
;;; backend Cairo was compiled to use; You generally should only use the
;;; PangoFontMap and PangoCairoFontMap interfaces on the returned object.
;;;
;;; Returns :
;;;     the newly allocated PangoFontMap, which should be freed with
;;;     g_object_unref().
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_new_for_font_type ()
;;;
;;; PangoFontMap *  pango_cairo_font_map_new_for_font_type
;;;                                                 (cairo_font_type_t fonttype)
;;;
;;; Creates a new PangoCairoFontMap object of the type suitable to be used with
;;; cairo font backend of type fonttype.
;;;
;;; In most cases one should simply use pango_cairo_font_map_new(), or in fact
;;; in most of those cases, just use pango_cairo_font_map_get_default().
;;;
;;; fonttype :
;;;     desired cairo_font_type_t
;;;
;;; Returns :
;;;     (transfer full) : the newly allocated PangoFontMap of suitable type
;;;     which should be freed with g_object_unref(), or NULL if the requested
;;;     cairo font backend is not supported / compiled in.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_get_font_type ()
;;;
;;; cairo_font_type_t pango_cairo_font_map_get_font_type
;;;                                                 (PangoCairoFontMap *fontmap)
;;;
;;; Gets the type of Cairo font backend that fontmap uses.
;;;
;;; fontmap :
;;;     a PangoCairoFontMap
;;;
;;; Returns :
;;;     the cairo_font_type_t cairo font backend type
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_set_resolution ()
;;;
;;; void pango_cairo_font_map_set_resolution (PangoCairoFontMap *fontmap,
;;;                                           double dpi);
;;;
;;; Sets the resolution for the fontmap. This is a scale factor between points
;;; specified in a PangoFontDescription and Cairo units. The default value is
;;; 96, meaning that a 10 point font will be 13 units high.
;;; (10 * 96. / 72. = 13.3).
;;;
;;; fontmap :
;;;     a PangoCairoFontMap
;;;
;;; dpi :
;;;     the resolution in "dots per inch". (Physical inches aren't actually
;;;     involved; the terminology is conventional.)
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_get_resolution ()
;;;
;;; double pango_cairo_font_map_get_resolution (PangoCairoFontMap *fontmap);
;;;
;;; Gets the resolution for the fontmap. See
;;; pango_cairo_font_map_set_resolution()
;;;
;;; fontmap :
;;;     a PangoCairoFontMap
;;;
;;; Returns :
;;;     the resolution in "dots per inch"
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_create_context ()
;;;
;;; PangoContext * pango_cairo_font_map_create_context
;;;                                                 (PangoCairoFontMap *fontmap)
;;;
;;; Warning
;;;
;;; pango_cairo_font_map_create_context has been deprecated since version 1.22
;;; and should not be used in newly-written code. Use
;;; pango_font_map_create_context() instead.
;;;
;;; Create a PangoContext for the given fontmap.
;;;
;;; fontmap :
;;;     a PangoCairoFontMap
;;;
;;; Returns :
;;;     the newly created context; free with g_object_unref().
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_get_scaled_font ()
;;;
;;; cairo_scaled_font_t * pango_cairo_font_get_scaled_font
;;;                                                       (PangoCairoFont *font)
;;;
;;; Gets the cairo_scaled_font_t used by font. The scaled font can be referenced
;;; and kept using cairo_scaled_font_reference().
;;;
;;; font :
;;;     a PangoFont from a PangoCairoFontMap
;;;
;;; Returns :
;;;     the cairo_scaled_font_t used by font, or NULL if font is NULL.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_set_resolution ()
;;;
;;; void pango_cairo_context_set_resolution (PangoContext *context,
;;;                                          double dpi);
;;;
;;; Sets the resolution for the context. This is a scale factor between points
;;; specified in a PangoFontDescription and Cairo units. The default value is
;;; 96, meaning that a 10 point font will be 13 units high.
;;; (10 * 96. / 72. = 13.3).
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; dpi :
;;;     the resolution in "dots per inch". (Physical inches aren't actually
;;;     involved; the terminology is conventional.) A 0 or negative value means
;;;     to use the resolution from the font map.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_get_resolution ()
;;;
;;; double pango_cairo_context_get_resolution (PangoContext *context);
;;;
;;; Gets the resolution for the context. See
;;; pango_cairo_context_set_resolution()
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; Returns :
;;;     the resolution in "dots per inch". A negative value will be returned if
;;;     no resolution has previously been set.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_set_font_options ()
;;;
;;; void pango_cairo_context_set_font_options
;;;                                        (PangoContext *context,
;;;                                         const cairo_font_options_t *options)
;;;
;;; Sets the font options used when rendering text with this context. These
;;; options override any options that pango_cairo_update_context() derives from
;;; the target surface.
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; options :
;;;     a cairo_font_options_t, or NULL to unset any previously set options. A
;;;     copy is made.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_get_font_options ()
;;;
;;; const cairo_font_options_t * pango_cairo_context_get_font_options
;;;                                                      (PangoContext *context)
;;;
;;; Retrieves any font rendering options previously set with
;;; pango_cairo_font_map_set_font_options(). This function does not report
;;; options that are derived from the target surface by
;;; pango_cairo_update_context()
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; Returns :
;;;     the font options previously set on the context, or NULL if no options
;;;     have been set. This value is owned by the context and must not be
;;;     modified or freed.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoCairoShapeRendererFunc ()
;;;
;;; void (*PangoCairoShapeRendererFunc) (cairo_t *cr,
;;;                                      PangoAttrShape *attr,
;;;                                      gboolean do_path,
;;;                                      gpointer data);
;;;
;;; Function type for rendering attributes of type PANGO_ATTR_SHAPE with Pango's
;;; Cairo renderer.
;;;
;;; cr :
;;;     a Cairo context with current point set to where the shape should be
;;;     rendered
;;;
;;; attr :
;;;     the PANGO_ATTR_SHAPE to render
;;;
;;; do_path :
;;;     whether only the shape path should be appended to current path of cr and
;;;     no filling/stroking done. This will be set to TRUE when called from
;;;     pango_cairo_layout_path() and pango_cairo_layout_line_path() rendering
;;;     functions.
;;;
;;; data :
;;;     user data passed to pango_cairo_context_set_shape_renderer()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_set_shape_renderer ()
;;;
;;; void pango_cairo_context_set_shape_renderer
;;;                                           (PangoContext *context,
;;;                                            PangoCairoShapeRendererFunc func,
;;;                                            gpointer data,
;;;                                            GDestroyNotify dnotify);
;;;
;;; Sets callback function for context to use for rendering attributes of type
;;; PANGO_ATTR_SHAPE. See PangoCairoShapeRendererFunc for details.
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; func :
;;;     Callback function for rendering attributes of type PANGO_ATTR_SHAPE, or
;;;     NULL to disable shape rendering.
;;;
;;; data :
;;;     User data that will be passed to func.
;;;
;;; dnotify :
;;;     Callback that will be called when the context is freed to release data,
;;;     or NULL.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_get_shape_renderer ()
;;;
;;; PangoCairoShapeRendererFunc pango_cairo_context_get_shape_renderer
;;;                                                      (PangoContext *context,
;;;                                                       gpointer *data);
;;;
;;; Sets callback function for context to use for rendering attributes of type
;;; PANGO_ATTR_SHAPE. See PangoCairoShapeRendererFunc for details.
;;;
;;; Retrieves callback function and associated user data for rendering
;;; attributes of type PANGO_ATTR_SHAPE as set by
;;; pango_cairo_context_set_shape_renderer(), if any.
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; data :
;;;     Pointer to gpointer to return user data
;;;
;;; Returns :
;;;     the shape rendering callback previously set on the context, or NULL if
;;;     no shape rendering callback have been set.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_create_context ()
;;;
;;; PangoContext * pango_cairo_create_context (cairo_t *cr);
;;;
;;; Creates a context object set up to match the current transformation and
;;; target surface of the Cairo context. This context can then be used to create
;;; a layout using pango_layout_new().
;;;
;;; This function is a convenience function that creates a context using the
;;; default font map, then updates it to cr. If you just need to create a layout
;;; for use with cr and do not need to access PangoContext directly, you can use
;;; pango_cairo_create_layout() instead.
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; Returns :
;;;     The newly created PangoContext. Free with g_object_unref().
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_update_context ()
;;;
;;; void pango_cairo_update_context (cairo_t *cr, PangoContext *context);
;;;
;;; Updates a PangoContext previously created for use with Cairo to match the
;;; current transformation and target surface of a Cairo context. If any layouts
;;; have been created for the context, it's necessary to call
;;; pango_layout_context_changed() on those layouts.
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; context :
;;;     a PangoContext, from a pangocairo font map
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_create_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_cairo_create_layout" pango-cairo-create-layout)
    (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-4}
  @argument[cr]{a Cairo context}
  @return{The newly created @class{pango-layout}.}
  @begin{short}
    Creates a layout object set up to match the current transformation and
    target surface of the Cairo context.
  @end{short}
  This layout can then be used for text measurement with functions like
  @fun{pango-layout-size} or drawing with functions like
  @fun{pango-cairo-show-layout}. If you change the transformation or target
  surface for @arg{cr}, you need to call the function
  @fun{pango-cairo-update-layout}.

  This function is the most convenient way to use Cairo with Pango, however it
  is slightly inefficient since it creates a separate @class{pango-context}
  object for each layout. This might matter in an application that was laying
  out large amounts of text.

  Since 1.10
  @see-symbol{cairo-t}
  @see-class{pango-layout}
  @see-class{pango-context}
  @see-function{pango-layout-size}
  @see-function{pango-cairo-show-layout}
  @see-function{pango-cairo-update-layout}"
  (cr (:pointer (:struct cairo-t))))

(export 'pango-cairo-create-layout)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_update_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_cairo_update_layout" pango-cairo-update-layout) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-4}
  @argument[cr]{a Cairo context}
  @argument[layout]{a @class{pango-layout}, from
    the function @fun{pango-cairo-create-layout}.}
  @begin{short}
    Updates the private @class{pango-context} of a @class{pango-layout} created
    with the function @fun{pango-cairo-create-layout} to match the current
    transformation and target surface of a Cairo context.
  @end{short}

  Since 1.10
  @see-symbol{cairo-t}
  @see-class{pango-layout}
  @see-class{pango-context}
  @see-function{pango-cairo-create-layout}"
  (cr (:pointer (:struct cairo-t)))
  (layout (g-object pango-layout)))

(export 'pango-cairo-update-layout)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_glyph_string ()
;;;
;;; void pango_cairo_show_glyph_string (cairo_t *cr,
;;;                                     PangoFont *font,
;;;                                     PangoGlyphString *glyphs);
;;;
;;; Draws the glyphs in glyphs in the specified Cairo context. The origin of the
;;; glyphs (the left edge of the baseline) will be drawn at the current point of
;;; the Cairo context.
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; font :
;;;     a PangoFont from a PangoCairoFontMap
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_glyph_item ()
;;;
;;; void pango_cairo_show_glyph_item (cairo_t *cr,
;;;                                   const char *text,
;;;                                   PangoGlyphItem *glyph_item);
;;;
;;; Draws the glyphs in glyph_item in the specified Cairo context, embedding the
;;; text associated with the glyphs in the output if the output format supports
;;; it (PDF for example), otherwise it acts similar to
;;; pango_cairo_show_glyph_string().
;;;
;;; The origin of the glyphs (the left edge of the baseline) will be drawn at
;;; the current point of the Cairo context.
;;;
;;; Note that text is the start of the text for layout, which is then indexed by
;;; glyph_item->item->offset.
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; text :
;;;     the UTF-8 text that glyph_item refers to
;;;
;;; glyph_item :
;;;     a PangoGlyphItem
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_layout_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_cairo_show_layout_line" pango-cairo-show-layout-line) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-28}
  @argument[cr]{a Cairo context}
  @argument[line]{a @class{pango-layout-line} structure}
  @begin{short}
    Draws a @class{pango-layout-line} in the specified Cairo context.
  @end{short}
  The origin of the glyphs, the left edge of the line, will be drawn at the
  current point of the Cairo context.

  Since 1.10
  @see-symbol{cairo-t}
  @see-class{pango-layout-line}"
  (cr (:pointer (:struct cairo-t)))
  (line (g-boxed-foreign pango-layout-line)))

(export 'pango-cairo-show-layout-line)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_cairo_show_layout" pango-cairo-show-layout) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-3}
  @argument[cr]{a Cairo context}
  @argument[layout]{a Pango layout}
  @begin{short}
    Draws a @class{pango-layout} in the specified Cairo context.
  @end{short}
  The top-left corner of the @class{pango-layout} will be drawn at the current
  point of the Cairo context.

  Since 1.10
  @see-symbol{cairo-t}
  @see-class{pango-layout}"
  (cr (:pointer (:struct cairo-t)))
  (layout (g-object pango-layout)))

(export 'pango-cairo-show-layout)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_error_underline ()
;;;
;;; void pango_cairo_show_error_underline (cairo_t *cr,
;;;                                        double x,
;;;                                        double y,
;;;                                        double width,
;;;                                        double height);
;;;
;;; Draw a squiggly line in the specified Cairo context that approximately
;;; covers the given rectangle in the style of an underline used to indicate a
;;; spelling error. (The width of the underline is rounded to an integer number
;;; of up/down segments and the resulting rectangle is centered in the original
;;; rectangle)
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; x :
;;;     The X coordinate of one corner of the rectangle
;;;
;;; y :
;;;     The Y coordinate of one corner of the rectangle
;;;
;;; width :
;;;     Non-negative width of the rectangle
;;;
;;; height :
;;;     Non-negative height of the rectangle
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_glyph_string_path ()
;;;
;;; void pango_cairo_glyph_string_path (cairo_t *cr,
;;;                                     PangoFont *font,
;;;                                     PangoGlyphString *glyphs);
;;;
;;; Adds the glyphs in glyphs to the current path in the specified cairo
;;; context. The origin of the glyphs (the left edge of the baseline) will be at
;;; the current point of the Cairo context.
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; font :
;;;     a PangoFont from a PangoCairoFontMap
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_layout_line_path ()
;;;
;;; void pango_cairo_layout_line_path (cairo_t *cr, PangoLayoutLine *line);
;;;
;;; Adds the text in PangoLayoutLine to the current path in the specified cairo
;;; context. The origin of the glyphs (the left edge of the line) will be at the
;;; current point of the Cairo context.
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; line :
;;;     a PangoLayoutLine
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_cairo_layout_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_cairo_layout_path" pango-cairo-layout-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-4}
  @argument[cr]{a Cairo context}
  @argument[layout]{a Pango layout}
  @begin{short}
    Adds the text in a @class{pango-layout} to the current path in the
    specified Cairo context.
  @end{short}
  The top-left corner of the @class{pango-layout} will be at the current point
  of the Cairo context.

  Since 1.10
  @see-symbol{cairo-t}
  @see-class{pango-layout}"
  (cr (:pointer (:struct cairo-t)))
  (layout (g-object pango-layout)))

(export 'pango-cairo-layout-path)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_error_underline_path ()
;;;
;;; void pango_cairo_error_underline_path (cairo_t *cr,
;;;                                        double x,
;;;                                        double y,
;;;                                        double width,
;;;                                        double height);
;;;
;;; Add a squiggly line to the current path in the specified Cairo context that
;;; approximately covers the given rectangle in the style of an underline used
;;; to indicate a spelling error. (The width of the underline is rounded to an
;;; integer number of up/down segments and the resulting rectangle is centered
;;; in the original rectangle)
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; x :
;;;     The X coordinate of one corner of the rectangle
;;;
;;; y :
;;;     The Y coordinate of one corner of the rectangle
;;;
;;; width :
;;;     Non-negative width of the rectangle
;;;
;;; height :
;;;     Non-negative height of the rectangle
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.cairo-rendering.lisp ---------------------------------
