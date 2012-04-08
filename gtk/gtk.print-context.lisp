;;; ----------------------------------------------------------------------------
;;; gtk.print-operation.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkPrintContext
;;; 
;;; Encapsulates context for drawing pages
;;; 
;;; Synopsis
;;; 
;;;     GtkPrintContext
;;;
;;;     gtk_print_context_get_cairo_context
;;;     gtk_print_context_set_cairo_context
;;;     gtk_print_context_get_page_setup
;;;     gtk_print_context_get_width
;;;     gtk_print_context_get_height
;;;     gtk_print_context_get_dpi_x
;;;     gtk_print_context_get_dpi_y
;;;     gtk_print_context_get_pango_fontmap
;;;     gtk_print_context_create_pango_context
;;;     gtk_print_context_create_pango_layout
;;;     gtk_print_context_get_hard_margins
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkPrintContext
;;; 
;;; Description
;;; 
;;; A GtkPrintContext encapsulates context information that is required when
;;; drawing pages for printing, such as the cairo context and important
;;; parameters like page size and resolution. It also lets you easily create
;;; PangoLayout and PangoContext objects that match the font metrics of the
;;; cairo surface.
;;; 
;;; GtkPrintContext objects gets passed to the "begin-print", "end-print",
;;; "request-page-setup" and "draw-page" signals on the GtkPrintOperation.
;;; 
;;; Example 96. Using GtkPrintContext in a "draw-page" callback
;;; 
;;; static void
;;; draw_page (GtkPrintOperation *operation,
;;;        GtkPrintContext   *context,
;;;        int                page_nr)
;;; {
;;;   cairo_t *cr;
;;;   PangoLayout *layout;
;;;   PangoFontDescription *desc;
;;; 
;;;   cr = gtk_print_context_get_cairo_context (context);
;;; 
;;;   // Draw a red rectangle, as wide as the paper (inside the margins)
;;;   cairo_set_source_rgb (cr, 1.0, 0, 0);
;;;   cairo_rectangle (cr, 0, 0, gtk_print_context_get_width (context), 50);
;;; 
;;;   cairo_fill (cr);
;;; 
;;;   // Draw some lines
;;;   cairo_move_to (cr, 20, 10);
;;;   cairo_line_to (cr, 40, 20);
;;;   cairo_arc (cr, 60, 60, 20, 0, M_PI);
;;;   cairo_line_to (cr, 80, 20);
;;; 
;;;   cairo_set_source_rgb (cr, 0, 0, 0);
;;;   cairo_set_line_width (cr, 5);
;;;   cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
;;;   cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
;;; 
;;;   cairo_stroke (cr);
;;; 
;;;   // Draw some text
;;;   layout = gtk_print_context_create_layout (context);
;;;   pango_layout_set_text (layout, "Hello World! Printing is easy", -1);
;;;   desc = pango_font_description_from_string ("sans 28");
;;;   pango_layout_set_font_description (layout, desc);
;;;   pango_font_description_free (desc);
;;; 
;;;   cairo_move_to (cr, 30, 20);
;;;   pango_cairo_layout_path (cr, layout);
;;; 
;;;   // Font Outline
;;;   cairo_set_source_rgb (cr, 0.93, 1.0, 0.47);
;;;   cairo_set_line_width (cr, 0.5);
;;;   cairo_stroke_preserve (cr);
;;; 
;;;   // Font Fill
;;;   cairo_set_source_rgb (cr, 0, 0.0, 1.0);
;;;   cairo_fill (cr);
;;; 
;;;   g_object_unref (layout);
;;; }
;;; 
;;; Printing support was added in GTK+ 2.10.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintContext
;;; 
;;; typedef struct _GtkPrintContext GtkPrintContext;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintContext" gtk-print-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_context_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_cairo_context ()
;;; 
;;; cairo_t * gtk_print_context_get_cairo_context (GtkPrintContext *context);
;;; 
;;; Obtains the cairo context that is associated with the GtkPrintContext.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the cairo context of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_set_cairo_context ()
;;; 
;;; void gtk_print_context_set_cairo_context (GtkPrintContext *context,
;;;                                           cairo_t *cr,
;;;                                           double dpi_x,
;;;                                           double dpi_y);
;;; 
;;; Sets a new cairo context on a print context.
;;; 
;;; This function is intended to be used when implementing an internal print
;;; preview, it is not needed for printing, since GTK+ itself creates a suitable
;;; cairo context in that case.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; cr :
;;;     the cairo context
;;; 
;;; dpi_x :
;;;     the horizontal resolution to use with cr
;;; 
;;; dpi_y :
;;;     the vertical resolution to use with cr
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_page_setup ()
;;; 
;;; GtkPageSetup * gtk_print_context_get_page_setup (GtkPrintContext *context)
;;; 
;;; Obtains the GtkPageSetup that determines the page dimensions of the
;;; GtkPrintContext.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the page setup of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_width ()
;;; 
;;; gdouble gtk_print_context_get_width (GtkPrintContext *context);
;;; 
;;; Obtains the width of the GtkPrintContext, in pixels.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the width of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_height ()
;;; 
;;; gdouble gtk_print_context_get_height (GtkPrintContext *context);
;;; 
;;; Obtains the height of the GtkPrintContext, in pixels.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the height of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_x ()
;;; 
;;; gdouble gtk_print_context_get_dpi_x (GtkPrintContext *context);
;;; 
;;; Obtains the horizontal resolution of the GtkPrintContext, in dots per inch.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the horizontal resolution of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_y ()
;;; 
;;; gdouble gtk_print_context_get_dpi_y (GtkPrintContext *context);
;;; 
;;; Obtains the vertical resolution of the GtkPrintContext, in dots per inch.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the vertical resolution of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_pango_fontmap ()
;;; 
;;; PangoFontMap * gtk_print_context_get_pango_fontmap
;;;                                                   (GtkPrintContext *context)
;;; 
;;; Returns a PangoFontMap that is suitable for use with the GtkPrintContext.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     the font map of context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_context ()
;;; 
;;; PangoContext * gtk_print_context_create_pango_context
;;;                                                   (GtkPrintContext *context)
;;; 
;;; Creates a new PangoContext that can be used with the GtkPrintContext.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     a new Pango context for context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_layout ()
;;; 
;;; PangoLayout * gtk_print_context_create_pango_layout
;;;                                                   (GtkPrintContext *context)
;;; 
;;; Creates a new PangoLayout that is suitable for use with the GtkPrintContext.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; Returns :
;;;     a new Pango layout for context
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_hard_margins ()
;;; 
;;; gboolean gtk_print_context_get_hard_margins (GtkPrintContext *context,
;;;                                              gdouble *top,
;;;                                              gdouble *bottom,
;;;                                              gdouble *left,
;;;                                              gdouble *right);
;;; 
;;; Obtains the hardware printer margins of the GtkPrintContext, in units.
;;; 
;;; context :
;;;     a GtkPrintContext
;;; 
;;; top :
;;;     top hardware printer margin
;;; 
;;; bottom :
;;;     bottom hardware printer margin
;;; 
;;; left :
;;;     left hardware printer margin
;;; 
;;; right :
;;;     right hardware printer margin
;;; 
;;; Returns :
;;;     TRUE if the hard margins were retrieved
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.print-context.lisp -------------------------------------
