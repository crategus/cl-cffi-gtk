;;; ----------------------------------------------------------------------------
;;; gtk.print-context.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     Encapsulates context for drawing pages
;;;
;;; Types and Values
;;;
;;;     GtkPrintContext
;;;
;;; Functions
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
;;;     GObject
;;;     ╰── GtkPrintContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintContext" gtk-print-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_context_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-context 'type)
 "@version{2013-5-30}
  @begin{short}
    A @sym{gtk-print-context} encapsulates context information that is required
    when drawing pages for printing, such as the cairo context and important
    parameters like page size and resolution. It also lets you easily create
    @class{pango-layout} and @class{pango-context} objects that match the font
    metrics of the cairo surface.
  @end{short}

  @sym{gtk-print-context} objects gets passed to the \"begin-print\",
  \"end-print\", \"request-page-setup\" and \"draw-page\" signals on the
  @class{gtk-print-operation}.

  @b{Example:} Using @sym{gtk-print-context} in a \"draw-page\" callback
  @begin{pre}
   static void
   draw_page (GtkPrintOperation *operation,
              GtkPrintContext   *context,
              int                page_nr)
   {
     cairo_t *cr;
     PangoLayout *layout;
     PangoFontDescription *desc;

     cr = gtk_print_context_get_cairo_context (context);

     // Draw a red rectangle, as wide as the paper (inside the margins)
     cairo_set_source_rgb (cr, 1.0, 0, 0);
     cairo_rectangle (cr, 0, 0, gtk_print_context_get_width (context), 50);

     cairo_fill (cr);

     // Draw some lines
     cairo_move_to (cr, 20, 10);
     cairo_line_to (cr, 40, 20);
     cairo_arc (cr, 60, 60, 20, 0, M_PI);
     cairo_line_to (cr, 80, 20);

     cairo_set_source_rgb (cr, 0, 0, 0);
     cairo_set_line_width (cr, 5);
     cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
     cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);

     cairo_stroke (cr);

     // Draw some text
     layout = gtk_print_context_create_layout (context);
     pango_layout_set_text (layout, \"Hello World! Printing is easy\", -1);
     desc = pango_font_description_from_string (\"sans 28\");
     pango_layout_set_font_description (layout, desc);
     pango_font_description_free (desc);

     cairo_move_to (cr, 30, 20);
     pango_cairo_layout_path (cr, layout);

     // Font Outline
     cairo_set_source_rgb (cr, 0.93, 1.0, 0.47);
     cairo_set_line_width (cr, 0.5);
     cairo_stroke_preserve (cr);

     // Font Fill
     cairo_set_source_rgb (cr, 0, 0.0, 1.0);
     cairo_fill (cr);

     g_object_unref (layout);
   @}
  @end{pre}
  Printing support was added in GTK+ 2.10.")

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_cairo_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_cairo_context"
           gtk-print-context-get-cairo-context) (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The cairo context of @arg{context}.}
  @begin{short}
    Obtains the cairo context that is associated with the
    @class{gtk-print-context}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_set_cairo_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_set_cairo_context"
           gtk-print-context-set-cairo-context) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-15}
  @argument[context]{a @class{gtk-print-context} object}
  @argument[cr]{the cairo context}
  @argument[dpi-x]{the horizontal resolution to use with @arg{cr}}
  @argument[dpi-y]{the vertical resolution to use with @arg{cr}}
  @begin{short}
    Sets a new cairo context on a print context.
  @end{short}

  This function is intended to be used when implementing an internal print
  preview, it is not needed for printing, since GTK+ itself creates a suitable
  cairo context in that case.

  Since 2.10
  @see-class{gtk-print-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-print-context))
  (cr (:pointer (:struct cairo-t)))
  (dpi-x :double)
  (dip-y :double))

(export 'gtk-print-context-set-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_page_setup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_page_setup" gtk-print-context-get-page-setup)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-16}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The page setup of context.}
  @begin{short}
    Obtains the @class{gtk-page-setup} that determines the page dimensions of
    the @class{gtk-print-context}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-class{gtk-page-setup}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_width" gtk-print-context-get-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The width of @arg{context}.}
  @begin{short}
    Obtains the width of the @class{gtk-print-context}, in pixels.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-get-height}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-width)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_height" gtk-print-context-get-height) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The height of @arg{context}.}
  @begin{short}
    Obtains the height of the @class{gtk-print-context}, in pixels.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-get-width}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-height)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_x ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_dpi_x" gtk-print-context-get-dpi-x) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The horizontal resolution of @arg{context}.}
  @begin{short}
    Obtains the horizontal resolution of the @class{gtk-print-context} object,
    in dots per inch.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-get-dpi-y}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-dpi-x)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_y ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_dpi_y" gtk-print-context-get-dpi-y) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The vertical resolution of @arg{context}.}
  @begin{short}
    Obtains the vertical resolution of the @class{gtk-print-context}, in dots
    per inch.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-dpi-y)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_pango_fontmap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_pango_fontmap"
           gtk-print-context-get-pango-fontmap) (g-object pango-font-map)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The font map of @arg{context}.}
  @begin{short}
    Returns a @class{pango-font-map} that is suitable for use with the
    @class{gtk-print-context} object.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-class{pango-font-map}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-pango-fontmap)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_create_pango_context"
           gtk-print-context-create-pango-context) (g-object pango-context)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A new Pango context for @arg{context}.}
  @begin{short}
    Creates a new @class{pango-context} object that can be used with the
    @class{gtk-print-context}.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-class{pango-context}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-create-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_create_pango_layout"
           gtk-print-context-create-pango-layout) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A new Pango layout for @arg{context}.}
  @begin{short}
    Creates a new @class{pango-layout} that is suitable for use with the
    @class{gtk-print-context} object.
  @end{short}

  Since 2.10
  @see-class{gtk-print-context}
  @see-class{pango-layout}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_hard_margins ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_hard_margins"
          %gtk-print-context-get-hard-margins) :boolean
  (context (g-object gtk-print-context))
  (top (:pointer :int))
  (bottom (:pointer :int))
  (left (:pointer :int))
  (right (:pointer :int)))

(defun gtk-print-context-get-hard-margins (context)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-3}
  @argument[context]{a @class{gtk-print-context} object}
  @begin{return}
    @code{top} -- top hardware printer margin @br{}
    @code{bottom} -- bottom hardware printer margin @br{}
    @code{left} -- left hardware printer margin @br{}
    @code{right} -- right hardware printer margin
  @end{return}
  @begin{short}
    Obtains the hardware printer margins of the @class{gtk-print-context},
    in units.
  @end{short}

  Since 2.20
  @see-class{gtk-print-context}"
  (with-foreign-objects ((top :int) (bottom :int) (left :int) (right :int))
    (%gtk-print-context-get-hard-margins context top bottom left right)))

(export 'gtk-print-context-get-hard-margins)

;;; --- End of file gtk.print-context.lisp -------------------------------------
