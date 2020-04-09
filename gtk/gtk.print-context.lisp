;;; ----------------------------------------------------------------------------
;;; gtk.print-context.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
 "@version{2020-4-9}
  @begin{short}
    A @sym{gtk-print-context} encapsulates context information that is required
    when drawing pages for printing, such as the cairo context and important
    parameters like page size and resolution.
  @end{short}
  It also lets you easily create @class{pango-layout} and @class{pango-context}
  objects that match the font metrics of the cairo surface.

  @sym{gtk-print-context} objects gets passed to the \"begin-print\",
  \"end-print\", \"request-page-setup\" and \"draw-page\" signals on the print
  operation.
  @begin[Example]{dictionary}
    Using @sym{gtk-print-context} in a \"draw-page\" callback.
    @begin{pre}
(defun draw-page (operation context page-nr)
  (declare (ignore operation page-nr))
  (let ((cr (gtk-print-context-get-cairo-context context))
        (layout (gtk-print-context-create-pango-layout context)))

    ;; Draw a red rectangle, as wide as the paper (inside the margins)
    (cairo-set-source-rgb cr 1.0 0 0)
    (cairo-rectangle cr 0 0 (gtk-print-context-width context) 50)
    (cairo-fill cr)

    ;; Draw some lines
    (cairo-move-to cr 20 10)
    (cairo-line-to cr 40 20)
    (cairo-arc cr 60 60 20 0 3.14)
    (cairo-line-to cr 80 20)

    (cairo-set-source-rgb cr 0 0 0)
    (cairo-set-line-width cr 5)
    (cairo-set-line-cap cr :round)
    (cairo-set-line-join cr :round)

    (cairo-stroke cr)

    ;; Draw some text
    (setf (pango-layout-text layout) \"Hello World! Printing is easy\")
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string \"sans 28\"))
    (cairo-move-to cr 30 20)
    (pango-cairo-layout-path cr layout)

    ;; Font Outline
    (cairo-set-source-rgb cr 0.93 1.0 0.47)
    (cairo-set-line-width cr 0.5)
    (cairo-stroke-preserve cr)

    ;; Font Fill
    (cairo-set-source-rgb cr 0 0.0 1.0)
    (cairo-fill cr)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-print-operation}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_cairo_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_cairo_context"
           gtk-print-context-get-cairo-context) (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The cairo context of @arg{context}.}
  @begin{short}
    Obtains the cairo context that is associated with the
    @class{gtk-print-context}.
  @end{short}
  @see-class{gtk-print-context}
  @see-symbol{cairo-t}
  @see-function{gtk-print-context-set-cairo-context}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-get-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_set_cairo_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_set_cairo_context"
          %gtk-print-context-set-cairo-context) :void
  (context (g-object gtk-print-context))
  (cr (:pointer (:struct cairo-t)))
  (dpi-x :double)
  (dpi-y :double))

(defun gtk-print-context-set-cairo-context (context cr dpi-x dpi-y)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
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
  @see-class{gtk-print-context}
  @see-symbol{cairo-t}
  @see-function{gtk-print-context-get-cairo-context}"
  (%gtk-print-context-set-cairo-context context
                                        cr
                                        (coerce dpi-x 'double-float)
                                        (coerce dpi-y 'double-float)))

(export 'gtk-print-context-set-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_page_setup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_page_setup" gtk-print-context-page-setup)
    (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The page setup of type @class{gtk-page-setup} of the print context.}
  @begin{short}
    Obtains the page setup that determines the page dimensions of the print
    context.
  @end{short}
  @see-class{gtk-print-context}
  @see-class{gtk-page-setup}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_width" gtk-print-context-width) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A @code{:double} with the width of @arg{context}.}
  @begin{short}
    Obtains the width of the print context, in pixels.
  @end{short}
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-height}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-width)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_height" gtk-print-context-height) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A @code{:double} with the height of @arg{context}.}
  @begin{short}
    Obtains the height of the print context, in pixels.
  @end{short}
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-width}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-height)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_x ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_dpi_x" gtk-print-context-dpi-x) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A @code{:double} with the horizontal resolution of @arg{context}.}
  @begin{short}
    Obtains the horizontal resolution of the print context, in dots per inch.
  @end{short}
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-dpi-y}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-dpi-x)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_y ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_dpi_y" gtk-print-context-dpi-y) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A @code{:double} with the vertical resolution of @arg{context}.}
  @begin{short}
    Obtains the vertical resolution of the print context, in dots per inch.
  @end{short}
  @see-class{gtk-print-context}
  @see-function{gtk-print-context-dpi-x}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-dpi-y)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_pango_fontmap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_pango_fontmap"
           gtk-print-context-pango-fontmap) (g-object pango-font-map)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{The font map of type @class{pango-font-map} of @arg{context}.}
  @begin{short}
    Returns a @class{pango-font-map} that is suitable for use with the
    @class{gtk-print-context} object.
  @end{short}
  @see-class{gtk-print-context}
  @see-class{pango-font-map}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-pango-fontmap)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_create_pango_context"
           gtk-print-context-create-pango-context) (g-object pango-context)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A new Pango context of type @class{pango-context} for @arg{context}.}
  @begin{short}
    Creates a new Pango context that can be used with the print context.
  @end{short}
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
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @return{A new Pango layout of type @class{pango-layout} for @arg{context}.}
  @begin{short}
    Creates a new Pango layout that is suitable for use with the print context.
  @end{short}
  @see-class{gtk-print-context}
  @see-class{pango-layout}"
  (context (g-object gtk-print-context)))

(export 'gtk-print-context-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_hard_margins ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_context_get_hard_margins"
          %gtk-print-context-hard-margins) :boolean
  (context (g-object gtk-print-context))
  (top (:pointer :int))
  (bottom (:pointer :int))
  (left (:pointer :int))
  (right (:pointer :int)))

(defun gtk-print-context-hard-margins (context)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-9}
  @argument[context]{a @class{gtk-print-context} object}
  @begin{return}
    @code{top} -- top hardware printer margin @br{}
    @code{bottom} -- bottom hardware printer margin @br{}
    @code{left} -- left hardware printer margin @br{}
    @code{right} -- right hardware printer margin
  @end{return}
  @begin{short}
    Obtains the hardware printer margins of the print context, in units.
  @end{short}
  @see-class{gtk-print-context}"
  (with-foreign-objects ((top :int) (bottom :int) (left :int) (right :int))
    (%gtk-print-context-hard-margins context top bottom left right)
    (values (mem-ref top :int)
            (mem-ref bottom :int)
            (mem-ref left :int)
            (mem-ref right :int))))

(export 'gtk-print-context-hard-margins)

;;; --- End of file gtk.print-context.lisp -------------------------------------
