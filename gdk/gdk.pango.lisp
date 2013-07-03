;;; ----------------------------------------------------------------------------
;;; gdk.pango.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org. See <http://www.gtk.org>.
;;; The API  documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.

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
;;; Pango Interaction
;;;
;;; Using Pango in GDK
;;;
;;; Synopsis
;;;
;;;     gdk_pango_layout_get_clip_region
;;;     gdk_pango_layout_line_get_clip_region
;;;     gdk_pango_context_get
;;;     gdk_pango_context_get_for_screen
;;;
;;; Description
;;;
;;; Pango is the text layout system used by GDK and GTK+. The functions and
;;; types in this section are used to obtain clip regions for PangoLayouts, and
;;; to get PangoContexts that can be used with GDK.
;;;
;;; Creating a PangoLayout object is the first step in rendering text, and
;;; requires getting a handle to a PangoContext. For GTK+ programs, you'll
;;; usually want to use gtk_widget_get_pango_context(), or
;;; gtk_widget_create_pango_layout(), rather than using the lowlevel
;;; gdk_pango_context_get_for_screen(). Once you have a PangoLayout, you can set
;;; the text and attributes of it with Pango functions like
;;; pango_layout_set_text() and get its size with pango_layout_get_size(). (Note
;;; that Pango uses a fixed point system internally, so converting between Pango
;;; units and pixels using PANGO_SCALE or the PANGO_PIXELS() macro.)
;;;
;;; Rendering a Pango layout is done most simply with pango_cairo_show_layout();
;;; you can also draw pieces of the layout with pango_cairo_show_layout_line().
;;;
;;; Example 5. Draw transformed text with Pango and cairo
;;;
;;;   #define RADIUS 100
;;;   #define N_WORDS 10
;;;   #define FONT "Sans Bold 18"
;;;
;;;   PangoContext *context;
;;;   PangoLayout *layout;
;;;   PangoFontDescription *desc;
;;;
;;;   double radius;
;;;   int width, height;
;;;   int i;
;;;
;;;   /* Set up a transformation matrix so that the user space coordinates for
;;;    * where we are drawing are [-RADIUS, RADIUS], [-RADIUS, RADIUS]
;;;    * We first center, then change the scale */
;;;
;;;   width = gdk_window_get_width (window);
;;;   height = gdk_window_get_height (window);
;;;   radius = MIN (width, height) / 2.;
;;;
;;;   cairo_translate (cr,
;;;                    radius + (width - 2 * radius) / 2,
;;;                    radius + (height - 2 * radius) / 2);
;;;                    cairo_scale (cr, radius / RADIUS, radius / RADIUS);
;;;
;;;   /* Create a PangoLayout, set the font and text */
;;;   context = gdk_pango_context_get_for_screen (screen);
;;;   layout = pango_layout_new (context);
;;;   pango_layout_set_text (layout, "Text", -1);
;;;   desc = pango_font_description_from_string (FONT);
;;;   pango_layout_set_font_description (layout, desc);
;;;   pango_font_description_free (desc);
;;;
;;;   /* Draw the layout N_WORDS times in a circle */
;;;   for (i = 0; i < N_WORDS; i++)
;;;     {
;;;       double red, green, blue;
;;;       double angle = 2 * G_PI * i / n_words;
;;;
;;;       cairo_save (cr);
;;;
;;;       /* Gradient from red at angle == 60 to blue at angle == 300 */
;;;       red = (1 + cos (angle - 60)) / 2;
;;;       green = 0;
;;;       blue = 1 - red;
;;;
;;;       cairo_set_source_rgb (cr, red, green, blue);
;;;       cairo_rotate (cr, angle);
;;;
;;;       /* Inform Pango to re-layout the text with the new transformation
;;;          matrix */
;;;       pango_cairo_update_layout (cr, layout);
;;;
;;;       pango_layout_get_size (layout, &width, &height);
;;;
;;;       cairo_move_to (cr, - width / 2 / PANGO_SCALE, - DEFAULT_TEXT_RADIUS);
;;;       pango_cairo_show_layout (cr, layout);
;;;
;;;       cairo_restore (cr);
;;;     }
;;;
;;;   g_object_unref (layout);
;;;   g_object_unref (context);
;;;
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_get_clip_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_get_clip_region" %gdk-pango-layout-get-clip-region)
    cairo-region-t
  (layout (g-object pango-layout))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-get-clip-region (layout x-origin y-origin index-ranges)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[layout]{a @class{pango-layout} object}
  @argument[x-origin]{x pixel where you intend to draw the layout with this
    clip}
  @argument[y-origin]{y pixel where you intend to draw the layout with this
    clip}
  @argument[index-ranges]{array of byte indexes into the layout, where even
    members of array are start indexes and odd elements are end indexes}
  @return{A clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  @arg{x-origin} and @arg{y-origin} are the top left point to center the
  layout. @arg{index-ranges} should contain ranges of bytes in the layout's
  text.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn layout may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected."
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_line_get_clip_region"
          %gdk-pango-layout-line-get-clip-region)
    cairo-region-t
  (layout-line (g-boxed-foreign pango-layout-line))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-line-get-clip-region (line x-origin y-origin
                                                          index-ranges)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[line]{a @class{pango-layout-line} structure}
  @argument[x-origin]{x pixel where you intend to draw the layout line with this
    clip}
  @argument[y-origin]{baseline pixel where you intend to draw the layout line
    with this clip}
  @argument[index-ranges]{array of byte indexes into the layout, where even
    members of array are start indexes and odd elements are end indexes}
  @return{A clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  @arg{x-origin} and @arg{y-origin} are the top left position of the layout.
  @arg{index-ranges} should contain ranges of bytes in the layout's text. The
  clip region will include space to the left or right of the line (to the layout
  bounding box) if you have indexes above or below the indexes contained inside
  the line. This is to draw the selection all the way to the side of the layout.
  However, the clip region is in line coordinates, not layout coordinates.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn line may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected."
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
        (%gdk-pango-layout-line-get-clip-region line
                                                x-origin
                                                y-origin
                                                index-ranges
                                                n-ranges)))))

(export 'gdk-pango-layout-line-get-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get" gdk-pango-context-get)
    (g-object pango-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @return{A new @class{pango-context} for the default display.}
  @begin{short}
    Creates a @class{pango-context} object for the default GDK screen.
  @end{short}

  The context must be freed when you are finished with it.

  When using GTK+, normally you should use the function
  @fun{gtk-widget-get-pango-context} instead of this function, to get the
  appropriate context for the widget you intend to render text onto.

  The newly created context will have the default font options (see
  @symbol{cairo-font-options-t}) for the default screen; if these options change
  it will not be updated. Using the function @fun{gtk-widget-get-pango-context}
  is more convenient if you want to keep a context around and track changes to
  the screen's font rendering settings.
  @see-function{gtk-widget-get-pango-context}")

(export 'gdk-pango-context-get)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get_for_screen" gdk-pango-context-get-for-screen)
    (g-object pango-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[screen]{the @class{gdk-screen} for which the context is to be created}
  @return{a new @class{pango-context} object for @arg{screen}}
  @begin{short}
    Creates a @class{pango-context} object for @arg{screen}.
  @end{short}

  The context must be freed when you are finished with it.

  When using GTK+, normally you should use the function
  @fun{gtk-widget-get-pango-context} instead of this function, to get the
  appropriate context for the widget you intend to render text onto.

  The newly created context will have the default font options (see
  @symbol{cairo-font-options-t}) for the @arg{screen}; if these options change
  it will not be updated. Using the function @fun{gtk-widget-get-pango-context}
  is more convenient if you want to keep a context around and track changes to
  the @arg{screen}'s font rendering settings.

  Since 2.2
  @see-function{gtk-widget-get-pango-context}
  @see-symbol{cairo-font-options-t}"
  (screen (g-object gdk-screen)))

(export 'gdk-pango-context-get-for-screen)

;;; --- End of file gdk.pango.lisp ---------------------------------------------
