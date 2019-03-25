;;; ----------------------------------------------------------------------------
;;; gdk.pango.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; Pango Interaction
;;;
;;;     Using Pango in GDK
;;;
;;; Functions
;;;
;;;     gdk_pango_layout_get_clip_region
;;;     gdk_pango_layout_line_get_clip_region
;;;     gdk_pango_context_get
;;;     gdk_pango_context_get_for_screen
;;;     gdk_pango_context_get_for_display
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_get_clip_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_get_clip_region" %gdk-pango-layout-get-clip-region)
    (:pointer (:struct cairo-region-t))
  (layout (g-object pango-layout))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-get-clip-region (layout x-origin y-origin index-ranges)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
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
  text, such as when text is selected.
  @see-class{pango-layout}
  @see-symbol{cairo-region-t}"
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
    (:pointer (:struct cairo-region-t))
  (layout-line (g-boxed-foreign pango-layout-line))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-line-get-clip-region (line
                                              x-origin y-origin index-ranges)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[line]{a @class{pango-layout-line} structure}
  @argument[x-origin]{x pixel where you intend to draw the layout line with this
    clip}
  @argument[y-origin]{baseline pixel where you intend to draw the layout line
    with this clip}
  @argument[index-ranges]{list of byte indexes into the layout, where even
    members of list are start indexes and odd elements are end indexes}
  @return{A clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  @arg{x-origin} and @arg{y-origin} are the top left position of the layout.
  @arg{index-ranges} should contain ranges of bytes in the layout's text. The
  clip region will include space to the left or right of the line, to the layout
  bounding box, if you have indexes above or below the indexes contained inside
  the line. This is to draw the selection all the way to the side of the layout.
  However, the clip region is in line coordinates, not layout coordinates.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn line may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango-layout-line}
  @see-symbol{cairo-region-t}"
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
 "@version{2013-12-7}
  @return{A new @class{pango-context} for the default display.}
  @begin{short}
    Creates a @class{pango-context} object for the default GDK screen.
  @end{short}

  The context must be freed when you are finished with it.

  When using GTK+, normally you should use the function
  @fun{gtk-widget-get-pango-context} instead of this function, to get the
  appropriate context for the widget you intend to render text onto.

  The newly created context will have the default font options, see
  @symbol{cairo-font-options-t}, for the default screen; if these options change
  it will not be updated. Using the function @fun{gtk-widget-get-pango-context}
  is more convenient if you want to keep a context around and track changes to
  the screen's font rendering settings.
  @see-class{pango-context}
  @see-symbol{cairo-font-options-t}
  @see-function{gtk-widget-get-pango-context}")

(export 'gdk-pango-context-get)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get_for_screen" gdk-pango-context-get-for-screen)
    (g-object pango-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[screen]{the @class{gdk-screen} for which the context is to be
    created}
  @return{a new @class{pango-context} object for @arg{screen}}
  @begin{short}
    Creates a @class{pango-context} object for @arg{screen}.
  @end{short}

  The context must be freed when you are finished with it.

  When using GTK+, normally you should use the function
  @fun{gtk-widget-get-pango-context} instead of this function, to get the
  appropriate context for the widget you intend to render text onto.

  The newly created context will have the default font options, see
  @symbol{cairo-font-options-t}, for the @arg{screen}; if these options change
  it will not be updated. Using the function @fun{gtk-widget-get-pango-context}
  is more convenient if you want to keep a context around and track changes to
  the @arg{screen}'s font rendering settings.
  @see-class{pango-context}
  @see-class{gdk-screen}
  @see-symbol{cairo-font-options-t}
  @see-function{gtk-widget-get-pango-context}"
  (screen (g-object gdk-screen)))

(export 'gdk-pango-context-get-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_display ()
;;;
;;; PangoContext *
;;; gdk_pango_context_get_for_display (GdkDisplay *display);
;;;
;;; Creates a PangoContext for display .
;;;
;;; The context must be freed when you’re finished with it.
;;;
;;; When using GTK+, normally you should use gtk_widget_get_pango_context()
;;; instead of this function, to get the appropriate context for the widget you
;;; intend to render text onto.
;;;
;;; The newly created context will have the default font options (see
;;; cairo_font_options_t) for the display; if these options change it will not
;;; be updated. Using gtk_widget_get_pango_context() is more convenient if you
;;; want to keep a context around and track changes to the font rendering
;;; settings.
;;;
;;; display :
;;;     the GdkDisplay for which the context is to be created
;;;
;;; Returns :
;;;     a new PangoContext for display .
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.pango.lisp ---------------------------------------------
