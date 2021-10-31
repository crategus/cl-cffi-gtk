;;; ----------------------------------------------------------------------------
;;; gdk.pango.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; gdk_pango_layout_get_clip_region () -> gdk-pango-layout-clip-region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_get_clip_region" %gdk-pango-layout-clip-region)
    (:pointer (:struct cairo-region-t))
  (layout (g-object pango-layout))
  (x-origin :int)
  (y-origin :int)
  (ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-clip-region (layout x-origin y-origin ranges)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-5}
  @argument[layout]{a @class{pango-layout} object}
  @argument[x-origin]{an integer with the x pixel where you intend to draw the
    layout with this clip}
  @argument[y-origin]{an integer with the y pixel where you intend to draw the
    layout with this clip}
  @argument[ranges]{a list of byte indexes into the layout, where even members
    of the list are start indexes and odd elements are end indexes}
  @return{A @symbol{cairo-region-t} clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  The argumentes @arg{x-origin} and @arg{y-origin} are the top left point to
  center the layout. The @arg{ranges} list should contain ranges of bytes in
  the layout's text.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn layout may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango-layout}
  @see-symbol{cairo-region-t}"
  (let ((n (length ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (with-foreign-object (ranges-ar :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (mem-aref ranges-ar :int i) x)
                 (incf i))
               ranges))
        (%gdk-pango-layout-clip-region layout
                                       x-origin
                                       y-origin
                                       ranges-ar
                                       n-ranges)))))

(export 'gdk-pango-layout-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_line_get_clip_region ()
;;; -> gdk-pango-layout-line-clip-region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_layout_line_get_clip_region"
          %gdk-pango-layout-line-clip-region)
    (:pointer (:struct cairo-region-t))
  (layout-line (g-boxed-foreign pango-layout-line))
  (x-origin :int)
  (y-origin :int)
  (ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-line-clip-region (line x-origin y-origin ranges)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-5}
  @argument[line]{a @class{pango-layout-line} instance}
  @argument[x-origin]{an integer with the x pixel where you intend to draw the
    layout line with this clip}
  @argument[y-origin]{an integer with the baseline pixel where you intend to
    draw the layout line with this clip}
  @argument[ranges]{list of byte indexes into the layout, where even members of
    list are start indexes and odd elements are end indexes}
  @return{A @symbol{cairo-region-t} clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  The arguments @arg{x-origin} and @arg{y-origin} are the top left position of
  the layout. The @arg{ranges} list should contain ranges of bytes in the
  layout's text. The clip region will include space to the left or right of the
  line, to the layout bounding box, if you have indexes above or below the
  indexes contained inside the line. This is to draw the selection all the way
  to the side of the layout. However, the clip region is in line coordinates,
  not layout coordinates.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn line may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango-layout-line}
  @see-symbol{cairo-region-t}"
  (let ((n (length ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (with-foreign-object (ranges-ar :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (mem-aref ranges-ar :int i) x)
                 (incf i))
               ranges))
        (%gdk-pango-layout-line-clip-region line
                                            x-origin
                                            y-origin
                                            ranges-ar
                                            n-ranges)))))

(export 'gdk-pango-layout-line-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get" gdk-pango-context-get)
    (g-object pango-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @return{A new @class{pango-context} instance for the default GDK screen.}
  @begin{short}
    Creates a @class{pango-context} instance for the default GDK screen.
  @end{short}

  When using GTK, normally you should use the @fun{gtk-widget-pango-context}
  function instead of this function, to get the appropriate Pango context for
  the widget you intend to render text onto.

  The newly created Pango context will have the default font options, see the
  @symbol{cairo-font-options-t} API, for the default screen. If these options
  change it will not be updated. Using the @fun{gtk-widget-pango-context}
  function is more convenient if you want to keep a Pango context around and
  track changes to the font rendering settings of the screen.
  @see-class{pango-context}
  @see-symbol{cairo-font-options-t}
  @see-function{gtk-widget-pango-context}")

(export 'gdk-pango-context-get)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_screen () -> gdk-pango-context-for-screen
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pango_context_get_for_screen" gdk-pango-context-for-screen)
    (g-object pango-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @argument[screen]{the @class{gdk-screen} object for which the Pango context
    is to be created}
  @return{A new @class{pango-context} instance for @arg{screen}.}
  @begin{short}
    Creates a Pango context for the screen.
  @end{short}

  When using GTK, normally you should use the @fun{gtk-widget-pango-context}
  function instead of this function, to get the appropriate Pango context for
  the widget you intend to render text onto.

  The newly created Pango context will have the default font options, see the
  @symbol{cairo-font-options-t} API, for the screen. If these options change
  it will not be updated. Using the @fun{gtk-widget-pango-context} function is
  more convenient if you want to keep a Pango context around and track changes
  to the screens font rendering settings.
  @see-class{pango-context}
  @see-class{gdk-screen}
  @see-symbol{cairo-font-options-t}
  @see-function{gtk-widget-pango-context}"
  (screen (g-object gdk-screen)))

(export 'gdk-pango-context-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_display () -> gdk-pango-context-for-display
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun ("gdk_pango_context_get_for_display" gdk-pango-context-for-display)
    (g-object pango-context :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-28}
  @argument[display]{the @class{gdk-display} object for which the Pango context
    is to be created}
  @return{A new @class{pango-context} instance for @arg{display}.}
  @begin{short}
    Creates a Pango context for the display.
  @end{short}

  When using GTK, normally you should use the @fun{gtk-widget-pango-context}
  function instead of this function, to get the appropriate Pango context for
  the widget you intend to render text onto.

  The newly created Pango context will have the default font options, see the
  @symbol{cairo-font-options-t} API, for the display. If these options change
  it will not be updated. Using the @fun{gtk-widget-pango-context} function is
  more convenient if you want to keep a Pango context around and track changes
  to the font rendering settings.

  Since 3.22
  @see-class{gdk-display}
  @see-class{pango-context}
  @see-symbol{cairo-font-options-t}
  @see-function{gtk-widget-pango-context}"
  (display (g-object gdk-display)))

#+gdk-3-22
(export 'gdk-pango-context-for-display)

;;; --- End of file gdk.pango.lisp ---------------------------------------------
