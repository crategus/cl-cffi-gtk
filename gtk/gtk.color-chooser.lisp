;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; GtkColorChooser
;;;
;;; Interface implemented by widgets for choosing colors
;;;
;;; Synopsis
;;;
;;;     GtkColorChooser
;;;
;;;     gtk_color_chooser_get_rgba
;;;     gtk_color_chooser_set_rgba
;;;     gtk_color_chooser_get_use_alpha
;;;     gtk_color_chooser_set_use_alpha
;;;     gtk_color_chooser_add_palette
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GtkColorChooser
;;;
;;; Prerequisites
;;;
;;; GtkColorChooser requires GObject.
;;;
;;; Known Implementations
;;;
;;; GtkColorChooser is implemented by GtkColorButton, GtkColorChooserDialog and
;;; GtkColorChooserWidget.
;;;
;;; Properties
;;;
;;;   "rgba"                     GdkRGBA*              : Read / Write
;;;   "use-alpha"                gboolean              : Read / Write
;;;
;;; Signals
;;;
;;;   "color-activated"                                : Run First
;;;
;;; Description
;;;
;;; GtkColorChooser is an interface that is implemented by widgets for choosing
;;; colors. Depending on the situation, colors may be allowed to have alpha
;;; (translucency).
;;;
;;; In GTK+, the main widgets that implement this interface are
;;; GtkColorChooserWidget, GtkColorChooserDialog and GtkColorButton.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "rgba" property
;;;
;;;   "rgba"                     GdkRGBA*              : Read / Write
;;;
;;; The ::rgba property contains the currently selected color, as a GdkRGBA
;;; struct. The property can be set to change the current selection
;;; programmatically.
;;;
;;; Since 3.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-alpha" property
;;;
;;;   "use-alpha"                gboolean              : Read / Write
;;;
;;; When ::use-alpha is TRUE, colors may have alpha (translucency) information.
;;; When it is FALSE, the GdkRGBA struct obtained via the "rgba" property will
;;; be forced to have alpha == 1.
;;;
;;; Implementations are expected to show alpha by rendering the color over a
;;; non-uniform background (like a checkerboard pattern).
;;;
;;; Default value: TRUE
;;;
;;; Since 3.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "color-activated" signal
;;;
;;; void user_function (GtkColorChooser *chooser,
;;;                     GdkRGBA         *color,
;;;                     gpointer         user_data)      : Run First
;;;
;;; Emitted when a color is activated from the color chooser. This usually
;;; happens when the user clicks a color swatch, or a color is selected and the
;;; user presses one of the keys Space, Shift+Space, Return or Enter.
;;;
;;; chooser :
;;;     the object which received the signal
;;;
;;; color :
;;;     the color
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorChooser
;;;
;;; typedef struct _GtkColorChooser GtkColorChooser;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkColorChooser" gtk-color-chooser
  (:export t
   :type-initializer "gtk_color_chooser_get_type")
  (rgba
   gtk-color-chooser-rgba
   "rgba" "GdkRGBA" t t)
  (use-alpha
   gtk-color-chooser-use-alpha
   "use-alpha" "gboolean" t t))

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_get_rgba ()
;;;
;;; void gtk_color_chooser_get_rgba (GtkColorChooser *chooser,
;;;                                  GdkRGBA *color);
;;;
;;; Gets the currently-selected color.
;;;
;;; chooser :
;;;     a GtkColorChooser
;;;
;;; color :
;;;     return location for the color
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-get-rgba))

(defun gtk-color-chooser-get-rgba (chooser)
  (gtk-color-chooser-rgba chooser))

(export 'gtk-color-chooser-get-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_set_rgba ()
;;;
;;; void gtk_color_chooser_set_rgba (GtkColorChooser *chooser,
;;;                                  const GdkRGBA *color);
;;;
;;; Sets the color.
;;;
;;; chooser :
;;;     a GtkColorChooser
;;;
;;; color :
;;;     the new color
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-set-rgba))

(defun gtk-color-chooser-set-rgba (chooser color)
  (setf (gtk-color-chooser-rgba chooser) color))

(export 'gtk-color-chooser-set-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_get_use_alpha ()
;;;
;;; gboolean gtk_color_chooser_get_use_alpha (GtkColorChooser *chooser);
;;;
;;; Returns whether the color chooser shows the alpha channel.
;;;
;;; chooser :
;;;     a GtkColorChooser
;;;
;;; Returns :
;;;     TRUE if the color chooser uses the alpha channel, FALSE if not
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-get-use-alpha))

(defun gtk-color-chooser-get-use-alpha (chooser)
  (gtk-color-chooser-use-alpha chooser))

(export 'gtk-color-chooser-get-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_set_use_alpha ()
;;;
;;; void gtk_color_chooser_set_use_alpha (GtkColorChooser *chooser,
;;;                                       gboolean use_alpha);
;;;
;;; Sets whether or not the color chooser should use the alpha channel.
;;;
;;; chooser :
;;;     a GtkColorChooser
;;;
;;; use_alpha :
;;;     TRUE if color chooser should use alpha channel, FALSE if not
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-set-use-alpha))

(defun gtk-color-chooser-set-use-alpha (chooser use-alpha)
  (setf (gtk-color-chooser-use-alpha chooser) use-alpha))

(export 'gtk-color-chooser-set-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_add_palette ()
;;;
;;; void gtk_color_chooser_add_palette (GtkColorChooser *chooser,
;;;                                     GtkOrientation orientation,
;;;                                     gint colors_per_line,
;;;                                     gint n_colors,
;;;                                     GdkRGBA *colors);
;;;
;;; Adds a palette to the color chooser. If orientation is horizontal, the
;;; colors are grouped in rows, with colors_per_line colors in each row. If
;;; horizontal is FALSE, the colors are grouped in columns instead.
;;;
;;; The default color palette of GtkColorChooserWidget has 27 colors, organized
;;; in columns of 3 colors. The default gray palette has 9 grays in a single
;;; row.
;;;
;;; The layout of the color chooser widget works best when the palettes have
;;; 9-10 columns.
;;;
;;; Calling this function for the first time has the side effect of removing the
;;; default color and gray palettes from the color chooser.
;;;
;;; If colors is NULL, removes all previously added palettes.
;;;
;;; chooser :
;;;     a GtkColorChooser
;;;
;;; orientation :
;;;     GTK_ORIENTATION_HORIZONTAL if the palette should be displayed in rows,
;;;     GTK_ORIENTATION_VERTICAL for columns
;;;
;;; colors_per_line :
;;;     the number of colors to show in each row/column
;;;
;;; n_colors :
;;;     the total number of elements in colors
;;;
;;; colors :
;;;     the colors of the palette, or NULL
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_chooser_add_palette" %gtk-color-chooser-add-palette) :void
  (chooser (g-object gtk-color-chooser))
  (orientation gtk-orientation)
  (colors-per-line :int)
  (n-colors :int)
  (colors :pointer))

(defun gtk-color-chooser-add-palette (chooser
                                      orientation
                                      colors-per-line
                                      colors)
  (with-foreign-boxed-array (n-colors colors-ptr gdk-rgba colors)
    (%gtk-color-chooser-add-palette chooser
                                    orientation
                                    colors-per-line
                                    n-colors
                                    colors-ptr)))

(export 'gtk-color-chooser-add-palette)

;;; --- End of file gtk.color-chooser.lisp -------------------------------------
