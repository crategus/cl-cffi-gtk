;;; ----------------------------------------------------------------------------
;;; gtk.color-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkColorButton
;;;
;;; A button to launch a color selection dialog
;;;
;;; Synopsis
;;;
;;;     GtkColorButton
;;;
;;;     gtk_color_button_new
;;;     gtk_color_button_new_with_color
;;;     gtk_color_button_new_with_rgba
;;;     gtk_color_button_set_color
;;;     gtk_color_button_get_color
;;;     gtk_color_button_set_alpha
;;;     gtk_color_button_get_alpha
;;;     gtk_color_button_set_rgba
;;;     gtk_color_button_get_rgba
;;;     gtk_color_button_set_use_alpha
;;;     gtk_color_button_get_use_alpha
;;;     gtk_color_button_set_title
;;;     gtk_color_button_get_title
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkColorButton
;;;
;;; Implemented Interfaces
;;;
;;; GtkColorButton implements AtkImplementorIface, GtkBuildable, GtkActionable,
;;; GtkActivatable and GtkColorChooser.
;;;
;;; Properties
;;;
;;;   "alpha"                    guint                 : Read / Write
;;;   "color"                    GdkColor*             : Read / Write
;;;   "rgba"                     GdkRGBA*              : Read / Write
;;;   "title"                    gchar*                : Read / Write
;;;   "use-alpha"                gboolean              : Read / Write
;;;
;;; Signals
;;;
;;;   "color-set"                                      : Run First
;;;
;;; Description
;;;
;;; The GtkColorButton is a button which displays the currently selected color
;;; and allows to open a color selection dialog to change the color. It is
;;; suitable widget for selecting a color in a preference dialog.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "alpha" property
;;;
;;;   "alpha"                    guint                 : Read / Write
;;;
;;; The selected opacity value (0 fully transparent, 65535 fully opaque).
;;;
;;; Allowed values: <= 65535
;;;
;;; Default value: 65535
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "color" property
;;;
;;;   "color"                    GdkColor*             : Read / Write
;;;
;;; Warning
;;;
;;; GtkColorButton:color has been deprecated since version 3.4 and should not be
;;; used in newly-written code. Use "rgba" instead.
;;;
;;; The selected color.
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "rgba" property
;;;
;;;   "rgba"                     GdkRGBA*              : Read / Write
;;;
;;; The RGBA color.
;;;
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" property
;;;
;;;   "title"                    gchar*                : Read / Write
;;;
;;; The title of the color selection dialog
;;;
;;; Default value: "Pick a Color"
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-alpha" property
;;;
;;;   "use-alpha"                gboolean              : Read / Write
;;;
;;; If this property is set to TRUE, the color swatch on the button is rendered
;;; against a checkerboard background to show its opacity and the opacity slider
;;; is displayed in the color selection dialog.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "color-set" signal
;;;
;;; void user_function (GtkColorButton *widget,
;;;                     gpointer        user_data)      : Run First
;;;
;;; The ::color-set signal is emitted when the user selects a color. When
;;; handling this signal, use gtk_color_button_get_color() and
;;; gtk_color_button_get_alpha() (or gtk_color_button_get_rgba()) to find out
;;; which color was just selected.
;;;
;;; Note that this signal is only emitted when the user changes the color. If
;;; you need to react to programmatic color changes as well, use the
;;; notify::color signal.
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorButton
;;;
;;; struct GtkColorButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorButton" gtk-color-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable"
                "GtkColorChooser")
   :type-initializer "gtk_color_button_get_type")
  ((alpha
    gtk-color-button-alpha
    "alpha" "guint" t t)
   (color gtk-color-button-color
    "color" "GdkColor" t t)
   (rgba
    gtk-color-button-rgba
    "rgba" "GdkRGBA" t t)
   (title
    gtk-color-button-title
    "title" "gchararray" t t)
   (use-alpha
    gtk-color-button-use-alpha
    "use-alpha" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new ()
;;;
;;; GtkWidget * gtk_color_button_new (void);
;;;
;;; Creates a new color button.
;;;
;;; This returns a widget in the form of a small button containing a swatch
;;; representing the current selected color. When the button is clicked, a
;;; color-selection dialog will open, allowing the user to select a color. The
;;; swatch will be updated to reflect the new color when the user finishes.
;;;
;;; Returns :
;;;     a new color button
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new))

(defun gtk-color-button-new ()
  (make-instance 'gtk-color-button))

(export 'gtk-color-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_color ()
;;;
;;; GtkWidget * gtk_color_button_new_with_color (const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_color_button_new_with_color has been deprecated since version 3.4 and
;;; should not be used in newly-written code. Use
;;; gtk_color_button_new_with_rgba() instead.
;;;
;;; Creates a new color button.
;;;
;;; color :
;;;     A GdkColor to set the current color with
;;;
;;; Returns :
;;;     a new color button
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new-with-color))

(defun gtk-color-button-new-with-color (color)
  (make-instance 'gtk-color-button
                 :color color))

(export 'gtk-color-button-new-with-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_rgba ()
;;;
;;; GtkWidget * gtk_color_button_new_with_rgba (const GdkRGBA *rgba);
;;;
;;; Creates a new color button.
;;;
;;; rgba :
;;;     A GdkRGBA to set the current color with
;;;
;;; Returns :
;;;     a new color button
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new-with-rgba))

(defun gtk-color-button-new-with-rgba (rgba)
  (make-instance 'gtk-color-button
                 :rgba rgba))

(export 'gtk-color-button-new-with-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_color ()
;;;
;;; void gtk_color_button_set_color (GtkColorButton *button,
;;;                                  const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_color_button_set_color is deprecated and should not be used in
;;; newly-written code. Use gtk_color_chooser_set_rgba() instead.
;;;
;;; Sets the current color to be color.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; color :
;;;     A GdkColor to set the current color with
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-color))

(defun gtk-color-button-set-color (button color)
  (setf (gtk-color-button-color button) color))

(export 'gtk-color-button-set-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_color ()
;;;
;;; void gtk_color_button_get_color (GtkColorButton *button, GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_color_button_get_color has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gtk_color_chooser_get_rgba() instead.
;;;
;;; Sets color to be the current color in the GtkColorButton widget.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; color :
;;;     a GdkColor to fill in with the current color
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-color))

(defun gtk-color-button-get-color (button)
  (gtk-color-button-color button))

(export 'gtk-color-button-get-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_alpha ()
;;;
;;; void gtk_color_button_set_alpha (GtkColorButton *button, guint16 alpha);
;;;
;;; Warning
;;;
;;; gtk_color_button_set_alpha has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gtk_color_chooser_set_rgba() instead.
;;;
;;; Sets the current opacity to be alpha.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; alpha :
;;;     an integer between 0 and 65535
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-alpha))

(defun gtk-color-button-set-alpha (button alpha)
  (setf (gtk-color-button-alpha button) alpha))

(export 'gtk-color-button-set-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_alpha ()
;;;
;;; guint16 gtk_color_button_get_alpha (GtkColorButton *button);
;;;
;;; Warning
;;;
;;; gtk_color_button_get_alpha has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gtk_color_chooser_get_rgba() instead.
;;;
;;; Returns the current alpha value.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; Returns :
;;;     an integer between 0 and 65535
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-alpha))

(defun gtk-color-button-get-alpha (button)
  (gtk-color-button-alpha button))

(export 'gtk-color-button-get-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_rgba ()
;;;
;;; void gtk_color_button_set_rgba (GtkColorButton *button, const GdkRGBA *rgba)
;;;
;;; Warning
;;;
;;; gtk_color_button_set_rgba has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gtk_color_chooser_set_rgba() instead.
;;;
;;; Sets the current color to be rgba.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; rgba :
;;;     a GdkRGBA to set the current color with
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-rgba))

(defun gtk-color-button-set-rgba (button rgba)
  (setf (gtk-color-button-rgba button) rgba))

(export 'gtk-color-button-set-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_rgba ()
;;;
;;; void gtk_color_button_get_rgba (GtkColorButton *button, GdkRGBA *rgba);
;;;
;;; Warning
;;;
;;; gtk_color_button_get_rgba has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gtk_color_chooser_get_rgba() instead.
;;;
;;; Sets rgba to be the current color in the GtkColorButton widget.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; rgba :
;;;     a GdkRGBA to fill in with the current color
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-rgba))

(defun gtk-color-button-get-rgba (button)
  (gtk-color-button-rgba button))

(export 'gtk-color-button-get-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_use_alpha ()
;;;
;;; void gtk_color_button_set_use_alpha (GtkColorButton *button,
;;;                                      gboolean use_alpha);
;;;
;;; Warning
;;;
;;; gtk_color_button_set_use_alpha has been deprecated since version 3.4 and
;;; should not be used in newly-written code. Use
;;; gtk_color_chooser_set_use_alpha() instead.
;;;
;;; Sets whether or not the color button should use the alpha channel.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; use_alpha :
;;;     TRUE if color button should use alpha channel, FALSE if not
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-buuton-set-use-alpha))

(defun gtk-color-button-set-use-alpha (button use-alpha)
  (setf (gtk-color-button-use-alpha button) use-alpha))

(export 'gtk-color-button-set-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_use_alpha ()
;;;
;;; gboolean gtk_color_button_get_use_alpha (GtkColorButton *button);
;;;
;;; Warning
;;;
;;; gtk_color_button_get_use_alpha has been deprecated since version 3.4 and
;;; should not be used in newly-written code. Use
;;; gtk_color_chooser_get_use_alpha() instead.
;;;
;;; Does the color selection dialog use the alpha channel ?
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; Returns :
;;;     TRUE if the color sample uses alpha channel, FALSE if not
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-use-alpha))

(defun gtk-color-button-get-use-alpha (button)
  (gtk-color-button-use-alpha button))

(export 'gtk-color-button-get-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_title ()
;;;
;;; void gtk_color_button_set_title (GtkColorButton *button, const gchar *title)
;;;
;;; Sets the title for the color selection dialog.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; title :
;;;     String containing new window title
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-title))

(defun gtk-color-button-set-title (button title)
  (setf (gtk-color-button-title button) title))

(export 'gtk-color-button-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_title ()
;;;
;;; const gchar * gtk_color_button_get_title (GtkColorButton *button);
;;;
;;; Gets the title of the color selection dialog.
;;;
;;; button :
;;;     a GtkColorButton
;;;
;;; Returns :
;;;     An internal string, do not free the return value
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-title))

(defun gtk-color-button-get-title (button)
  (gtk-color-button-title button))

(export 'gtk-color-button-get-title)

;;; --- End of file gtk.color-button.lisp --------------------------------------
