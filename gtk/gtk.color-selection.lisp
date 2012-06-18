;;; ----------------------------------------------------------------------------
;;; gtk.color-selection.lisp
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
;;; GtkColorSelection
;;; 
;;; A widget used to select a color
;;; 
;;; Synopsis
;;; 
;;;     GtkColorSelection
;;;     
;;;     gtk_color_selection_new
;;;     gtk_color_selection_set_has_opacity_control
;;;     gtk_color_selection_get_has_opacity_control
;;;     gtk_color_selection_set_has_palette
;;;     gtk_color_selection_get_has_palette
;;;     gtk_color_selection_get_current_alpha
;;;     gtk_color_selection_set_current_alpha
;;;     gtk_color_selection_get_current_color
;;;     gtk_color_selection_set_current_color
;;;     gtk_color_selection_get_previous_alpha
;;;     gtk_color_selection_set_previous_alpha
;;;     gtk_color_selection_get_previous_color
;;;     gtk_color_selection_set_previous_color
;;;     gtk_color_selection_get_current_rgba
;;;     gtk_color_selection_set_current_rgba
;;;     gtk_color_selection_get_previous_rgba
;;;     gtk_color_selection_set_previous_rgba
;;;     gtk_color_selection_is_adjusting
;;;     gtk_color_selection_palette_from_string
;;;     gtk_color_selection_palette_to_string
;;;     gtk_color_selection_set_change_palette_with_screen_hook
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkColorSelection
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkColorSelection implements AtkImplementorIface, GtkBuildable
;;; and GtkOrientable.
;;;
;;; Properties
;;; 
;;;   "current-alpha"            guint                 : Read / Write
;;;   "current-color"            GdkColor*             : Read / Write
;;;   "current-rgba"             GdkRGBA*              : Read / Write
;;;   "has-opacity-control"      gboolean              : Read / Write
;;;   "has-palette"              gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "color-changed"                                  : Run First
;;; 
;;; Description
;;; 
;;; The GtkColorSelection is a widget that is used to select a color. It
;;; consists of a color wheel and number of sliders and entry boxes for color
;;; parameters such as hue, saturation, value, red, green, blue, and opacity. It
;;; is found on the standard color selection dialog box GtkColorSelectionDialog.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-alpha" property
;;; 
;;;   "current-alpha"            guint                 : Read / Write
;;; 
;;; The current opacity value (0 fully transparent, 65535 fully opaque).
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 65535
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-color" property
;;; 
;;;   "current-color"            GdkColor*             : Read / Write
;;; 
;;; The current color.
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-rgba" property
;;; 
;;;   "current-rgba"             GdkRGBA*              : Read / Write
;;; 
;;; The current RGBA color.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-opacity-control" property
;;; 
;;;   "has-opacity-control"      gboolean              : Read / Write
;;; 
;;; Whether the color selector should allow setting opacity.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-palette" property
;;; 
;;;   "has-palette"              gboolean              : Read / Write
;;; 
;;; Whether a palette should be used.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "color-changed" signal
;;; 
;;; void user_function (GtkColorSelection *colorselection,
;;;                     gpointer           user_data)           : Run First
;;; 
;;; This signal is emitted when the color changes in the GtkColorSelection
;;; according to its update policy.
;;; 
;;; colorselection :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorSelection
;;; 
;;; struct GtkColorSelection;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkColorSelection" 'gtk-color-selection))

(define-g-object-class "GtkColorSelection" gtk-color-selection
  (:superclass gtk-vbox
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_color_selection_get_type")
  ((current-alpha
    gtk-color-selection-current-alpha
    "current-alpha" "guint" t t)
   (current-color
    gtk-color-selection-current-color
    "current-color" "GdkColor" t t)
   (has-opacity-control
    gtk-color-selection-has-opacity-control
    "has-opacity-control" "gboolean" t t)
   (has-palette
    gtk-color-selection-has-palette
    "has-palette" "gboolean" t t)
   (:cffi previous-alpha
          gtk-color-selection-previous-alpha :uint16
          "gtk_color_selection_get_previous_alpha"
          "gtk_color_selection_set_previous_alpha")
   (:cffi previous-color
          gtk-color-selection-previous-color (g-boxed-foreign gdk-color)
          gtk-color-selection-get-previous-color
          gtk-color-selection-set-previous-color)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_new ()
;;; 
;;; GtkWidget * gtk_color_selection_new (void);
;;; 
;;; Creates a new GtkColorSelection.
;;; 
;;; Returns :
;;;     a new GtkColorSelection
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-new ()
  (make-instance 'gtk-color-selection))

(export 'gtk-color-selection-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_has_opacity_control ()
;;; 
;;; void gtk_color_selection_set_has_opacity_control
;;;                                                (GtkColorSelection *colorsel,
;;;                                                 gboolean has_opacity);
;;; 
;;; Sets the colorsel to use or not use opacity.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; has_opacity :
;;;     TRUE if colorsel can set the opacity, FALSE otherwise
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-has-opacity-control (colorsel has-opacity)
  (setf (gtk-color-selection-has-opacity-control colorsel) has-opacity))

(export 'gtk-color-selection-set-has-opacity-control)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_has_opacity_control ()
;;; 
;;; gboolean gtk_color_selection_get_has_opacity_control
;;;                                                (GtkColorSelection *colorsel)
;;; 
;;; Determines whether the colorsel has an opacity control.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; Returns :
;;;     TRUE if the colorsel has an opacity control, FALSE if it does't
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-has-opacity-control (colorsel)
  (gtk-color-selection-has-opacity-control colorsel))

(export 'gtk-color-selection-get-has-opacity-control)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_has_palette ()
;;; 
;;; void gtk_color_selection_set_has_palette (GtkColorSelection *colorsel,
;;;                                           gboolean has_palette);
;;; 
;;; Shows and hides the palette based upon the value of has_palette.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; has_palette :
;;;     TRUE if palette is to be visible, FALSE otherwise
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-has-palette (colorsel has-palette)
  (setf (gtk-color-selection-has-palette colorsel) has-palette))

(export 'gtk-color-selection-set-has-palette)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_has_palette ()
;;; 
;;; gboolean gtk_color_selection_get_has_palette (GtkColorSelection *colorsel);
;;; 
;;; Determines whether the color selector has a color palette.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; Returns :
;;;     TRUE if the selector has a palette, FALSE if it hasn't
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-has-palette (colorsel)
  (gtk-color-selection-has-palette colorsel))

(export 'gtk-color-selection-get-has-palette)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_current_alpha ()
;;; 
;;; guint16 gtk_color_selection_get_current_alpha (GtkColorSelection *colorsel);
;;; 
;;; Returns the current alpha value.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; Returns :
;;;     an integer between 0 and 65535
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-current-alpha (colorsel)
  (gtk-color-selection-current-alpha colorsel))

(export 'gtk-color-selection-get-current-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_current_alpha ()
;;; 
;;; void gtk_color_selection_set_current_alpha (GtkColorSelection *colorsel,
;;;                                             guint16 alpha);
;;; 
;;; Sets the current opacity to be alpha.
;;; 
;;; The first time this is called, it will also set the original opacity to be
;;; alpha too.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; alpha :
;;;     an integer between 0 and 65535
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-current-alpha (colorsel alpha)
  (setf (gtk-color-selection-current-alpha colorsel) alpha))

(export 'gtk-color-selection-set-current-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_current_color ()
;;; 
;;; void gtk_color_selection_get_current_color (GtkColorSelection *colorsel,
;;;                                             GdkColor *color);
;;; 
;;; Sets color to be the current color in the GtkColorSelection widget.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; color :
;;;     a GdkColor to fill in with the current color.
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-current-color (colorsel)
  (gtk-color-selection-current-color colorsel))

(export 'gtk-color-selection-get-current-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_current_color ()
;;; 
;;; void gtk_color_selection_set_current_color (GtkColorSelection *colorsel,
;;;                                             const GdkColor *color);
;;; 
;;; Sets the current color to be color.
;;; 
;;; The first time this is called, it will also set the original color to be
;;; color too.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; color :
;;;     a GdkColor to set the current color with
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-current-color (colorsel color)
  (setf (gtk-color-selection-current-color colorsel) color))

(export 'gtk-color-selection-set-current-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_alpha ()
;;; 
;;; guint16 gtk_color_selection_get_previous_alpha (GtkColorSelection *colorsel)
;;; 
;;; Returns the previous alpha value.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; Returns :
;;;     an integer between 0 and 65535
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-previous-alpha (colorsel)
  (gtk-color-selection-previous-alpha colorsel))

(export 'gtk-color-selection-get-previous-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_previous_alpha ()
;;; 
;;; void gtk_color_selection_set_previous_alpha (GtkColorSelection *colorsel,
;;;                                              guint16 alpha);
;;; 
;;; Sets the 'previous' alpha to be alpha.
;;; 
;;; This function should be called with some hesitations, as it might seem
;;; confusing to have that alpha change.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; alpha :
;;;     an integer between 0 and 65535
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-previous-alpha (colorsel alpha)
  (setf (gtk-color-selection-previous-alpha colorsel) alpha))

(export 'gtk-color-selection-set-previous-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_color ()
;;; 
;;; void gtk_color_selection_get_previous_color (GtkColorSelection *colorsel,
;;;                                              GdkColor *color);
;;; 
;;; Fills color in with the original color value.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; color :
;;;     a GdkColor to fill in with the original color value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_get_previous_color"
          %gtk-color-selection-get-previous-color) :void
  (color-selection (g-object gtk-color-selection))
  (color (g-boxed-foreign gdk-color)))

(defun gtk-color-selection-get-previous-color (color-selection)
  (let ((color (make-gdk-color)))
    (%gtk-color-selection-get-previous-color color-selection color)
    color))

(export 'gtk-color-selection-get-previous-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_previous_color ()
;;; 
;;; void gtk_color_selection_set_previous_color (GtkColorSelection *colorsel,
;;;                                              const GdkColor *color);
;;; 
;;; Sets the 'previous' color to be color.
;;; 
;;; This function should be called with some hesitations, as it might seem
;;; confusing to have that color change. Calling
;;; gtk_color_selection_set_current_color() will also set this color the first
;;; time it is called.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; color :
;;;     a GdkColor to set the previous color with
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_set_previous_color"
          gtk-color-selection-set-previous-color) :void
  (color-selection (g-object gtk-color-selection))
  (color (g-boxed-foreign gdk-color)))

(export 'gtk-color-selection-set-previous-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_current_rgba ()
;;; 
;;; void gtk_color_selection_get_current_rgba (GtkColorSelection *colorsel,
;;;                                            GdkRGBA *rgba);
;;; 
;;; Sets rgba to be the current color in the GtkColorSelection widget.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; rgba :
;;;     a GdkRGBA to fill in with the current color. [out]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_current_rgba ()
;;; 
;;; void gtk_color_selection_set_current_rgba (GtkColorSelection *colorsel,
;;;                                            const GdkRGBA *rgba);
;;; 
;;; Sets the current color to be rgba.
;;; 
;;; The first time this is called, it will also set the original color to be
;;; rgba too.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; rgba :
;;;     A GdkRGBA to set the current color with
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_rgba ()
;;; 
;;; void gtk_color_selection_get_previous_rgba (GtkColorSelection *colorsel,
;;;                                             GdkRGBA *rgba);
;;; 
;;; Fills rgba in with the original color value.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; rgba :
;;;     a GdkRGBA to fill in with the original color value
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_previous_rgba ()
;;; 
;;; void gtk_color_selection_set_previous_rgba (GtkColorSelection *colorsel,
;;;                                             const GdkRGBA *rgba);
;;; 
;;; Sets the 'previous' color to be rgba.
;;; 
;;; This function should be called with some hesitations, as it might seem
;;; confusing to have that color change. Calling
;;; gtk_color_selection_set_current_rgba() will also set this color the first
;;; time it is called.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; rgba :
;;;     a GdkRGBA to set the previous color with
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_is_adjusting ()
;;; 
;;; gboolean gtk_color_selection_is_adjusting (GtkColorSelection *colorsel);
;;; 
;;; Gets the current state of the colorsel.
;;; 
;;; colorsel :
;;;     a GtkColorSelection
;;; 
;;; Returns :
;;;     TRUE if the user is currently dragging a color around, and FALSE if the
;;;     selection has stopped
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_is_adjusting" gtk-color-selection-is-adjusting)
    :boolean
  (color-selection g-object))

(export 'gtk-color-selection-is-adjusting)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_from_string ()
;;; 
;;; gboolean gtk_color_selection_palette_from_string (const gchar *str,
;;;                                                   GdkColor **colors,
;;;                                                   gint *n_colors);
;;; 
;;; Parses a color palette string; the string is a colon-separated list of color
;;; names readable by gdk_color_parse().
;;; 
;;; str :
;;;     a string encoding a color palette
;;; 
;;; colors :
;;;     return location for allocated array of GdkColor
;;; 
;;; n_colors :
;;;     return location for length of array
;;; 
;;; Returns :
;;;     TRUE if a palette was successfully parsed
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_palette_from_string"
          %gtk-color-selection-palette-from-string) :boolean
  (str :string)
  (colors :pointer)
  (n-colors :pointer))

(defun gtk-color-selection-palette-from-string (str)
  (with-foreign-objects ((colors :pointer) (n-colors :int))
    (when (%gtk-color-selection-palette-from-string str colors n-colors)
      (iter (with colors-ar = (mem-ref colors :pointer))
            (for i from 0 below (mem-ref n-colors :int))
            (for color-ptr =
                 (inc-pointer colors-ar
                              (* i (foreign-type-size 'gdk-color-cstruct))))
            (for color = (convert-from-foreign color-ptr
                                               '(g-boxed-foreign gdk-color)))
            (collect color)
            (finally (g-free colors-ar))))))

(export 'gtk-color-selection-palette-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_to_string ()
;;; 
;;; gchar * gtk_color_selection_palette_to_string (const GdkColor *colors,
;;;                                                gint n_colors);
;;; 
;;; Encodes a palette as a string, useful for persistent storage.
;;; 
;;; colors :
;;;     an array of colors
;;; 
;;; n_colors :
;;;     length of the array
;;; 
;;; Returns :
;;;     allocated string encoding the palette
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_palette_to_string"
          gtk-color-selection-palette-to-string)
    (g-string :free-from-foreign t)
  (colors :pointer)
  (n-colors :int))

(export 'gtk-color-selection-palette-to-string)

;;; ----------------------------------------------------------------------------
;;; GtkColorSelectionChangePaletteFunc ()
;;; 
;;; void (*GtkColorSelectionChangePaletteFunc) (const GdkColor *colors,
;;;                                             gint n_colors);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_change_palette_with_screen_hook ()
;;; 
;;; GtkColorSelectionChangePaletteWithScreenFunc
;;; gtk_color_selection_set_change_palette_with_screen_hook
;;;                          (GtkColorSelectionChangePaletteWithScreenFunc func)
;;; 
;;; Installs a global function to be called whenever the user tries to modify
;;; the palette in a color selection.
;;; 
;;; This function should save the new palette contents, and update the
;;; "gtk-color-palette" GtkSettings property so all GtkColorSelection widgets
;;; will be modified.
;;; 
;;; func :
;;;     a function to call when the custom palette needs saving
;;; 
;;; Returns :
;;;     the previous change palette hook (that was replaced)
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkColorSelectionChangePaletteWithScreenFunc ()
;;; 
;;; void (*GtkColorSelectionChangePaletteWithScreenFunc)
;;;                                                     (GdkScreen *screen,
;;;                                                      const GdkColor *colors,
;;;                                                      gint n_colors);
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.color-selection.lisp -----------------------------------
