;;; ----------------------------------------------------------------------------
;;; gtk.scale.lisp
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
;;; GtkScale
;;; 
;;; A slider widget for selecting a value from a range
;;;     
;;; Synopsis
;;; 
;;;     GtkScale
;;;
;;;     gtk_scale_new
;;;     gtk_scale_new_with_range
;;;     gtk_scale_set_digits
;;;     gtk_scale_set_draw_value
;;;     gtk_scale_set_value_pos
;;;     gtk_scale_get_digits
;;;     gtk_scale_get_draw_value
;;;     gtk_scale_get_value_pos
;;;     gtk_scale_get_layout
;;;     gtk_scale_get_layout_offsets
;;;     gtk_scale_add_mark
;;;     gtk_scale_clear_marks
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                            +----GtkHScale
;;;                            +----GtkVScale
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Properties
;;; 
;;;   "digits"                   gint                  : Read / Write
;;;   "draw-value"               gboolean              : Read / Write
;;;   "value-pos"                GtkPositionType       : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "slider-length"            gint                  : Read
;;;   "value-spacing"            gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "format-value"                                   : Run Last
;;; 
;;; Description
;;; 
;;; A GtkScale is a slider control used to select a numeric value. To use it,
;;; you'll probably want to investigate the methods on its base class,
;;; GtkRange, in addition to the methods for GtkScale itself. To set the value
;;; of a scale, you would normally use gtk_range_set_value(). To detect changes
;;; to the value, you would normally use the "value-changed" signal.
;;; 
;;; Note that using the same upper and lower bounds for the GtkScale (through
;;; the GtkRange methods) will hide the slider itself. This is useful for
;;; applications that want to show an undeterminate value on the scale, without
;;; changing the layout of the application (such as movie or music players).
;;; 
;;; GtkScale as GtkBuildable
;;;
;;; GtkScale supports a custom <marks> element, which can contain multiple
;;; <mark> elements. The "value" and "position" attributes have the same
;;; meaning as gtk_scale_add_mark() parameters of the same name. If the element
;;; is not empty, its content is taken as the markup to show at the mark. It
;;; can be translated with the usual "translatable and "context" attributes.
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "digits" property
;;; 
;;;   "digits"                   gint                  : Read / Write
;;; 
;;; The number of decimal places that are displayed in the value.
;;; 
;;; Allowed values: [G_MAXULONG,64]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw-value" property
;;; 
;;;   "draw-value"               gboolean              : Read / Write
;;; 
;;; Whether the current value is displayed as a string next to the slider.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "value-pos" property
;;; 
;;;   "value-pos"                GtkPositionType       : Read / Write
;;; 
;;; The position in which the current value is displayed.
;;; 
;;; Default value: GTK_POS_TOP
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "slider-length" style property
;;; 
;;;   "slider-length"            gint                  : Read
;;; 
;;; Length of scale's slider.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 31
;;;
;;; ----------------------------------------------------------------------------
;;; The "value-spacing" style property
;;; 
;;;   "value-spacing"            gint                  : Read
;;; 
;;; Space between value text and the slider/trough area.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "format-value" signal
;;; 
;;; gchar* user_function (GtkScale *scale,
;;;                       gdouble   value,
;;;                       gpointer  user_data)      : Run Last
;;; 
;;; Signal which allows you to change how the scale value is displayed. Connect
;;; a signal handler which returns an allocated string representing value. That
;;; string will then be used to display the scale's value.
;;; 
;;; Here's an example signal handler which displays a value 1.0 as
;;; with "-->1.0<--".
;;; 
;;; static gchar*
;;; format_value_callback (GtkScale *scale,
;;;                        gdouble   value)
;;; {
;;;   return g_strdup_printf ("-->%0.*g<--",
;;;                           gtk_scale_get_digits (scale), value);
;;;  }
;;; 
;;; scale :
;;;     the object which received the signal
;;; 
;;; value :
;;;     the value to format
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; 
;;; Returns :
;;;     allocated string representing value
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScale
;;; 
;;; struct GtkScale;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScale" gtk-scale
  (:superclass gtk-range
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_scale_get_type")
  ((digits
    gtk-scale-digits
    "digits" "gint" t t)
   (draw-value
    gtk-scale-draw-value
    "draw-value" "gboolean" t t)
   (value-pos
    gtk-scale-value-pos
    "value-pos" "GtkPositionType" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new ()
;;; 
;;; GtkWidget * gtk_scale_new (GtkOrientation orientation,
;;;                            GtkAdjustment *adjustment);
;;; 
;;; Creates a new GtkScale.
;;; 
;;; orientation :
;;;     the scale's orientation
;;; 
;;; adjustment :
;;;     the GtkAdjustment which sets the range of the scale, or NULL to create
;;;     a new adjustment
;;; 
;;; Returns :
;;;     a new GtkScale
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-new))

(defun gtk-scale-new (orientation adjustment)
  (make-instance 'gtk-scale
                 :orientation orientation
                 :adjustment adjustment))

(export 'gtk-scale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new_with_range ()
;;; 
;;; GtkWidget * gtk_scale_new_with_range (GtkOrientation orientation,
;;;                                       gdouble min,
;;;                                       gdouble max,
;;;                                       gdouble step);
;;; 
;;; Creates a new scale widget with the given orientation that lets the user
;;; input a number between min and max (including min and max) with the
;;; increment step. step must be nonzero; it's the distance the slider moves
;;; when using the arrow keys to adjust the scale value.
;;; 
;;; Note that the way in which the precision is derived works best if step is
;;; a power of ten. If the resulting precision is not suitable for your needs,
;;; use gtk_scale_set_digits() to correct it.
;;; 
;;; orientation :
;;;     the scale's orientation
;;; 
;;; min :
;;;     minimum value
;;; 
;;; max :
;;;     maximum value
;;; 
;;; step :
;;;     step increment (tick size) used with keyboard shortcuts
;;; 
;;; Returns :
;;;     a new GtkScale
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-new-with-range))

(defun gtk-scale-new-with-range (orientation min max step)
  (make-instance 'gtk-scale
                 :orientation orientation
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-scale-new-with-range)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_digits ()
;;; 
;;; void gtk_scale_set_digits (GtkScale *scale, gint digits);
;;; 
;;; Sets the number of decimal places that are displayed in the value. Also
;;; causes the value of the adjustment to be rounded off to this number of
;;; digits, so the retrieved value matches the value the user saw.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; digits :
;;;     the number of decimal places to display, e.g. use 1 to display
;;;     1.0, 2 to display 1.00, etc
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-digits))

(defun gtk-scale-set-digits (scale digits)
  (setf (gtk-scale-digits scale) digits))

(export 'gtk-scale-set-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_draw_value ()
;;; 
;;; void gtk_scale_set_draw_value (GtkScale *scale, gboolean draw_value);
;;; 
;;; Specifies whether the current value is displayed as a string next to the
;;; slider.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; draw_value :
;;;     TRUE to draw the value
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-draw-value))

(defun gtk-scale-set-draw-value (scale draw-value)
  (setf (gtk-scale-draw-value scale) draw-value))

(export 'gtk-scale-set-draw-value)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_value_pos ()
;;; 
;;; void gtk_scale_set_value_pos (GtkScale *scale, GtkPositionType pos);
;;; 
;;; Sets the position in which the current value is displayed.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; pos :
;;;     the position in which the current value is displayed
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-value-pos))

(defun gtk-scale-set-value-pos (scale pos)
  (setf (gtk-scale-value-pos scale) pos))

(export 'gtk-scale-set-value-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_digits ()
;;; 
;;; gint gtk_scale_get_digits (GtkScale *scale);
;;; 
;;; Gets the number of decimal places that are displayed in the value.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; Returns :
;;;     the number of decimal places that are displayed
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-get-digits))

(defun gtk-scale-get-digits (scale)
  (gtk-scale-digits scale))
  
(export 'gtk-scale-get-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_draw_value ()
;;; 
;;; gboolean gtk_scale_get_draw_value (GtkScale *scale);
;;; 
;;; Returns whether the current value is displayed as a string next to the
;;; slider.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; Returns :
;;;     whether the current value is displayed as a string
;;; ----------------------------------------------------------------------------

(declaim (inline gkt-scale-get-draw-value))

(defun gtk-scale-get-draw-value (scale)
  (gtk-scale-draw-value scale))

(export 'gtk-scale-get-draw-value)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_value_pos ()
;;; 
;;; GtkPositionType gtk_scale_get_value_pos (GtkScale *scale);
;;; 
;;; Gets the position in which the current value is displayed.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; Returns :
;;;     the position in which the current value is displayed
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-get-value-pos))

(defun gtk-scale-get-value-pos (scale)
  (gtk-scale-value-pos scale))

(export 'gtk-scale-get-value-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout ()
;;; 
;;; PangoLayout * gtk_scale_get_layout (GtkScale *scale);
;;; 
;;; Gets the PangoLayout used to display the scale. The returned object is
;;; owned by the scale so does not need to be freed by the caller.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; Returns :
;;;     the PangoLayout for this scale, or NULL if the "draw-value" property
;;;     is FALSE
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout" gtk-scale-get-layout) (g-object pango-layout)
  (scale (g-object gtk-scale)))

(export 'gtk-scale-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout_offsets ()
;;; 
;;; void gtk_scale_get_layout_offsets (GtkScale *scale, gint *x, gint *y);
;;; 
;;; Obtains the coordinates where the scale will draw the PangoLayout
;;; representing the text in the scale. Remember when using the PangoLayout
;;; function you need to convert to and from pixels using PANGO_PIXELS() or
;;; PANGO_SCALE.
;;; 
;;; If the "draw-value" property is FALSE, the return values are undefined.
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; x :
;;;     location to store X offset of layout, or NULL
;;; 
;;; y :
;;;     location to store Y offset of layout, or NULL
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout_offsets" %gtk-scale-get-layout-offsets) :void
  (scale (g-object gtk-scale))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-scale-get-layout-offsets (scale)
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-scale-get-layout-offsets scale x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-scale-get-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_add_mark ()
;;; 
;;; void gtk_scale_add_mark (GtkScale *scale,
;;;                          gdouble value,
;;;                          GtkPositionType position,
;;;                          const gchar *markup);
;;; 
;;; Adds a mark at value.
;;; 
;;; A mark is indicated visually by drawing a tick mark next to the scale, and
;;; GTK+ makes it easy for the user to position the scale exactly at the marks
;;; value.
;;; 
;;; If markup is not NULL, text is shown next to the tick mark.
;;; 
;;; To remove marks from a scale, use gtk_scale_clear_marks().
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; value :
;;;     The value at which the mark is placed, must be between the lower and
;;;     upper limits of the scales' adjustment.
;;; 
;;; position :
;;;     Where to draw the mark. For a horizontal scale, GTK_POS_TOP and
;;;     GTK_POS_LEFT are drawn above the scale, anything else below. For a
;;;     vertical scale, GTK_POS_LEFT and GTK_POS_TOP are drawn to the left of
;;;     the scale, anything else to the right.
;;; 
;;; markup :
;;;     Text to be shown at the mark, using Pango markup, or NULL.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_add_mark" gtk-scale-add-mark) :void
  (scale (g-object gtk-scale))
  (value :double)
  (pos gtk-position-type)
  (markup :string))

(export 'gtk-scale-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_clear_marks ()
;;; 
;;; void gtk_scale_clear_marks (GtkScale *scale);
;;; 
;;; Removes any marks that have been added with gtk_scale_add_mark().
;;; 
;;; scale :
;;;     a GtkScale
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_clear_marks" gtk-scale-clear-marks) :void
  (scale (g-object gtk-scale)))

(export 'gtk-scale-clear-marks)

;;; ----------------------------------------------------------------------------
;;;
;;; GtkHScale
;;; 
;;; A horizontal slider widget for selecting a value from a range
;;; 	
;;; Synopsis
;;; 
;;;     GtkHScale
;;;
;;;     gtk_hscale_new
;;;     gtk_hscale_new_with_range
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                            +----GtkHScale
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Description
;;; 
;;; The GtkHScale widget is used to allow the user to select a value using a
;;; horizontal slider. To create one, use gtk_hscale_new_with_range().
;;; 
;;; The position to show the current value, and the number of decimal places
;;; shown can be set using the parent GtkScale class's functions.
;;; 
;;; GtkHScale has been deprecated, use GtkScale instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHScale
;;; 
;;; struct GtkHScale;
;;; 
;;; Warning
;;; 
;;; GtkHScale is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHScale" 'gtk-hscale)
  (setf *lisp-name-exceptions*
        (append '(("GtkHScale" GTK-HSCALE)) *lisp-name-exceptions*)))

(define-g-object-class "GtkHScale" gtk-hscale
  (:superclass gtk-scale
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hscale_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new ()
;;; 
;;; GtkWidget * gtk_hscale_new (GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_hscale_new has been deprecated since version 3.2 and should not be used
;;; in newly-written code. Use gtk_scale_new() with GTK_ORIENTATION_HORIZONTAL
;;; instead
;;; 
;;; Creates a new GtkHScale.
;;; 
;;; adjustment :
;;; 	the GtkAdjustment which sets the range of the scale
;;; 
;;; Returns :
;;; 	a new GtkHScale
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscale-new))

(defun gtk-hscale-new (adjustment)
  (make-instance 'gtk-scale
                 :orientation :horizontal
                 :adjustment adjustment))

(export 'gtk-hscale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new_with_range ()
;;; 
;;; GtkWidget * gtk_hscale_new_with_range (gdouble min,
;;;                                        gdouble max,
;;;                                        gdouble step);
;;; 
;;; Warning
;;; 
;;; gtk_hscale_new_with_range has been deprecated since version 3.2 and should
;;; not be used in newly-written code. Use gtk_scale_new_with_range() with
;;; GTK_ORIENTATION_HORIZONTAL instead
;;; 
;;; Creates a new horizontal scale widget that lets the user input a number
;;; between min and max (including min and max) with the increment step. step
;;; must be nonzero; it's the distance the slider moves when using the arrow
;;; keys to adjust the scale value.
;;; 
;;; Note that the way in which the precision is derived works best if step is a
;;; power of ten. If the resulting precision is not suitable for your needs,
;;; use gtk_scale_set_digits() to correct it.
;;; 
;;; min :
;;; 	minimum value
;;; 
;;; max :
;;; 	maximum value
;;; 
;;; step :
;;; 	step increment (tick size) used with keyboard shortcuts
;;; 
;;; Returns :
;;; 	a new GtkHScale
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscale-new-with-range))

(defun gtk-hscale-new-with-range (min max step)
  (make-instance 'gtk-scale
                 :orientation :horizontal
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-hscale-new-with-range)

;;; ----------------------------------------------------------------------------
;;;
;;; GtkVScale
;;; 
;;; A vertical slider widget for selecting a value from a range
;;;     
;;; Synopsis
;;; 
;;;     GtkVScale
;;;
;;;     gtk_vscale_new
;;;     gtk_vscale_new_with_range
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                            +----GtkVScale
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Description
;;; 
;;; The GtkVScale widget is used to allow the user to select a value using a
;;; vertical slider. To create one, use gtk_hscale_new_with_range().
;;; 
;;; The position to show the current value, and the number of decimal places
;;; shown can be set using the parent GtkScale class's functions.
;;; 
;;; GtkVScale has been deprecated, use GtkScale instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVScale
;;; 
;;; struct GtkVScale;
;;; 
;;; Warning
;;; 
;;; GtkVScale is deprecated and should not be used in newly-written code.
;;; 
;;; The GtkVScale struct contains private data only, and should be accessed
;;; using the functions below.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVScale" 'gtk-hscale)
  (setf *lisp-name-exceptions*
        (append '(("GtkVScale" GTK-VSCALE)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVScale" gtk-vscale
  (:superclass gtk-scale
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vscale_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_vscale_new ()
;;; 
;;; GtkWidget * gtk_vscale_new (GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_vscale_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_scale_new() with
;;; GTK_ORIENTATION_VERTICAL instead
;;; 
;;; Creates a new GtkVScale.
;;; 
;;; adjustment :
;;;     the GtkAdjustment which sets the range of the scale
;;; 
;;; Returns :
;;;     a new GtkVScale
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscale-new))

(defun gtk-vscale-new (adjustment)
  (make-instance 'gtk-scale
                 :orientation :vertical
                 :adjustment adjustment))

(export 'gtk-vscale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_vscale_new_with_range ()
;;; 
;;; GtkWidget * gtk_vscale_new_with_range (gdouble min,
;;;                                        gdouble max,
;;;                                        gdouble step);
;;; 
;;; Warning
;;; 
;;; gtk_vscale_new_with_range has been deprecated since version 3.2 and should
;;; not be used in newly-written code. Use gtk_scale_new_with_range() with
;;; GTK_ORIENTATION_VERTICAL instead
;;; 
;;; Creates a new vertical scale widget that lets the user input a number
;;; between min and max (including min and max) with the increment step. step
;;; must be nonzero; it's the distance the slider moves when using the arrow
;;; keys to adjust the scale value.
;;; 
;;; Note that the way in which the precision is derived works best if step is
;;; a power of ten. If the resulting precision is not suitable for your needs,
;;; use gtk_scale_set_digits() to correct it.
;;; 
;;; min :
;;;     minimum value
;;; 
;;; max :
;;;     maximum value
;;; 
;;; step :
;;;     step increment (tick size) used with keyboard shortcuts
;;; 
;;; Returns :
;;;     a new GtkVScale
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscale-new-with-range))

(defun gtk-vscale-new-with-range (min max step)
  (make-instance 'gtk-scale
                 :orientation :vertical
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-vscale-new-with-range)

;;; --- End of file gtk.scale.lisp ---------------------------------------------
