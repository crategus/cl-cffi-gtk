;;; ----------------------------------------------------------------------------
;;; gtk.hsv.lisp
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
;;; GtkHSV
;;; 
;;; A 'color wheel' widget
;;; 
;;; Synopsis
;;; 
;;;     GtkHSV
;;;
;;;     gtk_hsv_new
;;;     gtk_hsv_set_color
;;;     gtk_hsv_get_color
;;;     gtk_hsv_set_metrics
;;;     gtk_hsv_get_metrics
;;;     gtk_hsv_is_adjusting
;;;     gtk_hsv_to_rgb
;;;     gtk_rgb_to_hsv
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkHSV
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHSV implements AtkImplementorIface and GtkBuildable.
;;;
;;; Signals
;;; 
;;;   "changed"                                        : Run First
;;;   "move"                                           : Action
;;; 
;;; Description
;;; 
;;; GtkHSV is the 'color wheel' part of a complete color selector widget. It
;;; allows to select a color by determining its HSV components in an intuitive
;;; way. Moving the selection around the outer ring changes the hue, and moving
;;; the selection point inside the inner triangle changes value and saturation.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;; 
;;; void user_function (GtkHSV  *hsv, gpointer user_data)      : Run First
;;;
;; ----------------------------------------------------------------------------- 
;;; The "move" signal
;;; 
;;; void user_function (GtkHSV *hsv,
;;;                     GtkDirectionType arg1,
;;;                     gpointer user_data)                    : Action
;;; 
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkHSV
;;; 
;;; struct GtkHSV;
;;; 
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkHSV" gtk-hsv
  (:superclass gtk-widget 
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_hsv_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_new ()
;;; 
;;; GtkWidget * gtk_hsv_new (void)
;;; 
;;; Creates a new HSV color selector.
;;; 
;;; Returns :
;;;     A newly-created HSV color selector.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_set_color ()
;;; 
;;; void gtk_hsv_set_color (GtkHSV *hsv, double h, double s, double v)
;;; 
;;; Sets the current color in an HSV color selector. Color component values
;;; must be in the [0.0, 1.0] range.
;;; 
;;; hsv :
;;;     An HSV color selector
;;; 
;;; h :
;;;     Hue
;;; 
;;; s :
;;;     Saturation
;;; 
;;; v :
;;;     Value
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_set_color" gtk-hsv-set-color) :void
  (hsv (g-object gtk-hsv))
  (h :double)
  (s :double)
  (v :double))

(export 'gtk-hsv-set-color)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_get_color ()
;;; 
;;; void gtk_hsv_get_color (GtkHSV *hsv, gdouble *h, gdouble *s, gdouble *v)
;;; 
;;; Queries the current color in an HSV color selector. Returned values will be
;;; in the [0.0, 1.0] range.
;;; 
;;; hsv :
;;;     An HSV color selector
;;; 
;;; h :
;;;     Return value for the hue. [out]
;;; 
;;; s :
;;;     Return value for the saturation. [out]
;;; 
;;; v :
;;;     Return value for the value. [out]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_get_color" %gtk-hsv-get-color) :void
  (hsv (g-object gtk-hsv))
  (h (:pointer :double))
  (s (:pointer :double))
  (v (:pointer :double)))

(defun gtk-hsv-get-color (hsv)
  (with-foreign-objects ((h :double) (s :double) (v :double))
    (%gtk-hsv-get-color hsv h s v)
    (values (mem-ref h :double) (mem-ref s :double) (mem-ref v :double))))

(export 'gtk-hsv-get-color)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_set_metrics ()
;;; 
;;; void gtk_hsv_set_metrics (GtkHSV *hsv, gint size, gint ring_width)
;;; 
;;; Sets the size and ring width of an HSV color selector.
;;; 
;;; hsv :
;;;     An HSV color selector
;;; 
;;; size :
;;;     Diameter for the hue ring
;;; 
;;; ring_width :
;;;     Width of the hue ring
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_set_metrics" gtk-hsv-set-metrics) :void
  (hsv (g-object gtk-hsv))
  (size :int)
  (ring-width :int))

(export 'gtk-hsv-set-metrics)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_get_metrics ()
;;; 
;;; void gtk_hsv_get_metrics (GtkHSV *hsv, gint *size, gint *ring_width)
;;; 
;;; Queries the size and ring width of an HSV color selector.
;;; 
;;; hsv :
;;;     An HSV color selector
;;; 
;;; size :
;;;     Return value for the diameter of the hue ring. [out]
;;; 
;;; ring_width :
;;;     Return value for the width of the hue ring. [out]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_get_metrics" %gtk-hsv-get-metrics) :void
  (hsv (g-object gtk-hsv))
  (size (:pointer :int))
  (ring-width (:pointer :int)))

(defun gtk-hsv-get-metrics (hsv)
  (with-foreign-objects ((size :int) (ring-width :int))
    (%gtk-hsv-get-metrics hsv size ring-width)
    (values (mem-ref size :int) (mem-ref ring-width :int))))

(export 'gtk-hsv-get-metrics)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_is_adjusting ()
;;; 
;;; gboolean gtk_hsv_is_adjusting (GtkHSV *hsv)
;;; 
;;; An HSV color selector can be said to be adjusting if multiple rapid changes
;;; are being made to its value, for example, when the user is adjusting the
;;; value with the mouse. This function queries whether the HSV color selector
;;; is being adjusted or not.
;;; 
;;; hsv :
;;;     A GtkHSV
;;; 
;;; Returns :
;;;     TRUE if clients can ignore changes to the color value, since they may
;;;     be transitory, or FALSE if they should consider the color value status
;;;     to be final.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_is_adjusting" gtk-hsv-is-adjusting) :boolean
  (hsv (g-object gtk-hsv)))

(export 'gtk-hsv-is-adjusting)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_to_rgb ()
;;; 
;;; void gtk_hsv_to_rgb (gdouble h,  gdouble s,  gdouble v,
;;;                      gdouble *r, gdouble *g, gdouble *b)
;;; 
;;; Converts a color from HSV space to RGB. Input values must be in the
;;; [0.0, 1.0] range; output values will be in the same range.
;;; 
;;; h :
;;;     Hue
;;; 
;;; s :
;;;     Saturation
;;; 
;;; v :
;;;     Value
;;; 
;;; r :
;;;     Return value for the red component. [out]
;;; 
;;; g :
;;;     Return value for the green component. [out]
;;; 
;;; b :
;;;     Return value for the blue component. [out]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_to_rgb" %gtk-hsv-to-rgb) :void
  (h :double)
  (s :double)
  (v :double)
  (r (:pointer :double))
  (g (:pointer :double))
  (b (:pointer :double)))

(defun gtk-hsv-to-rgb (h s v)
  (with-foreign-objects ((r :double) (g :double) (b :double))
    (%gtk-hsv-to-rgb h s v r g b)
    (values (mem-ref r :double) (mem-ref g :double) (mem-ref b :double))))

(export 'gtk-hsv-to-rgb)

;;; ----------------------------------------------------------------------------
;;; gtk_rgb_to_hsv ()
;;; 
;;; void gtk_rgb_to_hsv (gdouble r,  gdouble g,  gdouble b,
;;;                      gdouble *h, gdouble *s, gdouble *v)
;;; 
;;; Converts a color from RGB space to HSV. Input values must be in the
;;; [0.0, 1.0] range; output values will be in the same range.
;;; 
;;; r :
;;;     Red
;;; 
;;; g :
;;;     Green
;;; 
;;; b :
;;;     Blue
;;; 
;;; h :
;;;     Return value for the hue component.
;;; 
;;; s :
;;;     Return value for the saturation component.
;;; 
;;; v :
;;;     Return value for the value component.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_rgb_to_hsv" %gtk-rgb-to-hsv) :void
  (r :double)
  (g :double)
  (b :double)
  (h (:pointer :double))
  (s (:pointer :double))
  (v (:pointer :double)))

(defun gtk-rgb-to-hsv (r g b)
  (with-foreign-objects ((h :double) (s :double) (v :double))
    (%gtk-rgb-to-hsv r g b h s v)
    (values (mem-ref h :double) (mem-ref s :double) (mem-ref v :double))))

(export 'gtk-rgb-to-hsv)

;;; --- End of file gtk.hsv.lisp -----------------------------------------------
