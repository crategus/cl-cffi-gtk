;;; ----------------------------------------------------------------------------
;;; gtk.hsv.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Signals
;;;
;;;   "changed"                                        : Run First
;;;   "move"                                           : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkHSV
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkHSV" gtk-hsv
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_hsv_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hsv 'type)
 "@version{2013-6-3}
  @begin{short}
    @sym{gtk-hsv} is the \"color wheel\" part of a complete color selector
    widget. It allows to select a color by determining its HSV components in an
    intuitive way. Moving the selection around the outer ring changes the hue,
    and moving the selection point inside the inner triangle changes value and
    saturation.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (hsv)   : Run First
      @end{pre}

    @subheading{The \"move\" signal}
      @begin{pre}
 lambda (hsv arg)   : Action
      @end{pre}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hsv-new))

(defun gtk-hsv-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-27}
  @return{A newly-created HSV color selector.}
  @begin{short}
    Creates a new HSV color selector.
  @end{short}

  Since 2.14
  @see-class{gtk-hsv}"
  (make-instance 'gtk-hsv))

(export 'gtk-hsv-new)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_set_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_set_color" gtk-hsv-set-color) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[hsv]{an HSV color selector}
  @argument[h]{hue}
  @argument[s]{saturation}
  @argument[v]{value}
  @begin{short}
    Sets the current color in an HSV color selector. Color component values
    must be in the [0.0, 1.0] range.
  @end{short}

  Since 2.14"
  (hsv (g-object gtk-hsv))
  (h :double)
  (s :double)
  (v :double))

(export 'gtk-hsv-set-color)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_get_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_get_color" %gtk-hsv-get-color) :void
  (hsv (g-object gtk-hsv))
  (h (:pointer :double))
  (s (:pointer :double))
  (v (:pointer :double)))

(defun gtk-hsv-get-color (hsv)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[hsv]{an HSV color selector}
  @begin{return}
    @code{h} -- the hue @br{}
    @code{s} -- the saturation @br{}
    @code{v} -- the value
  @end{return}
  @begin{short}
    Queries the current color in an HSV color selector. Returned values will be
    in the [0.0, 1.0] range.
  @end{short}

  Since 2.14"
  (with-foreign-objects ((h :double) (s :double) (v :double))
    (%gtk-hsv-get-color hsv h s v)
    (values (mem-ref h :double) (mem-ref s :double) (mem-ref v :double))))

(export 'gtk-hsv-get-color)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_set_metrics ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_set_metrics" gtk-hsv-set-metrics) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[hsv]{an HSV color selector}
  @argument[size]{diameter for the hue ring}
  @argument[ring-width]{width of the hue ring}
  @short{Sets the size and ring width of an HSV color selector.}

  Since 2.14"
  (hsv (g-object gtk-hsv))
  (size :int)
  (ring-width :int))

(export 'gtk-hsv-set-metrics)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_get_metrics ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_get_metrics" %gtk-hsv-get-metrics) :void
  (hsv (g-object gtk-hsv))
  (size (:pointer :int))
  (ring-width (:pointer :int)))

(defun gtk-hsv-get-metrics (hsv)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[hsv]{an HSV color selector}
  @begin{return}
    @code{size} -- the diameter of the hue ring @br{}
    @code{ring-width} -- the width of the hue ring
  @end{return}
  @short{Queries the size and ring width of an HSV color selector.}

  Since 2.14"
  (with-foreign-objects ((size :int) (ring-width :int))
    (%gtk-hsv-get-metrics hsv size ring-width)
    (values (mem-ref size :int) (mem-ref ring-width :int))))

(export 'gtk-hsv-get-metrics)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_is_adjusting ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_is_adjusting" gtk-hsv-is-adjusting) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[hsv]{a @class{gtk-hsv} object}
  @begin{return}
    @em{True} if clients can ignore changes to the color value, since they may
    be transitory, or @code{nil} if they should consider the color value status
    to be final.
  @end{return}
  @begin{short}
    An HSV color selector can be said to be adjusting if multiple rapid changes
    are being made to its value, for example, when the user is adjusting the
    value with the mouse. This function queries whether the HSV color selector
    is being adjusted or not.
  @end{short}

  Since 2.14"
  (hsv (g-object gtk-hsv)))

(export 'gtk-hsv-is-adjusting)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_to_rgb ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_hsv_to_rgb" %gtk-hsv-to-rgb) :void
  (h :double)
  (s :double)
  (v :double)
  (r (:pointer :double))
  (g (:pointer :double))
  (b (:pointer :double)))

(defun gtk-hsv-to-rgb (h s v)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[h]{hue}
  @argument[s]{saturation}
  @argument[v]{value}
  @begin{return}
    @code{r} -- the red component @br{}
    @code{g} -- the green component @br{}
    @code{b} -- the blue component
  @end{return}
  @begin{short}
    Converts a color from HSV space to RGB. Input values must be in the
    [0.0, 1.0] range; output values will be in the same range.
  @end{short}

  Since 2.14"
  (with-foreign-objects ((r :double) (g :double) (b :double))
    (%gtk-hsv-to-rgb h s v r g b)
    (values (mem-ref r :double) (mem-ref g :double) (mem-ref b :double))))

(export 'gtk-hsv-to-rgb)

;;; ----------------------------------------------------------------------------
;;; gtk_rgb_to_hsv ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_rgb_to_hsv" %gtk-rgb-to-hsv) :void
  (r :double)
  (g :double)
  (b :double)
  (h (:pointer :double))
  (s (:pointer :double))
  (v (:pointer :double)))

(defun gtk-rgb-to-hsv (r g b)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[r]{red}
  @argument[g]{green}
  @argument[b]{blue}
  @begin{return}
    @code{h} -- the hue component @br{}
    @code{s} -- the saturation component @br{}
    @code{v} -- the value component
  @end{return}
  @begin{short}
    Converts a color from RGB space to HSV. Input values must be in the
    [0.0, 1.0] range; output values will be in the same range.
  @end{short}

  Since 2.14"
  (with-foreign-objects ((h :double) (s :double) (v :double))
    (%gtk-rgb-to-hsv r g b h s v)
    (values (mem-ref h :double) (mem-ref s :double) (mem-ref v :double))))

(export 'gtk-rgb-to-hsv)

;;; --- End of file gtk.hsv.lisp -----------------------------------------------
