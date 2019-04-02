;;; ----------------------------------------------------------------------------
;;; gtk.scale-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkScaleButton
;;;
;;;     A button which pops up a scale
;;;
;;; Types and Values
;;;
;;;     GtkScaleButton
;;;
;;; Functions
;;;
;;;     gtk_scale_button_new
;;;     gtk_scale_button_set_adjustment                    Accessor
;;;     gtk_scale_button_set_icons                         Accessor
;;;     gtk_scale_button_set_value                         Accessor
;;;     gtk_scale_button_get_adjustment                    Accessor
;;;     gtk_scale_button_get_value                         Accessor
;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button
;;;
;;; Properties
;;;
;;;     GtkAdjustment*  adjustment    Read / Write
;;;             GStrv   icons         Read / Write
;;;       GtkIconSize   size          Read / Write
;;;           gdouble   value         Read / Write
;;;
;;; Signals
;;;
;;;     void  popdown           Action
;;;     void  popup             Action
;;;     void  value-changed     Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkScaleButton
;;;                             ╰── GtkVolumeButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScaleButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable, GtkActivatable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScaleButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScaleButton" gtk-scale-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable"
                "GtkOrientable")
   :type-initializer "gtk_scale_button_get_type")
  ((adjustment
    gtk-scale-button-adjustment
    "adjustment" "GtkAdjustment" t t)
   (icons
    gtk-scale-button-icons
    "icons" "GStrv" t t)
   (size
    gtk-scale-button-size
    "size" "GtkIconSize" t t)
   (value
    gtk-scale-button-value
    "value" "gdouble" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-scale-button 'type)
 "@version{2013-4-12}
  @begin{short}
    @sym{gtk-scale-button} provides a button which pops up a scale widget. This
    kind of widget is commonly used for volume controls in multimedia
    applications, and GTK+ provides a @class{gtk-volume-button} subclass that
    is tailored for this use case.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"popdown\" signal}
      @begin{pre}
 lambda (button)    : Action
      @end{pre}
      The \"popdown\" signal is a keybinding signal which gets emitted to
      popdown the scale widget.
      The default binding for this signal is Escape.
      @begin[code]{table}
        @entry[button]{The object which received the signal.}
      @end{table}
    @subheading{The \"popup\" signal}
      @begin{pre}
 lambda (button)    : Action
      @end{pre}
      The \"popup\" signal is a keybinding signal which gets emitted to popup
      the scale widget.
      The default bindings for this signal are Space, Enter and Return.
      @begin[code]{table}
        @entry[button]{The object which received the signal.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
 lambda (button value)    : Run Last
      @end{pre}
      The \"value-changed\" signal is emitted when the value field has changed.
      @begin[code]{table}
        @entry[button]{The object which received the signal.}
        @entry[value]{The new value.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-scale-button-adjustment}
  @see-slot{gtk-scale-button-icons}
  @see-slot{gtk-scale-button-size}
  @see-slot{gtk-scale-button-value}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-scale-button-adjustment --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "adjustment"
                                               'gtk-scale-button) 't)
 "The @code{adjustment} property of type @class{gtk-adjustment}
  (Read / Write) @br{}
  The @class{gtk-adjustment} object that contains the current value of this
  scale button object.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-button-adjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-button-adjustment 'function)
 "@version{2014-4-19}
  @argument[object]{a @class{gtk-scale-button} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @syntax[]{(gtk-scale-button-adjustment object object) => adjustment}
  @syntax[]{(setf (gtk-scale-button-adjustment object) adjustment)}
  @begin{short}
    Accessor of the slot @slot[gtk-scale-button]{adjustment} of the
    @class{gtk-scale-button} class.
  @end{short}

  The generic function @sym{gtk-scale-button-adjustment} gets the
  @class{gtk-adjustment} object associated with the @class{gtk-scale-button}'s
  scale.

  The generic function @sym{(setf gtk-scale-button-adjustment)} sets the
  @class{gtk-adjustment} object to be used as a model for the 
  @class{gtk-scale-button}'s scale.
  @see-class{gtk-scale-button}")

;;; --- gtk-scale-button-icons -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icons" 'gtk-scale-button) 't)
 "The @code{icons} property of type @type{g-strv} (Read / Write) @br{}
  The names of the icons to be used by the scale button. The first item in
  the list will be used in the button when the current value is the lowest
  value, the second item for the highest value. All the subsequent icons will
  be used for all the other values, spread evenly over the range of values.
  If there is only one icon name in the icons array, it will be used for all
  the values. If only two icon names are in the icons array, the first one
  will be used for the bottom 50% of the scale, and the second one for the
  top 50%.
  It is recommended to use at least 3 icons so that the @sym{gtk-scale-button}
  reflects the current value of the scale better for the users. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-button-icons atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-button-icons 'function)
 "@version{2014-4-19}
  @argument[object]{a @class{gtk-scale-button} widget}
  @argument[icons]{a list of icon names}
  @begin{short}
    Accessor of the slot @slot[gtk-scale-button]{icons} of the
    @class{gtk-scale-button} class.
  @end{short}

  The generic function @sym{(setf gtk-scale-button-icons} sets the icons to be
  used by the scale button. For details, see the @slot[gtk-scale-button]{icons}
  property.
  @see-class{gtk-scale-button}")

;;; --- gtk-scale-button-size --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-scale-button) 't)
 "The @code{size} property of type @symbol{gtk-icon-size}
  (Read / Write) @br{}
  The icon size. @br{}
  Default value: @code{:small-toolbar}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-button-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-button-size 'function)
 "@version{2014-4-19}
  Accessor of the slot @slot[gtk-scale-button]{size} of the
  @class{gtk-scale-button} class.
  @see-class{gtk-scale-button}")

;;; --- gtk-scale-button-value -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-scale-button) 't)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  The value of the scale. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-button-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-button-value 'function)
 "@version{2014-4-19}
  @argument[object]{a @class{gtk-scale-button} widget}
  @argument[value]{new value of the scale button}
  @syntax[]{(gtk-scale-button-value object) => value}
  @syntax[]{(setf (gtk-scale-button-value object) value)}
  @begin{short}
    Accessor of the slot @slot[gtk-scale-button]{value} of the
    @class{gtk-scale-button} class.
  @end{short}

  The generic function @sym{gtk-scale-button-value} gets the current value of
  the scale button.

  The generic function @sym{(setf gtk-scale-button-value)} sets the current
  value of the scale; if the value is outside the minimum or maximum range
  values, it will be clamped to fit inside them. The scale button emits the
  \"value-changed\" signal if the value changes.
  @see-class{gtk-scale-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-button-new))

(defun gtk-scale-button-new (size min max step icons)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[size]{a stock icon size}
  @argument[min]{the minimum value of the scale (usually 0)}
  @argument[max]{the maximum value of the scale (usually 100)}
  @argument[step]{the stepping of value when a scroll-wheel event, or up/down
    arrow event occurs (usually 2)}
  @argument[icons]{a list of icon names, or @code{nil} if you want to set the
    list later with the function @fun{gtk-scale-button-set-icons}}
  @return{A new @class{gtk-scale-button} widget.}
  @begin{short}
    Creates a @class{gtk-scale-button} widget, with a range between @arg{min}
    and @arg{max}, with a stepping of @arg{step}.
  @end{short}
  @see-class{gtk-scale-button}
  @see-function{gtk-scale-button-set-icons}"
  (make-instance 'gtk-scale-button
                 :size size
                 :icons icons
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-scale-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_popup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_button_get_popup" gtk-scale-button-get-popup)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[button]{a @class{gtk-scale-button} widget}
  @return{The popup of the @class{gtk-scale-button} widget.}
  @short{Retrieves the popup of the @class{gtk-scale-button} widget.}
  @see-class{gtk-scale-button}"
  (scale-button (g-object gtk-scale-button)))

(export 'gtk-scale-button-get-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_plus_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_button_get_plus_button" gtk-scale-button-get-plus-button)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[button]{a @class{gtk-scale-button} widget}
  @return{The plus button of the @class{gtk-scale-button} widget.}
  @short{Retrieves the plus button of the @class{gtk-scale-button} widget.}
  @see-class{gtk-scale-button}
  @see-function{gtk-sacle-button-get-minus-button}"
  (scale-button (g-object gtk-scale-button)))

(export 'gtk-scale-button-get-plus-button)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_minus_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_button_get_minus_button" gtk-scale-button-get-minus-button)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[button]{a @class{gtk-scale-button} widget}
  @return{The minus button of the @class{gtk-scale-button} widget.}
  @short{Retrieves the minus button of the @class{gtk-scale-button} widget.}
  @see-class{gtk-scale-button}
  @see-function{gtk-scale-button-get-plus-button}"
  (scale-button (g-object gtk-scale-button)))

(export 'gtk-scale-button-get-minus-button)

;;; --- End of file gtk.scale-button.lisp --------------------------------------
