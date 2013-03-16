;;; ----------------------------------------------------------------------------
;;; gtk.button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkButton
;;; 
;;; A widget that emits a signal when clicked on
;;;     
;;; Synopsis
;;; 
;;;     GtkButton
;;;
;;;     gtk_button_new
;;;     gtk_button_new_with_label
;;;     gtk_button_new_with_mnemonic
;;;     gtk_button_new_from_stock
;;;     gtk_button_pressed                * deprecated *
;;;     gtk_button_released               * deprecated *
;;;     gtk_button_clicked
;;;     gtk_button_enter                  * deprecated *
;;;     gtk_button_leave                  * deprecated *
;;;     gtk_button_set_relief
;;;     gtk_button_get_relief
;;;     gtk_button_get_label
;;;     gtk_button_set_label
;;;     gtk_button_get_use_stock
;;;     gtk_button_set_use_stock
;;;     gtk_button_get_use_underline
;;;     gtk_button_set_use_underline
;;;     gtk_button_set_focus_on_click
;;;     gtk_button_get_focus_on_click
;;;     gtk_button_set_alignment
;;;     gtk_button_get_alignment
;;;     gtk_button_set_image
;;;     gtk_button_get_image
;;;     gtk_button_set_image_position
;;;     gtk_button_get_image_position
;;;     gtk_button_get_event_window
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkButton
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkButton" 'gtk-button))

(define-g-object-class "GtkButton" gtk-button
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_button_get_type")
  ((focus-on-click
    gtk-button-focus-on-click
    "focus-on-click" "gboolean" t t)
   (image
    gtk-button-image
    "image" "GtkWidget" t t)
   (image-position
    gtk-button-image-position
    "image-position" "GtkPositionType" t t)
   (label
    gtk-button-label
    "label" "gchararray" t t)
   (relief
    gtk-button-relief
    "relief" "GtkReliefStyle" t t)
   (use-stock
    gtk-button-use-stock
    "use-stock" "gboolean" t t)
   (use-underline
    gtk-button-use-underline
    "use-underline" "gboolean" t t)
   (xalign
    gtk-button-xalign
    "xalign" "gfloat" t t)
   (yalign
    gtk-button-yalign
    "yalign" "gfloat" t t)))

;;; --- gtk-button -------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-button 'type)
 "@version{2013-1-18}
  @short{A widget that emits a signal when clicked on.}

  The @sym{gtk-button} widget is generally used to trigger a callback function
  that is called when the button is pressed. The various signals and how to use
  them are outlined below.

  The @sym{gtk-button} widget can hold any valid child widget. That is, it can
  hold almost any other standard @class{gtk-widget}. The most commonly used
  child is the @class{gtk-label}.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"child-displacement-x\" style property}
    @code{\"child-displacement-x\"} @code{gint} (Read)@br{}
    How far in the x direction to move the child when the button is
    depressed.@br{}
    Default value: @code{0}

    @subheading{The \"child-displacement-y\" style property}
    @code{\"child-displacement-y\"} @code{gint} (Read)@br{}
    How far in the y direction to move the child when the button is
    depressed.@br{}
    Default value: @code{0}

    @subheading{The \"default-border\" style property}
    @code{\"default-border\"} @class{gtk-border} (Read)@br{}
    The @code{\"default-border\"} style property defines the extra space to add
    around a button that can become the default widget of its window. For more
    information about default widgets, see @fun{gtk-widget-grab-default}.

    @subheading{The \"default-outside-border\" style property}
    @code{\"default-outside-border\"} @class{gtk-border} (Read)@br{}
    The @code{\"default-outside-border\"} style property defines the extra
    outside space to add around a button that can become the default widget of
    its window. Extra outside space is always drawn outside the button border.
    For more information about default widgets,
    see @fun{gtk-widget-grab-default}.

    @subheading{The \"displace-focus\" style property}
    @code{\"displace-focus\"} @code{gboolean} (Read)@br{}
    Whether the @code{child_displacement_x/child_displacement_y} properties
    should also affect the focus rectangle.@br{}
    Default value: @code{nil}@br{}
    Since 2.6

    @subheading{The \"image-spacing\" style property}
    @code{\"image-spacing\"} @code{gint} (Read)@br{}
    Spacing in pixels between the image and label.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{2}

    @subheading{The \"inner-border\" style property}
    @code{\"inner-border\"} @class{gtk-border} (Read)@br{}
    @b{Warning:}
    @code{GtkButton:inner-border} has been deprecated since version 3.4 and
    should not be used in newly-written code. Use the standard border and
    padding CSS properties; the value of this style property is ignored.@br{}
    Sets the border between the button edges and child.@br{}
    Since 2.10
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @b{The \"activate\" signal}
    @begin{pre}
 void user_function (GtkButton *widget, gpointer user_data)      : Action
    @end{pre}
    The @code{::activate} signal on @sym{gtk-button} is an action signal and
    emitting it causes the button to animate press then release. Applications
    should never connect to this signal, but use the \"clicked\" signal.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[user-data]{user data set when the signal handler was connected.}
    @end{table}
    @b{The \"clicked\" signal}
    @begin{pre}
 void user_function (GtkButton *button, gpointer user_data)      : Action
    @end{pre}
    Emitted when the button has been activated (pressed and released).
    @begin[code]{table}
      @entry[button]{the object that received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    @b{The \"enter\" signal}
    @begin{pre}
 void user_function (GtkButton *button, gpointer user_data)      : Run First
    @end{pre}
    @b{Warning:}
    @code{GtkButton::enter} has been deprecated since version 2.8 and should not
    be used in newly-written code. Use the \"enter-notify-event\" signal.@br{}
    Emitted when the pointer enters the button.@br{}
    @begin[code]{table}
      @entry[button]{the object that received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    @b{The \"leave\" signal}
    @begin{pre}
 void user_function (GtkButton *button, gpointer user_data)      : Run First
    @end{pre}
    @b{Warning:}
    @code{GtkButton::leave} has been deprecated since version 2.8 and should not
    be used in newly-written code. Use the \"leave-notify-event\" signal.@br{}
    Emitted when the pointer leaves the button.
    @begin[code]{table}
      @entry[button]{the object that received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    @b{The \"pressed\" signal}
    @begin{pre}
 void user_function (GtkButton *button, gpointer user_data)      : Run First
    @end{pre} 
    @b{Warning:}
    @code{GtkButton::pressed} has been deprecated since version 2.8 and should
    not be used in newly-written code. Use the \"button-press-event\"
    signal.@br{}
    Emitted when the button is pressed.
    @begin[code]{table}
      @entry[button]{the object that received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    @b{The \"released\" signal}
    @begin{pre}
 void user_function (GtkButton *button, gpointer user_data)      : Run First
    @end{pre} 
    @b{Warning:}
    @code{GtkButton::released} has been deprecated since version 2.8 and should
    not be used in newly-written code. Use the \"button-release-event\"
    signal.@br{}
    Emitted when the button is released.
    @begin[code]{table}
      @entry[button]{the object that received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-button-focus-on-click}
  @see-slot{gtk-button-image}
  @see-slot{gtk-button-image-position}
  @see-slot{gtk-button-label}
  @see-slot{gtk-button-relief}
  @see-slot{gtk-button-use-stock}
  @see-slot{gtk-button-use-underline}
  @see-slot{gtk-button-xalign}
  @see-slot{gtk-button-yalign}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-click" 'gtk-button) 't)
 "The @arg{\"focus-on-click\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Whether the button grabs focus when it is clicked with the mouse.@br{}
  Default value: @arg{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image" 'gtk-button) 't)
 "The @arg{\"image\"} property of type @class{gtk-widget} (Read / Write)@br{}
  The child widget to appear next to the button text.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image-position" 'gtk-button) 't)
 "The @arg{\"image-position\"} property of type @symbol{gtk-position-type}
  (Read / Write)@br{}
  The position of the image relative to the text inside the button.@br{}
  Default value: @code{:left}@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-button) 't)
 "The @arg{\"label\"} property of type @code{gchar*}
  (Read / Write / Construct)@br{}
  Text of the label widget inside the button, if the button contains a
  label widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "relief" 'gtk-button) 't)
 "The @arg{\"relief\"} property of type @symbol{gtk-relief-style}
  (Read / Write)@br{}
  The border relief style.@br{}
  Default value: @code{:normal}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-stock" 'gtk-button) 't)
 "The @arg{\"use-stock\"} property of type @code{gboolean}
  (Read / Write / Construct)@br{}
  If set, the label is used to pick a stock item instead of being
  displayed.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline" 'gtk-button) 't)
 "The @arg{\"use-underline\"} property of type @code{gboolean}
  (Read / Write / Construct)@br{}
  @begin{short}
    If set, an underline in the text indicates the next character should be used
    for the mnemonic accelerator key.
  @end{short}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-button) 't)
 "The @arg{\"xalign\"} property of type @code{gfloat} (Read / Write)@br{}
  If the child of the button is a @class{gtk-misc} or @class{gtk-alignment},
  this property can be used to control its horizontal alignment. @code{0.0} is
  left aligned, @code{1.0} is right aligned.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0.5}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-button) 't)
 "The @arg{\"yalign\"} property of type @code{gfloat} (Read / Write)@br{}
  If the child of the button is a @class{gtk-misc} or @class{gtk-alignment},
  this property can be used to control its vertical alignment. @code{0.0} is top
  aligned, @code{1.0} is bottom aligned.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0.5}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-button-focus-on-click ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-focus-on-click atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-focus-on-click 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"focus-on-click\"} of the @class{gtk-button}
    class.
  @end{short}
  @see-function{gtk-button-set-focus-on-click}
  @see-function{gtk-button-get-focus-on-click}")

;;; --- gtk-button-image -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-image atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-image 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"image\"} of the @class{gtk-button} class.
  @end{short}
  @see-function{gtk-button-set-image}
  @see-function{gtk-button-get-image}")

;;; --- gtk-button-image-position ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-image-position atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-image-position 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"image-position\"} of the @class{gtk-button}
    class.
  @end{short}
  @see-function{gtk-button-set-image-position}
  @see-function{gtk-button-get-image-position}")

;;; --- gtk-button-label -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-label atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-label 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"label\"} of the @class{gtk-button} class.
  @end{short}
  @see-function{gtk-button-set-label}
  @see-function{gtk-button-get-label}")

;;; --- gtk-button-relief ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-relief atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-relief 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"relief\"} of the @class{gtk-button} class.
  @end{short}
  @see-function{gtk-button-set-relief}
  @see-function{gtk-button-get-relief}")

;;; --- gtk-button-use-stock ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-use-stock atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-use-stock 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"use-stock\"} of the @class{gtk-button} class.
  @end{short}
  @see-function{gtk-button-set-use-stock}
  @see-function{gtk-button-get-use-stock}")

;;; --- gtk-button-use-underline -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-use-underline atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-use-underline 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @arg{\"use-underline\"} of the @class{gtk-button}
    class.
  @end{short}
  @see-function{gtk-button-set-use-underline}
  @see-function{gtk-button-get-use-underline}")

;;; --- gtk-button-xalign ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-xalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-xalign 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @code{\"xalign\"} of the @class{gtk-button}
    class.
  @end{short}
  @see-function{gtk-button-set-alignment}
  @see-function{gtk-button-get-alignment}")

;;; --- gtk-button-yalign ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-button-yalign atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-button-yalign 'function)
 "@version{2013-1-18}
  @begin{short}
    Accessor of the slot @code{\"yalign\"} of the @class{gtk-button}
    class.
  @end{short}
  @see-function{gtk-button-set-alignment}
  @see-function{gtk-button-get-alignment}")

;;; ----------------------------------------------------------------------------
;;; gtk_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-new))

(defun gtk-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @return{The newly created @class{gtk-button} widget.}
  @begin{short}
    Creates a new @class{gtk-button} widget.
  @end{short}
  To add a child widget to the button, use @fun{gtk-container-add}."
  (make-instance 'gtk-button))

(export 'gtk-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-new-with-label))

(defun gtk-button-new-with-label (label)
 #+cl-cffi-gtk-button
 "@version{2013-1-18}
  @argument[label]{The text you want the @class{gtk-label} to hold.}
  @return{The newly created @class{gtk-button} widget.}
  @begin{short}
    Creates a @class{gtk-button} widget with a @class{gtk-label} child
    containing the given text.
  @end{short}"
  (make-instance 'gtk-button :label label))

(export 'gtk-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_button_new_with_mnemonic" gtk-button-new-with-mnemonic)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[label]{The text of the button, with an underscore in front of the
    mnemonic character}
  @return{a new @class{gtk-button}}
  @begin{short}
    Creates a new @class{gtk-button} containing a label.
  @end{short}
  If characters in label are preceded by an underscore, they are underlined. If
  you need a literal underscore character in a label, use '__'
  (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. Pressing Alt and that key activates the
  button."
  (label :string))

(export 'gtk-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_button_new_from_stock" gtk-button-new-from-stock)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[stock-id]{the name of the stock item}
  @return{a new @class{gtk-button}}
  @begin{short}
    Creates a new @class{gtk-button} containing the image and text from a stock
    item.
  @end{short}
  Some stock ids have preprocessor macros like @code{gtk-stock-ok} and
  @code{gtk-stock-apply}.

  If @arg{stock-id} is unknown, then it will be treated as a mnemonic label
  (as for @fun{gtk-button-new-with-mnemonic})."
  (stock-id :string))

(export 'gtk-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_button_pressed ()
;;; 
;;; void gtk_button_pressed (GtkButton *button);
;;; 
;;; Warning
;;; 
;;; gtk_button_pressed has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "button-press-event" signal.
;;; 
;;; Emits a "pressed" signal to the given GtkButton.
;;; 
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_released ()
;;; 
;;; void gtk_button_released (GtkButton *button);
;;; 
;;; Warning
;;; 
;;; gtk_button_released has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Use the "button-release-event" signal.
;;; 
;;; Emits a "released" signal to the given GtkButton.
;;; 
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_clicked ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_button_clicked" gtk-button-clicked) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{The @class{gtk-button} you want to send the signal to.}
  @short{Emits a \"clicked\" signal to the given @class{gtk-button}.}"
  (button (g-object gtk-button)))

(export 'gtk-button-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_button_enter ()
;;; 
;;; void gtk_button_enter (GtkButton *button);
;;; 
;;; Warning
;;; 
;;; gtk_button_enter has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "enter-notify-event" signal.
;;; 
;;; Emits a "enter" signal to the given GtkButton.
;;; 
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_leave ()
;;; 
;;; void gtk_button_leave (GtkButton *button);
;;; 
;;; Warning
;;; 
;;; gtk_button_leave has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Use the "leave-notify-event" signal.
;;; 
;;; Emits a "leave" signal to the given GtkButton.
;;; 
;;; button :
;;;     The GtkButton you want to send the signal to.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_relief ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-relief))

(defun gtk-button-set-relief (button newstyle)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{The GtkButton you want to set relief styles of.}
  @argument[newstyle]{The GtkReliefStyle as described above.}
  @begin{short}
    Sets the relief style of the edges of the given GtkButton widget.
  @end{short}
  Three styles exist, GTK_RELIEF_NORMAL, GTK_RELIEF_HALF, GTK_RELIEF_NONE. The
  default style is, as one can guess, GTK_RELIEF_NORMAL."
  (setf (gtk-button-relief button) newstyle))

(export 'gtk-button-set-relief)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_relief ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-relief))

(defun gtk-button-get-relief (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{The GtkButton you want the GtkReliefStyle from.}
  @return{The current GtkReliefStyle.}
  @short{Returns the current relief style of the given GtkButton.}"
  (gtk-button-relief button))

(export 'gtk-button-get-relief)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-label))

(defun gtk-button-get-label (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{The text of the label widget. This string is owned by the widget and
    must not be modified or freed.}
  @begin{short}
    Fetches the text from the label of the button, as set by
    @fun{gtk-button-set-label}.
  @end{short}
  If the label text has not been set the return value will be NULL. This will be
  the case if you create an empty button with @fun{gtk-button-new} to use as a
  container."
  (gtk-button-label button))

(export 'gtk-button-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-label))

(defun gtk-button-set-label (button label)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument[label]{a string}
  @begin{short}
    Sets the text of the label of the button to str.
  @end{short}
  This text is also used to select the stock item if
  @fun{gtk-button-set-use-stock} is used.

  This will also clear any previously set labels."
  (setf (gtk-button-label button) label))

(export 'gtk-button-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_use_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-use-stock))

(defun gtk-button-get-use-stock (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{TRUE if the button label is used to select a stock item instead of
    being used directly as the label text.}
  @short{Returns whether the button label is a stock item.}"
  (gtk-button-use-stock button))

(export 'gtk-button-get-use-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_use_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-use-stock))

(defun gtk-button-set-use-stock (button use-stock)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument[use-stock]{TRUE if the button should use a stock item}
  @begin{short}
    If TRUE, the label set on the button is used as a stock id to select the
    stock item for the button.
  @end{short}"
  (setf (gtk-button-use-stock button) use-stock))

(export 'gtk-button-set-use-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-use-underline))

(defun gtk-button-get-use-underline (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{TRUE if an embedded underline in the button label indicates the
    mnemonic accelerator keys.}
  @begin{short}
    Returns whether an embedded underline in the button label indicates a
    mnemonic.
  @end{short}
  See @fun{gtk-button-set-use-underline}."
  (gtk-button-use-underline button))

(export 'gtk-button-get-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-use-underline))

(defun gtk-button-set-use-underline (button use-underline)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument[use-underline]{TRUE if underlines in the text indicate mnemonics}
  @begin{short}
    If true, an underline in the text of the button label indicates the next
    character should be used for the mnemonic accelerator key.
  @end{short}"
  (setf (gtk-button-use-underline button) use-underline))

(export 'gtk-button-set-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_focus_on_click ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-focus-on-click))

(defun gtk-button-set-focus-on-click (button focus-on-click)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument{focus-on-click]{whether the button grabs focus when clicked with the
    mouse}
  @begin{short}
    Sets whether the button will grab focus when it is clicked with the mouse.
  @end{short}
  Making mouse clicks not grab focus is useful in places like toolbars where
  you don't want the keyboard focus removed from the main area of the
  application.

  Since 2.4"
  (setf (gtk-button-focus-on-click button) focus-on-click))

(export 'gtk-button-set-focus-on-click)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_focus_on_click ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-focus-on-click))

(defun gtk-button-get-focus-on-click (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{TRUE if the button grabs focus when it is clicked with the mouse.}
  @begin{short}
    Returns whether the button grabs focus when it is clicked with the mouse.
  @end{short}
  See @fun{gtk-button-set-focus-on-click}.

  Since 2.4"
  (gtk-button-focus-on-click button))

(export 'gtk-button-get-focus-on-click)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-alignment))

(defun gtk-button-set-alignment (button xalign yalign)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument[xalign]{the horizontal position of the child, @code{0.0} is left
    aligned, @code{1.0} is right aligned}
  @argument[yalign]{the vertical position of the child, @code{0.0} is top
    aligned, @code{1.0} is bottom aligned}
  @begin{short}
    Sets the alignment of the child.
  @end{short}
  This property has no effect unless the child is a @class{gtk-misc} or a
  @class{gtk-alignment}.

  Since 2.4"
  (setf (gtk-button-xalign button) xalign
        (gtk-button-yalign button) yalign))

(export 'gtk-button-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-alignment))

(defun gtk-button-get-alignment (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{return location for horizontal and vertical alignment}
  @begin{short}
    Gets the alignment of the child in the @arg{button}.
  @end{short}

  Since 2.4"
  (values (gtk-button-xalign button)
          (gtk-button-yalign button)))

(export 'gtk-button-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_image ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-image))

(defun gtk-button-set-image (button image)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument[image]{a widget to set as the image for the button}
  @begin{short}
    Set the image of button to the given widget.
  @end{short}
  Note that it depends on the \"gtk-button-images\" setting whether the image
  will be displayed or not, you don't have to call gtk_widget_show() on image
  yourself.

  Since 2.6"
  (setf (gtk-button-image button) image))

(export 'gtk-button-set-image)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_image ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-image))

(defun gtk-button-get-image (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{a @class{gtk-widget} or @code{null}-pointer in case there is no image}
  @begin{short}
    Gets the widget that is currently set as the image of button.
  @end{short}
 This may have been explicitly set by @fun{gtk-button-set-image} or constructed
 by @fun{gtk-button-new-from-stock}.

  Since 2.6"
  (gtk-button-image button))

(export 'gtk-button-get-image)

;;; ----------------------------------------------------------------------------
;;; gtk_button_set_image_position ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-set-image-position))

(defun gtk-button-set-image-position (button position)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @argument[position]{the position}
  @begin{short}
    Sets the position of the image relative to the text inside the button.
  @end{short}

  Since 2.10"
  (setf (gtk-button-image-position button) position))

(export 'gtk-button-set-image-position)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_image_position ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-button-get-image-position))

(defun gtk-button-get-image-position (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{the position}
  @begin{short}
    Gets the position of the image relative to the text inside the button.
  @end{short}

  Since 2.10"
  (gtk-button-image-position button))

(export 'gtk-button-get-image-position)

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_event_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_button_get_event_window" gtk-button-get-event-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-18}
  @argument[button]{a @class{gtk-button}}
  @return{button's event window}
  @begin{short}
    Returns the button's event window if it is realized, @code{nil} otherwise.
  @end{short}
  This function should be rarely needed.

  Since 2.22"
  (button (g-object gtk-button)))

(export 'gtk-button-get-event-window)

;;; --- End of file gtk.button.lisp --------------------------------------------
