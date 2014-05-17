;;; ----------------------------------------------------------------------------
;;; gtk.spin-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
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
;;; GtkSpinButton
;;;
;;; Retrieve an integer or floating-point number from the user
;;;
;;; Synopsis
;;;
;;;     GtkSpinButton
;;;     GtkSpinButtonUpdatePolicy
;;;     GtkSpinType
;;;
;;;     gtk_spin_button_configure
;;;     gtk_spin_button_new
;;;     gtk_spin_button_new_with_range
;;;     gtk_spin_button_set_adjustment
;;;     gtk_spin_button_get_adjustment
;;;     gtk_spin_button_set_digits
;;;     gtk_spin_button_set_increments
;;;     gtk_spin_button_set_range
;;;     gtk_spin_button_get_value_as_int
;;;     gtk_spin_button_set_value
;;;     gtk_spin_button_set_update_policy
;;;     gtk_spin_button_set_numeric
;;;     gtk_spin_button_spin
;;;     gtk_spin_button_set_wrap
;;;     gtk_spin_button_set_snap_to_ticks
;;;     gtk_spin_button_update
;;;     gtk_spin_button_get_digits
;;;     gtk_spin_button_get_increments
;;;     gtk_spin_button_get_numeric
;;;     gtk_spin_button_get_range
;;;     gtk_spin_button_get_snap_to_ticks
;;;     gtk_spin_button_get_update_policy
;;;     gtk_spin_button_get_value
;;;     gtk_spin_button_get_wrap
;;;     GTK_INPUT_ERROR
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSpinButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSpinButton" gtk-spin-button
  (:superclass gtk-entry
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkEditable"
                "GtkCellEditable"
                "GtkOrientable")
   :type-initializer "gtk_spin_button_get_type")
  ((adjustment
    gtk-spin-button-adjustment
    "adjustment" "GtkAdjustment" t t)
   (climb-rate
    gtk-spin-button-climb-rate
    "climb-rate" "gdouble" t t)
   (digits
    gtk-spin-button-digits
    "digits" "guint" t t)
   (numeric
    gtk-spin-button-numeric
    "numeric" "gboolean" t t)
   (snap-to-ticks
    gtk-spin-button-snap-to-ticks
    "snap-to-ticks" "gboolean" t t)
   (update-policy
    gtk-spin-button-update-policy
    "update-policy" "GtkSpinButtonUpdatePolicy" t t)
   (value
    gtk-spin-button-value
    "value" "gdouble" t t)
   (wrap
    gtk-spin-button-wrap
    "wrap" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-spin-button 'type)
 "@version{2014-4-28}
  @begin{short}
    A @sym{gtk-spin-button} is an ideal way to allow the user to set the value
    of some attribute. Rather than having to directly type a number into a
    @class{gtk-entry}, @sym{gtk-spin-button} allows the user to click on one of
    two arrows to increment or decrement the displayed value. A value can still
    be typed in, with the bonus that it can be checked to ensure it is in a
    given range.
  @end{short}

  The main properties of a @sym{gtk-spin-button} are through an adjustment. See
  the @class{gtk-adjustment} section for more details about an adjustment's
  properties.

  @b{Example:} Using a @class{gtk-spin-button} to get an integer
  @begin{pre}
 /* Provides a function to retrieve an integer value from a
  * GtkSpinButton and creates a spin button to model percentage
  * values.
  */

 gint
 grab_int_value (GtkSpinButton *button,
                 gpointer       user_data)
 {
   return gtk_spin_button_get_value_as_int (button);
 @}

 void
 create_integer_spin_button (void)
 {

   GtkWidget *window, *button;
   GtkAdjustment *adjustment;

   adjustment = gtk_adjustment_new (50.0, 0.0, 100.0, 1.0, 5.0, 0.0);

   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_container_set_border_width (GTK_CONTAINER (window), 5);

   /* creates the spinbutton, with no decimal places */
   button = gtk_spin_button_new (adjustment, 1.0, 0);
    gtk_container_add (GTK_CONTAINER (window), button);

   gtk_widget_show_all (window);
 @}
  @end{pre}
  @b{Example:} Using a @sym{gtk-spin-button} to get a floating point value
  @begin{pre}
 /* Provides a function to retrieve a floating point value from a
  * GtkSpinButton, and creates a high precision spin button.
  */

 gfloat
 grab_float_value (GtkSpinButton *button,
                   gpointer       user_data)
 {
   return gtk_spin_button_get_value (button);
 @}

 void
 create_floating_spin_button (void)
 {
   GtkWidget *window, *button;
   GtkAdjustment *adjustment;

   adjustment = gtk_adjustment_new (2.500, 0.0, 5.0, 0.001, 0.1, 0.0);

   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_container_set_border_width (GTK_CONTAINER (window), 5);

   /* creates the spinbutton, with three decimal places */
   button = gtk_spin_button_new (adjustment, 0.001, 3);
   gtk_container_add (GTK_CONTAINER (window), button);

   gtk_widget_show_all (window);
 @}
  @end{pre}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"shadow-type\" style property}
      @code{\"shadow-type\"} of type @symbol{gtk-shadow-type} (Read) @br{}
      Style of bevel around the spin button. @br{}
      Default value: @code{:in}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-value\" signal}
      @begin{pre}
 lambda (spinbutton arg)   : Action
      @end{pre}
    @subheading{The \"input\" signal}
      @begin{pre}
 lambda (spin-button new-value)   : Run Last
      @end{pre}
      The \"input\" signal can be used to influence the conversion of the users
      input into a double value. The signal handler is expected to use the
      function @fun{gtk-entry-get-text} to retrieve the text of the entry and
      set new_value to the new value.
      The default conversion uses @code{g_strtod()}.
      @begin[code]{table}
        @entry[spin-button]{The object on which the signal was emitted.}
        @entry[new-value]{Return location for the new value.}
        @entry[Returns]{@em{True} for a successful conversion, @code{nil} if the
          input was not handled, and @code{GTK_INPUT_ERROR} if the conversion
          failed.}
      @end{table}
    @subheading{The \"output\" signal}
      @begin{pre}
 lambda (spin-button)   : Run Last
      @end{pre}
      The \"output\" signal can be used to change to formatting of the value
      that is displayed in the spin buttons entry.
      @begin{pre}
 /* show leading zeros */
 static gboolean
 on_output (GtkSpinButton *spin,
            gpointer       data)
 {
    GtkAdjustment *adjustment;
    gchar *text;
    int value;
    adjustment = gtk_spin_button_get_adjustment (spin);
    value = (int)gtk_adjustment_get_value (adjustment);
    text = g_strdup_printf (\"%02d\", value);
    gtk_entry_set_text (GTK_ENTRY (spin), text);
    g_free (text);

    return TRUE;
 @}
      @end{pre}
      @begin[code]{table}
        @entry[spin-button]{The object which received the signal.}
        @entry[Returns]{@em{True} if the value has been displayed.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
 lambda (spinbutton)   : Run Last
      @end{pre}
    @subheading{The \"wrapped\" signal}
      @begin{pre}
 lambda (spinbutton)   : Run Last
      @end{pre}
      The wrapped signal is emitted right after the spinbutton wraps from its
      maximum to minimum value or vice-versa.
      @begin[code]{table}
        @entry[spinbutton]{The object which received the signal.}
      @end{table}
      Since 2.10
  @end{dictionary}
  @see-slot{gtk-spin-button-adjustment}
  @see-slot{gtk-spin-button-climb-rate}
  @see-slot{gtk-spin-button-digits}
  @see-slot{gtk-spin-button-numeric}
  @see-slot{gtk-spin-button-snap-to-ticks}
  @see-slot{gtk-spin-button-update-policy}
  @see-slot{gtk-spin-button-value}
  @see-slot{gtk-spin-button-wrap}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "adjustment"
                                               'gtk-spin-button) 't)
 "The @code{\"adjustment\"} property of type @class{gtk-adjustment}
  (Read / Write) @br{}
  The adjustment that holds the value of the spin button.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "climb-rate"
                                               'gtk-spin-button) 't)
 "The @code{\"climb-rate\"} property of type @code{:double} (Read / Write) @br{}
  The acceleration rate when you hold down a button. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "digits" 'gtk-spin-button) 't)
 "The @code{\"digits\"} property of type @code{:uint} (Read / Write) @br{}
  The number of decimal places to display. @br{}
  Allowed values: <= 20 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "numeric" 'gtk-spin-button) 't)
 "The @code{\"numeric\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether non-numeric characters should be ignored. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "snap-to-ticks"
                                               'gtk-spin-button) 't)
 "The @code{\"snap-to-ticks\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether erroneous values are automatically changed to a spin button's
  nearest step increment. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "update-policy"
                                               'gtk-spin-button) 't)
 "The @code{\"update-policy\"} property of type
  @symbol{gtk-spin-button-update-policy} (Read / Write) @br{}
  Whether the spin button should update always, or only when the value is
  legal. @br{}
  Default value: @code{:always}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-spin-button) 't)
 "The @code{\"value\"} property of type @code{:double} (Read / Write) @br{}
  Reads the current value, or sets a new value. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap" 'gtk-spin-button) 't)
 "The @code{\"wrap\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether a spin button should wrap upon reaching its limits. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-adjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-adjustment 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"adjustment\"} of the @class{gtk-spin-button}
  class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-adjustment}
  @see-function{gtk-spin-button-set-adjustment}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-climb-rate atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-climb-rate 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"climb-rate\"} of the @class{gtk-spin-button}
  class.
  @see-class{gtk-spin-button}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-digits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-digits 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"digits\"} of the @class{gtk-spin-button} class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-digits}
  @see-function{gtk-spin-button-set-digits}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-numeric atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-numeric 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"numeric\"} of the @class{gtk-spin-button} class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-numeric}
  @see-function{gtk-spin-button-set-numeric}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-snap-to-ticks atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-snap-to-ticks 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"snap-to-ticks\"} of the @class{gtk-spin-button}
  class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-snap-to-ticks}
  @see-function{gtk-spin-button-set-snap-to-ticks}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-update-policy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-update-policy 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"update-policy\"} of the @class{gtk-spin-button}
  class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-update-policy}
  @see-function{gtk-spin-button-set-update-policy}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-value 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"value\"} of the @class{gtk-spin-button} class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-value}
  @see-function{gtk-spin-button-set-value}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-wrap 'function)
 "@version{2013-11-30}
  Accessor of the slot @code{\"wrap\"} of the @class{gtk-spin-button} class.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-wrap}
  @see-function{gtk-spin-button-set-wrap}")

;;; ----------------------------------------------------------------------------
;;; enum GtkSpinButtonUpdatePolicy
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSpinButtonUpdatePolicy" gtk-spin-button-update-policy
  (:export t
   :type-initializer "gtk_spin_button_update_policy_get_type")
  (:always 0)
  (:if-valid 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-update-policy atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-spin-button-update-policy atdoc:*external-symbols*)
 "@version{2013-4-28}
  @begin{short}
    The spin button update policy determines whether the spin button displays
    values even if they are outside the bounds of its adjustment. See the
    function @fun{gtk-spin-button-set-update-policy}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkSpinButtonUpdatePolicy\" gtk-spin-button-update-policy
  (:export t
   :type-initializer \"gtk_spin_button_update_policy_get_type\")
  (:always 0)
  (:if-valid 1))
  @end{pre}
  @begin[code]{table}
    @entry[:always]{When refreshing your @class{gtk-spin-button}, the value is
      always displayed}
    @entry[:if-valid]{When refreshing your @class{gtk-spin-button}, the value is
      only displayed if it is valid within the bounds of the spin button's
      adjustment.}
  @end{table}
  @see-function{gtk-spin-button-set-update-policy}")

;;; ----------------------------------------------------------------------------
;;; enum GtkSpinType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSpinType" gtk-spin-type
  (:export t
   :type-initializer "gtk_spin_type_get_type")
  (:step-forward 0)
  (:step-backward 1)
  (:page-forward 2)
  (:page-backward 3)
  (:home 4)
  (:end 5)
  (:user-defined 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-spin-type atdoc:*external-symbols*)
 "@version{2013-4-28}
  @begin{short}
    The values of the @sym{gtk-spin-type} enumeration are used to specify the
    change to make in the function @fun{gtk-spin-button-spin}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkSpinType\" gtk-spin-type
  (:export t
   :type-initializer \"gtk_spin_type_get_type\")
  (:step-forward 0)
  (:step-backward 1)
  (:page-forward 2)
  (:page-backward 3)
  (:home 4)
  (:end 5)
  (:user-defined 6))
  @end{pre}
  @begin[code]{table}
    @entry[:step-forward]{Increment by the adjustments step increment.}
    @entry[:step-backward]{Decrement by the adjustments step increment.}
    @entry[:page-forward]{Increment by the adjustments page increment.}
    @entry[:page-backward]{Decrement by the adjustments page increment.}
    @entry[:home]{Go to the adjustments lower bound.}
    @entry[:end]{Go to the adjustments upper bound.}
    @entry[:user-defined]{Change by a specified amount.}
  @end{table}
  @see-function{gtk-spin-button-spin}")

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_configure" gtk-spin-button-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[adjustment]{a @class{gtk-adjustment}}
  @argument[climb-rate]{the new climb rate}
  @argument[digits]{the number of decimal places to display in the spin button}
  @begin{short}
    Changes the properties of an existing spin button.
  @end{short}
  The adjustment, climb rate, and number of decimal places are all changed
  accordingly, after this function call.
  @see-class{gtk-spin-button}"
  (spin-button (g-object gtk-spin-button))
  (adjustment (g-object gtk-adjustment))
  (climb-rate :double)
  (digits :uint))

(export 'gtk-spin-button-configure)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-new))

(defun gtk-spin-button-new (adjustment climb-rate digits)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[adjustment]{the @class{gtk-adjustment} object that this spin button
    should use, or @code{nil}}
  @argument[climb-rate]{specifies how much the spin button changes when an
    arrow is clicked on}
  @argument[digits]{the number of decimal places to display}
  @return{The new spin button.}
  Creates a new @class{gtk-spin-button}.
  @see-class{gtk-spin-button}
  @see-class{gtk-adjustment}"
  (make-instance 'gtk-spin-button
                 :adjustment adjustment
                 :climb-rate climb-rate
                 :digits digits))

(export 'gtk-spin-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_new_with_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_new_with_range" gtk-spin-button-new-with-range)
    (g-object gtk-spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[min]{minimum allowable value}
  @argument[max]{maximum allowable value}
  @argument[step]{increment added or subtracted by spinning the widget}
  @return{The new spin button.}
  @begin{short}
    This is a convenience constructor that allows creation of a numeric
    @class{gtk-spin-button} without manually creating an adjustment.
  @end{short}
  The value is initially set to the minimum value and a page increment of
  10 * @arg{step} is the default. The precision of the spin button is equivalent
  to the precision of @arg{step}.

  Note that the way in which the precision is derived works best if step is a
  power of ten. If the resulting precision is not suitable for your needs, use
  the function @fun{gtk-spin-button-set-digits} to correct it.
  @see-function{gtk-spin-button}
  @see-function{gtk-spin-button-set-digits}"
  (min :double)
  (max :double)
  (step :double))

(export 'gtk-spin-button-new-with-range)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_adjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-set-adjustment))

(defun gtk-spin-button-set-adjustment (spin-button adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[adjustment]{a @class{gtk-adjustment} to replace the existing
    adjustment}
  Replaces the @class{gtk-adjustment} associated with @arg{spin-button}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-adjustment}"
  (setf (gtk-spin-button-adjustment spin-button) adjustment))

(export 'gtk-spin-button-set-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_adjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-adjustment))

(defun gtk-spin-button-get-adjustment (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{The @class{gtk-adjustment} of @arg{spin-button}.}
  Get the adjustment associated with a @class{gtk-spin-button}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-adjustment}"
  (gtk-spin-button-adjustment spin-button))

(export 'gtk-spin-button-get-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_digits ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-set-digits))

(defun gtk-spin-button-set-digits (spin-button digits)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[digits]{the number of digits after the decimal point to be displayed
    for the spin button's value}
  Set the precision to be displayed by @arg{spin-button}. Up to 20 digit
  precision is allowed.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-digits}"
  (setf (gtk-spin-button-digits spin-button) digits))

(export 'gtk-spin-button-set-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_increments ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_set_increments" gtk-spin-button-set-increments) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[step]{increment applied for a button 1 press}
  @argument[page]{increment applied for a button 2 press}
  @begin{short}
    Sets the step and page increments for @arg{spin-button}.
  @end{short}
  This affects how quickly the value changes when the spin button's arrows are
  activated.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-increments}"
  (spin-button (g-object gtk-spin-button))
  (step :double)
  (page :double))

(export 'gtk-spin-button-set-increments)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_set_range" gtk-spin-button-set-range) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[min]{minimum allowable value}
  @argument[max]{maximum allowable value}
  @begin{short}
    Sets the minimum and maximum allowable values for @arg{spin-button}.
  @end{short}

  If the current value is outside this range, it will be adjusted to fit
  within the range, otherwise it will remain unchanged.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-range}"
  (spin-button (g-object gtk-spin-button))
  (min :double)
  (max :double))

(export 'gtk-spin-button-set-range)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_value_as_int ()
;;; ----------------------------------------------------------------------------

(defun gtk-spin-button-get-value-as-int (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{The value of @arg{spin-button}.}
  Get the value @arg{spin-button} represented as an integer.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-value}"
  (truncate (gtk-spin-button-value spin-button)))

(export 'gtk-spin-button-get-value-as-int)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-value))

(defun gtk-spin-button-set-value (spin-button value)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[value]{the new value}
  Sets the value of @arg{spin-button}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-value}
  @see-function{gtk-spin-button-get-value-as-int}"
  (setf (gtk-spin-button-value spin-button) value))

(export 'gtk-spin-button-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_update_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-set-update-policy))

(defun gtk-spin-button-set-update-policy (spin-button policy)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[policy]{a @symbol{gtk-spin-button-update-policy} value}
  @begin{short}
    Sets the update behavior of a spin button.
  @end{short}
  This determines wether the spin button is always updated or only when a valid
  value is set.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-upadate-policy}"
  (setf (gtk-spin-button-update-policy spin-button) policy))

(export 'gtk-spin-button-set-update-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_numeric ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-set-numeric))

(defun gtk-spin-button-set-numeric (spin-button numeric)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[numeric]{flag indicating if only numeric entry is allowed}
  Sets the flag that determines if non-numeric text can be typed into the spin
  button.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-numeric}"
  (setf (gtk-spin-button-numeric spin-button) numeric))

(export 'gtk-spin-button-set-numeric)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_spin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_spin" gtk-spin-button-spin) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-28}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[direction]{a @symbol{gtk-spin-type} indicating the direction to
    spin}
  @argument[increment]{step increment to apply in the specified direction}
  Increment or decrement a spin button's value in a specified direction by a
  specified amount.
  @see-class{gtk-spin-button}
  @see-symbol{gtk-spin-type}"
  (spin-button (g-object gtk-spin-button))
  (direction gtk-spin-type)
  (increment :double))

(export 'gtk-spin-button-spin)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_wrap ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-set-wrap))

(defun gtk-spin-button-set-wrap (spin-button wrap)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[wrap]{a flag indicating if wrapping behavior is performed}
  Sets the flag that determines if a spin button value wraps around to the
  opposite limit when the upper or lower limit of the range is exceeded.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-wrap}"
  (setf (gtk-spin-button-wrap spin-button) wrap))

(export 'gtk-spin-button-set-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_set_snap_to_ticks ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-set-snap-to-ticks))

(defun gtk-spin-button-set-snap-to-ticks (spin-button snap-to-ticks)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[snap-to-ticks]{a flag indicating if invalid values should be
    corrected}
  Sets the policy as to whether values are corrected to the nearest step
  increment when a spin button is activated after providing an invalid value.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-snap-to-ticks}"
  (setf (gtk-spin-button-snap-to-ticks spin-button) snap-to-ticks))

(export 'gtk-spin-button-set-snap-to-ticks)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_update ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_update" gtk-spin-button-update) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  Manually force an update of the spin button."
  (spin-button (g-object gtk-spin-button)))

(export 'gtk-spin-button-update)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_digits ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-digits))

(defun gtk-spin-button-get-digits (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{The current precision.}
  @short{Fetches the precision of @arg{spin-button}.}
  See the function @fun{gtk-spin-button-set-digits}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-digits}"
  (gtk-spin-button-get-digits spin-button))

(export 'gtk-spin-button-get-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_increments ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_get_increments" %gtk-spin-button-get-increments)
    :void
  (spin-button (g-object gtk-spin-button))
  (step (:pointer :double))
  (page (:pointer :double)))

(defun gtk-spin-button-get-increments (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @begin{return}
    @code{step} -- step increment, or @code{nil} @br{}
    @code{page} -- page increment, or @code{nil}
  @end{return}
  @begin{short}
    Gets the current step and page the increments used by @arg{spin-button}.
  @end{short}
  See the function @fun{gtk-spin-button-set-increments}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-increments}"
  (with-foreign-objects ((min :double) (max :double))
    (%gtk-spin-button-get-increments spin-button min max)
    (values (mem-ref min :double)
            (mem-ref max :double))))

(export 'gtk-spin-button-get-increments)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_numeric ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-numeric))

(defun gtk-spin-button-get-numeric (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{@em{True} if only numeric text can be entered.}
  @begin{short}
    Returns whether non-numeric text can be typed into the spin button.
  @end{short}
  See the function @fun{gtk-spin-button-set-numeric}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-numeric}"
  (gtk-spin-button-numeric spin-button))

(export 'gtk-spin-button-get-numeric)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_get_range" %gtk-spin-button-get-range) :void
  (spin-button (g-object gtk-spin-button))
  (min (:pointer :double))
  (max (:pointer :double)))

(defun gtk-spin-button-get-range (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @begin{return}
    @code{min} -- minimum allowed value, or @code{nil} @br{}
    @code{max} -- maximum allowed value, or @code{nil}
  @end{return}
  @short{Gets the range allowed for @arg{spin-button}.}
  See the function @fun{gtk-spin-button-set-range}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-range}"
  (with-foreign-objects ((min :double) (max :double))
    (%gtk-spin-button-get-range spin-button min max)
    (values (mem-ref min :double)
            (mem-ref max :double))))

(export 'gtk-spin-button-get-range)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_snap_to_ticks ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-snap-to-ticks))

(defun gtk-spin-button-get-snap-to-ticks (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{@em{True} if values are snapped to the nearest step.}
  @begin{short}
    Returns whether the values are corrected to the nearest step.
  @end{short}
  See the function @fun{gtk-spin-button-set-snap-to-ticks}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-snap-to-ticks}"
  (gtk-spin-button-snap-to-ticks spin-button))

(export 'gtk-spin-button-get-snap-to-ticks)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_update_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-update-policy))

(defun gtk-spin-button-get-update-policy (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{The current update policy.}
  @short{Gets the update behavior of a spin button.}
  See the function @fun{gtk-spin-button-set-update-policy}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-update-policy}"
  (gtk-spin-button-update-policy spin-button))

(export 'gtk-spin-button-get-update-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-value))

(defun gtk-spin-button-get-value (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{The value of @arg{spin-button}.}
  Get the value in the @arg{spin-button}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-get-value-as-int}
  @see-function{gtk-spin-button-set-value}"
  (gtk-spin-button-value spin-button))

(export 'gtk-spin-button-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_get_wrap ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spin-button-get-wrap))

(defun gtk-spin-button-get-wrap (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{@em{True} if the spin button wraps around.}
  @begin{short}
    Returns whether the spin button's value wraps around to the opposite limit
    when the upper or lower limit of the range is exceeded.
  @end{short}
  See the function @fun{gtk-spin-button-set-wrap}.
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-set-wrap}"
  (gtk-spin-button-wrap spin-button))

(export 'gtk-spin-button-get-wrap)

;;; ----------------------------------------------------------------------------
;;; GTK_INPUT_ERROR
;;;
;;; #define GTK_INPUT_ERROR -1
;;;
;;; Constant to return from a signal handler for the "input" signal in case of
;;; conversion failure.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.spin-button.lisp ---------------------------------------
