;;; ----------------------------------------------------------------------------
;;; gtk.spin-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Retrieve an integer or floating-point number from the user.
;;;
;;; Types and Values
;;;
;;;     GtkSpinButton
;;;     GtkSpinButtonUpdatePolicy
;;;     GtkSpinType
;;;
;;;     GTK_INPUT_ERROR
;;;
;;; Functions
;;;
;;;     gtk_spin_button_configure
;;;     gtk_spin_button_new
;;;     gtk_spin_button_new_with_range
;;;     gtk_spin_button_set_increments
;;;     gtk_spin_button_set_range
;;;     gtk_spin_button_get_value_as_int
;;;     gtk_spin_button_spin
;;;     gtk_spin_button_update
;;;     gtk_spin_button_get_increments
;;;     gtk_spin_button_get_range
;;;
;;; Properties
;;;
;;;             GtkAdjustment*   adjustment       Read / Write
;;;                   gdouble    climb-rate       Read / Write
;;;                     guint    digits           Read / Write
;;;                  gboolean    numeric          Read / Write
;;;                  gboolean    snap-to-ticks    Read / Write
;;; GtkSpinButtonUpdatePolicy    update-policy    Read / Write
;;;                   gdouble    value            Read / Write
;;;                  gboolean    wrap             Read / Write
;;;
;;; Style Properties
;;;
;;;             GtkShadowType    shadow-type      Read
;;;
;;; Signals
;;;
;;;                      void    change-value     Action
;;;                      gint    input            Run Last
;;;                  gboolean    output           Run Last
;;;                      void    value-changed    Run Last
;;;                      void    wrapped          Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;            ╰── GtkEntry
;;;                 ╰── GtkSpinButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSpinButton implements AtkImplementorIface, GtkBuildable, GtkEditable,
;;;     GtkCellEditable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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
 "@version{2020-5-31}
  @begin{short}
    The spin button update policy determines whether the spin button displays
    values even if they are outside the bounds of its adjustment. See the
    function @fun{gtk-spin-button-update-policy}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkSpinButtonUpdatePolicy\" gtk-spin-button-update-policy
  (:export t
   :type-initializer \"gtk_spin_button_update_policy_get_type\")
  (:always 0)
  (:if-valid 1))
  @end{pre}
  @begin[code]{table}
    @entry[:always]{When refreshing the spin button, the value is always
      displayed}
    @entry[:if-valid]{When refreshing the spin button, the value is only
      displayed if it is valid within the bounds of the spin button's
      adjustment.}
  @end{table}
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-update-policy}")

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
 "@version{2020-5-31}
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
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-spin}")

;;; ----------------------------------------------------------------------------
;;; GTK_INPUT_ERROR
;;;
;;; #define GTK_INPUT_ERROR -1
;;;
;;; Constant to return from a signal handler for the "input" signal in case of
;;; conversion failure.
;;; ----------------------------------------------------------------------------

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
 "@version{2020-5-31}
  @begin{short}
    A @sym{gtk-spin-button} is an ideal way to allow the user to set the value
    of some attribute. Rather than having to directly type a number into a
    @class{gtk-entry}, @sym{gtk-spin-button} allows the user to click on one of
    two arrows to increment or decrement the displayed value. A value can still
    be typed in, with the bonus that it can be checked to ensure it is in a
    given range.
  @end{short}

  @image[spinbutton]{}

  The main properties of a @sym{gtk-spin-button} are through an adjustment. See
  the @class{gtk-adjustment} section for more details about an adjustment's
  properties.
  @begin[Example]{dictionary}
    Code fragment for creating a spin button. The value from the spin button is
    retrieved in a signal handler.
    @begin{pre}
 (let (...
       (spinner (make-instance 'gtk-spin-button
                               :adjustment
                               (make-instance 'gtk-adjustment
                                              :value 50.0
                                              :lower 0.0
                                              :upper 100.0
                                              :step-increment 1.0
                                              :page-increment 5.0
                                              :page-size 0.0)
                               :climb-rate 0
                               :digits 0
                               :wrap t)))

   (g-signal-connect spinner \"value-changed\"
                     (lambda (widget)
                       (declare (ignore widget))
                       (let ((value (gtk-spin-button-value spinner)))))
                         ... )
  )
    @end{pre}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[shadow-type]{entry}
        The \"shadow-type\" style property of type @symbol{gtk-shadow-type}
        (Read) @br{}
        Style of bevel around the spin button. @br{}
        @em{Warning:} The @code{shadow-type} style property has been deprecated
        since version 3.20 and should not be used in newly-written code. Use CSS
        to determine the style of the border. The value of this style property
        is ignored. @br{}
        Default value: @code{:in}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-value\" signal}
      @begin{pre}
 lambda (spin-button scroll)    : Action
      @end{pre}
      The \"change-value\" signal is a keybinding signal which gets emitted when
      the user initiates a value change. Applications should not connect to it,
      but may emit it with the function @fun{g-signal-emit} if they need to
      control the cursor programmatically. The default bindings for this signal
      are Up/Down and PageUp and/PageDown.
      @begin[code]{table}
        @entry[spin-button]{The @sym{gtk-spin-button} widget on which the signal
          was emitted.}
        @entry[scroll]{A @symbol{gtk-scroll-type} to specify the speed and
          amount of change.}
      @end{table}
    @subheading{The \"input\" signal}
      @begin{pre}
 lambda (spin-button new-value)    : Run Last
      @end{pre}
      The \"input\" signal can be used to influence the conversion of the users
      input into a double float value. The signal handler is expected to use
      the function @fun{gtk-entry-text} to retrieve the text of the entry and
      set @arg{new-value} to the new value. The default conversion uses
      @code{g_strtod()}.
      @begin[code]{table}
        @entry[spin-button]{The @sym{gtk-spin-button} widget on which the signal
          was emitted.}
        @entry[new-value]{Return location for the new value.}
        @entry[Returns]{@em{True} for a successful conversion, @em{false} if the
          input was not handled, and @code{GTK_INPUT_ERROR} if the conversion
          failed.}
      @end{table}
    @subheading{The \"output\" signal}
      @begin{pre}
 lambda (spin-button)    : Run Last
      @end{pre}
      The \"output\" signal can be used to change the formatting of the value
      that is displayed in the spin buttons entry.
      @begin{pre}
 (let (...
       ;; A spin button for a number
       (spinner1 (make-instance 'gtk-spin-button
                                :adjustment
                                (make-instance 'gtk-adjustment
                                               :value 1.0
                                               :lower -10000.0
                                               :upper  10000.0
                                               :step-increment 0.5
                                               :page-increment 100.0
                                               :page-size 0.0)
                                :climb-rate 1.0
                                :digits 2
                                :wrap t))
       ;; A spin button for the digits to display
       (spinner2 (make-instance 'gtk-spin-button
                                :adjustment
                                (make-instance 'gtk-adjustment
                                               :value 2
                                               :lower 1
                                               :upper 5
                                               :step-increment 1
                                               :page-increment 1
                                               :page-size 0)
                                 :climb-rate 0.0
                                 :digits 0
                                 :wrap t)))
   ;; Customize the appearance of the number
   (g-signal-connect spinner1 \"output\"
     (lambda (spin-button)
       (let ((value (gtk-adjustment-value
                      (gtk-spin-button-adjustment spin-button)))
             (digits (truncate
                       (gtk-adjustment-value
                         (gtk-spin-button-adjustment spinner2)))))
         (setf (gtk-entry-text spin-button)
               (format nil \"~@@?\" (format nil \"~~,~d@@f\" digits) value)))))
   ... )
      @end{pre}
      @begin[code]{table}
        @entry[spin-button]{The @sym{gtk-spin-button} widget which received the
          signal.}
        @entry[Returns]{@em{True} if the value has been displayed.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
 lambda (spin-button)    : Run Last
      @end{pre}
      The \"value-changed\" signal is emitted when the value represented by
      @arg{spin-button} changes. Also see the \"output\" signal.
      @begin[code]{table}
        @entry[spin-button]{The @sym{gtk-spin-button} widget on which the signal
          was emitted.}
      @end{table}
    @subheading{The \"wrapped\" signal}
      @begin{pre}
 lambda (spin-button)    : Run Last
      @end{pre}
      The wrapped signal is emitted right after the spin button wraps from its
      maximum to minimum value or vice-versa.
      @begin[code]{table}
        @entry[spin-button]{The @sym{gtk-spin-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-entry}
  @see-slot{gtk-spin-button-adjustment}
  @see-slot{gtk-spin-button-climb-rate}
  @see-slot{gtk-spin-button-digits}
  @see-slot{gtk-spin-button-numeric}
  @see-slot{gtk-spin-button-snap-to-ticks}
  @see-slot{gtk-spin-button-update-policy}
  @see-slot{gtk-spin-button-value}
  @see-slot{gtk-spin-button-wrap}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-spin-button-adjustment ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "adjustment"
                                               'gtk-spin-button) 't)
 "The @code{adjustment} property of type @class{gtk-adjustment} (Read / Write)
  @br{}
  The adjustment that holds the value of the spin button.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-adjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-adjustment 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-adjustment object) => adjustment}
  @syntax[]{(setf (gtk-spint-button-adjustment object) adjustment)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[adjustment]{a @class{gtk-adjustment} to replace the existing
    adjustment}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{adjustment} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button-adjustment} gets the adjustment
  associated with a spin button. The slot access function
  @sym{(setf gtk-spin-button-adjustment)} replaces the adjustment associated
  with the spin button.
  @see-class{gtk-spin-button}
  @see-class{gtk-adjustment}")

;;; --- gtk-spin-button-climb-rate ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "climb-rate"
                                               'gtk-spin-button) 't)
 "The @code{climb-rate} property of type @code{:double} (Read / Write) @br{}
  The acceleration rate when you hold down a button. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-climb-rate atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-climb-rate 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-climb-rate object) => climb-rate}
  @syntax[]{(setf (gtk-spint-button-climb-rate object) climb-rate)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[climb-rate]{a @code{:double} with the acceleration rate}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{climb-rate} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The acceleration rate when you hold down a button.
  @see-class{gtk-spin-button}")

;;; --- gtk-spin-button-digits -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "digits" 'gtk-spin-button) 't)
 "The @code{digits} property of type @code{:uint} (Read / Write) @br{}
  The number of decimal places to display. @br{}
  Allowed values: <= 20 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-digits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-digits 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-digits object) => digits}
  @syntax[]{(setf (gtk-spint-button-digits object) digits)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[digits]{an unsigned integer with the number of digits after the
    decimal point to be displayed for the spin button's value}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{digits} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button-digits} fetches the precision
  of the spin button. The slot access function
  @sym{(setf gtk-spin-button-digits)} sets the precision to be displayed by the
  spin button. Up to 20 digit precision is allowed.
  @see-class{gtk-spin-button}")

;;; --- gtk-spin-button-numeric ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "numeric" 'gtk-spin-button) 't)
 "The @code{numeric} property of type @code{:boolean} (Read / Write) @br{}
  Whether non-numeric characters should be ignored. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-numeric atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-numeric 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-numeric object) => numeric}
  @syntax[]{(setf (gtk-spin-button-numeric object) numeric)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[numeric]{a boolean indicating if only numeric entry is allowed}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{numeric} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button-numeric} returns whether
  non-numeric text can be typed into the spin button. The slot access function
  @sym{(setf gtk-spin-button-numeric)} sets the flag that determines if
  non-numeric text can be typed into the spin button.
  @see-class{gtk-spin-button}")

;;; --- gtk-spin-button-snap-to-ticks ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "snap-to-ticks"
                                               'gtk-spin-button) 't)
 "The @code{snap-to-ticks} property of type @code{:boolean} (Read / Write) @br{}
  Whether erroneous values are automatically changed to a spin button's nearest
  step increment. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-snap-to-ticks atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-snap-to-ticks 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-snap-to-ticks object) => snap-to-ticks}
  @syntax[]{(setf (gtk-spin-button-snap-to-ticks object) snap-to-ticks)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[snap-to-ticks]{a boolean indicating if invalid values should be
    corrected}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{snap-to-ticks} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button-snap-to-ticks} returns wether
  the values are corrected to the nearest step. The slot access function
  @sym{(setf gtk-spin-button-snap-to-ticks)} sets the policy as to whether
  values are corrected to the nearest step increment when a spin button is
  activated after providing an invalid value.
  @see-class{gtk-spin-button}")

;;; --- gtk-spin-button-update-policy ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "update-policy"
                                               'gtk-spin-button) 't)
 "The @code{update-policy} property of type
  @symbol{gtk-spin-button-update-policy} (Read / Write) @br{}
  Whether the spin button should update always, or only when the value is
  legal. @br{}
  Default value: @code{:always}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-update-policy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-update-policy 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-update-policy object) => update-policy}
  @syntax[]{(setf (gtk-spin-button-upadate-policy object) update-policy)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[policy]{a @symbol{gtk-spin-button-update-policy} value}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{update-policy} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button-update-policy} gets the update
  behavior of a spin button. The slot access function
  @sym{(setf gtk-spin-button-update-policy)} sets the update behavior of a spin
  button. This determines wether the spin button is always updated or only when
  a valid value is set.
  @see-class{gtk-spin-button}")

;;; --- gtk-spin-button-value --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-spin-button) 't)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  Reads the current value, or sets a new value. @br{}
  Default value: 0.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-value 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-value object) => value}
  @syntax[]{(setf (gtk-spin-button-value object) value)}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[value]{a @code{:double} with the value of the spin button}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{value} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button-value} gets the value of the
  spin button. The slot access function @sym{(setf gtk-spin-button-value)} sets
  the value of the spin button.
  @see-class{gtk-spin-button}")

;;; --- gtk-spin-button-wrap ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap" 'gtk-spin-button) 't)
 "The @code{wrap} property of type @code{:boolean} (Read / Write) @br{}
  Whether a spin button should wrap upon reaching its limits. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spin-button-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-spin-button-wrap 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-spin-button-value object) => wrap}
  @syntax[]{(setf (gtk-spin-button-value object) wrap)}
  @argument[object]{a @class{gtk-spin-button} widget}
  @argument[wrap]{a boolean indicating if wrapping behavior is performed}
  @begin{short}
    Accessor of the @slot[gtk-spin-button]{wrap} slot of the
    @class{gtk-spin-button} class.
  @end{short}

  The slot access function @sym{gtk-spin-button} returns whether the spin
  button's value wraps around to the opposite limit when the upper or lower
  limit of the range is exceeded. The slot access function
  @sym{(setf gtk-spin-button)} sets the flag that determines if a spin button
  value wraps around to the opposite limit when the upper or lower limit of the
  range is exceeded.
  @see-class{gtk-spin-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_configure" gtk-spin-button-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[adjustment]{a @class{gtk-adjustment}}
  @argument[climb-rate]{a @code{:double} with the climb rate}
  @argument[digits]{an unsigned integer with the number of decimal places to
    display in the spin button}
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
 "@version{2020-5-31}
  @argument[adjustment]{the @class{gtk-adjustment} object that this spin button
    should use, or @code{nil}}
  @argument[climb-rate]{a @code{:double} which specifies how much the spin
    button changes when an arrow is clicked on}
  @argument[digits]{an unsigned integer with the number of decimal places to
    display}
  @return{The new @class{gtk-spin-button} widget.}
  @begin{short}
    Creates a new spin button.
  @end{short}
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
 "@version{2020-5-31}
  @argument[min]{a @code{:double} with the minimum allowable value}
  @argument[max]{a @code{:double} with the maximum allowable value}
  @argument[step]{a @code{:double} with the increment added or subtracted by
    spinning the widget}
  @return{The new @class{gtk-spin-button} widget.}
  @begin{short}
    This is a convenience constructor that allows creation of a numeric
    spin button without manually creating an adjustment.
  @end{short}
  The value is initially set to the minimum value and a page increment of
  10 * @arg{step} is the default. The precision of the spin button is equivalent
  to the precision of @arg{step}.

  Note that the way in which the precision is derived works best if step is a
  power of ten. If the resulting precision is not suitable for your needs, use
  the function @fun{gtk-spin-button-digits} to correct it.
  @see-function{gtk-spin-button}
  @see-function{gtk-spin-button-digits}"
  (min :double)
  (max :double)
  (step :double))

(export 'gtk-spin-button-new-with-range)

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
;;; gtk_spin_button_get_value_as_int () -> gtk-spin-button-value-as-int
;;; ----------------------------------------------------------------------------

(defun gtk-spin-button-value-as-int (spin-button)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @return{The value of @arg{spin-button}.}
  @begin{short}
    Get the value of the spin button represented as an integer.
  @end{short}
  @see-class{gtk-spin-button}
  @see-function{gtk-spin-button-value}"
  (truncate (gtk-spin-button-value spin-button)))

(export 'gtk-spin-button-value-as-int)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_spin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_spin" gtk-spin-button-spin) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @argument[direction]{a @symbol{gtk-spin-type} indicating the direction to
    spin}
  @argument[increment]{a @code{:double} with the step increment to apply in the
    specified direction}
  @begin{short}
    Increment or decrement a spin button's value in a specified direction by a
    specified amount.
  @end{short}
  @see-class{gtk-spin-button}
  @see-symbol{gtk-spin-type}"
  (spin-button (g-object gtk-spin-button))
  (direction gtk-spin-type)
  (increment :double))

(export 'gtk-spin-button-spin)

;;; ----------------------------------------------------------------------------
;;; gtk_spin_button_update ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spin_button_update" gtk-spin-button-update) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[spin-button]{a @class{gtk-spin-button} widget}
  @begin{short}
    Manually force an update of the spin button.
  @end{short}
  @see-class{gtk-spin-button}"
  (spin-button (g-object gtk-spin-button)))

(export 'gtk-spin-button-update)

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

;;; --- End of file gtk.spin-button.lisp ---------------------------------------
