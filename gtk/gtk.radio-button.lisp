;;; ----------------------------------------------------------------------------
;;; gtk.radio-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkRadioButton
;;;
;;;     A choice from multiple check buttons
;;;
;;; Types and Values
;;;
;;;     GtkRadioButton
;;;
;;; Functions
;;;
;;;     gtk_radio_button_new
;;;     gtk_radio_button_new_from_widget
;;;     gtk_radio_button_new_with_label
;;;     gtk_radio_button_new_with_label_from_widget
;;;     gtk_radio_button_new_with_mnemonic
;;;     gtk_radio_button_new_with_mnemonic_from_widget
;;;     gtk_radio_button_set_group
;;;     gtk_radio_button_get_group
;;;     gtk_radio_button_join_group
;;;
;;; Properties
;;;
;;;     GtkRadioButton*   group            Write
;;;
;;; Signals
;;;
;;;               void    group-changed    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkToggleButton
;;;                             ╰── GtkCheckButton
;;;                                 ╰── GtkRadioButton
;;;
;;; Implemented Interfaces
;;;     GtkRadioButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class gtk-radio-button
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRadioButton" gtk-radio-button
  (:superclass gtk-check-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_radio_button_get_type")
  ((group
    gtk-radio-button-group
    "group" "GtkRadioButton" nil t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-radio-button 'type)
 "@version{*2021-12-4}
  @begin{short}
    A single radio button performs the same basic function as a
    @class{gtk-check-button} widget, as its position in the object hierarchy
    reflects.
  @end{short}
  It is only when multiple radio buttons are grouped together that they become
  a different user interface component in their own right.

  @image[radio-group]{}

  Every radio button is a member of some group of radio buttons. When one is
  selected, all other radio buttons in the same group are deselected. A
  @sym{gtk-radio-button} widget is one way of giving the user a choice from
  many options.

  Radio buttons are created with the @fun{gtk-radio-button-new} function,
  passing @code{nil} as the argument if this is the first radio button in a
  group. In subsequent calls, the group you wish to add this button to should
  be passed as an argument. Optionally, the
  @fun{gtk-radio-button-new-with-label} function can be used if you want a text
  label on the radio button.

  Alternatively, when adding widgets to an existing group of radio buttons,
  use the @fun{gtk-radio-button-new-from-widget} function with a
  @sym{gtk-radio-button} widget that already has a group assigned to it. The
  convenience @fun{gtk-radio-button-new-with-label-from-widget} function is
  also provided.

  To retrieve the group a @sym{gtk-radio-button} widget is assigned to, use the
  @fun{gtk-radio-button-get-group} function.

  To remove a @sym{gtk-radio-button} widget from one group and make it part of
  a new one, use the @fun{gtk-radio-button-set-group} function.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 radiobutton
 ├── radio
 ╰── <child>
    @end{pre}
    A @sym{gtk-radio-button} widget with indicator, see the
    @fun{gtk-toggle-button-mode} function, has a main CSS node with name
    @code{radiobutton} and a subnode with name @code{radio}.
    @begin{pre}
 button.radio
 ├── radio
 ╰── <child>
    @end{pre}
    A @sym{gtk-radio-button} implementation without indicator changes the name
    of its main node to @code{button} and adds a @code{.radio} style class to
    it. The subnode is invisible in this case.
  @end{dictionary}
  @begin[Example]{dictionary}
    How to create a group of two radio buttons.
    @begin{pre}
(defun example-radio-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title \"Example Radio Button\"
                                 :border-width 12
                                 :default-width 300
                                 :default-height 120))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :halign :center
                               :valign :center
                               :row-spacing 18)))

      (g-signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (let ((radio (gtk-radio-button-new nil)))
        (gtk-container-add radio (gtk-entry-new))
        (gtk-container-add grid radio)
        (setf radio
              (gtk-radio-button-new-with-label-from-widget radio
                                                           \"Second Button\"))
        (gtk-container-add grid radio))

      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
    @end{pre}
    When an unselected radio button in the group is clicked the clicked radio
    button receives the \"toggled\" signal, as does the previously selected
    radio button. Inside the \"toggled\" handler, the
    @fun{gtk-toggle-button-active} function can be used to determine if the
    button has been selected or deselected.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"group-changed\" signal}
      @begin{pre}
 lambda (button)    :run-first
      @end{pre}
      Emitted when the group of radio buttons that a radio button belongs to
      changes. This is emitted when a radio button switches from being alone to
      being part of a group of two or more buttons, or vice-versa, and when a
      button is moved from one group of two or more buttons to a different one,
      but not when the composition of the group that a button belongs to
      changes.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-radio-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-radio-button-group}
  @see-class{gtk-button}
  @see-class{gtk-toggle-button}
  @see-class{gtk-check-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group" 'gtk-radio-button) 't)
 "The @code{group} property of type @sym{gtk-radio-button} (Write) @br{}
  Sets a new group for a radio button.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-button-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-button-group 'function)
 "@version{2021-12-4}
  @begin{short}
    Accessor of the @slot[gtk-radio-button]{group} slot of the
    @class{gtk-radio-button} class.
  @end{short}

  Sets a new group for a radio button.
  @see-class{gtk-radio-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new" gtk-radio-button-new)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-4}
  @argument[group]{an existing @class{gtk-radio-button} group, or @code{nil} if
    you are creating a new group}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new radio button.
  @end{short}
  To be of any practical value, a widget should then be packed into the radio
  button.
  @see-class{gtk-radio-button}"
  (group (g-slist (g-object gtk-radio-button))))

(export 'gtk-radio-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_from_widget" gtk-radio-button-new-from-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-4}
  @argument[member]{an existing @class{gtk-radio-button} widget}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new radio button, adding it to the same group as @arg{member}.
  @end{short}
  As with the @fun{gtk-radio-button-new} function, a widget should be packed
  into the radio button.
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-new}"
  (member (g-object gtk-radio-button)))

(export 'gtk-radio-button-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label" gtk-radio-button-new-with-label)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-4}
  @argument[group]{an existing @class{gtk-radio-button} group, or @code{nil} if
    you are creating a new group}
  @argument[label]{a string with the text label to display next to the radio
    button}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new radio button with a text label.
  @end{short}
  @see-class{gtk-radio-button}"
  (group (g-slist (g-object gtk-radio-button)))
  (label :string))

(export 'gtk-radio-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label_from_widget"
           gtk-radio-button-new-with-label-from-widget) (g-object widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-4}
  @argument[member]{a @class{gtk-radio-button} widget to get the radio group
    from or @code{nil}}
  @argument[label]{a text string to display next to the radio button}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new radio button with a text label, adding it to the same group
    as @arg{member}.
  @end{short}
  @see-class{gtk-radio-button}"
  (member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-label-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic"
           gtk-radio-button-new-with-mnemonic) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-4}
  @argument[group]{the @class{gtk-radio-button} group}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new radio button containing a label, adding it to the same group
    as group.
  @end{short}
  The label will be created using the @fun{gtk-label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the button.
  @see-class{gtk-radio-button}
  @see-function{gtk-label-new-with-mnemonic}"
  (group (g-slist (g-object gtk-radio-button)))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic_from_widget"
           gtk-radio-button-new-with-mnemonic-from-widget) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-4}
  @argument[member]{a @class{gtk-radio-button} widget to get radio group from or
    @code{nil}}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new radio button containing a label.
  @end{short}
  The label will be created using the @fun{gtk-label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the button.
  @see-class{gtk-radio-button}
  @see-function{gtk-label-new-with-mnemonic}"
  (member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_set_group ()
;;; ----------------------------------------------------------------------------

;;; TODO: Check the implementation of gtk-radio-button-set-group and
;;; gtk-radio-button-get-group.

(defun gtk-radio-button-set-group (button group)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-4}
  @argument[button]{a @class{gtk-radio-button} widget}
  @argument[group]{an existing @class{gtk-radio-button} group, such as one
    returned from the @fun{gtk-radio-button-get-group} function}
  @begin{short}
    Sets a the group of the radio button.
  @end{short}
  It should be noted that this does not change the layout of your interface in
  any way, so if you are changing the group, it is likely you will need to
  re-arrange the user interface to reflect these changes.
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-get-group}"
  (setf (gtk-radio-button-group button) group))

(export 'gtk-radio-button-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_get_group ()
;;; ----------------------------------------------------------------------------

;;; TODO: Check the implementation of gtk-radio-button-set-group and
;;; gtk-radio-button-get-group.

(defcfun ("gtk_radio_button_get_group" gtk-radio-button-get-group)
    (g-slist (g-object gtk-radio-button) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-4}
  @argument[button]{a @class{gtk-radio-button} widget}
  @begin{return}
    A list containing all the @class{gtk-radio-button} widgets in the same
    group as @arg{button}.
  @end{return}
  @begin{short}
    Retrieves the group assigned to a radio button.
  @end{short}
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-set-group}"
  (button (g-object gtk-radio-button)))

(export 'gtk-radio-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_join_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_join_group" gtk-radio-button-join-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-4}
  @argument[button]{a @class{gtk-radio-button} widget}
  @argument[group]{a @class{gtk-radio-button} widget whose group we are joining,
    or @code{nil} to remove the radio button from its group}
  @begin{short}
    Joins a radio button to the group of another radio button widget.
  @end{short}
  Use this in language bindings instead of the the
  @fun{gtk-radio-button-get-group} and @fun{gtk-radio-button-set-group}
  functions.
  @begin[Example]{dictionary}
    A common way to set up a group of radio buttons with a label is the
    following:
    @begin{pre}
(let (button lastbutton)
  ;; Add three buttons to a group
  (dolist (label '(\"First Button\" \"Second Button\" \"Third Button\"))
    (setf button (gtk-radio-button-new-with-label nil label))
    (gtk-radio-button-join-group button lastbutton)
    (setf lastbutton button)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-get-group}
  @see-function{gtk-radio-button-set-group}"
  (button (g-object gtk-radio-button))
  (group (g-object gtk-radio-button)))

(export 'gtk-radio-button-join-group)

;;; --- End of file gtk.radio-button.lisp --------------------------------------
