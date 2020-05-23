;;; ----------------------------------------------------------------------------
;;; gtk.radio-button.lisp
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
;;;     GtkRadioButton*  group    Write
;;;
;;; Signals
;;;
;;;     void  group-changed    Run First
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
 "@version{2014-11-9}
  @begin{short}
    A single radio button performs the same basic function as a
    @class{gtk-check-button}, as its position in the object hierarchy reflects.
    It is only when multiple radio buttons are grouped together that they
    become a different user interface component in their own right.
  @end{short}

  @image[radio-group]{}

  Every radio button is a member of some group of radio buttons. When one is
  selected, all other radio buttons in the same group are deselected. A
  @sym{gtk-radio-button} is one way of giving the user a choice from many
  options.

  Radio button widgets are created with @fun{gtk-radio-button-new}, passing
  @code{nil} as the argument if this is the first radio button in a group. In
  subsequent calls, the group you wish to add this button to should be passed
  as an argument. Optionally, the function @fun{gtk-radio-button-new-with-label}
  can be used if you want a text label on the radio button.

  Alternatively, when adding widgets to an existing group of radio buttons,
  use the function @fun{gtk-radio-button-new-from-widget} with a
  @sym{gtk-radio-button} that already has a group assigned to it. The
  convenience function @fun{gtk-radio-button-new-with-label-from-widget} is
  also provided.

  To retrieve the group a @sym{gtk-radio-button} is assigned to, use the
  function @fun{gtk-radio-button-get-group}.

  To remove a @sym{gtk-radio-button} from one group and make it part of a new
  one, use the function @fun{gtk-radio-button-set-group}.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 radiobutton
 ├── radio
 ╰── <child>
    @end{pre}
    A @sym{gtk-radio-button} with indicator, see the function
    @fun{gtk-toggle-button-mode}, has a main CSS node with name
    @code{radiobutton} and a subnode with name @code{radio}.
    @begin{pre}
 button.radio
 ├── radio
 ╰── <child>
    @end{pre}
    A @sym{gtk-radio-button} without indicator changes the name of its main node
    to @code{button} and adds a @code{.radio} style class to it. The subnode is
    invisible in this case.
  @end{dictionary}
  @begin[Example]{dictionary}
    How to create a group of two radio buttons.
    @begin{pre}
 void create_radio_buttons (void) {

    GtkWidget *window, *radio1, *radio2, *box, *entry;
    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, TRUE, 2);

    /* Create a radio button with a GtkEntry widget */
    radio1 = gtk_radio_button_new (NULL);
    entry = gtk_entry_new ();
    gtk_container_add (GTK_CONTAINER (radio1), entry);


    /* Create a radio button with a label */
    radio2 = gtk_radio_button_new_with_label_from_widget
                                            (GTK_RADIO_BUTTON (radio1),
                                             \"I'm the second radio button.\");

    /* Pack them into a box, then show all the widgets */
    gtk_box_pack_start (GTK_BOX (box), radio1, TRUE, TRUE, 2);
    gtk_box_pack_start (GTK_BOX (box), radio2, TRUE, TRUE, 2);
    gtk_container_add (GTK_CONTAINER (window), box);
    gtk_widget_show_all (window);
    return;
 @}
    @end{pre}
    When an unselected button in the group is clicked the clicked button
    receives the \"toggled\" signal, as does the previously selected button.
    Inside the \"toggled\" handler, the @fun{gtk-toggle-button-active} function
    can be used to determine if the button has been selected or deselected.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"group-changed\" signal}
      @begin{pre}
 lambda (button)    : Run First
      @end{pre}
      Emitted when the group of radio buttons that a radio button belongs to
      changes. This is emitted when a radio button switches from being alone to
      being part of a group of 2 or more buttons, or vice-versa, and when a
      button is moved from one group of 2 or more buttons to a different one,
      but not when the composition of the group that a button belongs to
      changes.
      @begin[code]{table}
        @entry[button]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-radio-button-group}
  @see-class{gtk-combo-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group" 'gtk-radio-button) 't)
 "The @code{group} property of type @sym{gtk-radio-button} (Write) @br{}
  Sets a new group for a radio button.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-button-group atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-radio-button-group 'function)
 "@version{2014-11-9}
  Accessor of the slot @slot[gtk-radio-button]{group} of the
  @class{gtk-radio-button} class.
  @see-class{gtk-radio-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new" gtk-radio-button-new)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[group]{an existing radio button group, or @code{nil} if you are
    creating a new group}
  @return{A new radio button widget.}
  @begin{short}
    Creates a new @class{gtk-radio-button}.
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
 "@version{2014-11-9}
  @argument[radio-group-member]{an existing @class{gtk-radio-button} widget}
  @return{A new radio button widget.}
  @begin{short}
    Creates a new @class{gtk-radio-button} widget, adding it to the same group
    as @arg{radio-group-member}.
  @end{short}
  As with the function @fun{gtk-radio-button-new}, a widget should be packed
  into the radio button.
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-new}"
  (radio-group-member (g-object gtk-radio-button)))

(export 'gtk-radio-button-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label" gtk-radio-button-new-with-label)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[group]{an existing radio button group, or @code{nil} if you are
    creating a new group}
  @argument[label]{the text label to display next to the radio button}
  @return{A new radio button widget.}
  Creates a new @class{gtk-radio-button} with a text label.
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
 "@version{2014-11-9}
  @argument[radio-group-member]{widget to get radio group from or @code{nil}}
  @argument[label]{a text string to display next to the radio button}
  @return{A new radio button widget.}
  Creates a new @class{gtk-radio-button} with a text label, adding it to the
  same group as @arg{radio-group-member}.
  @see-class{gtk-radio-button}"
  (radio-group-member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-label-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic"
           gtk-radio-button-new-with-mnemonic) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[group]{the radio button group}
  @argument[label]{the text of the button, with an underscore in front of the
    mnemonic character}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new @class{gtk-radio-button} widget containing a label, adding it
    to the same group as group.
  @end{short}
  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in label indicate the
  mnemonic for the button.
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
 "@version{2014-9-11}
  @argument[radio-group-member]{widget to get radio group from or @code{nil}}
  @argument[label]{the text of the button, with an underscore in front of the
    mnemonic character}
  @return{A new @class{gtk-radio-button} widget.}
  @begin{short}
    Creates a new @class{gtk-radio-button} widget containing a label.
  @end{short}
  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in label indicate the
  mnemonic for the button.
  @see-class{gtk-radio-button}
  @see-function{gtk-label-new-with-mnemonic}"
  (radio-group-member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_set_group ()
;;; ----------------------------------------------------------------------------

;;; TODO: Check the implementation of gtk-radio-button-set-group and
;;; gtk-radio-button-get-group. How can this be combined in a generic function?

(declaim (inline gtk-radio-button-set-group))

(defun gtk-radio-button-set-group (radio-button group)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[radio-button]{a @class{gtk-radio-button} widget}
  @argument[group]{an existing radio button group, such as one returned from
    the function @fun{gtk-radio-button-get-group}}
  @begin{short}
    Sets a @class{gtk-radio-button}'s group.
  @end{short}
  It should be noted that this does not change the layout of your interface in
  any way, so if you are changing the group, it is likely you will need to
  re-arrange the user interface to reflect these changes.
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-get-group}"
  (setf (gtk-radio-button-group radio-button) group))

(export 'gtk-radio-button-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_get_group ()
;;; ----------------------------------------------------------------------------

;;; TODO: Check the implementation of gtk-radio-button-set-group and
;;; gtk-radio-button-get-group. How can this be combined in a generic function?

(defcfun ("gtk_radio_button_get_group" gtk-radio-button-get-group)
    (g-slist (g-object gtk-radio-button) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[radio-button]{a @class{gtk-radio-button} widget}
  @begin{return}
    A linked list containing all the radio buttons in the same group as
    @arg{radio-button}.
  @end{return}
  Retrieves the group assigned to a radio button.
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-set-group}"
  (radio-button (g-object gtk-radio-button)))

(export 'gtk-radio-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_join_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_join_group" gtk-radio-button-join-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[radio-button]{the @class{gtk-radio-button} widget}
  @argument[group-source]{a radio button object whose group we are joining, or
    @code{nil} to remove the radio button from its group}
  @begin{short}
    Joins a @class{gtk-radio-button} object to the group of another
    @class{gtk-radio-button} object.
  @end{short}

  Use this in language bindings instead of the the functions
  @fun{gtk-radio-button-get-group} and @fun{gtk-radio-button-set-group} methods.

  A common way to set up a group of radio buttons is the following:
  @begin{pre}
 GtkRadioButton *radio_button;
 GtkRadioButton *last_button;

 while (/* more buttons to add */)
   {
      radio_button = gtk_radio_button_new (...);

      gtk_radio_button_join_group (radio_button, last_button);
      last_button = radio_button;
   @}
  @end{pre}
  @see-class{gtk-radio-button}
  @see-function{gtk-radio-button-get-group}
  @see-function{gtk-radio-button-set-group}"
  (radio-button (g-object radio-button))
  (group-source (g-slist (g-object radio-button))))

(export 'gtk-radio-button-join-group)

;;; --- End of file gtk.radio-button.lisp --------------------------------------
