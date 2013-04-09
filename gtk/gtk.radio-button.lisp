;;; ----------------------------------------------------------------------------
;;; gtk.radio-button.lisp
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-radio-button 'type)
 "@version{2013-2-4}
  @begin{short}
    A single radio button performs the same basic function as a
    @class{gtk-check-button}, as its position in the object hierarchy reflects.
    It is only when multiple radio buttons are grouped together that they become
    a different user interface component in their own right.
  @end{short}

  Every radio button is a member of some group of radio buttons. When one is
  selected, all other radio buttons in the same group are deselected. A
  @sym{gtk-radio-button} is one way of giving the user a choice from many
  options.

  Radio button widgets are created with @fun{gtk-radio-button-new}, passing
  @code{nil} as the argument if this is the first radio button in a group. In
  subsequent calls, the group you wish to add this button to should be passed as
  an argument. Optionally, @fun{gtk-radio-button-new-with-label} can be used if
  you want a text label on the radio button.

  Alternatively, when adding widgets to an existing group of radio buttons,
  use @fun{gtk-radio-button-new-from-widget} with a @sym{gtk-radio-button} that
  already has a group assigned to it. The convenience function
  @fun{gtk-radio-button-new-with-label-from-widget} is also provided.

  To retrieve the group a @sym{gtk-radio-button} is assigned to, use
  @fun{gtk-radio-button-get-group}.

  To remove a @sym{gtk-radio-button} from one group and make it part of a new
  one, use @fun{gtk-radio-button-set-group}.

  The group list does not need to be freed, as each @sym{gtk-radio-button} will
  remove itself and its list item when it is destroyed.

  Example 53. How to create a group of two radio buttons.
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
  Inside the \"toggled\" handler, @fun{gtk-toggle-button-get-active} can be used
  to determine if the button has been selected or deselected.
  @begin[Signal Details]{dictionary}
    @subheading{The \"group-changed\" signal}
      Emitted when the group of radio buttons that a radio button belongs to
      changes. This is emitted when a radio button switches from being alone to
      being part of a group of 2 or more buttons, or vice-versa, and when a
      button is moved from one group of 2 or more buttons to a different one,
      but not when the composition of the group that a button belongs to
      changes.
      @begin{pre}
 void user_function (GtkRadioButton *button, gpointer user_data)  : Run First
      @end{pre}
      @begin[code]{table}
        @entry[button]{the object which received the signal}
        @entry[user_data]{user data set when the signal handler was connected}
      @end{table}
      Since 2.4
  @end{dictionary}
  @see-slot{gtk-radio-button-group}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group" 'gtk-radio-button) 't)
 "The @code{\"group\"} property of type @sym{gtk-radio-button} (Write)@br{}
  Sets a new group for a radio button.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-button-group atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-radio-button-group 'function)
 "@version{2013-2-4}
  @begin{short}
    Accessor of the slot \"group\" of the @class{gtk-radio-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new" gtk-radio-button-new)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[group]{an existing radio button group, or NULL if you are creating
    a new group}
  @return{a new radio button}
  Creates a new GtkRadioButton. To be of any practical value, a widget should
  then be packed into the radio button."
  (group (g-slist (g-object gtk-radio-button))))

(export 'gtk-radio-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_from_widget" gtk-radio-button-new-from-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[radio_group_member]{an existing GtkRadioButton}
  @return{a new radio button}
  Creates a new GtkRadioButton, adding it to the same group as
  radio_group_member. As with gtk_radio_button_new(), a widget should be
  packed into the radio button."
  (radio-group-member (g-object gtk-radio-button)))

(export 'gtk-radio-button-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label" gtk-radio-button-new-with-label)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[group]{an existing radio button group, or NULL if you are creating
    a new group}
  @argument[label]{the text label to display next to the radio button}
  @return{a new radio button}
  Creates a new GtkRadioButton with a text label."
  (group (g-slist (g-object gtk-radio-button)))
  (label :string))

(export 'gtk-radio-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label_from_widget"
          gtk-radio-button-new-with-label-from-widget) (g-object widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[radio_group_member]{widget to get radio group from or NULL}
  @argument[label]{a text string to display next to the radio button}
  @return{a new radio button}
  Creates a new GtkRadioButton with a text label, adding it to the same group
  as radio_group_member."
  (radio-group-member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-label-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic"
          gtk-radio-button-new-with-mnemonic) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[group]{the radio button group}
  @argument[label]{the text of the button, with an underscore in front of the
    mnemonic character}
  @return{a new GtkRadioButton}
  Creates a new GtkRadioButton containing a label, adding it to the same group
  as group. The label will be created using gtk_label_new_with_mnemonic(), so
  underscores in label indicate the mnemonic for the button."
  (group (g-slist (g-object gtk-radio-button)))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic_from_widget"
          gtk-radio-button-new-with-mnemonic-from-widget) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[radio_group_member]{widget to get radio group from or NULL}
  @argument[label]{the text of the button, with an underscore in front of the
    mnemonic character}
  @return{a new GtkRadioButton}
  Creates a new GtkRadioButton containing a label. The label will be created
  using gtk_label_new_with_mnemonic(), so underscores in label indicate the
  mnemonic for the button."
  (radio-group-member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_set_group ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-radio-button-set-group))

(defun gtk-radio-button-set-group (radio-button group)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[radio_button]{a GtkRadioButton}
  @argument[group]{an existing radio button group, such as one returned from
    gtk_radio_button_get_group()}
  Sets a GtkRadioButton's group. It should be noted that this does not change
  the layout of your interface in any way, so if you are changing the group,
  it is likely you will need to re-arrange the user interface to reflect these
  changes."
  (setf (gtk-radio-button-group radio-button) group))

(export 'gtk-radio-button-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_get_group" gtk-radio-button-get-group)
    (g-slist (g-object gtk-radio-button))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[radio_button]{a GtkRadioButton}
  @begin{return}
    a linked list containing all the radio buttons in the same group as
    radio_button. The returned list is owned by the radio button and must
    not be modified or freed
  @end{return}
  Retrieves the group assigned to a radio button."
  (radio-button (g-object gtk-radio-button)))

(export 'gtk-radio-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_join_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_join_group" gtk-radio-button-join-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[radio_button]{the GtkRadioButton object}
  @argument[group_source]{a radio button object whos group we are joining, or
    NULL to remove the radio button from its group}
  @begin{short}
    Joins a GtkRadioButton object to the group of another GtkRadioButton object
  @end{short}

  Use this in language bindings instead of the gtk_radio_button_get_group()
  and gtk_radio_button_set_group() methods

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
  Since 3.0"
  (radio-button (g-object radio-button))
  (group-source (g-slist (g-object radio-button))))

(export 'gtk-radio-button-join-group)

;;; --- End of file gtk.radio-button.lisp --------------------------------------
