;;; ----------------------------------------------------------------------------
;;; gtk.radio-button.lisp
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
;;; GtkRadioButton
;;; 
;;; A choice from multiple check buttons
;;; 	
;;; Synopsis
;;; 
;;;     GtkRadioButton
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
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkToggleButton
;;;                                        +----GtkCheckButton
;;;                                              +----GtkRadioButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkRadioButton implements AtkImplementorIface, GtkBuildable
;;; and GtkActivatable.
;;;
;;; Properties
;;; 
;;;   "group"                    GtkRadioButton*       : Write
;;; 
;;; Signals
;;; 
;;;   "group-changed"                                  : Run First
;;; 
;;; Description
;;; 
;;; A single radio button performs the same basic function as a GtkCheckButton,
;;; as its position in the object hierarchy reflects. It is only when multiple
;;; radio buttons are grouped together that they become a different user
;;; interface component in their own right.
;;; 
;;; Every radio button is a member of some group of radio buttons. When one is
;;; selected, all other radio buttons in the same group are deselected. A
;;; GtkRadioButton is one way of giving the user a choice from many options.
;;; 
;;; Radio button widgets are created with gtk_radio_button_new(), passing NULL
;;; as the argument if this is the first radio button in a group. In subsequent
;;; calls, the group you wish to add this button to should be passed as an
;;; argument. Optionally, gtk_radio_button_new_with_label() can be used if you
;;; want a text label on the radio button.
;;; 
;;; Alternatively, when adding widgets to an existing group of radio buttons,
;;; use gtk_radio_button_new_from_widget() with a GtkRadioButton that already
;;; has a group assigned to it. The convenience function
;;; gtk_radio_button_new_with_label_from_widget() is also provided.
;;; 
;;; To retrieve the group a GtkRadioButton is assigned to, use
;;; gtk_radio_button_get_group().
;;; 
;;; To remove a GtkRadioButton from one group and make it part of a new one,
;;; use gtk_radio_button_set_group().
;;; 
;;; The group list does not need to be freed, as each GtkRadioButton will
;;; remove itself and its list item when it is destroyed.
;;; 
;;; Example 53. How to create a group of two radio buttons.
;;; 
;;; void create_radio_buttons (void) {
;;; 
;;;    GtkWidget *window, *radio1, *radio2, *box, *entry;
;;;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, TRUE, 2);
;;; 
;;;    /* Create a radio button with a GtkEntry widget */
;;;    radio1 = gtk_radio_button_new (NULL);
;;;    entry = gtk_entry_new ();
;;;    gtk_container_add (GTK_CONTAINER (radio1), entry);
;;; 
;;; 
;;;    /* Create a radio button with a label */
;;;    radio2 = gtk_radio_button_new_with_label_from_widget
;;;                                             (GTK_RADIO_BUTTON (radio1),
;;;                                              "I'm the second radio button.")
;;; 
;;;    /* Pack them into a box, then show all the widgets */
;;;    gtk_box_pack_start (GTK_BOX (box), radio1, TRUE, TRUE, 2);
;;;    gtk_box_pack_start (GTK_BOX (box), radio2, TRUE, TRUE, 2);
;;;    gtk_container_add (GTK_CONTAINER (window), box);
;;;    gtk_widget_show_all (window);
;;;    return;
;;; }
;;; 
;;; When an unselected button in the group is clicked the clicked button
;;; receives the "toggled" signal, as does the previously selected button.
;;; Inside the "toggled" handler, gtk_toggle_button_get_active() can be used
;;; to determine if the button has been selected or deselected.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "group" property
;;; 
;;;   "group"                    GtkRadioButton*       : Write
;;; 
;;; Sets a new group for a radio button.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "group-changed" signal
;;; 
;;; void user_function (GtkRadioButton *button, gpointer user_data)  : Run First
;;; 
;;; Emitted when the group of radio buttons that a radio button belongs to
;;; changes. This is emitted when a radio button switches from being alone to
;;; being part of a group of 2 or more buttons, or vice-versa, and when a
;;; button is moved from one group of 2 or more buttons to a different one, but
;;; not when the composition of the group that a button belongs to changes.
;;; 
;;; button :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioButton
;;; 
;;; struct GtkRadioButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRadioButton" gtk-radio-button
  (:superclass gtk-check-button
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_radio_button_get_type")
  ((:cffi group
          gtk-radio-button-group
          (g-slist g-object :free-from-foreign nil :free-to-foreign nil)
          "gtk_radio_button_get_group"
          "gtk_radio_button_set_group")))

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new ()
;;; 
;;; GtkWidget * gtk_radio_button_new (GSList *group);
;;; 
;;; Creates a new GtkRadioButton. To be of any practical value, a widget should
;;; then be packed into the radio button.
;;; 
;;; group :
;;;     an existing radio button group, or NULL if you are creating a new group.
;;; 
;;; Returns :
;;;     a new radio button
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new" gtk-radio-button-new)
    (g-object gtk-widget)
  (group (g-slist (g-object gtk-radio-button))))

(export 'gtk-radio-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_from_widget ()
;;; 
;;; GtkWidget * gtk_radio_button_new_from_widget
;;;                                         (GtkRadioButton *radio_group_member)
;;; 
;;; Creates a new GtkRadioButton, adding it to the same group as
;;; radio_group_member. As with gtk_radio_button_new(), a widget should be
;;; packed into the radio button.
;;; 
;;; radio_group_member :
;;;     an existing GtkRadioButton.
;;; 
;;; Returns :
;;;     a new radio button.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_from_widget" gtk-radio-button-new-from-widget)
    (g-object gtk-widget)
  (radio-group-member (g-object gtk-radio-button)))

(export 'gtk-radio-button-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label ()
;;; 
;;; GtkWidget * gtk_radio_button_new_with_label (GSList *group,
;;;                                              const gchar *label);
;;; 
;;; Creates a new GtkRadioButton with a text label.
;;; 
;;; group :
;;;     an existing radio button group, or NULL if you are creating a new group.
;;; 
;;; label :
;;;     the text label to display next to the radio button.
;;; 
;;; Returns :
;;;     a new radio button.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label" gtk-radio-button-new-with-label)
    (g-object gtk-widget)
  (group (g-slist (g-object gtk-radio-button)))
  (label :string))

(export 'gtk-radio-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_label_from_widget ()
;;; 
;;; GtkWidget * gtk_radio_button_new_with_label_from_widget
;;;                                         (GtkRadioButton *radio_group_member,
;;;                                          const gchar *label);
;;; 
;;; Creates a new GtkRadioButton with a text label, adding it to the same group
;;; as radio_group_member.
;;; 
;;; radio_group_member :
;;;     widget to get radio group from or NULL.
;;; 
;;; label :
;;;     a text string to display next to the radio button.
;;; 
;;; Returns :
;;;     a new radio button.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_label_from_widget"
          gtk-radio-button-new-with-label-from-widget) (g-object widget)
  (radio-group-member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-label-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic ()
;;; 
;;; GtkWidget * gtk_radio_button_new_with_mnemonic  (GSList *group,
;;;                                                  const gchar *label);
;;; 
;;; Creates a new GtkRadioButton containing a label, adding it to the same
;;; group as group. The label will be created using
;;; gtk_label_new_with_mnemonic(), so underscores in label indicate the
;;; mnemonic for the button.
;;; 
;;; group :
;;;     the radio button group.
;;; 
;;; label :
;;;     the text of the button, with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;;     a new GtkRadioButton
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic"
          gtk-radio-button-new-with-mnemonic) (g-object gtk-widget)
  (group (g-slist (g-object gtk-radio-button)))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_new_with_mnemonic_from_widget ()
;;; 
;;; GtkWidget * gtk_radio_button_new_with_mnemonic_from_widget
;;;                                         (GtkRadioButton *radio_group_member,
;;;                                          const gchar *label);
;;; 
;;; Creates a new GtkRadioButton containing a label. The label will be created
;;; using gtk_label_new_with_mnemonic(), so underscores in label indicate the
;;; mnemonic for the button.
;;; 
;;; radio_group_member :
;;;     widget to get radio group from or NULL.
;;; 
;;; label :
;;;     the text of the button, with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;;     a new GtkRadioButton.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_button_new_with_mnemonic_from_widget"
          gtk-radio-button-new-with-mnemonic-from-widget) (g-object gtk-widget)
  (radio-group-member (g-object gtk-radio-button))
  (label :string))

(export 'gtk-radio-button-new-with-mnemonic-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_set_group ()
;;; 
;;; void gtk_radio_button_set_group (GtkRadioButton *radio_button,
;;;                                  GSList *group);
;;; 
;;; Sets a GtkRadioButton's group. It should be noted that this does not change
;;; the layout of your interface in any way, so if you are changing the group,
;;; it is likely you will need to re-arrange the user interface to reflect
;;; these changes.
;;; 
;;; radio_button :
;;;     a GtkRadioButton.
;;; 
;;; group :
;;;     an existing radio button group, such as one returned from
;;;     gtk_radio_button_get_group()
;;; ----------------------------------------------------------------------------

(defun gtk-radio-button-set-group (radio-button group)
  (setf (gtk-radio-button-group radio-button) group))

(export 'gtk-radio-button-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_get_group ()
;;; 
;;; GSList * gtk_radio_button_get_group (GtkRadioButton *radio_button);
;;; 
;;; Retrieves the group assigned to a radio button.
;;; 
;;; radio_button :
;;;     a GtkRadioButton.
;;; 
;;; Returns :
;;;     a linked list containing all the radio buttons in the same group as
;;;     radio_button. The returned list is owned by the radio button and must
;;;     not be modified or freed
;;; ----------------------------------------------------------------------------

(defun gtk-radio-button-get-group (radio-button)
  (gtk-radio-button-group radio-button))

(export 'gtk-radio-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_button_join_group ()
;;; 
;;; void gtk_radio_button_join_group (GtkRadioButton *radio_button,
;;;                                   GtkRadioButton *group_source);
;;; 
;;; Joins a GtkRadioButton object to the group of another GtkRadioButton object
;;; 
;;; Use this in language bindings instead of the gtk_radio_button_get_group()
;;; and gtk_radio_button_set_group() methods
;;; 
;;; A common way to set up a group of radio buttons is the following:
;;; 
;;; GtkRadioButton *radio_button;
;;; GtkRadioButton *last_button;
;;; 
;;; while (/* more buttons to add */)
;;;   {
;;;      radio_button = gtk_radio_button_new (...);
;;; 
;;;      gtk_radio_button_join_group (radio_button, last_button);
;;;      last_button = radio_button;
;;;   }
;;; 
;;; radio_button :
;;;     the GtkRadioButton object
;;; 
;;; group_source :
;;;     a radio button object whos group we are joining, or NULL to remove the
;;;     radio button from its group.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;(defcfun ("gtk_radio_button_join_group" gtk-radio-button-join-group) :void
;  (radio-button (g-object radio-button))
;  (group-source (g-slist (g-object radio-button))))

;(export 'gtk-radio-button-join-group)

;;; --- End of file gtk.radio-button.lisp --------------------------------------
