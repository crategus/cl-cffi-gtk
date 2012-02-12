;;; ----------------------------------------------------------------------------
;;; gtk.toggle-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkToggleButton
;;; 
;;; Create buttons which retain their state
;;; 
;;; Synopsis
;;; 
;;;     GtkToggleButton
;;;
;;;     gtk_toggle_button_new
;;;     gtk_toggle_button_new_with_label
;;;     gtk_toggle_button_new_with_mnemonic
;;;     gtk_toggle_button_set_mode
;;;     gtk_toggle_button_get_mode
;;;     gtk_toggle_button_toggled
;;;     gtk_toggle_button_get_active
;;;     gtk_toggle_button_set_active
;;;     gtk_toggle_button_get_inconsistent
;;;     gtk_toggle_button_set_inconsistent
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
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkToggleButton implements AtkImplementorIface, GtkBuildable and
;;; GtkActivatable.
;;;
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;;   "draw-indicator"           gboolean              : Read / Write
;;;   "inconsistent"             gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "toggled"                                        : Run First
;;; 
;;; Description
;;; 
;;; A GtkToggleButton is a GtkButton which will remain 'pressed-in' when
;;; clicked. Clicking again will cause the toggle button to return to its
;;; normal state.
;;; 
;;; A toggle button is created by calling either gtk_toggle_button_new() or
;;; gtk_toggle_button_new_with_label(). If using the former, it is advisable to
;;; pack a widget, (such as a GtkLabel and/or a GtkPixmap), into the toggle
;;; button's container. (See GtkButton for more information).
;;; 
;;; The state of a GtkToggleButton can be set specifically using
;;; gtk_toggle_button_set_active(), and retrieved using
;;; gtk_toggle_button_get_active().
;;; 
;;; To simply switch the state of a toggle button, use
;;; gtk_toggle_button_toggled().
;;; 
;;; Example 54. Creating two GtkToggleButton widgets.
;;; 
;;; void make_toggles (void) {
;;;    GtkWidget *dialog, *toggle1, *toggle2;
;;; 
;;;    dialog = gtk_dialog_new ();
;;;    toggle1 = gtk_toggle_button_new_with_label ("Hi, i'm a toggle button.");
;;; 
;;;    // Makes this toggle button invisible
;;;    gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (toggle1), TRUE);
;;; 
;;;    g_signal_connect (toggle1, "toggled",
;;;                      G_CALLBACK (output_state), NULL);
;;;    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
;;;                        toggle1, FALSE, FALSE, 2);
;;; 
;;;    toggle2 = gtk_toggle_button_new_with_label ("Hi, i'm another toggle button.");
;;;    gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (toggle2), FALSE);
;;;    g_signal_connect (toggle2, "toggled",
;;;                      G_CALLBACK (output_state), NULL);
;;;    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
;;;                        toggle2, FALSE, FALSE, 2);
;;; 
;;;    gtk_widget_show_all (dialog);
;;; }
;;;  
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; If the toggle button should be pressed in.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw-indicator" property
;;; 
;;;   "draw-indicator"           gboolean              : Read / Write
;;; 
;;; If the toggle part of the button is displayed.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "inconsistent" property
;;; 
;;;   "inconsistent"             gboolean              : Read / Write
;;; 
;;; If the toggle button is in an "in between" state.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggled" signal
;;; 
;;; void user_function (GtkToggleButton *togglebutton,
;;;                     gpointer user_data)                : Run First
;;; 
;;; Should be connected if you wish to perform an action whenever the
;;; GtkToggleButton's state is changed.
;;; 
;;; togglebutton :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleButton
;;; 
;;; struct GtkToggleButton;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToggleButton" 'gtk-toggle-button))

(define-g-object-class "GtkToggleButton" gtk-toggle-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_toggle_button_get_type")
  ((active gtk-toggle-button-active
    "active" "gboolean" t t)
   (draw-indicator gtk-toggle-button-draw-indicator
    "draw-indicator" "gboolean" t t)
   (inconsistent gtk-toggle-button-inconsistent
    "inconsistent" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new ()
;;; 
;;; GtkWidget * gtk_toggle_button_new (void);
;;; 
;;; Creates a new toggle button. A widget should be packed into the button, as
;;; in gtk_button_new().
;;; 
;;; Returns :
;;;     a new toggle button.
;;; ----------------------------------------------------------------------------

(defun gtk-toggle-button-new ()
  (make-instance 'gtk-toggle-button))

(export 'gtk-toggle-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new_with_label ()
;;; 
;;; GtkWidget * gtk_toggle_button_new_with_label (const gchar *label);
;;; 
;;; Creates a new toggle button with a text label.
;;; 
;;; label :
;;;     a string containing the message to be placed in the toggle button.
;;; 
;;; Returns :
;;;     a new toggle button.
;;; ----------------------------------------------------------------------------

(defun gtk-toggle-button-new-with-label (label)
  (make-instance 'gtk-toggle-button :label label))

(export 'gtk-toggle-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new_with_mnemonic ()
;;; 
;;; GtkWidget * gtk_toggle_button_new_with_mnemonic (const gchar *label);
;;; 
;;; Creates a new GtkToggleButton containing a label. The label will be created
;;; using gtk_label_new_with_mnemonic(), so underscores in label indicate the
;;; mnemonic for the button.
;;; 
;;; label :
;;;     the text of the button, with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;;     a new GtkToggleButton
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_set_mode ()
;;; 
;;; void gtk_toggle_button_set_mode (GtkToggleButton *toggle_button,
;;;                                  gboolean draw_indicator);
;;; 
;;; Sets whether the button is displayed as a separate indicator and label. You
;;; can call this function on a checkbutton or a radiobutton with
;;; draw_indicator = FALSE to make the button look like a normal button
;;; 
;;; This function only affects instances of classes like GtkCheckButton and
;;; GtkRadioButton that derive from GtkToggleButton, not instances of
;;; GtkToggleButton itself.
;;; 
;;; toggle_button :
;;;     a GtkToggleButton
;;; 
;;; draw_indicator :
;;;     if TRUE, draw the button as a separate indicator and label; if FALSE,
;;;     draw the button like a normal button
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_get_mode ()
;;; 
;;; gboolean gtk_toggle_button_get_mode (GtkToggleButton *toggle_button);
;;; 
;;; Retrieves whether the button is displayed as a separate indicator and label.
;;; See gtk_toggle_button_set_mode().
;;; 
;;; toggle_button :
;;;     a GtkToggleButton
;;; 
;;; Returns :
;;;     TRUE if the togglebutton is drawn as a separate indicator and label.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_toggled ()
;;; 
;;; void gtk_toggle_button_toggled (GtkToggleButton *toggle_button);
;;; 
;;; Emits the "toggled" signal on the GtkToggleButton. There is no good reason
;;; for an application ever to call this function.
;;; 
;;; toggle_button :
;;;     a GtkToggleButton.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_get_active ()
;;; 
;;; gboolean gtk_toggle_button_get_active (GtkToggleButton *toggle_button);
;;; 
;;; Queries a GtkToggleButton and returns its current state. Returns TRUE if
;;; the toggle button is pressed in and FALSE if it is raised.
;;; 
;;; toggle_button :
;;;     a GtkToggleButton.
;;; 
;;; Returns :
;;;     a gboolean value.
;;; ----------------------------------------------------------------------------

(defun gtk-toggle-button-get-active (toggle-button)
  (gtk-toggle-button-active toggle-button))

(export 'gtk-toggle-button-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_set_active ()
;;; 
;;; void gtk_toggle_button_set_active (GtkToggleButton *toggle_button,
;;;                                    gboolean is_active);
;;; 
;;; Sets the status of the toggle button. Set to TRUE if you want the
;;; GtkToggleButton to be 'pressed in', and FALSE to raise it. This action
;;; causes the toggled signal to be emitted.
;;; 
;;; toggle_button :
;;;     a GtkToggleButton.
;;; 
;;; is_active :
;;;     TRUE or FALSE.
;;; ----------------------------------------------------------------------------

(defun gtk-toggle-button-set-active (toggle-button is-active)
  (setf (gtk-toggle-button-active toggle-button) is-active))

(export 'gtk-toggle-button-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_get_inconsistent ()
;;; 
;;; gboolean gtk_toggle_button_get_inconsistent (GtkToggleButton *toggle_button)
;;; 
;;; Gets the value set by gtk_toggle_button_set_inconsistent().
;;; 
;;; toggle_button :
;;;     a GtkToggleButton
;;; 
;;; Returns :
;;;     TRUE if the button is displayed as inconsistent, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_set_inconsistent ()
;;; 
;;; void gtk_toggle_button_set_inconsistent (GtkToggleButton *toggle_button,
;;;                                          gboolean setting);
;;; 
;;; If the user has selected a range of elements (such as some text or
;;; spreadsheet cells) that are affected by a toggle button, and the current
;;; values in that range are inconsistent, you may want to display the toggle
;;; in an "in between" state. This function turns on "in between" display.
;;; Normally you would turn off the inconsistent state again if the user
;;; toggles the toggle button. This has to be done manually,
;;; gtk_toggle_button_set_inconsistent() only affects visual appearance, it
;;; doesn't affect the semantics of the button.
;;; 
;;; toggle_button :
;;;     a GtkToggleButton
;;; 
;;; setting :
;;;     TRUE if state is inconsistent
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.toogle-button.lisp -------------------------------------
