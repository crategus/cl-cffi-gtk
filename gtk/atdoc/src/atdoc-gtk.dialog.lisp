;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(setf (documentation 'gtk-dialog 'type)
 "@version{2012-12-22}
  @begin{short}
    The @sym{gtk-dialog} struct contains only private fields and should not be
    directly accessed.
  @end{short}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-dialog-flags atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-dialog-flags atdoc:*external-symbols*)
 "@version{2012-12-22}
  @short{Flags used to influence dialog construction.}
  @begin{pre}
(define-g-flags \"GtkDialogFlags\" gtk-dialog-flags
  (:export t
   :type-initializer \"gtk_dialog_flags_get_type\")
  (:modal 1)
  (:destroy-with-parent 2)
  (:no-separator 4))
  @end{pre}
  @begin{table}
    @entry[GTK_DIALOG_MODAL]{Make the constructed dialog modal, see
      gtk_window_set_modal()}
    @entry[GTK_DIALOG_DESTROY_WITH_PARENT]{Destroy the dialog when its parent is
      destroyed, see gtk_window_set_destroy_with_parent()}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-response-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-response-type atdoc:*external-symbols*)
 "@version{2012-12-22}
  @begin{short}
    Predefined values for use as response ids in gtk_dialog_add_button().
  @end{short}
  All predefined values are negative, GTK+ leaves positive values for
  application-defined response ids.
  @begin{pre}
(define-g-enum \"GtkResponseType\" gtk-response-type
  (:export t
   :type-initializer \"gtk_response_type_get_type\")
  (:none -1)
  (:reject -2)
  (:accept -3)
  (:delete-event -4)
  (:ok -5)
  (:cancel -6)
  (:close -7)
  (:yes -8)
  (:no -9)
  (:apply -10)
  (:help -11))
  @end{pre}
  @begin{table}
    @endtry[GTK_RESPONSE_NONE]{Returned if an action widget has no response id,
      or if the dialog gets programmatically hidden or destroyed}
    @entry[GTK_RESPONSE_REJECT]{Generic response id, not used by GTK+ dialogs}
    @entry[GTK_RESPONSE_ACCEPT]{ Generic response id, not used by GTK+ dialogs}
    @entry[GTK_RESPONSE_DELETE_EVENT]{Returned if the dialog is deleted}
    @entry[GTK_RESPONSE_OK]{Returned by OK buttons in GTK+ dialogs}
    @entry[GTK_RESPONSE_CANCEL]{Returned by Cancel buttons in GTK+ dialogs}
    @entry[GTK_RESPONSE_CLOSE]{Returned by Close buttons in GTK+ dialogs}
    @entry[GTK_RESPONSE_YES]{Returned by Yes buttons in GTK+ dialogs}
    @entry[GTK_RESPONSE_NO]{Returned by No buttons in GTK+ dialogs}
    @entry[GTK_RESPONSE_APPLY]{Returned by Apply buttons in GTK+ dialogs}
    @entry[GTK_RESPONSE_HELP]{Returned by Help buttons in GTK+ dialogs}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-new 'function)
 "@version{2012-12-21}
  @return{The new dialog as a @class{gtk-widget}}
  @short{Creates a new dialog box.}

  Widgets should not be packed into this @class{gtk-window} directly, but into
  the @code{vbox} and @code{action_area}, as described above.")

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new_with_buttons ()
;;;
;;; GtkWidget *gtk_dialog_new_with_buttons(const gchar *title,
;;;                                        GtkWindow *parent,
;;;                                        GtkDialogFlags flags,
;;;                                        const gchar *first_button_text,
;;;                                        ...);
;;;
;;; Creates a new GtkDialog with title title (or NULL for the default title;
;;; see gtk_window_set_title()) and transient parent parent (or NULL for none;
;;; see gtk_window_set_transient_for()). The flags argument can be used to
;;; make the dialog modal (GTK_DIALOG_MODAL) and/or to have it destroyed along
;;; with its transient parent (GTK_DIALOG_DESTROY_WITH_PARENT). After flags,
;;; button text/response ID pairs should be listed, with a NULL pointer ending
;;; the list. Button text can be either a stock ID such as GTK_STOCK_OK, or
;;; some arbitrary text. A response ID can be any positive number, or one of
;;; the values in the GtkResponseType enumeration. If the user clicks one of
;;; these dialog buttons, GtkDialog will emit the "response" signal with the
;;; corresponding response ID. If a GtkDialog receives the "delete-event"
;;; signal, it will emit ::response with a response ID of
;;; GTK_RESPONSE_DELETE_EVENT. However, destroying a dialog does not emit the
;;; ::response signal; so be careful relying on ::response when using the
;;; GTK_DIALOG_DESTROY_WITH_PARENT flag. Buttons are from left to right,
;;; so the first button in the list will be the leftmost button in the dialog.
;;;
;;; Here's a simple example:
;;;
;;; GtkWidget *dialog = gtk_dialog_new_with_buttons
;;;                          ("My dialog",
;;;                           main_app_window,
;;;                           GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                           GTK_STOCK_OK,
;;;                           GTK_RESPONSE_ACCEPT,
;;;                           GTK_STOCK_CANCEL,
;;;                           GTK_RESPONSE_REJECT,
;;;                           NULL);
;;;
;;; title :
;;;     Title of the dialog, or NULL
;;;
;;; parent :
;;;     Transient parent of the dialog, or NULL
;;;
;;; flags :
;;;     from GtkDialogFlags
;;;
;;; first_button_text :
;;;     stock ID or text to go in first button, or NULL
;;;
;;; ... :
;;;     response ID for first button, then additional buttons, ending with NULL
;;;
;;; Returns :
;;;     a new GtkDialog
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-run 'function)
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog}}
  @return{response ID}
  @begin{short}
    Blocks in a recursive main loop until the dialog either emits the
    \"response\" signal, or is destroyed.
  @end{short}
  If the dialog is destroyed during the call to @sym{gtk-dialog-run},
  @sym{gtk-dialog-run} returns @code{:NONE}. Otherwise, it returns
  the response ID from the @code{::response} signal emission.

  Before entering the recursive main loop, @sym{gtk-dialog-run} calls
  @fun{gtk-widget-show} on the dialog for you. Note that you still need to show
  any children of the dialog yourself.

  During @sym{gtk-dialog-run}, the default behavior of \"delete-event\" is
  disabled; if the dialog receives @code{::delete_event}, it will not be
  destroyed as windows usually are, and @sym{gtk-dialog-run} will return
  @code{:DELETE-EVENT}. Also, during @sym{gtk-dialog-run} the dialog will be
  modal. You can force @sym{gtk-dialog-run} to return at any time by calling
  @fun{gtk-dialog-response} to emit the @code{::response} signal. Destroying the
  dialog during @sym{gtk-dialog-run} is a very bad idea, because your post-run
  code won't know whether the dialog was destroyed or not.

  After @sym{gtk-dialog-run} returns, you are responsible for hiding or
  destroying the dialog if you wish to do so.

  Typical usage of this function might be:
  @begin{pre}
 gint result = gtk_dialog_run (GTK_DIALOG (dialog));
 switch (result)
   {
    case GTK_RESPONSE_ACCEPT:
        do_application_specific_something ();
        break;
     default:
        do_nothing_since_dialog_was_cancelled ();
        break;
   @}
 gtk_widget_destroy (dialog);
  @end{pre}
  Note that even though the recursive main loop gives the effect of a modal
  dialog (it prevents the user from interacting with other windows in the same
  window group while the dialog is run), callbacks such as timeouts, IO
  channel watches, DND drops, etc, will be triggered during a
  @sym{gtk-dialog-run} call.
  @see-class{gtk-dialog}
  @see-function{gtk-widget-show}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-response 'function)
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog}}
  @argument[response-id]{response ID}
  @begin{short}
    Emits the \"response\" signal with the given response ID.
  @end{short}
  Used to indicate that the user has responded to the dialog in some way;
  typically either you or @fun{gtk-dialog-run} will be monitoring the
  @code{::response} signal and take appropriate action.
  @see-function{gtk-dialog-run}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-add-button 'function)
 "@version{2012-12-20}
  @argument[dialog]{a @class{gtk-dialog}}
  @argument[button-text]{text of button, or stock ID}
  @argument[response_id]{response ID for the button}
  @return{The @class{gtk-button} widget that was added.}
  @begin{short}
    Adds a button with the given text (or a stock button, if button_text is a
    stock ID) and sets things up so that clicking the button will emit the
    \"response\" signal with the given response_id.
  @end{short}
  The button is appended to the end of the dialog's action area. The button
  widget is returned, but usually you don't need it.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-add-buttons 'function)
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog}}
  @argument[buttons]{a list of pairs with a button text or stock ID and the
    response IDfor the button}
  @begin{short}
    Adds more buttons, same as calling @fun{gtk-dialog-add-button} repeatedly.
  @end{short}
  Each button must have both text and response ID.
  @begin[Note]{dictionary}
    The Lisp implementation does not call the C function, but the function
    @fun{gtk-dialog-add-button} in a loop to add the buttons.
  @end{dictionary}
  @see-function{gtk-dialog-add-button}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-add-action-widget 'function)
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog}}
  @argument[child]{an activatable widget}
  @argument[response-id]{response ID for child}
  @begin{short}
    Adds an activatable widget to the action area of a @class{gtk-dialog},
    connecting a signal handler that will emit the \"response\" signal on the
    dialog when the widget is activated.
  @end{short}
  The widget is appended to the end of the dialog's action area. If you want to
  add a non-activatable widget, simply pack it into the @code{action-area} field
  of the @class{gtk-dialog} struct.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-set-default-response 'function)
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog}}
  @argument[response-id]{a response ID}
  @begin{short}
    Sets the last widget in the dialog's action area with the given response_id
    as the default widget for the dialog.
  @end{short}
  Pressing \"Enter\" normally activates the default widget.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-set-response-sensitive 'function)
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog}}
  @argument[response-id]{a response ID}
  @argument[setting]{TRUE for sensitive}
  @begin{short}
    Calls @code{(@fun{gtk-widget-set-sensitive} widget @arg{setting})} for each
    widget in the dialog's action area with the given @arg{response-id}. A
    convenient way to sensitize/desensitize dialog buttons.
  @end{short}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-get-response-for-widget 'function)
 "@version{2012-12-22}
  @argument[dialog]{a GtkDialog}
  @argument[widget]{a widget in the action area of dialog}
  @return{the response id of widget, or GTK_RESPONSE_NONE if widget doesn't have
    a response id set.}
  @short{Gets the response id of a widget in the action area of a dialog.}

  Since 2.8")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-get-widget-for-response 'function)
 "@version{2012-12-22}
  @argument[dialog]{a GtkDialog}
  @argument[response_id]{the response ID used by the dialog widget}
  @return{the widget button that uses the given response_id, or NULL}
  @begin{short}
    Gets the widget button that uses the given response ID in the action area
    of a dialog.
  @end{short}

  Since 2.20")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-get-action-area 'function)
 "@version{2012-12-22}
  @argument[dialog]{a GtkDialog}
  @return{the action area}
  @short{Returns the action area of dialog.}

  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-get-content-area 'function)
 "@version{2012-12-22}
  @argument[dialog]{a GtkDialog}
  @return{the content area GtkBox}
  @short{Returns the content area of dialog.}

  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-alternative-dialog-button-order 'function)
 "@version{2012-12-22}
  @argument[screen]{a GdkScreen, or NULL to use the default screen}
  @return{Whether the alternative button order should be used}
  @begin{short}
    Returns TRUE if dialogs are expected to use an alternative button order on
    the screen screen.
  @end{short}
  See gtk_dialog_set_alternative_button_order() for more details about
  alternative button order.

  If you need to use this function, you should probably connect to the
  ::notify:gtk-alternative-button-order signal on the GtkSettings object
  associated to screen, in order to be notified if the button order setting
  changes.

  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-set-alternative-button-order 'function)
 "@version{2012-12-22}
  @argument[dialog]{a GtkDialog}
  @argument[first_response_id]{a response id used by one dialog's buttons}
  @argument[...]{a list of more response ids of dialog's buttons, terminated
     by -1}
  @begin{short}
    Sets an alternative button order.
  @end{short}
  If the \"gtk-alternative-button-order\" setting is set to TRUE, the dialog
  buttons are reordered according to the order of the response ids passed to
  this function.

  By default, GTK+ dialogs use the button order advocated by the Gnome Human
  Interface Guidelines with the affirmative button at the far right, and the
  cancel button left of it. But the builtin GTK+ dialogs and GtkMessageDialogs
  do provide an alternative button order, which is more suitable on some
  platforms, e.g. Windows.

  Use this function after adding all the buttons to your dialog, as the
  following example shows:
  @begin{pre}
 cancel_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                        GTK_STOCK_CANCEL,
                                        GTK_RESPONSE_CANCEL);

 ok_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                    GTK_STOCK_OK,
                                    GTK_RESPONSE_OK);

 gtk_widget_grab_default (ok_button);

 help_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                      GTK_STOCK_HELP,
                                      GTK_RESPONSE_HELP);

 gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                          GTK_RESPONSE_OK,
                                          GTK_RESPONSE_CANCEL,
                                          GTK_RESPONSE_HELP,
                                          -1);
  @end{pre}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-dialog-set-alternative-button-order-from-array 'function)
 "@version{2012-12-22}
  @argument[dialog]{a GtkDialog}
  @argument[n_params]{the number of response ids in new_order}
  @argument[new_order]{an array of response ids of dialog's buttons}
  @short{Sets an alternative button order.}
  If the \"gtk-alternative-button-order\" setting is set to TRUE, the dialog
  buttons are reordered according to the order of the response ids in new_order.

  See gtk_dialog_set_alternative_button_order() for more information.

  This function is for use by language bindings.

  Since 2.6")

;;; --- End of file atdoc-gtk.dialog.lisp --------------------------------------
