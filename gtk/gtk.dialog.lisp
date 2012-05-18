;;; ----------------------------------------------------------------------------
;;; gtk.dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkDialog
;;;
;;; Create popup windows
;;;
;;; Synopsis
;;;
;;;     GtkDialog
;;;
;;;     GtkDialogFlags
;;;     GtkResponseType
;;;
;;;     gtk_dialog_new
;;;     gtk_dialog_new_with_buttons
;;;     gtk_dialog_run
;;;     gtk_dialog_response
;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_add_action_widget
;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_get_action_area
;;;     gtk_dialog_get_content_area
;;;
;;;     gtk_alternative_dialog_button_order
;;;     gtk_dialog_set_alternative_button_order
;;;     gtk_dialog_set_alternative_button_order_from_array
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                        +----GtkAboutDialog
;;;                                        +----GtkAppChooserDialog
;;;                                        +----GtkColorChooserDialog
;;;                                        +----GtkColorSelectionDialog
;;;                                        +----GtkFileChooserDialog
;;;                                        +----GtkFontChooserDialog
;;;                                        +----GtkFontSelectionDialog
;;;                                        +----GtkMessageDialog
;;;                                        +----GtkPageSetupUnixDialog
;;;                                        +----GtkPrintUnixDialog
;;;                                        +----GtkRecentChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;; GtkDialog implements AtkImplementorIface and GtkBuildable.
;;;
;;; Style Properties
;;;
;;;   "action-area-border"       gint                  : Read
;;;   "button-spacing"           gint                  : Read
;;;   "content-area-border"      gint                  : Read
;;;   "content-area-spacing"     gint                  : Read
;;;
;;; Signals
;;;
;;;   "close"                                          : Action
;;;   "response"                                       : Run Last
;;;
;;; Description
;;;
;;; Dialog boxes are a convenient way to prompt the user for a small amount of
;;; input, e.g. to display a message, ask a question, or anything else that does
;;; not require extensive effort on the user's part.
;;;
;;; GTK+ treats a dialog as a window split vertically. The top section is a
;;; GtkVBox, and is where widgets such as a GtkLabel or a GtkEntry should be
;;; packed. The bottom area is known as the action_area. This is generally used
;;; for packing buttons into the dialog which may perform functions such as
;;; cancel, ok, or apply.
;;;
;;; GtkDialog boxes are created with a call to gtk_dialog_new() or
;;; gtk_dialog_new_with_buttons(). gtk_dialog_new_with_buttons() is recommended;
;;; it allows you to set the dialog title, some convenient flags, and add simple
;;; buttons.
;;;
;;; If 'dialog' is a newly created dialog, the two primary areas of the window
;;; can be accessed through gtk_dialog_get_content_area() and
;;; gtk_dialog_get_action_area(), as can be seen from the example below.
;;;
;;; A 'modal' dialog (that is, one which freezes the rest of the application
;;; from user input), can be created by calling gtk_window_set_modal() on the
;;; dialog. Use the GTK_WINDOW() macro to cast the widget returned from
;;; gtk_dialog_new() into a GtkWindow. When using gtk_dialog_new_with_buttons()
;;; you can also pass the GTK_DIALOG_MODAL flag to make a dialog modal.
;;;
;;; If you add buttons to GtkDialog using gtk_dialog_new_with_buttons(),
;;; gtk_dialog_add_button(), gtk_dialog_add_buttons(), or
;;; gtk_dialog_add_action_widget(), clicking the button will emit a signal
;;; called "response" with a response ID that you specified. GTK+ will never
;;; assign a meaning to positive response IDs; these are entirely
;;; user-defined. But for convenience, you can use the response IDs in the
;;; GtkResponseType enumeration (these all have values less than zero). If a
;;; dialog receives a delete event, the "response" signal will be emitted with
;;; a response ID of GTK_RESPONSE_DELETE_EVENT.
;;;
;;; If you want to block waiting for a dialog to return before returning
;;; control flow to your code, you can call gtk_dialog_run(). This function
;;; enters a recursive main loop and waits for the user to respond to the
;;; dialog, returning the response ID corresponding to the button the user
;;; clicked.
;;;
;;; For the simple dialog in the following example, in reality you'd probably
;;; use GtkMessageDialog to save yourself some effort. But you'd need to create
;;; the dialog contents manually if you had more than a simple message in the
;;; dialog.
;;;
;;; Example 44. Simple GtkDialog usage
;;;
;;; /* Function to open a dialog box displaying the message provided. */
;;; void
;;; quick_message (gchar *message)
;;; {
;;;    GtkWidget *dialog, *label, *content_area;
;;;
;;;    /* Create the widgets */
;;;    dialog = gtk_dialog_new_with_buttons ("Message",
;;;                                          main_application_window,
;;;                                          GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                                          GTK_STOCK_OK,
;;;                                          GTK_RESPONSE_NONE,
;;;                                          NULL);
;;;    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
;;;    label = gtk_label_new (message);
;;;
;;;    /* Ensure that the dialog box is destroyed when the user responds */
;;;    g_signal_connect_swapped (dialog,
;;;                              "response",
;;;                              G_CALLBACK (gtk_widget_destroy),
;;;                              dialog);
;;;
;;;    /* Add the label, and show everything we've added to the dialog */
;;;
;;;    gtk_container_add (GTK_CONTAINER (content_area), label);
;;;    gtk_widget_show_all (dialog);
;;; }
;;;
;;; GtkDialog as GtkBuildable
;;;
;;; The GtkDialog implementation of the GtkBuildable interface exposes the
;;; vbox and action_area as internal children with the names "vbox" and
;;; "action_area".
;;;
;;; GtkDialog supports a custom <action-widgets> element, which can contain
;;; multiple <action-widget> elements. The "response" attribute specifies a
;;; numeric response, and the content of the element is the id of widget (which
;;; should be a child of the dialogs action_area).
;;;
;;; Example 45. A GtkDialog UI definition fragment.
;;;
;;; <object class="GtkDialog" id="dialog1">
;;;   <child internal-child="vbox">"
;;;     <object class="GtkVBox" id="vbox">
;;;       <child internal-child="action_area">
;;;         <object class="GtkHButtonBox" id="button_box">
;;;           <child>
;;;             <object class="GtkButton" id="button_cancel"/>
;;;           </child>
;;;           <child>
;;;             <object class="GtkButton" id="button_ok"/>
;;;           </child>
;;;         </object>
;;;       </child>
;;;     </object>
;;;   </child>
;;;   <action-widgets>
;;;     <action-widget response="3">button_ok</action-widget>
;;;     <action-widget response="-5">button_cancel</action-widget>
;;;   </action-widgets>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-area-border" style property
;;;
;;;   "action-area-border"       gint                  : Read
;;;
;;; Width of border around the button area at the bottom of the dialog.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 5
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-spacing" style property
;;;
;;;   "button-spacing"           gint                  : Read
;;;
;;; Spacing between buttons.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 6
;;;
;;; ----------------------------------------------------------------------------
;;; The "content-area-border" style property
;;;
;;;   "content-area-border"      gint                  : Read
;;;
;;; Width of border around the main dialog area.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 2
;;;
;;; ----------------------------------------------------------------------------
;;; The "content-area-spacing" style property
;;;
;;;   "content-area-spacing"     gint                  : Read
;;;
;;; The default spacing used between elements of the content area of the
;;; dialog, as returned by gtk_dialog_get_content_area(), unless
;;; gtk_box_set_spacing() was called on that widget directly.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;;
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "close" signal
;;;
;;; void user_function (GtkDialog *arg0,
;;;                     gpointer   user_data)      : Action
;;;
;;; The ::close signal is a keybinding signal which gets emitted when the user
;;; uses a keybinding to close the dialog.
;;;
;;; The default binding for this signal is the Escape key.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "response" signal
;;;
;;; void user_function (GtkDialog *dialog,
;;;                     gint       response_id,
;;;                     gpointer   user_data)        : Run Last
;;;
;;; Emitted when an action widget is clicked, the dialog receives a delete
;;; event, or the application programmer calls gtk_dialog_response(). On a
;;; delete event, the response ID is GTK_RESPONSE_DELETE_EVENT. Otherwise, it
;;; depends on which action widget was clicked.
;;;
;;; dialog :
;;;     the object on which the signal is emitted
;;;
;;; response_id :
;;;     the response ID
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkDialog
;;;
;;; struct GtkDialog;
;;;
;;; The GtkDialog struct contains only private fields and should not be
;;; directly accessed.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkDialog" gtk-dialog
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_dialog_get_type")
  ((has-separator
    gtk-dialog-has-separator
    "has-separator" "gboolean" t t)
   (:cffi content-area
          gtk-dialog-content-area (g-object gtk-vbox)
          "gtk_dialog_get_content_area" nil)
   (:cffi action-area
          gtk-dialog-action-area (g-object gtk-widget)
          "gtk_dialog_get_action_area" nil)
   (:cffi default-response
          gtk-dialog-default-response gtk-response-type
          nil "gtk_dialog_set_default_response")))

;;; ----------------------------------------------------------------------------
;;; enum GtkDialogFlags
;;;
;;; typedef enum {
;;;   GTK_DIALOG_MODAL               = 1 << 0,
;;;   GTK_DIALOG_DESTROY_WITH_PARENT = 1 << 1
;;; } GtkDialogFlags;
;;;
;;; Flags used to influence dialog construction.
;;;
;;; GTK_DIALOG_MODAL
;;;     Make the constructed dialog modal, see gtk_window_set_modal()
;;;
;;; GTK_DIALOG_DESTROY_WITH_PARENT
;;;     Destroy the dialog when its parent is destroyed, see
;;;     gtk_window_set_destroy_with_parent()
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkDialogFlags" gtk-dialog-flags
  (:export t
   :type-initializer "gtk_dialog_flags_get_type")
  (:modal 1)
  (:destroy-with-parent 2)
  (:no-separator 4))

;;; ----------------------------------------------------------------------------
;;; enum GtkResponseType
;;;
;;; typedef enum {
;;;   GTK_RESPONSE_NONE         = -1,
;;;   GTK_RESPONSE_REJECT       = -2,
;;;   GTK_RESPONSE_ACCEPT       = -3,
;;;   GTK_RESPONSE_DELETE_EVENT = -4,
;;;   GTK_RESPONSE_OK           = -5,
;;;   GTK_RESPONSE_CANCEL       = -6,
;;;   GTK_RESPONSE_CLOSE        = -7,
;;;   GTK_RESPONSE_YES          = -8,
;;;   GTK_RESPONSE_NO           = -9,
;;;   GTK_RESPONSE_APPLY        = -10,
;;;   GTK_RESPONSE_HELP         = -11
;;; } GtkResponseType;
;;;
;;; Predefined values for use as response ids in gtk_dialog_add_button(). All
;;; predefined values are negative, GTK+ leaves positive values for
;;; application-defined response ids.
;;;
;;; GTK_RESPONSE_NONE
;;;     Returned if an action widget has no response id, or if the dialog gets
;;;     programmatically hidden or destroyed
;;;
;;; GTK_RESPONSE_REJECT
;;;     Generic response id, not used by GTK+ dialogs
;;;
;;; GTK_RESPONSE_ACCEPT
;;;     Generic response id, not used by GTK+ dialogs
;;;
;;; GTK_RESPONSE_DELETE_EVENT
;;;     Returned if the dialog is deleted
;;;
;;; GTK_RESPONSE_OK
;;;     Returned by OK buttons in GTK+ dialogs
;;;
;;; GTK_RESPONSE_CANCEL
;;;     Returned by Cancel buttons in GTK+ dialogs
;;;
;;; GTK_RESPONSE_CLOSE
;;;     Returned by Close buttons in GTK+ dialogs
;;;
;;; GTK_RESPONSE_YES
;;;     Returned by Yes buttons in GTK+ dialogs
;;;
;;; GTK_RESPONSE_NO
;;;     Returned by No buttons in GTK+ dialogs
;;;
;;; GTK_RESPONSE_APPLY
;;;     Returned by Apply buttons in GTK+ dialogs
;;;
;;; GTK_RESPONSE_HELP
;;;     Returned by Help buttons in GTK+ dialogs
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkResponseType" gtk-response-type
  (:export t
   :type-initializer "gtk_response_type_get_type")
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

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new ()
;;;
;;; GtkWidget * gtk_dialog_new (void);
;;;
;;; Creates a new dialog box.
;;;
;;; Widgets should not be packed into this GtkWindow directly, but into the
;;; vbox and action_area, as described above.
;;;
;;; Returns :
;;;     the new dialog as a GtkWidget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-dialog-new))

(defun gtk-dialog-new ()
  (make-instance 'gtk-dialog))

(export 'gtk-dialog-new)

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

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_run ()
;;;
;;; gint gtk_dialog_run (GtkDialog *dialog);
;;;
;;; Blocks in a recursive main loop until the dialog either emits the
;;; "response" signal, or is destroyed. If the dialog is destroyed during the
;;; call to gtk_dialog_run(), gtk_dialog_run() returns GTK_RESPONSE_NONE.
;;; Otherwise, it returns the response ID from the ::response signal emission.
;;;
;;; Before entering the recursive main loop, gtk_dialog_run() calls
;;; gtk_widget_show() on the dialog for you. Note that you still need to show
;;; any children of the dialog yourself.
;;;
;;; During gtk_dialog_run(), the default behavior of "delete-event" is
;;; disabled; if the dialog receives ::delete_event, it will not be destroyed as
;;; windows usually are, and gtk_dialog_run() will return
;;; GTK_RESPONSE_DELETE_EVENT. Also, during gtk_dialog_run() the dialog will be
;;; modal. You can force gtk_dialog_run() to return at any time by calling
;;; gtk_dialog_response() to emit the ::response signal. Destroying the dialog
;;; during gtk_dialog_run() is a very bad idea, because your post-run code won't
;;; know whether the dialog was destroyed or not.
;;;
;;; After gtk_dialog_run() returns, you are responsible for hiding or
;;; destroying the dialog if you wish to do so.
;;;
;;; Typical usage of this function might be:
;;;
;;; gint result = gtk_dialog_run (GTK_DIALOG (dialog));
;;; switch (result)
;;;   {
;;;     case GTK_RESPONSE_ACCEPT:
;;;        do_application_specific_something ();
;;;        break;
;;;     default:
;;;        do_nothing_since_dialog_was_cancelled ();
;;;        break;
;;;   }
;;; gtk_widget_destroy (dialog);
;;;
;;; Note that even though the recursive main loop gives the effect of a modal
;;; dialog (it prevents the user from interacting with other windows in the same
;;; window group while the dialog is run), callbacks such as timeouts, IO
;;; channel watches, DND drops, etc, will be triggered during a
;;; gtk_dialog_run() call.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; Returns :
;;;     response ID
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_run" gtk-dialog-run) gtk-response-type
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-run)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_response ()
;;;
;;; void gtk_dialog_response (GtkDialog *dialog, gint response_id);
;;;
;;; Emits the "response" signal with the given response ID. Used to indicate
;;; that the user has responded to the dialog in some way; typically either you
;;; or gtk_dialog_run() will be monitoring the ::response signal and take
;;; appropriate action.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; response_id :
;;;     response ID
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_response" gtk-dialog-response) :void
  (dialog (g-object gtk-dialog))
  (response gtk-response-type))

(export 'gtk-dialog-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_button ()
;;;
;;; GtkWidget * gtk_dialog_add_button (GtkDialog *dialog,
;;;                                    const gchar *button_text,
;;;                                    gint response_id);
;;;
;;; Adds a button with the given text (or a stock button, if button_text is a
;;; stock ID) and sets things up so that clicking the button will emit the
;;; "response" signal with the given response_id. The button is appended to the
;;; end of the dialog's action area. The button widget is returned, but usually
;;; you don't need it.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; button_text :
;;;     text of button, or stock ID
;;;
;;; response_id :
;;;     response ID for the button
;;;
;;; Returns :
;;;     the GtkButton widget that was added
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_add_button" gtk-dialog-add-button) (g-object gtk-widget)
  (dialog (g-object gtk-dialog))
  (button-text :string)
  (response gtk-response-type))

(export 'gtk-dialog-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_buttons ()
;;;
;;; void gtk_dialog_add_buttons (GtkDialog *dialog,
;;;                              const gchar *first_button_text,
;;;                              ...);
;;;
;;; Adds more buttons, same as calling gtk_dialog_add_button() repeatedly. The
;;; variable argument list should be NULL-terminated as with
;;; gtk_dialog_new_with_buttons(). Each button must have both text and response
;;; ID.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; first_button_text :
;;;     button text or stock ID
;;;
;;; ... :
;;;     response ID for first button, then more text-response_id pairs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_action_widget ()
;;;
;;; void gtk_dialog_add_action_widget (GtkDialog *dialog,
;;;                                    GtkWidget *child,
;;;                                    gint response_id);
;;;
;;; Adds an activatable widget to the action area of a GtkDialog, connecting a
;;; signal handler that will emit the "response" signal on the dialog when the
;;; widget is activated. The widget is appended to the end of the dialog's
;;; action area. If you want to add a non-activatable widget, simply pack it
;;; into the action_area field of the GtkDialog struct.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; child :
;;;     an activatable widget
;;;
;;; response_id :
;;;     response ID for child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_add_action_widget" gtk-dialog-add-action-widget) :void
  (dialog (g-object gtk-dialog))
  (child (g-object gtk-widget))
  (response gtk-response-type))

(export 'gtk-dialog-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_default_response ()
;;;
;;; void gtk_dialog_set_default_response (GtkDialog *dialog, gint response_id);
;;;
;;; Sets the last widget in the dialog's action area with the given response_id
;;; as the default widget for the dialog. Pressing "Enter" normally activates
;;; the default widget.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; response_id :
;;;     a response ID
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-dialog-set-default-response))

(defun gtk-dialog-set-default-response (dialog response-id)
  (setf (gtk-dialog-default-response dialog) response-id))

(export 'gtk-dialog-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_response_sensitive ()
;;;
;;; void gtk_dialog_set_response_sensitive (GtkDialog *dialog,
;;;                                         gint response_id,
;;;                                         gboolean setting);
;;;
;;; Calls gtk_widget_set_sensitive (widget, setting) for each widget in the
;;; dialog's action area with the given response_id. A convenient way to
;;; sensitize/desensitize dialog buttons.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; response_id :
;;;     a response ID
;;;
;;; setting :
;;;     TRUE for sensitive
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_set_response_sensitive" gtk-dialog-set-response-sensitive)
    :void
  (dialog (g-object gtk-dialog))
  (response gtk-response-type)
  (setting :boolean))

(export 'gtk-dialog-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_response_for_widget ()
;;;
;;; gint gtk_dialog_get_response_for_widget (GtkDialog *dialog,
;;;                                          GtkWidget *widget);
;;;
;;; Gets the response id of a widget in the action area of a dialog.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; widget :
;;;     a widget in the action area of dialog
;;;
;;; Returns :
;;;     the response id of widget, or GTK_RESPONSE_NONE if widget doesn't have
;;;     a response id set.
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_response_for_widget"
          gtk-dialog-get-response-for-widget) :int
  (dialog (g-object gtk-dialog))
  (widget (g-object gtk-widget)))

(export 'gtk-dialog-get-response-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_widget_for_response ()
;;;
;;; GtkWidget * gtk_dialog_get_widget_for_response (GtkDialog *dialog,
;;;                                                 gint response_id);
;;;
;;; Gets the widget button that uses the given response ID in the action area
;;; of a dialog.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; response_id :
;;;     the response ID used by the dialog widget
;;;
;;; Returns :
;;;     the widget button that uses the given response_id, or NULL
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_widget_for_response"
          gtk-dialog-get-widget-for-response) g-object
  (dialog (g-object gtk-dialog))
  (response_id gtk-response-type))

(export 'gtk-dialog-get-widget-for-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_action_area ()
;;;
;;; GtkWidget * gtk_dialog_get_action_area (GtkDialog *dialog);
;;;
;;; Returns the action area of dialog.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; Returns :
;;;     the action area
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-dialog-get-action-area))

(defun gtk-dialog-get-action-area (dialog)
  (gtk-dialog-action-area dialog))

(export 'gtk-dialog-get-action-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_content_area ()
;;;
;;; GtkWidget * gtk_dialog_get_content_area (GtkDialog *dialog);
;;;
;;; Returns the content area of dialog.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; Returns :
;;;     the content area GtkBox
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-dialog-get-content-area))

(defun gtk-dialog-get-content-area (dialog)
  (gtk-dialog-content-area dialog))

(export 'gtk-dialog-get-content-area)

;;; ----------------------------------------------------------------------------
;;; gtk_alternative_dialog_button_order ()
;;;
;;; gboolean gtk_alternative_dialog_button_order (GdkScreen *screen);
;;;
;;; Returns TRUE if dialogs are expected to use an alternative button order on
;;; the screen screen. See gtk_dialog_set_alternative_button_order() for more
;;; details about alternative button order.
;;;
;;; If you need to use this function, you should probably connect to the
;;; ::notify:gtk-alternative-button-order signal on the GtkSettings object
;;; associated to screen, in order to be notified if the button order setting
;;; changes.
;;;
;;; screen :
;;;     a GdkScreen, or NULL to use the default screen
;;;
;;; Returns :
;;;     Whether the alternative button order should be used
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_alternative_dialog_button_order"
          gtk-alternative-dialog-button-order) :boolean
  (screen (g-object gdk-screen)))

(export 'gtk-alternative-dialog-button-order)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order ()
;;;
;;; void gtk_dialog_set_alternative_button_order (GtkDialog *dialog,
;;;                                               gint first_response_id,
;;;                                               ...);
;;;
;;; Sets an alternative button order. If the "gtk-alternative-button-order"
;;; setting is set to TRUE, the dialog buttons are reordered according to the
;;; order of the response ids passed to this function.
;;;
;;; By default, GTK+ dialogs use the button order advocated by the Gnome Human
;;; Interface Guidelines with the affirmative button at the far right, and the
;;; cancel button left of it. But the builtin GTK+ dialogs and GtkMessageDialogs
;;; do provide an alternative button order, which is more suitable on some
;;; platforms, e.g. Windows.
;;;
;;; Use this function after adding all the buttons to your dialog, as the
;;; following example shows:
;;;
;;; cancel_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
;;;                                        GTK_STOCK_CANCEL,
;;;                                        GTK_RESPONSE_CANCEL);
;;;
;;; ok_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
;;;                                    GTK_STOCK_OK,
;;;                                    GTK_RESPONSE_OK);
;;;
;;; gtk_widget_grab_default (ok_button);
;;;
;;; help_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
;;;                                      GTK_STOCK_HELP,
;;;                                      GTK_RESPONSE_HELP);
;;;
;;; gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
;;;                                          GTK_RESPONSE_OK,
;;;                                          GTK_RESPONSE_CANCEL,
;;;                                          GTK_RESPONSE_HELP,
;;;                                          -1);
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; first_response_id :
;;;     a response id used by one dialog's buttons
;;;
;;; ... :
;;;     a list of more response ids of dialog's buttons, terminated by -1
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-dialog-set-alternative-button-order (dialog response-list)
  (with-foreign-object (new-order 'gtk-response-type (length response-list))
    (loop
       for i from 0
       for response in response-list
       do (setf (mem-aref new-order 'gtk-response-type i) response))
    (gtk-dialog-set-alternative-button-order-from-array dialog
                                                        (length response-list)
                                                        new-order))
  response-list)

(export 'gtk-dialog-set-alternative-button-order)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order_from_array ()
;;;
;;; void gtk_dialog_set_alternative_button_order_from_array (GtkDialog *dialog,
;;;                                                          gint n_params,
;;;                                                          gint *new_order);
;;;
;;; Sets an alternative button order. If the "gtk-alternative-button-order"
;;; setting is set to TRUE, the dialog buttons are reordered according to the
;;; order of the response ids in new_order.
;;;
;;; See gtk_dialog_set_alternative_button_order() for more information.
;;;
;;; This function is for use by language bindings.
;;;
;;; dialog :
;;;     a GtkDialog
;;;
;;; n_params :
;;;     the number of response ids in new_order
;;;
;;; new_order :
;;;     an array of response ids of dialog's buttons
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_set_alternative_button_order_from_array"
          gtk-dialog-set-alternative-button-order-from-array) :void
  (dialog (g-object gtk-dialog))
  (n-params :int)
  (new-order (:pointer gtk-response-type)))

(export 'gtk-dialog-set-alternative-button-order-from-array)

;;; --- End of file gtk.dialog.lisp --------------------------------------------
