;;; ----------------------------------------------------------------------------
;;; gtk.dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkDialog" gtk-dialog
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_dialog_get_type")
 nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-dialog 'type)
 "@version{2013-3-16}
  @begin{short}
    Dialog boxes are a convenient way to prompt the user for a small amount of
    input, e. g. to display a message, ask a question, or anything else that
    does not require extensive effort on the user's part.
  @end{short}

  GTK+ treats a dialog as a window split vertically. The top section is a
  @class{gtk-vbox}, and is where widgets such as a @class{gtk-label} or a
  @class{gtk-entry} should be packed. The bottom area is known as the
  \"action-area\". This is generally used for packing buttons into the
  dialog which may perform functions such as @code{cancel}, @code{ok}, or
  @code{apply}.

  @sym{gtk-dialog} boxes are created with a call to @fun{gtk-dialog-new} or
  @fun{gtk-dialog-new-with-buttons}. @fun{gtk-dialog-new-with-buttons}
  is recommended; it allows you to set the dialog title, some convenient
  flags, and add simple buttons.

  If the dialog is a newly created dialog, the two primary areas of the window
  can be accessed through @fun{gtk-dialog-get-content-area} and
  @fun{gtk-dialog-get-action-area}, as can be seen from the example below.

  A modal dialog (that is, one which freezes the rest of the application
  from user input), can be created by calling @fun{gtk-window-set-modal} on
  the dialog. When using @code{gtk-dialog-new-with-buttons} you can also pass
  the @code{:modal} flag to make a dialog modal.

  If you add buttons to @class{gtk-dialog} using
  @code{gtk-dialog-new-with-buttons}, @fun{gtk-dialog-add-button},
  @fun{gtk-dialog-add-buttons}, or @fun{gtk-dialog-add-action-widget},
  clicking the button will emit a signal called \"response\" with a response
  ID that you specified. GTK+ will never assign a meaning to positive response
  IDs; these are entirely user-defined. But for convenience, you can use the
  response IDs in the @symbol{gtk-response-type} enumeration (these all have
  values less than zero). If a dialog receives a delete event, the
  \"response\" signal will be emitted with a response ID of
  @code{:delete-event}.

  If you want to block waiting for a dialog to return before returning
  control flow to your code, you can call @fun{gtk-dialog-run}. This function
  enters a recursive main loop and waits for the user to respond to the
  dialog, returning the response ID corresponding to the button the user
  clicked.

  For the simple dialog in the following example, in reality you'd probably
  use @class{gtk-message-dialog} to save yourself some effort. But you'd need
  to create the dialog contents manually if you had more than a simple message
  in the dialog.

  @b{Example.} Simple @sym{gtk-dialog} usage
  @begin{pre}
 /* Function to open a dialog box displaying the message provided. */
 void
 quick_message (gchar *message)
 {
    GtkWidget *dialog, *label, *content_area;

    /* Create the widgets */
    dialog = gtk_dialog_new_with_buttons (\"Message\",
                                          main_application_window,
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          GTK_STOCK_OK,
                                          GTK_RESPONSE_NONE,
                                          NULL);
    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    label = gtk_label_new (message);

    /* Ensure that the dialog box is destroyed when the user responds */
    g_signal_connect_swapped (dialog,
                              \"response\",
                              G_CALLBACK (gtk_widget_destroy),
                              dialog);

    /* Add the label, and show everything we've added to the dialog */

    gtk_container_add (GTK_CONTAINER (content_area), label);
    gtk_widget_show_all (dialog);
 @}
  @end{pre}
  @subheading{GtkDialog as GtkBuildable}
  The @sym{gtk-dialog} implementation of the @class{gtk-buildable} interface
  exposes the @code{vbox} and @code{action_area} as internal children with
  the names \"vbox\" and \"action_area\".

  @sym{gtk-dialog} supports a custom @code{<action-widgets>} element, which
  can contain multiple @code{<action-widget>} elements. The @code{\"response\"}
  attribute specifies a numeric response, and the content of the element is
  the id of widget (which should be a child of the dialogs
  @code{action_area}).

  @b{Example.} A @class{gtk-dialog} UI definition fragment.
  @begin{pre}
 <object class=\"GtkDialog\" id=\"dialog1\">
   <child internal-child=\"vbox\">
     <object class=\"GtkVBox\" id=\"vbox\">
       <child internal-child=\"action_area\">
         <object class=\"GtkHButtonBox\" id=\"button_box\">
           <child>
            <object class=\"GtkButton\" id=\"button_cancel\"/>
           </child>
           <child>
             <object class=\"GtkButton\" id=\"button_ok\"/>
           </child>
         </object>
       </child>
     </object>
   </child>
   <action-widgets>
     <action-widget response=\"3\">button_ok</action-widget>
     <action-widget response=\"-5\">button_cancel</action-widget>
   </action-widgets>
 </object>
  @end{pre}
")


;;; ----------------------------------------------------------------------------
;;; enum GtkDialogFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkDialogFlags" gtk-dialog-flags
  (:export t
   :type-initializer "gtk_dialog_flags_get_type")
  (:modal 1)
  (:destroy-with-parent 2))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-dialog-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-dialog-flags atdoc:*external-symbols*)
 "@version{2013-3-15}
  @short{Flags used to influence the dialog construction.}
  @begin{pre}
(define-g-flags \"GtkDialogFlags\" gtk-dialog-flags
  (:export t
   :type-initializer \"gtk_dialog_flags_get_type\")
  (:modal 1)
  (:destroy-with-parent 2))
  @end{pre}
  @begin[code]{table}
    @entry[:modal]{Make the constructed dialog modal,
      see @fun{gtk-window-set-modal}.}
    @entry[:destroy-with-parent]{Destroy the dialog when its parent is
      destroyed, see @fun{gtk-window-set-destroy-with-parent}}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum GtkResponseType
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-response-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-response-type atdoc:*external-symbols*)
 "@version{2013-3-16}
  @begin{short}
    Predefined values for use as response ids in @fun{gtk-dialog-add-button}.
  @end{short}
  All predefined values are negative, GTK+ leaves positive values for
  application defined response ids.
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
  @begin[code]{table}
    @entry[:none]{Returned if an action widget has no response id, or if the
      dialog gets programmatically hidden or destroyed.}
    @entry[:reject]{Generic response id, not used by GTK+ dialogs.}
    @entry[:accept]{Generic response id, not used by GTK+ dialogs.}
    @entry[:delete-event]{Returned if the dialog is deleted.}
    @entry[:ok]{Returned by OK buttons in GTK+ dialogs.}
    @entry[:cancel]{Returned by Cancel buttons in GTK+ dialogs.}
    @entry[:close]{Returned by Close buttons in GTK+ dialogs.}
    @entry[:yes]{Returned by Yes buttons in GTK+ dialogs.}
    @entry[:no]{Returned by No buttons in GTK+ dialogs.}
    @entry[:apply]{Returned by Apply buttons in GTK+ dialogs.}
    @entry[:help]{Returned by Help buttons in GTK+ dialogs.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-dialog-new))

(defun gtk-dialog-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @return{The new dialog as a @class{gtk-widget} widget.}
  @short{Creates a new dialog box.}

  Widgets should not be packed into this @class{gtk-window} directly, but into
  the @code{vbox} and @code{action_area}, as described above."
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_run" gtk-dialog-run) gtk-response-type
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @return{response ID}
  @begin{short}
    Blocks in a recursive main loop until the dialog either emits the
    \"response\" signal, or is destroyed.
  @end{short}
  If the dialog is destroyed during the call to @sym{gtk-dialog-run},
  @sym{gtk-dialog-run} returns @code{:none}. Otherwise, it returns
  the response ID from the \"response\" signal emission.

  Before entering the recursive main loop, @sym{gtk-dialog-run} calls
  @fun{gtk-widget-show} on the dialog for you. Note that you still need to show
  any children of the dialog yourself.

  During @sym{gtk-dialog-run}, the default behavior of \"delete-event\" is
  disabled; if the dialog receives \"delete-event\", it will not be
  destroyed as windows usually are, and @sym{gtk-dialog-run} will return
  @code{:delete-event}. Also, during @sym{gtk-dialog-run} the dialog will be
  modal. You can force @sym{gtk-dialog-run} to return at any time by calling
  @fun{gtk-dialog-response} to emit the \"response\" signal. Destroying the
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
  @see-function{gtk-widget-show}"
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-run)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_response" gtk-dialog-response) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[response-id]{response ID}
  @begin{short}
    Emits the \"response\" signal with the given response ID.
  @end{short}
  Used to indicate that the user has responded to the dialog in some way;
  typically either you or @fun{gtk-dialog-run} will be monitoring the
  \"response\" signal and take appropriate action.
  @see-function{gtk-dialog-run}"
  (dialog (g-object gtk-dialog))
  (response gtk-response-type))

(export 'gtk-dialog-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_add_button" gtk-dialog-add-button) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[button-text]{text of button, or stock ID}
  @argument[response-id]{response ID for the button}
  @return{The @class{gtk-button} widget that was added.}
  @begin{short}
    Adds a button with the given text (or a stock button, if @arg{button-text}
    is a stock ID) and sets things up so that clicking the button will emit the
    \"response\" signal with the given @arg{response-id}.
  @end{short}
  The button is appended to the end of the dialog's action area. The button
  widget is returned, but usually you don't need it."
  (dialog (g-object gtk-dialog))
  (button-text :string)
  (response-id gtk-response-type))

(export 'gtk-dialog-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_buttons ()
;;; ----------------------------------------------------------------------------

(defun gtk-dialog-add-buttons (dialog &rest buttons)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-21}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[buttons]{a list of pairs with a button text or stock ID and the
    response ID for the button}
  @begin{short}
    Adds more buttons, same as calling @fun{gtk-dialog-add-button} repeatedly.
  @end{short}
  Each button must have both text and response ID.
  @begin[Note]{dictionary}
    The Lisp implementation does not call the C function, but the function
    @fun{gtk-dialog-add-button} in a loop to add the buttons.
  @end{dictionary}
  @see-function{gtk-dialog-add-button}"
  (let ((n (/ (length buttons) 2)))
    (assert (eql n (truncate (length buttons) 2)))
    (dotimes (i n)
      (gtk-dialog-add-button dialog (pop buttons) (pop buttons)))))

(export 'gtk-dialog-add-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_add_action_widget" gtk-dialog-add-action-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[child]{an activatable widget}
  @argument[response-id]{response ID for @arg{child}}
  @begin{short}
    Adds an activatable widget to the action area of a @class{gtk-dialog},
    connecting a signal handler that will emit the \"response\" signal on the
    dialog when the widget is activated.
  @end{short}
  The widget is appended to the end of the dialog's action area. If you want to
  add a non-activatable widget, simply pack it into the @code{action-area} field
  of the @class{gtk-dialog} struct."
  (dialog (g-object gtk-dialog))
  (child (g-object gtk-widget))
  (response-id gtk-response-type))

(export 'gtk-dialog-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_default_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_set_default_response" gtk-dialog-set-default-response)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[response-id]{a response ID}
  @begin{short}
    Sets the last widget in the dialog's action area with the given
    @arg{response-id} as the default widget for the @arg{dialog}.
  @end{short}
  Pressing \"Enter\" normally activates the default widget."
  (dialog (g-object gtk-dialog))
  (response-id gtk-response-type))

(export 'gtk-dialog-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_response_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_set_response_sensitive" gtk-dialog-set-response-sensitive)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[response-id]{a response ID}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls @code{(@fun{gtk-widget-set-sensitive} widget @arg{setting})} for each
    widget in the dialog's action area with the given @arg{response-id}. A
    convenient way to sensitize/desensitize dialog buttons.
  @end{short}"
  (dialog (g-object gtk-dialog))
  (response gtk-response-type)
  (setting :boolean))

(export 'gtk-dialog-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_response_for_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_response_for_widget"
          gtk-dialog-get-response-for-widget) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[widget]{a widget in the action area of @arg{dialog}}
  @return{The response id of @arg{widget}, or @code{:none} if @arg{widget}
    doesn't have a response id set.}
  @begin{short}
    Gets the response id of a @arg{widget} in the action area of a @arg{dialog}.
  @end{short}

  Since 2.8"
  (dialog (g-object gtk-dialog))
  (widget (g-object gtk-widget)))

(export 'gtk-dialog-get-response-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_widget_for_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_widget_for_response"
          gtk-dialog-get-widget-for-response) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[response-id]{the response ID used by the dialog widget}
  @return{The widget button that uses the given @arg{response-id},
    or @code{nil}.}
  @begin{short}
    Gets the widget button that uses the given response ID in the action area
    of a dialog.
  @end{short}

  Since 2.20"
  (dialog (g-object gtk-dialog))
  (response-id gtk-response-type))

(export 'gtk-dialog-get-widget-for-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_action_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_action_area" gtk-dialog-get-action-area)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @return{The action area.}
  @short{Returns the action area of dialog.}

  Since 2.14"
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-get-action-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_content_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_content_area" gtk-dialog-get-content-area)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @return{The content area @class{gtk-box}.}
  @short{Returns the content area of dialog.}

  Since 2.14"
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-get-content-area)

;;; ----------------------------------------------------------------------------
;;; gtk_alternative_dialog_button_order ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_alternative_dialog_button_order"
          gtk-alternative-dialog-button-order) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[screen]{a @class{gdk-screen}, or @code{nil} to use the default
    screen}
  @return{Whether the alternative button order should be used.}
  @begin{short}
    Returns @em{true} if dialogs are expected to use an alternative button order
    on the screen screen.
  @end{short}
  See @fun{gtk-dialog-set-alternative-button-order} for more details about
  alternative button order.

  If you need to use this function, you should probably connect to the
  \"notify:gtk-alternative-button-order\" signal on the @class{gtk-settings}
  object associated to screen, in order to be notified if the button order
  setting changes.

  Since 2.6"
  (screen (g-object gdk-screen)))

(export 'gtk-alternative-dialog-button-order)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order ()
;;; ----------------------------------------------------------------------------

(defun gtk-dialog-set-alternative-button-order (dialog response-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[response-list]{a list of response ids of dialog's buttons}
  @begin{short}
    Sets an alternative button order.
  @end{short}
  If the @code{\"gtk-alternative-button-order\"} setting is set to @em{true},
  the dialog buttons are reordered according to the order of the response ids
  passed to this function.

  By default, GTK+ dialogs use the button order advocated by the Gnome Human
  Interface Guidelines with the affirmative button at the far right, and the
  cancel button left of it. But the builtin GTK+ dialogs and
  @class{gtk-message-dialog}s do provide an alternative button order, which is
  more suitable on some platforms, e. g. Windows.

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
  Since 2.6"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_set_alternative_button_order_from_array"
          gtk-dialog-set-alternative-button-order-from-array) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[dialog]{a @class{gtk-dialog} widget}
  @argument[n-params]{the number of response ids in @arg{new-order}}
  @argument[new-order]{an array of response ids of dialog's buttons}
  @short{Sets an alternative button order.}
  If the @code{\"gtk-alternative-button-order\"} setting is set to @em{true},
  the dialog buttons are reordered according to the order of the response ids
  in @arg{new-order}.

  See @fun{gtk-dialog-set-alternative-button-order} for more information.

  This function is for use by language bindings.

  Since 2.6"
  (dialog (g-object gtk-dialog))
  (n-params :int)
  (new-order (:pointer gtk-response-type)))

(export 'gtk-dialog-set-alternative-button-order-from-array)

;;; --- End of file gtk.dialog.lisp --------------------------------------------
