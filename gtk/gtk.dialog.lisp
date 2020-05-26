;;; ----------------------------------------------------------------------------
;;; gtk.dialog.lisp
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
;;; GtkDialog
;;;
;;;     Create popup windows
;;;
;;; Types and Values
;;;
;;;     GtkDialog
;;;     GtkDialogFlags
;;;     GtkResponseType
;;;
;;; Functions
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
;;;     gtk_dialog_get_header_bar
;;;     gtk_alternative_dialog_button_order                * deprecated
;;;     gtk_dialog_set_alternative_button_order            * deprecated
;;;     gtk_dialog_set_alternative_button_order_from_array * deprecated
;;;
;;; Properties
;;;
;;;     gint   use-header-bar          Read / Write / Construct Only
;;;
;;; Style Properties
;;;
;;;     gint   action-area-border      Read
;;;     gint   button-spacing          Read
;;;     gint   content-area-border     Read
;;;     gint   content-area-spacing    Read
;;;
;;; Signals
;;;
;;;     void   close                   Action
;;;     void   response                Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ├── GtkAboutDialog
;;;                             ├── GtkAppChooserDialog
;;;                             ├── GtkColorChooserDialog
;;;                             ├── GtkColorSelectionDialog
;;;                             ├── GtkFileChooserDialog
;;;                             ├── GtkFontChooserDialog
;;;                             ├── GtkFontSelectionDialog
;;;                             ├── GtkMessageDialog
;;;                             ├── GtkPageSetupUnixDialog
;;;                             ├── GtkPrintUnixDialog
;;;                             ╰── GtkRecentChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkDialogFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkDialogFlags" gtk-dialog-flags
  (:export t
   :type-initializer "gtk_dialog_flags_get_type")
  (:modal               #.(ash 1 0))
  (:destroy-with-parent #.(ash 1 1))
  #+gtk-3-12
  (:use-header-bar      #.(ash 1 2))
  )

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-dialog-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-dialog-flags atdoc:*external-symbols*)
 "@version{*2020-5-26}
  @short{Flags used to influence the dialog construction.}
  @begin{pre}
(define-g-flags \"GtkDialogFlags\" gtk-dialog-flags
  (:export t
   :type-initializer \"gtk_dialog_flags_get_type\")
  (:modal               #.(ash 1 0))
  (:destroy-with-parent #.(ash 1 1))
  (:use-header-bar      #.(ash 1 2)))
  @end{pre}
  @begin[code]{table}
    @entry[:modal]{Make the constructed dialog modal, see the function
      @fun{gtk-window-modal}.}
    @entry[:destroy-with-parent]{Destroy the dialog when its parent is
      destroyed, see the function @fun{gtk-window-destroy-with-parent}.}
    @entry[:use-header-bar]{Create the dialog with actions in the header bar
      instead of an action area. Since 3.12}
  @end{table}
  @see-class{gtk-dialog}
  @see-function{gtk-window-modal}
  @see-function{gtk-window-destroy-with-parent}")

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-response-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-response-type atdoc:*external-symbols*)
 "@version{*2020-5-26}
  @begin{short}
    Predefined values for use as response IDs in the function
    @fun{gtk-dialog-add-button}.
  @end{short}
  All predefined values are negative, GTK+ leaves positive values for
  application defined response IDs.
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
    @entry[:none]{Returned if an action widget has no response ID, or if the
      dialog gets programmatically hidden or destroyed.}
    @entry[:reject]{Generic response ID, not used by GTK+ dialogs.}
    @entry[:accept]{Generic response ID, not used by GTK+ dialogs.}
    @entry[:delete-event]{Returned if the dialog is deleted.}
    @entry[:ok]{Returned by OK buttons in GTK+ dialogs.}
    @entry[:cancel]{Returned by Cancel buttons in GTK+ dialogs.}
    @entry[:close]{Returned by Close buttons in GTK+ dialogs.}
    @entry[:yes]{Returned by Yes buttons in GTK+ dialogs.}
    @entry[:no]{Returned by No buttons in GTK+ dialogs.}
    @entry[:apply]{Returned by Apply buttons in GTK+ dialogs.}
    @entry[:help]{Returned by Help buttons in GTK+ dialogs.}
  @end{table}
  @see-class{gtk-dialog}
  @see-function{gtk-dialog-add-button}")

;;; ----------------------------------------------------------------------------
;;; struct GtkDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkDialog" gtk-dialog
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_dialog_get_type")
  (#+gtk-3-12
   (use-header-bar
    gtk-dialog-use-header-bar
    "use-header-bar" "gint" t t)
    ))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-dialog 'type)
 "@version{*2020-5-26}
  @begin{short}
    Dialog windows are a convenient way to prompt the user for a small amount
    of input, e. g. to display a message, ask a question, or anything else that
    does not require extensive effort on the user's part.
  @end{short}

  GTK+ treats a dialog as a window split vertically. The top section is known as
  the \"content area\" and is a @class{gtk-box} widget with a @code{:vertical}
  orientation. This is where widgets such as a @class{gtk-label} or a
  @class{gtk-entry} should be packed. The bottom area is known as the
  \"action area\". This is generally used for packing buttons into the dialog
  which may perform functions such as Cancel, OK, or Apply.

  @sym{gtk-dialog} windows are created with a call to to the functions
  @fun{gtk-dialog-new} or @fun{gtk-dialog-new-with-buttons}. The function
  @fun{gtk-dialog-new-with-buttons} is recommended. It allows you to set the
  dialog title, some convenient flags, and add simple buttons.

  If the dialog is a newly created dialog, the two primary areas of the window
  can be accessed through the functions @fun{gtk-dialog-content-area} and
  @fun{gtk-dialog-action-area}.

  A modal dialog, that is, one which freezes the rest of the application
  from user input, can be created by calling the function @fun{gtk-window-modal}
  on the dialog. When using the function @fun{gtk-dialog-new-with-buttons} you
  can also pass the @code{:modal} flag of type @symbol{gtk-dialog-flags} to make
  a dialog modal.

  If you add buttons to a @sym{gtk-dialog} window using the functions
  @fun{gtk-dialog-new-with-buttons}, @fun{gtk-dialog-add-button},
  @fun{gtk-dialog-add-buttons}, or @fun{gtk-dialog-add-action-widget}, clicking
  the button will emit a signal called \"response\" with a response ID that you
  specified. GTK+ will never assign a meaning to positive response IDs. These
  are entirely user-defined. But for convenience, you can use the response IDs
  in the @symbol{gtk-response-type} enumeration. These all have values less than
  zero. If a dialog receives a delete event, the \"response\" signal will be
  emitted with a response ID of @code{:delete-event}.

  If you want to block waiting for a dialog to return before returning
  control flow to your code, you can call the function @fun{gtk-dialog-run}.
  This function enters a recursive main loop and waits for the user to respond
  to the dialog, returning the response ID corresponding to the button the user
  clicked.

  For the simple dialog in the following example, in reality you would probably
  use a @class{gtk-message-dialog} window to save yourself some effort. But
  you would need to create the dialog contents manually if you had more than a
  simple message in the dialog.
  @begin[Example]{dictionary}
    Simple @sym{gtk-dialog} usage
    @begin{pre}
;; Function to open a dialog window displaying the message provided.
(defun quick-message (window message)
  (let (;; Create the widgets
        (dialog (gtk-dialog-new-with-buttons \"Message\"
                                             window
                                             '(:destroy-with-parent)
                                             \"gtk-ok\"
                                             :none))
        (label (gtk-label-new message)))
    ;; Ensure that the dialog window is destroyed when the user responds.
    (g-signal-connect dialog \"response\"
                      (lambda (dialog response-id)
                        (declare (ignore response-id))
                        (gtk-widget-destroy dialog)))
    ;; Add the label, and show everything we have added to the dialog.
    (gtk-container-add (gtk-dialog-content-area dialog) label)
    (gtk-widget-show-all dialog)))
    @end{pre}
    You can use a dialog window as a toplevel window from Lisp code. The
    following code shows a complete example of a function which displays a
    message in a dialog window. In this case you have to connect to the
    \"response\" signal. It is not possible to use the functions
    @fun{gtk-dialog-run} and @fun{gtk-dialog-response} for this toplevel dialog.
    In the Lisp binding your program will hang, when using this functions to run
    the dialog window and to get the response.

    A toplevel dialog which can be called from any Lisp code
    @begin{pre}
(defun demo-dialog-toplevel (message)
 (let ((response nil))
   (within-main-loop
    (let (;; Create the widgets
          (dialog (gtk-dialog-new-with-buttons \"Demo Toplevel Dialog\"
                                               nil ; No Parent window
                                               '(:modal)
                                               \"gtk-ok\"
                                               :none
                                               \"gtk-cancel\"
                                               :cancel))
          (label (gtk-label-new message)))
      ;; Signal handler for the dialog to handle the signal \"destroy\".
      (g-signal-connect dialog \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Quit the main loop and destroy the thread.
                          (leave-gtk-main)))
      ;; Get the response and destroy the dialog.
      (g-signal-connect dialog \"response\"
                        (lambda (dialog response-id)
                          (declare (ignore response-id))
                          (setf response response-id)
                          (gtk-widget-destroy dialog)))
      ;; Add the label, and show everything we have added to the dialog.
      (gtk-container-add (gtk-dialog-content-area dialog) label)
      (gtk-widget-show-all dialog)))
    ;; Wait until the dialog is destroyed.
    (join-gtk-main)
    (when response
      (format t \"The response ID is ~A\" response))))
    @end{pre}
  @end{dictionary}
  @begin[GtkDialog as GtkBuildable]{dictionary}
    The @sym{gtk-dialog} implementation of the @class{gtk-buildable} interface
    exposes the @code{vbox} and @code{action-area} as internal children with the
    names \"vbox\" and \"action_area\".

    @sym{gtk-dialog} supports a custom @code{<action-widgets>} element, which
    can contain multiple @code{<action-widget>} elements. The
    @code{\"response\"} attribute specifies a numeric response, and the content
    of the element is the ID of widget, which should be a child of the dialogs
    @code{action-area}. To mark a response as default, set the
    @code{\"default\"} attribute of the @code{<action-widget>} element to true.

    @sym{gtk-dialog} supports adding action widgets by specifying
    @code{\"action\"} as the @code{\"type\"} attribute of a @code{<child>}
    element. The widget will be added either to the action area or the headerbar
    of the dialog, depending on the @code{\"use-header-bar\"} property. The
    response ID has to be associated with the action widget using the
    @code{<action-widgets>} element.

    @b{Example:} A @sym{gtk-dialog} UI definition fragment.
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
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[action-area-border]{entry}
        The @code{action-area-border} style property of type @code{:int}
        (Read) @br{}
        Width of border around the button area at the bottom of the dialog.@br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[button-spacing]{entry}
        The @code{button-spacing} style property of type @code{:int}
        (Read) @br{}
        Spacing between buttons. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
      @begin[content-area-border]{entry}
        The @code{content-area-border} style property of type @code{:int}
        (Read) @br{}
        Width of border around the main dialog area. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
      @begin[content-area-spacing]{entry}
        The @code{content-area-spacing} style property of type @code{:int}
        Read) @br{}
        The default spacing used between elements of the content area of the
        dialog, as returned by the function @fun{gtk-dialog-content-area},
        unless the function @fun{gtk-box-spacing} was called on that widget
        directly. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"close\" signal}
      @begin{pre}
 lambda (dialog)    : Action
      @end{pre}
      The \"close signal\" is a keybinding signal which gets emitted when the
      user uses a keybinding to close the dialog. The default binding for this
      signal is the Escape key.
      @begin[code]{table}
        @entry[dialog]{The @sym{gtk-dialog} window on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"response\" signal}
      @begin{pre}
 lambda (dialog response-id)    : Run Last
      @end{pre}
      Emitted when an action widget is clicked, the dialog receives a delete
      event, or the application programmer calls the function
      @fun{gtk-dialog-response}. On a delete event, the response ID is the
      @code{:delete-event} value of the @symbol{gtk-response-type} enumeration.
      Otherwise, it depends on which action widget was clicked.
      @begin[code]{table}
        @entry[dialog]{The @sym{gtk-dialog} window on which the signal is
          emitted.}
        @entry[response-id]{The response ID of type @code{:int}.}
      @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-dialog-use-header-bar ----------------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "use-header-bar"
                                               'gtk-dialog) 't)
 "The @code{use-header-bar} property of type @code{:int}
  (Read / Write / Construct) @br{}
  @em{True} if the dialog uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}. Since 3.12 @br{}
  Allowed values: [-1, 1] @br{}
  Default value: -1")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-dialog-use-header-bar atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-dialog-use-header-bar 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-dialog-use-header-bar object) => header-bar}
  @syntax[]{(setf (gtk-dialog-use-header-bar object) header-bar)}
  @argument[object]{a @class{gtk-dialog} window}
  @argument[header-bar]{a @class{gtk-header-bar} widget}
  @begin{short}
    Accessor of the @slot[gtk-dialog]{use-header-bar} slot of the
    @class{gtk-dialog} class.
  @end{short}

  @em{True} if the dialog uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}.

  Since 3.12
  @see-class{gtk-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-dialog-new))

(defun gtk-dialog-new ()
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @return{The new @class{gtk-dialog} window.}
  @short{Creates a new dialog window.}

  Widgets should not be packed into this dialog window directly, but into
  the content area and action area, which can be accessed with the functions
  @fun{gtk-dialog-content-area} and @fun{gtk-dialog-action-area}.
  @see-class{gtk-dialog}
  @see-function{gtk-dialog-action-area}
  @see-function{gtk-dialog-content-area}"
  (make-instance 'gtk-dialog))

(export 'gtk-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new_with_buttons ()
;;; ----------------------------------------------------------------------------

(defun gtk-dialog-new-with-buttons (title parent flags &rest buttons)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk-window} transient parent of the dialog,
    or @code{nil}}
  @argument[flags]{a list of flags of type @symbol{gtk-dialog-flags}}
  @argument[buttons]{pairs with a button text or stock ID and the response ID
    for the button, which is a positive integer or a value of the
    @symbol{gtk-response-type} enumeration}
  @return{A new @class{gtk-dialog} window.}
  @begin{short}
    Creates a new dialog window with title @arg{title}, or @code{nil} for the
    default title, see the function @fun{gtk-window-title}, and transient
    parent @arg{parent}, or @code{nil} for none, see the function
    @fun{gtk-window-transient-for}.
  @end{short}
  The argument @arg{flags} can be used to make the dialog modal with the
  @code{:modal} flag of type @symbol{gtk-dialog-flags} and/or to have it
  destroyed along with its transient parent with the @code{:destroy-with-parent}
  flag.

  After the argument @arg{flags}, button text/response ID pairs should be
  listed. Button text can be either a stock ID such as @code{\"gtk-ok\"}, or
  some arbitrary text. A response ID can be any positive number, or one of the
  values in the @symbol{gtk-response-type} enumeration. If the user clicks one
  of these dialog buttons, @sym{gtk-dialog} will emit the \"response\" signal
  with the corresponding response ID. If a @sym{gtk-dialog} receives the
  \"delete-event\" signal, it will emit the \"response\" signal with a response
  ID of @code{:delete-event}. However, destroying a dialog does not emit the
  \"response\" signal. So be careful relying on the \"response\" signal when
  using the @code{:destroy-with-parent} flag. Buttons are from left to right,
  so the first button in the list will be the leftmost button in the dialog.
  @begin[Example]{dictionary}
    Here is a simple example:
    @begin{pre}
(let ((dialog (gtk-dialog-new-with-buttons \"My dialog\"
                                           main-app-window
                                           '(:modal :destroy-with-parent)
                                           \"gtk-ok\"
                                           :accept
                                           \"gtk-cancel\"
                                           :reject)))
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk-dialog}
  @see-symbol{gtk-dialog-flags}
  @see-symbol{gtk-response-type}
  @see-function{gtk-window-title}
  @see-function{gtk-window-transient-for}"
  (let ((dialog (make-instance 'gtk-dialog)))
    (when title
      (setf (gtk-window-title dialog) title))
    (when parent
      (setf (gtk-window-transient-for dialog) parent))
    (when (member :modal flags)
      (setf (gtk-window-modal dialog) t))
    (when (member :destroy-with-parent flags)
      (setf (gtk-window-destroy-with-parent dialog) t))
    (when buttons
     (apply #'gtk-dialog-add-buttons (cons dialog buttons)))
    dialog))

(export 'gtk-dialog-new-with-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_run" gtk-dialog-run) gtk-response-type
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @return{The response ID, which is a positive integer or a value of the
    @symbol{gtk-response-type} enumeration.}
  @begin{short}
    Blocks in a recursive main loop until the dialog window either emits the
    \"response\" signal, or is destroyed.
  @end{short}
  If the dialog window is destroyed during the call to the function
  @sym{gtk-dialog-run}, it returns the response ID @code{:none}. Otherwise, it
  returns the response ID from the \"response\" signal emission.

  Before entering the recursive main loop, the function @sym{gtk-dialog-run}
  calls the function @fun{gtk-widget-show} on the dialog window for you. Note
  that you still need to show any children of the dialog window yourself.

  During the execution of the function @sym{gtk-dialog-run}, the default
  behavior of the \"delete-event\" signal is disabled. If the dialog window
  receives the \"delete-event\" signal, it will not be destroyed as windows
  usually are, and the function @sym{gtk-dialog-run} will return the response
  ID @code{:delete-event}. Also, during @sym{gtk-dialog-run} the dialog window
  will be modal. You can force the function @sym{gtk-dialog-run} to return at
  any time by calling the function @fun{gtk-dialog-response} to emit the
  \"response\" signal. Destroying the dialog window during @sym{gtk-dialog-run}
  is a very bad idea, because your post-run code will not know whether the
  dialog was destroyed or not.

  After the function @sym{gtk-dialog-run} returns, you are responsible for
  hiding or destroying the dialog if you wish to do so.
  @begin[Example]{dictionary}
    Typical usage of this function might be:
    @begin{pre}
 (let ((response (gtk-dialog-run dialog)))
   (cond ((eql response :ok)
          (do-application-specific-something))
         (t
          (do-nothing-since-dialog-was-cancelled)))
   (gtk-widget-destroy dialog))
    @end{pre}
    Note that even though the recursive main loop gives the effect of a modal
    dialog, because it prevents the user from interacting with other windows in
    the same window group while the dialog is run, callbacks such as timeouts,
    IO channel watches, DND drops, etc, will be triggered during a
    @sym{gtk-dialog-run} call.
  @end{dictionary}
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-response}
  @see-function{gtk-widget-show}"
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-run)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_response" gtk-dialog-response) :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[response-id]{response ID, which is a positive integer or a value of
    the @symbol{gtk-response-type} enumeration}
  @begin{short}
    Emits the \"response\" signal with the given response ID.
  @end{short}
  Used to indicate that the user has responded to the dialog window in some way.
  Typically either you or the function @fun{gtk-dialog-run} will be monitoring
  the \"response\" signal and take appropriate action.
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-run}"
  (dialog (g-object gtk-dialog))
  (response-id gtk-response-type))

(export 'gtk-dialog-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_add_button" gtk-dialog-add-button) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[button-text]{a string with text of the button, or stock ID}
  @argument[response-id]{response ID for the button, wich is a positive integer
    or a value of the @symbol{gtk-response-type} enumeration}
  @return{The @class{gtk-button} widget that was added.}
  @begin{short}
    Adds a button with the given text or a stock button, if @arg{button-text}
    is a stock ID and sets things up so that clicking the button will emit the
    \"response\" signal with the given @arg{response-id}.
  @end{short}
  The button is appended to the end of the dialog's action area. The
  button widget is returned.
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-add-buttons}
  @see-function{gtk-dialog-add-action-widget}"
  (dialog (g-object gtk-dialog))
  (button-text :string)
  (response-id gtk-response-type))

(export 'gtk-dialog-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_buttons ()
;;; ----------------------------------------------------------------------------

(defun gtk-dialog-add-buttons (dialog &rest buttons)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[buttons]{pairs with a button text or stock ID and the response ID,
    which is a positive integer or a value of the @symbol{gtk-response-type}
    enumeration}
  @begin{short}
    Adds more buttons, same as calling the function @fun{gtk-dialog-add-button}
    repeatedly.
  @end{short}
  Each button must have both text and response ID.
  @begin[Note]{dictionary}
    The Lisp implementation does not call the C function, but the function
    @fun{gtk-dialog-add-button} is called in a loop to add the buttons.
  @end{dictionary}
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-add-button}
  @see-function{gtk-dialog-add-action-widget}"
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
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[child]{an activatable @class{gtk-widget} widget}
  @argument[response-id]{response ID for @arg{child}, which is a positive
    integer or a value of the @symbol{gtk-response-type} enumeration}
  @begin{short}
    Adds an activatable child widget to the action area of the dialog window,
    connecting a signal handler that will emit the \"response\" signal on the
    dialog when the child widget is activated.
  @end{short}
  The child widget is appended to the end of the dialog's action area. If you
  want to add a non-activatable widget, simply pack it into the action area of
  the dialog window.
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-add-button}
  @see-function{gtk-dialog-add-buttons}"
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
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[response-id]{a response ID, which is a positive integer or a value
    of the @symbol{gtk-response-type} enumeration}
  @begin{short}
    Sets the last widget in the dialog's action area with the given
    @arg{response-id} as the default widget for the dialog.
  @end{short}
  Pressing \"Enter\" normally activates the default widget.
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}"
  (dialog (g-object gtk-dialog))
  (response-id gtk-response-type))

(export 'gtk-dialog-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_response_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_set_response_sensitive" gtk-dialog-set-response-sensitive)
    :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[response-id]{a response ID, which is a positive integer or a value
    of the @symbol{gtk-response-type} enumeration}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls the function @fun{gtk-widget-sensitive} for each widget in the
    dialog's action area with the given @arg{response-id}.
  @end{short}
  A convenient way to sensitize/desensitize dialog buttons.
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-widget-sensitive}"
  (dialog (g-object gtk-dialog))
  (response gtk-response-type)
  (setting :boolean))

(export 'gtk-dialog-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_response_for_widget () -> gtk-dialog-response-for-widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_response_for_widget" gtk-dialog-response-for-widget)
    :int
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[widget]{a @class{gtk-widget} widget in the action area of
    @arg{dialog}}
  @return{The response ID of @arg{widget}, which is a positive integer or a
    value of the @symbol{gtk-response-type} enumeration, the value is
    @code{:none} if @arg{widget} does not have a response ID set}
  @begin{short}
    Gets the response ID of a the widget in the action area of the dialog.
  @end{short}
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-widget-for-response}"
  (dialog (g-object gtk-dialog))
  (widget (g-object gtk-widget)))

(export 'gtk-dialog-response-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_widget_for_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_widget_for_response" gtk-dialog-widget-for-response)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[response-id]{the response ID, which is a positive integer or a value
    of the @symbol{gtk-response-type} enumeration}
  @return{The @class{gtk-widget} button that uses the given @arg{response-id},
    or @code{nil}.}
  @begin{short}
    Gets the button that uses the given response ID in the action area of the
    dialog.
  @end{short}
  @see-class{gtk-dialog}
  @see-symbol{gtk-response-type}
  @see-function{gtk-dialog-response-for-widget}"
  (dialog (g-object gtk-dialog))
  (response-id gtk-response-type))

(export 'gtk-dialog-widget-for-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_action_area () -> gtk-dialog-action-area
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_action_area" gtk-dialog-action-area)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @return{The @class{gtk-widget} action area of the dialog.}
  @short{Returns the action area of the dialog.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-dialog-action-area} has been deprecated since version
    3.12 and should not be used in newly-written code. Direct access to the
    action area is discouraged. Use the function @fun{gtk-dialog-add-button},
    etc.
  @end{dictionary}
  @see-class{gtk-dialog}
  @see-function{gtk-dialog-content-area}"
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-action-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_content_area () -> gtk-dialog-content-area
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_dialog_get_content_area" gtk-dialog-content-area)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @begin{return}
    The @class{gtk-box} content area with a @code{:vertical} orientation.
  @end{return}
  @short{Returns the content area of the dialog.}
  @see-class{gtk-dialog}
  @see-class{gtk-box}
  @see-function{gtk-dialog-action-area}"
  (dialog (g-object gtk-dialog)))

(export 'gtk-dialog-content-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_header_bar () -> gtk-dialog-header-bar
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_dialog_get_header_bar" gtk-dialog-header-bar)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @return{The @class{gtk-header-bar} widget.}
  @begin{short}
    Returns the header bar of the dialog.
  @end{short}
  Note that the header bar is only used by the dialog if the
  @slot[gtk-dialog]{use-header-bar} property is @em{true}.

  Since 3.12
  @see-class{gtk-dialog}
  @see-class{gtk-box}
  @see-function{gtk-dialog-action-area}"
  (dialog (g-object gtk-dialog)))

#+gtk-3-12
(export 'gtk-dialog-header-bar)

;;; ----------------------------------------------------------------------------
;;; gtk_alternative_dialog_button_order ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_alternative_dialog_button_order"
          gtk-alternative-dialog-button-order) :boolean
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[screen]{a @class{gdk-screen} object, or @code{nil} to use the
    default screen}
  @return{A boolean whether the alternative button order should be used.}
  @begin{short}
    Returns @em{true} if dialogs are expected to use an alternative button order
    on the screen.
  @end{short}
  See the function @fun{gtk-dialog-set-alternative-button-order} for more
  details about alternative button order.

  If you need to use this function, you should probably connect to the
  \"notify:gtk-alternative-button-order\" signal on the @class{gtk-settings}
  object associated to the screen, in order to be notified if the button order
  setting changes.
  @begin[Warning]{dictionary}
    The function @sym{gtk-alternative-dialog-button-order} has been deprecated
    since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-dialog}
  @see-class{gdk-screen}
  @see-function{gtk-dialog-set-alternative-button-order}"
  (screen (g-object gdk-screen)))

(export 'gtk-alternative-dialog-button-order)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order ()
;;; ----------------------------------------------------------------------------

(defun gtk-dialog-set-alternative-button-order (dialog response-list)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[response-list]{a list of response IDs, which are positive integer
    or values of the  @symbol{gtk-response-type} enumeration}
  @begin{short}
    Sets an alternative button order.
  @end{short}
  If the @slot[gtk-settings]{gtk-alternative-button-order} setting is set to
  @em{true}, the dialog buttons are reordered according to the order of the
  response IDs passed to this function.

  By default, GTK+ dialogs use the button order advocated by the Gnome Human
  Interface Guidelines with the affirmative button at the far right, and the
  cancel button left of it. But the builtin GTK+ dialogs and message dialogs
  do provide an alternative button order, which is more suitable on some
  platforms, e. g. Windows.
  @begin[Example]{dictionary}
    Use this function after adding all the buttons to your dialog, as the
    following example shows:
    @begin{pre}
 (let (;; Create a dialog with three buttons
       (dialog (gtk-dialog-new-with-buttons \"Demo Dialog\"
                                            nil ; No Parent window
                                            '(:modal)
                                            \"gtk-cancel\"
                                            :cancel
                                            \"gtk-ok\"
                                            :ok
                                            \"gtk-apply\"
                                            :apply)))
   ;; Set the default button.
   (gtk-widget-grab-default (gtk-dialog-widget-for-response dialog :ok))

   ;; Allow alternative button order for the default screen.
   (setf (gtk-settings-gtk-alternative-button-order
           (gtk-settings-get-default))
         t)

   ;; Set the alternative button order.
   (gtk-dialog-set-alternative-button-order dialog '(:ok :cancel :apply))

   ...)
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-dialog-set-alternative-button-order} has been
    deprecated since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-dialog}
  @see-class{gtk-message-dialog}
  @see-symbol{gtk-response-type}"
  (with-foreign-object (new-order 'gtk-response-type (length response-list))
    (loop
       for i from 0
       for response in response-list
       do (setf (mem-aref new-order 'gtk-response-type i) response))
    (%gtk-dialog-set-alternative-button-order-from-array dialog
                                                         (length response-list)
                                                         new-order))
  response-list)

(export 'gtk-dialog-set-alternative-button-order)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order_from_array ()
;;; ----------------------------------------------------------------------------

;; This function is for internal use and not exported.
;; The function is called from gtk-dialog-set-alternative-button-order.

(defcfun ("gtk_dialog_set_alternative_button_order_from_array"
          %gtk-dialog-set-alternative-button-order-from-array) :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[dialog]{a @class{gtk-dialog} window}
  @argument[n-params]{the number of response IDs in @arg{new-order}}
  @argument[new-order]{an array of response IDs of dialog's buttons}
  @short{Sets an alternative button order.}
  If the @code{gtk-alternative-button-order} setting is set to @em{true},
  the dialog buttons are reordered according to the order of the response IDs
  in @arg{new-order}.

  See the function @fun{gtk-dialog-set-alternative-button-order} for more
  information.

  This function is for use by language bindings.
  @begin[Warning]{dictionary}
    The @sym{gtk-dialog-set-alternative-button-order} function has been
    deprecated since version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-dialog}"
  (dialog (g-object gtk-dialog))
  (n-params :int)
  (new-order (:pointer gtk-response-type)))

;;; --- End of file gtk.dialog.lisp --------------------------------------------
