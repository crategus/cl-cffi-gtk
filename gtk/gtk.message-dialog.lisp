;;; ----------------------------------------------------------------------------
;;; gtk.message-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkMessageDialog
;;; 
;;; A convenient message window
;;;     
;;; Synopsis
;;; 
;;;     GtkMessageDialog
;;;     GtkMessageType
;;;     GtkButtonsType
;;;     
;;;     gtk_message_dialog_new
;;;     gtk_message_dialog_new_with_markup
;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_set_image
;;;     gtk_message_dialog_get_image
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup
;;;     gtk_message_dialog_get_message_area
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; A function to show a simple message dialog

(defun show-message (message &key (buttons :ok) (message-type :info)
                                  (use-markup nil))
  (let ((dialog (make-instance 'gtk-message-dialog
                               :text message
                               :buttons buttons
                               :message-type message-type
                               :use-markup use-markup)))
    (prog1
      (gtk-dialog-run dialog)
      (gtk-widget-destroy dialog))))

(export 'show-message)

;;; Handle an error and show an error message dialog

(defmacro with-gtk-message-error-handler (&body body)
  (let ((dialog (gensym))
        (e (gensym)))
    `(handler-case
         (progn ,@body)
       (error (,e)
              (using* ((,dialog (make-instance
                                  'gtk-message-dialog
                                  :message-type :error
                                  :buttons :ok
                                  :text
                                  (format nil
                                          "Error~%~A~%during execution of~%~A"
                                          ,e
                                          '(progn ,@body)))))
                      (gtk-dialog-run ,dialog)
                      (gtk-widget-destroy ,dialog)
                      nil)))))

(export 'with-gtk-message-error-handler)

;;; ----------------------------------------------------------------------------
;;; struct GtkMessageDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMessageDialog" gtk-message-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_message_dialog_get_type")
  ((buttons
    gtk-message-dialog-buttons
    "buttons" "GtkButtonsType" nil nil)
   (image
    gtk-message-dialog-image
    "image" "GtkWidget" t t)
   (message-area
    gtk-message-dialog-message-area
    "message-area" "GtkWidget" t nil)
   (message-type
    gtk-message-dialog-message-type
    "message-type" "GtkMessageType" t t)
   (secondary-text
    gtk-message-dialog-secondary-text
    "secondary-text" "gchararray" t t)
   (secondary-use-markup
    gtk-message-dialog-secondary-use-markup
    "secondary-use-markup" "gboolean" t t)
   (text
    gtk-message-dialog-text
    "text" "gchararray" t t)
   (use-markup
    gtk-message-dialog-use-markup
    "use-markup" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-message-dialog 'type)
 "@version{2013-4-16}
  @begin{short}
    A @sym{gtk-message-dialog} window presents a dialog with an image
    representing the type of message (Error, Question, etc.) alongside some
    message text.
  @end{short}
  It is simply a convenience widget; you could construct the equivalent of a
  @sym{gtk-message-dialog} window from a @class{gtk-dialog} window without too
  much effort, but @sym{gtk-message-dialog} saves typing.

  One difference from @class{gtk-dialog} is that @sym{gtk-message-dialog} sets
  the @code{\"skip-taskbar-hint\"} property to @em{true}, so that the dialog is
  hidden from the taskbar by default.
 
  The easiest way to do a modal message dialog is to use the function
  @fun{gtk-dialog-run}, though you can also pass in the @code{:modal} flag, the
  function @fun{gtk-dialog-run} automatically makes the dialog modal and waits
  for the user to respond to it. @fun{gtk-dialog-run} returns when any dialog
  button is clicked.
  
  @b{Example:} A modal dialog.
  @begin{pre}
 dialog = gtk_message_dialog_new (main_application_window,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_CLOSE,
                                  \"Error loading file '%s': %s\",
                                  filename, g_strerror (errno));
 gtk_dialog_run (GTK_DIALOG (dialog));
 gtk_widget_destroy (dialog);
  @end{pre}
  You might do a non-modal @sym{gtk-message-dialog} as follows:
 
  @b{Example:} A non-modal dialog.
  @begin{pre}
 dialog = gtk_message_dialog_new (main_application_window,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_CLOSE,
                                  \"Error loading file '%s': %s\",
                                  filename, g_strerror (errno));
 
 /* Destroy the dialog when the user responds to it (e.g. clicks a button) */
 g_signal_connect_swapped (dialog, \"response\",
                           G_CALLBACK (gtk_widget_destroy),
                           dialog);
  @end{pre}
  @subheading{GtkMessageDialog as GtkBuildable}
    The @sym{gtk-message-dialog} implementation of the @class{gtk-buildable}
    interface exposes the message area as an internal child with the name
    \"message_area\".
  @begin[Style Properties]{dictionary}
    @subheading{The \"message-border\" style property}
      @code{\"message-border\"} of type @code{:int} (Read)@br{}
      Width of border around the label and image in the message dialog.@br{}
      Allowed values: >= 0@br{}
      Default value: 12
  @end{dictionary}
  @see-slot{gtk-message-dialog-buttons}
  @see-slot{gtk-message-dialog-image}
  @see-slot{gtk-message-dialog-message-area}
  @see-slot{gtk-message-dialog-message-type}
  @see-slot{gtk-message-dialog-secondary-text}
  @see-slot{gtk-message-dialog-secondary-use-markup}
  @see-slot{gtk-message-dialog-text}
  @see-slot{gtk-message-dialog-use-markup}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "buttons"
                                               'gtk-message-dialog) 't)
 "The @code{\"buttons\"} property of type @symbol{gtk-buttons-type}
  (Write / Construct Only)@br{}
  The buttons shown in the message dialog.@br{}
  Default value: @code{:none}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image" 'gtk-message-dialog) 't)
 "The @code{\"image\"} property of type @class{gtk-widget} (Read / Write)@br{}
  The image for this dialog.@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "message-area"
                                               'gtk-message-dialog) 't)
 "The @code{\"message-area\"} property of type @class{gtk-widget} (Read)@br{}
  The @class{gtk-box} widget of orientation @code{:vertical} that corresponds to
  the message area of this dialog. See @fun{gtk-message-dialog-get-message-area}
  for a detailed description of this area.@br{}
  Since 2.22")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "message-type"
                                               'gtk-message-dialog) 't)
 "The @code{\"message-type\"} property of type @symbol{gtk-message-type}
  (Read / Write / Construct)@br{}
  The type of the message. The type is used to determine the image that is
  shown in the dialog, unless the image is explicitly set by the
  @code{\"image\"} property.@br{}
  Default value: @code{:info}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-text"
                                               'gtk-message-dialog) 't)
 "The @code{\"secondary-text\"} property of type @code{:string}
  (Read / Write)@br{}
  The secondary text of the message dialog.@br{}
  Default value: @code{nil}@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-use-markup"
                                               'gtk-message-dialog) 't)
 "The @code{\"secondary-use-markup\"} property of type @code{:boolean}
  (Read / Write)@br{}
  @em{True} if the secondary text of the dialog includes Pango markup. See
  @symbol{pango-parse-markup}.@br{}
  Default value: @code{nil}@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-message-dialog) 't)
 "The @code{\"text\"} property of type @code{:string} (Read / Write)@br{}
  The primary text of the message dialog. If the dialog has a secondary text,
  this will appear as the title.@br{}
  Default value: \"\"@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-markup"
                                               'gtk-message-dialog) 't)
 "The @code{\"use-markup\"} property of type @code{:boolean}
  (Read / Write)@br{}
  @em{True} if the primary text of the dialog includes Pango markup. See
  @symbol{pango-parse-markup}.@br{}
  Default value: @code{nil}@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-buttons atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-buttons 'function)
 "@version{2013-4-16}
  Accessor of the slot @code{\"buttons\"} of the @class{gtk-message-dialog}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-image 'function)
 "@version{2013-4-16}
  Accessor of the slot @code{\"image\"} of the @class{gtk-message-dialog}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-message-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-message-area 'function)
 "@version{2013-4-16}
  @begin{short}
    Accessor of the slot @code{\"message-area\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-message-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-message-type 'function)
 "@version{2013-4-16}
  @begin{short}
    Accessor of the slot @code{\"message-type\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-secondary-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-secondary-text 'function)
 "@version{2013-4-16}
  @begin{short}
    Accessor of the slot @code{\"secondary-text\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-secondary-use-markup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-secondary-use-markup 'function)
 "@version{2013-4-16}
  @begin{short}
    Accessor of the slot @code{\"secondary-use-markup\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-text 'function)
 "@version{2013-4-16}
  @begin{short}
    Accessor of the slot @code{\"text\"} of the @class{gtk-message-dialog} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-use-markup 'function)
 "@version{2013-4-16}
  @begin{short}
    Accessor of the slot @code{\"use-markup\"} of the @class{gtk-message-dialog}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; enum GtkMessageType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMessageType" gtk-message-type
  (:export t
   :type-initializer "gtk_message_type_get_type")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-message-type atdoc:*external-symbols*)
 "@version{2013-4-16}
  @begin{short}
    The type of message being displayed in the dialog.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkMessageType\" gtk-message-type
  (:export t
   :type-initializer \"gtk_message_type_get_type\")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))
  @end{pre}
  @begin[code]{table}
    @entry[:info]{Informational message.}
    @entry[:warning]{Nonfatal warning message.}
    @entry[:question]{Question requiring a choice.}
    @entry[:error]{Fatal error message.}
    @entry[:other]{None of the above, does not get an icon.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum GtkButtonsType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkButtonsType" gtk-buttons-type
  (:export t
   :type-initializer "gtk_buttons_type_get_type")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-buttons-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-buttons-type atdoc:*external-symbols*)
 "@version{2013-4-16}
  @begin{short}
    Prebuilt sets of buttons for the dialog. If none of these choices are
    appropriate, simply use @code{:none} and call @fun{gtk-dialog-add-buttons}
    to add your own buttons.
  @end{short}

  @subheading{Note}
    Please note that @code{:ok}, @code{:yes-no} and @code{:ok-cancel} are
    discouraged by the GNOME HIG.
  @begin{pre}
(define-g-enum \"GtkButtonsType\" gtk-buttons-type
  (:export t
   :type-initializer \"gtk_buttons_type_get_type\")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No buttons at all.}
    @entry[:ok]{An OK button.}
    @entry[:close]{A Close button.}
    @entry[:cancel]{A Cancel button.}
    @entry[:yes-no]{Yes and No buttons.}
    @entry[:ok-cancel]{OK and Cancel buttons.}
  @end{table}
  @see-function{gtk-dialog-add-buttons}")

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new ()
;;; 
;;; GtkWidget * gtk_message_dialog_new (GtkWindow *parent,
;;;                                     GtkDialogFlags flags,
;;;                                     GtkMessageType type,
;;;                                     GtkButtonsType buttons,
;;;                                     const gchar *message_format,
;;;                                     ...);
;;; 
;;; Creates a new message dialog, which is a simple dialog with an icon
;;; indicating the dialog type (error, warning, etc.) and some text the user may
;;; want to see. When the user clicks a button a "response" signal is emitted
;;; with response IDs from GtkResponseType. See GtkDialog for more details.
;;; 
;;; parent :
;;;     transient parent, or NULL for none
;;; 
;;; flags :
;;;     flags
;;; 
;;; type :
;;;     type of message
;;; 
;;; buttons :
;;;     set of buttons to use
;;; 
;;; message_format :
;;;     printf()-style format string, or NULL
;;; 
;;; ... :
;;;     arguments for message_format
;;; 
;;; Returns :
;;;     a new GtkMessageDialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new_with_markup ()
;;; 
;;; GtkWidget * gtk_message_dialog_new_with_markup (GtkWindow *parent,
;;;                                                 GtkDialogFlags flags,
;;;                                                 GtkMessageType type,
;;;                                                 GtkButtonsType buttons,
;;;                                                 const gchar *message_format,
;;;                                                 ...);
;;; 
;;; Creates a new message dialog, which is a simple dialog with an icon
;;; indicating the dialog type (error, warning, etc.) and some text which is
;;; marked up with the Pango text markup language. When the user clicks a button
;;; a "response" signal is emitted with response IDs from GtkResponseType. See
;;; GtkDialog for more details.
;;; 
;;; Special XML characters in the printf() arguments passed to this function
;;; will automatically be escaped as necessary. (See g_markup_printf_escaped()
;;; for how this is implemented.) Usually this is what you want, but if you have
;;; an existing Pango markup string that you want to use literally as the label,
;;; then you need to use gtk_message_dialog_set_markup() instead, since you
;;; can't pass the markup string either as the format (it might contain '%'
;;; characters) or as a string argument.
;;; 
;;; GtkWidget *dialog;
;;; dialog = gtk_message_dialog_new (main_application_window,
;;;                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                                  GTK_MESSAGE_ERROR,
;;;                                  GTK_BUTTONS_CLOSE,
;;;                                  NULL);
;;; gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog),
;;;                                markup);
;;; 
;;; parent :
;;;     transient parent, or NULL for none
;;; 
;;; flags :
;;;     flags
;;; 
;;; type :
;;;     type of message
;;; 
;;; buttons :
;;;     set of buttons to use
;;; 
;;; message_format :
;;;     printf()-style format string, or NULL
;;; 
;;; ... :
;;;     arguments for message_format
;;; 
;;; Returns :
;;;     a new GtkMessageDialog
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-set-markup))

(defun gtk-message-dialog-set-markup (dialog text)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @argument[text]{markup string (see Pango markup format)}
  @begin{short}
    Sets the text of the message @arg{dialog} to be @arg{text}, which is marked
    up with the Pango text markup language.
  @end{short}

  Since 2.4"
  (setf (gtk-message-dialog-text dialog) text))

(export 'gtk-message-dialog-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_image ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-set-image))

(defun gtk-message-dialog-set-image (dialog image)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @argument[image]{the image}
  @begin{short}
    Sets the @arg{dialog}'s image to @arg{image}.
  @end{short}

  Since 2.10"
  (setf (gtk-message-dialog-image dialog) image))

(export 'gtk-message-dialog-set-image)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_get_image ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-get-image))

(defun gtk-message-dialog-get-image (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @return{The @arg{dialog}'s image.}
  @short{Gets the @arg{dialog}'s image.}

  Since 2.14"
  (gtk-message-dialog-image dialog))

(export 'gtk-message-dialog-get-image)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_text ()
;;; 
;;; void gtk_message_dialog_format_secondary_text
;;;                                           (GtkMessageDialog *message_dialog,
;;;                                            const gchar *message_format,
;;;                                            ...);
;;; 
;;; Sets the secondary text of the message dialog to be message_format (with
;;; printf()-style).
;;; 
;;; Note that setting a secondary text makes the primary text become bold,
;;; unless you have provided explicit markup.
;;; 
;;; message_dialog :
;;;     a GtkMessageDialog
;;; 
;;; message_format :
;;;     printf()-style format string, or NULL
;;; 
;;; ... :
;;;     arguments for message_format
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_markup ()
;;; 
;;; void gtk_message_dialog_format_secondary_markup
;;;                                           (GtkMessageDialog *message_dialog,
;;;                                            const gchar *message_format,
;;;                                            ...);
;;; 
;;; Sets the secondary text of the message dialog to be message_format (with
;;; printf()-style), which is marked up with the Pango text markup language.
;;; 
;;; Note that setting a secondary text makes the primary text become bold,
;;; unless you have provided explicit markup.
;;; 
;;; Due to an oversight, this function does not escape special XML characters
;;; like gtk_message_dialog_new_with_markup() does. Thus, if the arguments may
;;; contain special XML characters, you should use g_markup_printf_escaped() to
;;; escape it.
;;; 
;;; gchar *msg;
;;; 
;;; msg = g_markup_printf_escaped (message_format, ...);
;;; gtk_message_dialog_format_secondary_markup (message_dialog, "%s", msg);
;;; g_free (msg);
;;; 
;;; message_dialog :
;;;     a GtkMessageDialog
;;; 
;;; message_format :
;;;     printf()-style markup string (see Pango markup format), or NULL
;;; 
;;; ... :
;;;     arguments for message_format
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_get_message_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_message_dialog_get_message_area"
           gtk-message-dialog-get-message-area) (g-object g-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @begin{return}
    A @class{gtk-box} with orientation @code{:vertical} corresponding to
    the \"message area\" in the message @arg{dialog}.
  @end{return}
  @begin{short}
    Returns the message area of the @arg{dialog}. This is the box where the
    @arg{dialog}'s primary and secondary labels are packed.
  @end{short}
  You can add your own extra content to that box and it will appear below those
  labels, on the right side of the @arg{dialog}'s image (or on the left for
  right-to-left languages). See @fun{gtk-dialog-get-content-area} for the
  corresponding function in the parent @class{gtk-dialog} class.

  Since 2.22
  @see-function{gtk-dialog-get-content-area}"
  (dialog (g-object gtk-message-dialog)))

(export 'gtk-message-dialog-get-message-area)

;;; --- End of file gtk.message-dialog.lisp ------------------------------------
