;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.message-dialog.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

;;; A function to show a simple message dialog

#|
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
|#

;;; --- gtk-message-dialog -----------------------------------------------------

(setf (documentation 'gtk-message-dialog 'type)
 "@version{2013-1-30}
  @begin{short}
    GtkMessageDialog presents a dialog with an image representing the type of
    message (Error, Question, etc.) alongside some message text.
  @end{short}
  It's simply a convenience widget; you could construct the equivalent of
  GtkMessageDialog from GtkDialog without too much effort, but GtkMessageDialog
  saves typing.

  One difference from GtkDialog is that GtkMessageDialog sets the
  \"skip-taskbar-hint\" property to TRUE, so that the dialog is hidden from the
  taskbar by default.
 
  The easiest way to do a modal message dialog is to use gtk_dialog_run(),
  though you can also pass in the GTK_DIALOG_MODAL flag, gtk_dialog_run()
  automatically makes the dialog modal and waits for the user to respond to
  it. gtk_dialog_run() returns when any dialog button is clicked.
  
  Example 46. A modal dialog.
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
  You might do a non-modal GtkMessageDialog as follows:
 
  Example 47. A non-modal dialog.
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
  @heading{GtkMessageDialog as GtkBuildable}
  The GtkMessageDialog implementation of the GtkBuildable interface exposes
  the message area as an internal child with the name \"message_area\".
  @begin[Style Properties]{dictionary}
    @heading{The \"message-border\" style property}
    \"message-border\" of type @code{gint} (Read)@br{}
    Width of border around the label and image in the message dialog.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{12}
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

(setf (documentation (atdoc:get-slot-from-name "buttons" 'gtk-message-dialog) 't)
 "The @code{\"buttons\"} property of type @symbol{gtk-buttons-type}
  (Write / Construct Only)@br{}
  The buttons shown in the message dialog.@br{}
  Default value: @code{:none}")

(setf (documentation (atdoc:get-slot-from-name "image" 'gtk-message-dialog) 't)
 "The @code{\"image\"} property of type @code{gtk-widget} (Read / Write)@br{}
  The image for this dialog.@br{}
  Since 2.10")

(setf (documentation (atdoc:get-slot-from-name "message-area" 'gtk-message-dialog) 't)
 "The @code{\"message-area\"} property @class{gtk-widget} (Read)@br{}
  The GtkVBox that corresponds to the message area of this dialog. See
  gtk_message_dialog_get_message_area() for a detailed description of this
  area.@br{}
  Since 2.22")

(setf (documentation (atdoc:get-slot-from-name "message-type" 'gtk-message-dialog) 't)
 "The @code{\"message-type\"} property of type @symbol{gtk-message-type}
  (Read / Write / Construct)@br{}
  The type of the message. The type is used to determine the image that is
  shown in the dialog, unless the image is explicitly set by the ::image
  property.@br{}
  Default value: @code{:info}")

(setf (documentation (atdoc:get-slot-from-name "secondary-text" 'gtk-message-dialog) 't)
 "The @code{\"secondary-text\"} property of type @code{gchar*}
  (Read / Write)@br{}
  The secondary text of the message dialog.@br{}
  Default value: NULL@br{}
  Since 2.10")

(setf (documentation (atdoc:get-slot-from-name "secondary-use-markup" 'gtk-message-dialog) 't)
 "The @code{\"secondary-use-markup\"} property of type @code{gboolean}
  (Read / Write)@br{}
  TRUE if the secondary text of the dialog includes Pango markup. See
  pango_parse_markup().@br{}
  Default value: FALSE@br{}
  Since 2.10")

(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-message-dialog) 't)
 "The @code{\"text\"} property of type @code{gchar*} (Read / Write)@br{}
  The primary text of the message dialog. If the dialog has a secondary text,
  this will appear as the title.@br{}
  Default value: \"\"@br{}
  Since 2.10")

(setf (documentation (atdoc:get-slot-from-name "use-markup" 'gtk-message-dialog) 't)
 "The @code{\"use-markup\"} property of type @code{gboolean}
  (Read / Write)@br{}
  TRUE if the primary text of the dialog includes Pango markup. See
  pango_parse_markup().@br{}
  Default value: FALSE@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-message-dialog-buttons ---------------------------------------------

(setf (gethash 'gtk-message-dialog-buttons atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-buttons 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"buttons\"} of the @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-image -----------------------------------------------

(setf (gethash 'gtk-message-dialog-image atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-image 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"image\"} of the @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-message-area ----------------------------------------

(setf (gethash 'gtk-message-dialog-message-area atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-message-area 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"message-area\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-image -----------------------------------------------

(setf (gethash 'gtk-message-dialog-message-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-message-type 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"message-type\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-secondary-text --------------------------------------

(setf (gethash 'gtk-message-dialog-secondary-text atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-secondary-text 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"secondary-text\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-secondary-use-markup --------------------------------

(setf (gethash 'gtk-message-dialog-secondary-use-markup atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-secondary-use-markup 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"secondary-use-markup\"} of the
    @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-text ------------------------------------------------

(setf (gethash 'gtk-message-dialog-text atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-text 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"text\"} of the @class{gtk-message-dialog} class.
  @end{short}")

;;; --- gtk-message-dialog-use-markup ------------------------------------------

(setf (gethash 'gtk-message-dialog-use-markup atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-message-dialog-use-markup 'function)
 "@version{2013-1-30}
  @begin{short}
    Accessor of the slot @code{\"use-markup\"} of the @class{gtk-message-dialog}
    class.
  @end{short}")

;;; --- gtk-message-type -------------------------------------------------------

(setf (gethash 'gtk-message-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-message-type atdoc:*external-symbols*)
 "@version{2013-1-30}
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
    @entry[:info]{Informational message}
    @entry[:warning]{Nonfatal warning message}
    @entry[:question]{Question requiring a choice}
    @entry[:error]{Fatal error message}
    @entry[:other]{None of the above, doesn't get an icon}
  @end{table}")

;;; --- gtk-buttons-type -------------------------------------------------------

(setf (gethash 'gtk-buttons-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-buttons-type atdoc:*external-symbols*)
 "@version{2013-1-30}
  @begin{short}
    Prebuilt sets of buttons for the dialog. If none of these choices are
     appropriate, simply use GTK_BUTTONS_NONE then call
     gtk_dialog_add_buttons().
  @end{short}

  @b{Note}

  Please note that GTK_BUTTONS_OK, GTK_BUTTONS_YES_NO and
  GTK_BUTTONS_OK_CANCEL are discouraged by the GNOME HIG.
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
    @entry[:none]{no buttons at all}
    @entry[:ok]{an OK button}
    @entry[:close]{a Close button}
    @entry[:cancel]{a Cancel button}
    @entry[:yes-no]{Yes and No buttons}
    @entry[:ok-cancel]{OK and Cancel buttons}
  @end{table}")

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

;;; --- gtk-message-dialog-set-markup ------------------------------------------

(setf (documentation 'gtk-message-dialog-set-markup 'function)
 "@version{2013-1-30}
  @argument[message-dialog]{a GtkMessageDialog}
  @argument[str]{markup string (see Pango markup format)}
  @begin{short}
    Sets the text of the message dialog to be str, which is marked up with the
    Pango text markup language.
  @end{short}

  Since 2.4")

;;; --- gtk-message-dialog-set-image -------------------------------------------

(setf (documentation 'gtk-message-dialog-set-image 'function)
 "@version{2013-1-30}
  @argument[dialog]{a GtkMessageDialog}
  @argument[image]{the image}
  @begin{short}
    Sets the dialog's image to image.
  @end{short}

  Since 2.10")

;;; --- gtk-message-dialog-get-image -------------------------------------------

(setf (documentation 'gtk-message-dialog-get-image 'function)
 "@version{2013-1-30}
  @argument[dialog]{a GtkMessageDialog}
  @return{the dialog's image}
  @begin{short}
    Gets the dialog's image.
  @end{short}

  Since 2.14")

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

;;; --- gtk-message-dialog-get-message-area ------------------------------------

(setf (documentation 'gtk-message-dialog-get-message-area 'function)
 "@version{2013-1-30}
  @argument[message_dialog]{a GtkMessageDialog}
  @return{A GtkVBox corresponding to the \"message area\" in the
    message_dialog.}
  @begin{short}
    Returns the message area of the dialog. This is the box where the dialog's
    primary and secondary labels are packed.
  @end{short}
  You can add your own extra content to that box and it will appear below those
  labels, on the right side of the dialog's image (or on the left for
  right-to-left languages). See gtk_dialog_get_content_area() for the
  corresponding function in the parent GtkDialog.

  Since 2.22")

;;; --- End of file atdoc-gtk.message-dialog.lisp ------------------------------
