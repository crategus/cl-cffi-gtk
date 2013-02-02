;;; ----------------------------------------------------------------------------
;;; gtk.message-dialog.lisp
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
;;;                                        +----GtkMessageDialog
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMessageDialog implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Properties
;;; 
;;;   "buttons"                  GtkButtonsType       : Write / Construct Only
;;;   "image"                    GtkWidget*           : Read / Write
;;;   "message-area"             GtkWidget*           : Read
;;;   "message-type"             GtkMessageType       : Read / Write / Construct
;;;   "secondary-text"           gchar*               : Read / Write
;;;   "secondary-use-markup"     gboolean             : Read / Write
;;;   "text"                     gchar*               : Read / Write
;;;   "use-markup"               gboolean             : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "message-border"           gint                 : Read
;;; 
;;; Description
;;; 
;;; GtkMessageDialog presents a dialog with an image representing the type of
;;; message (Error, Question, etc.) alongside some message text. It's simply a
;;; convenience widget; you could construct the equivalent of GtkMessageDialog
;;; from GtkDialog without too much effort, but GtkMessageDialog saves typing.
;;; 
;;; One difference from GtkDialog is that GtkMessageDialog sets the
;;; "skip-taskbar-hint" property to TRUE, so that the dialog is hidden from the
;;; taskbar by default.
;;; 
;;; The easiest way to do a modal message dialog is to use gtk_dialog_run(),
;;; though you can also pass in the GTK_DIALOG_MODAL flag, gtk_dialog_run()
;;; automatically makes the dialog modal and waits for the user to respond to
;;; it. gtk_dialog_run() returns when any dialog button is clicked.
;;; 
;;; Example 46. A modal dialog.
;;; 
;;; dialog = gtk_message_dialog_new (main_application_window,
;;;                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                                  GTK_MESSAGE_ERROR,
;;;                                  GTK_BUTTONS_CLOSE,
;;;                                  "Error loading file '%s': %s",
;;;                                  filename, g_strerror (errno));
;;; gtk_dialog_run (GTK_DIALOG (dialog));
;;; gtk_widget_destroy (dialog);
;;; 
;;; You might do a non-modal GtkMessageDialog as follows:
;;; 
;;; Example 47. A non-modal dialog.
;;; 
;;; dialog = gtk_message_dialog_new (main_application_window,
;;;                                  GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                                  GTK_MESSAGE_ERROR,
;;;                                  GTK_BUTTONS_CLOSE,
;;;                                  "Error loading file '%s': %s",
;;;                                  filename, g_strerror (errno));
;;; 
;;; /* Destroy the dialog when the user responds to it (e.g. clicks a button) */
;;; g_signal_connect_swapped (dialog, "response",
;;;                           G_CALLBACK (gtk_widget_destroy),
;;;                           dialog);
;;; 
;;; GtkMessageDialog as GtkBuildable
;;; 
;;; The GtkMessageDialog implementation of the GtkBuildable interface exposes
;;; the message area as an internal child with the name "message_area".
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "buttons" property
;;; 
;;;   "buttons"                  GtkButtonsType        : Write / Construct Only
;;; 
;;; The buttons shown in the message dialog.
;;; 
;;; Default value: GTK_BUTTONS_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "image" property
;;; 
;;;   "image"                    GtkWidget*            : Read / Write
;;; 
;;; The image for this dialog.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "message-area" property
;;; 
;;;   "message-area"             GtkWidget*            : Read
;;; 
;;; The GtkVBox that corresponds to the message area of this dialog. See
;;; gtk_message_dialog_get_message_area() for a detailed description of this
;;; area.
;;; 
;;; Since 2.22
;;;
;;; ----------------------------------------------------------------------------
;;; The "message-type" property
;;; 
;;;   "message-type"             GtkMessageType       : Read / Write / Construct
;;; 
;;; The type of the message. The type is used to determine the image that is
;;; shown in the dialog, unless the image is explicitly set by the ::image
;;; property.
;;; 
;;; Default value: GTK_MESSAGE_INFO
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-text" property
;;; 
;;;   "secondary-text"           gchar*                : Read / Write
;;; 
;;; The secondary text of the message dialog.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-use-markup" property
;;; 
;;;   "secondary-use-markup"     gboolean              : Read / Write
;;; 
;;; TRUE if the secondary text of the dialog includes Pango markup. See
;;; pango_parse_markup().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; The primary text of the message dialog. If the dialog has a secondary text,
;;; this will appear as the title.
;;; 
;;; Default value: ""
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-markup" property
;;; 
;;;   "use-markup"               gboolean              : Read / Write
;;; 
;;; TRUE if the primary text of the dialog includes Pango markup. See
;;; pango_parse_markup().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "message-border" style property
;;; 
;;;   "message-border"           gint                  : Read
;;; 
;;; Width of border around the label and image in the message dialog.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 12
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
;;; 
;;; struct GtkMessageDialog;
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
;;; enum GtkMessageType
;;; 
;;; typedef enum {
;;;   GTK_MESSAGE_INFO,
;;;   GTK_MESSAGE_WARNING,
;;;   GTK_MESSAGE_QUESTION,
;;;   GTK_MESSAGE_ERROR,
;;;   GTK_MESSAGE_OTHER
;;; } GtkMessageType;
;;; 
;;; The type of message being displayed in the dialog.
;;; 
;;; GTK_MESSAGE_INFO
;;;     Informational message
;;; 
;;; GTK_MESSAGE_WARNING
;;;     Nonfatal warning message
;;; 
;;; GTK_MESSAGE_QUESTION
;;;     Question requiring a choice
;;; 
;;; GTK_MESSAGE_ERROR
;;;     Fatal error message
;;; 
;;; GTK_MESSAGE_OTHER
;;;     None of the above, doesn't get an icon
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMessageType" gtk-message-type
  (:export t
   :type-initializer "gtk_message_type_get_type")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))

;;; ----------------------------------------------------------------------------
;;; enum GtkButtonsType
;;; 
;;; typedef enum {
;;;   GTK_BUTTONS_NONE,
;;;   GTK_BUTTONS_OK,
;;;   GTK_BUTTONS_CLOSE,
;;;   GTK_BUTTONS_CANCEL,
;;;   GTK_BUTTONS_YES_NO,
;;;   GTK_BUTTONS_OK_CANCEL
;;; } GtkButtonsType;
;;; 
;;; Prebuilt sets of buttons for the dialog. If none of these choices are
;;; appropriate, simply use GTK_BUTTONS_NONE then call gtk_dialog_add_buttons().
;;;
;;; Note
;;;
;;; Please note that GTK_BUTTONS_OK, GTK_BUTTONS_YES_NO and
;;; GTK_BUTTONS_OK_CANCEL are discouraged by the GNOME HIG.
;;; 
;;; GTK_BUTTONS_NONE
;;;     no buttons at all
;;; 
;;; GTK_BUTTONS_OK
;;;     an OK button
;;; 
;;; GTK_BUTTONS_CLOSE
;;;     a Close button
;;; 
;;; GTK_BUTTONS_CANCEL
;;;     a Cancel button
;;; 
;;; GTK_BUTTONS_YES_NO
;;;     Yes and No buttons
;;; 
;;; GTK_BUTTONS_OK_CANCEL
;;;     OK and Cancel buttons
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
;;; 
;;; void gtk_message_dialog_set_markup (GtkMessageDialog *message_dialog,
;;;                                     const gchar *str);
;;; 
;;; Sets the text of the message dialog to be str, which is marked up with the
;;; Pango text markup language.
;;; 
;;; message_dialog :
;;;     a GtkMessageDialog
;;; 
;;; str :
;;;     markup string (see Pango markup format)
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-set-markup))

(defun gtk-message-dialog-set-markup (dialog text)
  (setf (gtk-message-dialog-text dialog) text))

(export 'gtk-message-dialog-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_image ()
;;; 
;;; void gtk_message_dialog_set_image (GtkMessageDialog *dialog,
;;;                                    GtkWidget *image);
;;; 
;;; Sets the dialog's image to image.
;;; 
;;; dialog :
;;;     a GtkMessageDialog
;;; 
;;; image :
;;;     the image
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-set-image))

(defun gtk-message-dialog-set-image (dialog image)
  (setf (gtk-message-dialog-image dialog) image))

(export 'gtk-message-dialog-set-image)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_get_image ()
;;; 
;;; GtkWidget * gtk_message_dialog_get_image (GtkMessageDialog *dialog);
;;; 
;;; Gets the dialog's image.
;;; 
;;; dialog :
;;;     a GtkMessageDialog
;;; 
;;; Returns :
;;;     the dialog's image
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-get-image))

(defun gtk-message-dialog-get-image (dialog)
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
;;; 
;;; GtkWidget * gtk_message_dialog_get_message_area
;;;                                          (GtkMessageDialog *message_dialog);
;;; 
;;; Returns the message area of the dialog. This is the box where the dialog's
;;; primary and secondary labels are packed. You can add your own extra content
;;; to that box and it will appear below those labels, on the right side of the
;;; dialog's image (or on the left for right-to-left languages). See
;;; gtk_dialog_get_content_area() for the corresponding function in the parent
;;; GtkDialog.
;;; 
;;; message_dialog :
;;;     a GtkMessageDialog
;;; 
;;; Returns :
;;;     A GtkVBox corresponding to the "message area" in the message_dialog.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_message_dialog_get_message_area"
           gtk-message-dialog-get-message-area) (g-object g-widget)
  (dialog (g-object gtk-message-dialog)))

(export 'gtk-message-dialog-get-message-area)

;;; --- End of file gtk.message-dialog.lisp ------------------------------------
