;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-button.lisp
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
;;; GtkFileChooserButton
;;; 
;;; A button to launch a file selection dialog
;;; 
;;; Synopsis
;;; 
;;;     GtkFileChooserButton
;;;
;;;     gtk_file_chooser_button_new
;;;     gtk_file_chooser_button_new_with_dialog
;;;     gtk_file_chooser_button_get_title
;;;     gtk_file_chooser_button_set_title
;;;     gtk_file_chooser_button_get_width_chars
;;;     gtk_file_chooser_button_set_width_chars
;;;     gtk_file_chooser_button_get_focus_on_click
;;;     gtk_file_chooser_button_set_focus_on_click
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBox
;;;                            +----GtkFileChooserButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkFileChooserButton implements AtkImplementorIface, GtkBuildable,
;;; GtkOrientable and GtkFileChooser.
;;;
;;; Properties
;;; 
;;;   "dialog"                   GtkFileChooser*       : Write / Construct Only
;;;   "focus-on-click"           gboolean              : Read / Write
;;;   "title"                    gchar*                : Read / Write
;;;   "width-chars"              gint                  : Read / Write
;;; 
;;; Signals
;;; 
;;;   "file-set"                                       : Run First
;;; 
;;; Description
;;; 
;;; The GtkFileChooserButton is a widget that lets the user select a file. It
;;; implements the GtkFileChooser interface. Visually, it is a file name with a
;;; button to bring up a GtkFileChooserDialog. The user can then use that dialog
;;; to change the file associated with that button. This widget does not support
;;; setting the "select-multiple" property to TRUE.
;;; 
;;; Example 88. Create a button to let the user select a file in /etc
;;; 
;;; {
;;;   GtkWidget *button;
;;; 
;;;   button = gtk_file_chooser_button_new (_("Select a file"),
;;;                                         GTK_FILE_CHOOSER_ACTION_OPEN);
;;;   gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (button),
;;;                                        "/etc");
;;; }
;;; 
;;; The GtkFileChooserButton supports the GtkFileChooserActions
;;; GTK_FILE_CHOOSER_ACTION_OPEN and GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER.
;;; 
;;; Important
;;;
;;; The GtkFileChooserButton will ellipsize the label, and thus will thus
;;; request little horizontal space. To give the button more space, you should
;;; call gtk_widget_get_preferred_size(),
;;; gtk_file_chooser_button_set_width_chars(), or pack the button in such a way
;;; that other interface elements give space to the widget.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "dialog" property
;;; 
;;;   "dialog"                   GtkFileChooser*       : Write / Construct Only
;;; 
;;; Instance of the GtkFileChooserDialog associated with the button.
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-on-click" property
;;; 
;;;   "focus-on-click"           gboolean              : Read / Write
;;; 
;;; Whether the GtkFileChooserButton button grabs focus when it is clicked with
;;; the mouse.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" property
;;; 
;;;   "title"                    gchar*                : Read / Write
;;; 
;;; Title to put on the GtkFileChooserDialog associated with the button.
;;; 
;;; Default value: "Select a File"
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "width-chars" property
;;; 
;;;   "width-chars"              gint                  : Read / Write
;;; 
;;; The width of the entry and label inside the button, in characters.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "file-set" signal
;;; 
;;; void user_function (GtkFileChooserButton *widget,
;;;                     gpointer              user_data)      : Run First
;;; 
;;; The ::file-set signal is emitted when the user selects a file.
;;; 
;;; Note that this signal is only emitted when the user changes the file.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserButton
;;; 
;;; struct GtkFileChooserButton;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFileChooserButton" 'gtk-file-chooser-button))

(define-g-object-class "GtkFileChooserButton" gtk-file-chooser-button
  (:superclass gtk-hbox
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                "GtkOrientable")
   :type-initializer "gtk_file_chooser_button_get_type")
  ((dialog
    gtk-file-chooser-button-dialog
    "dialog" "GtkFileChooser" nil nil)
   (focus-on-click
    gtk-file-chooser-button-focus-on-click
    "focus-on-click" "gboolean" t t)
   (title
    gtk-file-chooser-button-title
    "title" "gchararray" t t)
   (width-chars
    gtk-file-chooser-button-width-chars
    "width-chars" "gint" t t)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new ()
;;; 
;;; GtkWidget * gtk_file_chooser_button_new (const gchar *title,
;;;                                          GtkFileChooserAction action);
;;; 
;;; Creates a new file-selecting button widget.
;;; 
;;; title :
;;;     the title of the browse dialog.
;;; 
;;; action :
;;;     the open mode for the widget.
;;; 
;;; Returns :
;;;     a new button widget.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new_with_dialog ()
;;; 
;;; GtkWidget * gtk_file_chooser_button_new_with_dialog (GtkWidget *dialog);
;;; 
;;; Creates a GtkFileChooserButton widget which uses dialog as its file-picking
;;; window.
;;; 
;;; Note that dialog must be a GtkDialog (or subclass) which implements the
;;; GtkFileChooser interface and must not have GTK_DIALOG_DESTROY_WITH_PARENT
;;; set.
;;; 
;;; Also note that the dialog needs to have its confirmative button added with
;;; response GTK_RESPONSE_ACCEPT or GTK_RESPONSE_OK in order for the button to
;;; take over the file selected in the dialog.
;;; 
;;; dialog :
;;;     the widget to use as dialog
;;; 
;;; Returns :
;;;     a new button widget.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_get_title ()
;;; 
;;; const gchar * gtk_file_chooser_button_get_title
;;;                                               (GtkFileChooserButton *button)
;;; 
;;; Retrieves the title of the browse dialog used by button. The returned value
;;; should not be modified or freed.
;;; 
;;; button :
;;;     the button widget to examine.
;;; 
;;; Returns :
;;;     a pointer to the browse dialog's title.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_set_title ()
;;; 
;;; void gtk_file_chooser_button_set_title (GtkFileChooserButton *button,
;;;                                         const gchar *title);
;;; 
;;; Modifies the title of the browse dialog used by button.
;;; 
;;; button :
;;;     the button widget to modify.
;;; 
;;; title :
;;;     the new browse dialog title.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_get_width_chars ()
;;; 
;;; gint gtk_file_chooser_button_get_width_chars (GtkFileChooserButton *button)
;;; 
;;; Retrieves the width in characters of the button widget's entry and/or label.
;;; 
;;; button :
;;;     the button widget to examine.
;;; 
;;; Returns :
;;;     an integer width (in characters) that the button will use to size itself.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_set_width_chars ()
;;; 
;;; void gtk_file_chooser_button_set_width_chars (GtkFileChooserButton *button,
;;;                                               gint n_chars);
;;; 
;;; Sets the width (in characters) that button will use to n_chars.
;;; 
;;; button :
;;;     the button widget to examine.
;;; 
;;; n_chars :
;;;     the new width, in characters.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_get_focus_on_click ()
;;; 
;;; gboolean gtk_file_chooser_button_get_focus_on_click
;;;                                               (GtkFileChooserButton *button)
;;; 
;;; Returns whether the button grabs focus when it is clicked with the mouse.
;;; See gtk_file_chooser_button_set_focus_on_click().
;;; 
;;; button :
;;;     a GtkFileChooserButton
;;; 
;;; Returns :
;;;     TRUE if the button grabs focus when it is clicked with the mouse.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_set_focus_on_click ()
;;; 
;;; void gtk_file_chooser_button_set_focus_on_click
;;;                                               (GtkFileChooserButton *button,
;;;                                                gboolean focus_on_click);
;;; 
;;; Sets whether the button will grab focus when it is clicked with the mouse.
;;; Making mouse clicks not grab focus is useful in places like toolbars where
;;; you don't want the keyboard focus removed from the main area of the
;;; application.
;;; 
;;; button :
;;;     a GtkFileChooserButton
;;; 
;;; focus_on_click :
;;;     whether the button grabs focus when clicked with the mouse
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.file-chooser-button.lisp -------------------------------
