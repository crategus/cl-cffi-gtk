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
;;; GtkFileChooserDialog
;;; 
;;; A file chooser dialog, suitable for "File/Open" or "File/Save" commands
;;; 
;;; Synopsis
;;; 
;;;     GtkFileChooserDialog
;;;
;;;     gtk_file_chooser_dialog_new
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
;;;                                        +----GtkFileChooserDialog
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkFileChooserDialog implements AtkImplementorIface, GtkBuildable and
;;; GtkFileChooser.
;;;
;;; Description
;;; 
;;; GtkFileChooserDialog is a dialog box suitable for use with "File/Open" or
;;; "File/Save as" commands. This widget works by putting a GtkFileChooserWidget
;;; inside a GtkDialog. It exposes the GtkFileChooserIface interface, so you can
;;; use all of the GtkFileChooser functions on the file chooser dialog as well
;;; as those for GtkDialog.
;;; 
;;; Note that GtkFileChooserDialog does not have any methods of its own.
;;; Instead, you should use the functions that work on a GtkFileChooser.
;;; 
;;; Example 89. Typical usage
;;;
;;; In the simplest of cases, you can the following code to use
;;; GtkFileChooserDialog to select a file for opening:
;;; 
;;; GtkWidget *dialog;
;;; 
;;; dialog = gtk_file_chooser_dialog_new ("Open File",
;;;                                       parent_window,
;;;                                       GTK_FILE_CHOOSER_ACTION_OPEN,
;;;                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
;;;                                       GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
;;;                                       NULL);
;;; 
;;; if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
;;;   {
;;;     char *filename;
;;; 
;;;     filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
;;;     open_file (filename);
;;;     g_free (filename);
;;;   }
;;; 
;;; gtk_widget_destroy (dialog);
;;; 
;;; To use a dialog for saving, you can use this:
;;; 
;;; GtkWidget *dialog;
;;; 
;;; dialog = gtk_file_chooser_dialog_new ("Save File",
;;;                                       parent_window,
;;;                                       GTK_FILE_CHOOSER_ACTION_SAVE,
;;;                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
;;;                                       GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
;;;                                       NULL);
;;; gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog),
;;;                                                 TRUE);
;;; 
;;; if (user_edited_a_new_document)
;;;   gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog),
;;;                                      "Untitled document");
;;; else
;;;   gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog),
;;;                                  filename_for_existing_document);
;;; 
;;; if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
;;;   {
;;;     char *filename;
;;; 
;;;     filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
;;;     save_to_file (filename);
;;;     g_free (filename);
;;;   }
;;; 
;;; gtk_widget_destroy (dialog);
;;; 
;;; Setting up a file chooser dialog
;;;
;;; There are various cases in which you may need to use a GtkFileChooserDialog:
;;; 
;;;     To select a file for opening, as for a File/Open command. Use
;;;     GTK_FILE_CHOOSER_ACTION_OPEN.
;;;
;;;     To save a file for the first time, as for a File/Save command. Use
;;;     GTK_FILE_CHOOSER_ACTION_SAVE, and suggest a name such as "Untitled"
;;;     with gtk_file_chooser_set_current_name().
;;;
;;;     To save a file under a different name, as for a File/Save As command.
;;;     Use GTK_FILE_CHOOSER_ACTION_SAVE, and set the existing filename with
;;;     gtk_file_chooser_set_filename().
;;;
;;;     To choose a folder instead of a file.
;;;     Use GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER.
;;; 
;;; Note
;;; 
;;; Old versions of the file chooser's documentation suggested using
;;; gtk_file_chooser_set_current_folder() in various situations, with the
;;; intention of letting the application suggest a reasonable default folder.
;;; This is no longer considered to be a good policy, as now the file chooser
;;; is able to make good suggestions on its own. In general, you should only
;;; cause the file chooser to show a specific folder when it is appropriate to
;;; use gtk_file_chooser_set_filename(), i.e. when you are doing a File/Save As
;;; command and you already have a file saved somewhere.
;;; 
;;; Response Codes
;;;
;;; GtkFileChooserDialog inherits from GtkDialog, so buttons that go in its
;;; action area have response codes such as GTK_RESPONSE_ACCEPT and
;;; GTK_RESPONSE_CANCEL. For example, you could call
;;; gtk_file_chooser_dialog_new() as follows:
;;; 
;;; GtkWidget *dialog;
;;; 
;;; dialog = gtk_file_chooser_dialog_new ("Open File",
;;;                                       parent_window,
;;;                                       GTK_FILE_CHOOSER_ACTION_OPEN,
;;;                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
;;;                                       GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
;;;                                       NULL);
;;; 
;;; This will create buttons for "Cancel" and "Open" that use stock response
;;; identifiers from GtkResponseType. For most dialog boxes you can use your
;;; own custom response codes rather than the ones in GtkResponseType, but
;;; GtkFileChooserDialog assumes that its "accept"-type action, e.g. an "Open"
;;; or "Save" button, will have one of the following response codes:
;;; 
;;; GTK_RESPONSE_ACCEPT
;;; GTK_RESPONSE_OK
;;; GTK_RESPONSE_YES
;;; GTK_RESPONSE_APPLY
;;; 
;;; This is because GtkFileChooserDialog must intercept responses and switch to
;;; folders if appropriate, rather than letting the dialog terminate — the
;;; implementation uses these known response codes to know which responses can
;;; be blocked if appropriate.
;;; 
;;; Note
;;;
;;; To summarize, make sure you use a stock response code when you use
;;; GtkFileChooserDialog to ensure proper operation.
;;; ---------------------------------------------------------------------------- 

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserDialog
;;; 
;;; struct GtkFileChooserDialog;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileChooserDialog" gtk-file-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
   :type-initializer "gtk_file_chooser_dialog_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_dialog_new ()
;;; 
;;; GtkWidget * gtk_file_chooser_dialog_new (const gchar *title,
;;;                                          GtkWindow *parent,
;;;                                          GtkFileChooserAction action,
;;;                                          const gchar *first_button_text,
;;;                                          ...);
;;; 
;;; Creates a new GtkFileChooserDialog. This function is analogous to
;;; gtk_dialog_new_with_buttons().
;;; 
;;; title :
;;;     title of the dialog, or NULL
;;; 
;;; parent :
;;;     transient parent of the dialog, or NULL
;;; 
;;; action :
;;;     open or save mode for the dialog
;;; 
;;; first_button_text :
;;;     stock ID or text to go in the first button, or NULL
;;; 
;;; ... :
;;;     response ID for the first button, then additional (button, id) pairs,
;;;     ending with NULL
;;; 
;;; Returns :
;;;     a new GtkFileChooserDialog
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.file-chooser-dialog.lisp -------------------------------
