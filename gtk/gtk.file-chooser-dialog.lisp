;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkFileChooserDialog
;;;
;;; A file chooser dialog, suitable for "File/Open" or "File/Save" commands
;;;
;;; Synopsis
;;;
;;;     GtkFileChooserDialog
;;;
;;;     gtk_file_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileChooserDialog" gtk-file-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkFileChooser")
   :type-initializer "gtk_file_chooser_dialog_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-file-chooser-dialog 'type)
 "@version{2013-6-18}
  @begin{short}
    @sym{gtk-file-chooser-dialog} is a dialog box suitable for use with
    \"File/Open\" or \"File/Save as\" commands. This widget works by putting a
    @class{gtk-file-chooser-widget} inside a @class{gtk-dialog}. It exposes the
    @code{GtkFileChooserIface} interface, so you can use all of the
    @class{gtk-file-chooser} functions on the file chooser dialog as well as
    those for @class{gtk-dialog}.
  @end{short}

  Note that @sym{gtk-file-chooser-dialog} does not have any methods of its own.
  Instead, you should use the functions that work on a @class{gtk-file-chooser}.

  @b{Example:} Typical usage

  In the simplest of cases, you can the following code to use
  @sym{gtk-file-chooser-dialog} to select a file for opening:
  @begin{pre}
   GtkWidget *dialog;

   dialog = gtk_file_chooser_dialog_new
                                      (\"Open File\",
                                       parent_window,
                                       GTK_FILE_CHOOSER_ACTION_OPEN,
                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                       GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                       NULL);

   if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
     {
       char *filename;

       filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
       open_file (filename);
       g_free (filename);
     @}

   gtk_widget_destroy (dialog);
  @end{pre}
  To use a dialog for saving, you can use this:
  @begin{pre}
   GtkWidget *dialog;

   dialog = gtk_file_chooser_dialog_new
                                      (\"Save File\",
                                       parent_window,
                                       GTK_FILE_CHOOSER_ACTION_SAVE,
                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                       GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
                                       NULL);
   gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog),
                                                   TRUE);

   if (user_edited_a_new_document)
     gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog),
                                        \"Untitled document\");
   else
     gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog),
                                    filename_for_existing_document);

   if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
     {
       char *filename;

       filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
       save_to_file (filename);
       g_free (filename);
     @}

   gtk_widget_destroy (dialog);
  @end{pre}

  @subheading{Setting up a file chooser dialog}
    There are various cases in which you may need to use a
    @class{gtk-file-chooser-dialog}:
    @begin{itemize}
      @begin{item}
        To select a file for opening, as for a File/Open command. Use
        @code{:open}.
      @end{item}
      @begin{item}
        To save a file for the first time, as for a File/Save command. Use
        @code{:save}, and suggest a name such as \"Untitled\" with the function
        @fun{gtk-file-chooser-set-current-name}.
      @end{item}
      @begin{item}
        To save a file under a different name, as for a File/Save As command.
        Use @code{:save}, and set the existing filename with the function
        @fun{gtk-file-chooser-set-filename}.
      @end{item}
      @begin{item}
        To choose a folder instead of a file. Use @code{:select-folder}.
      @end{item}
    @end{itemize}

  @subheading{Note}
    Old versions of the file chooser's documentation suggested using the
    function @fun{gtk-file-chooser-set-current-folder} in various situations,
    with the intention of letting the application suggest a reasonable default
    folder. This is no longer considered to be a good policy, as now the file
    chooser is able to make good suggestions on its own. In general, you should
    only cause the file chooser to show a specific folder when it is appropriate
    to use the function @fun{gtk-file-chooser-set-filename}, i. e. when you are
    doing a File/Save As command and you already have a file saved somewhere.

  @subheading{Response Codes}
    @sym{gtk-file-chooser-dialog} inherits from @class{gtk-dialog}, so buttons
    that go in its action area have response codes such as @code{:accept} and
    @code{:cancel}. For example, you could call the function
    @fun{gtk-file-chooser-dialog-new} as follows:
    @begin{pre}
   GtkWidget *dialog;

   dialog = gtk_file_chooser_dialog_new (\"Open File\",
                                         parent_window,
                                         GTK_FILE_CHOOSER_ACTION_OPEN,
                                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                         GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                         NULL);
    @end{pre}
    This will create buttons for \"Cancel\" and \"Open\" that use stock response
    identifiers from @symbol{gtk-response-type}. For most dialog boxes you can
    use your own custom response codes rather than the ones in
    @symbol{gtk-response-type}, but @sym{gtk-file-chooser-dialog} assumes that
    its \"accept\"-type action, e. g. an \"Open\"
    or \"Save\" button, will have one of the following response codes:
    @begin{pre}
     @code{:accept}
     @code{:ok}
     @code{:yes}
     @code{:apply}
    @end{pre}
    This is because @sym{gtk-file-chooser-dialog} must intercept responses and
    switch to folders if appropriate, rather than letting the dialog terminate
    - the implementation uses these known response codes to know which responses
    can be blocked if appropriate.

  @subheading{Note}
    To summarize, make sure you use a stock response code when you use
    @class{gtk-file-chooser-dialog} to ensure proper operation.")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-file-chooser-dialog-new (title parent action &rest buttons)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[title]{title of the dialog, or @code{nil}}
  @argument[parent]{Transient parent of the dialog, or @code{nil}}
  @argument[action]{open or save mode for the dialog}
  @argument[first-button-text]{stock ID or text to go in the first button,
    or @code{nil}}
  @argument[...]{response ID for the first button, then additional (button, id)
    pairs}
  @return{A new @class{gtk-file-chooser-dialog} widget.}
  @begin{short}
    Creates a new @class{gtk-file-chooser-dialog} widget. This function is
    analogous to the function @fun{gtk-dialog-new-with-buttons}.
  @end{short}

  Since 2.4
  @see-function{gtk-dialog-new-with-buttons}"
  (let ((dialog (make-instance 'gtk-file-chooser-dialog
                               :title title
                               :parent parent
                               :action action)))
    (apply #'gtk-dialog-add-buttons (cons dialog buttons))
    dialog))

(export 'gtk-file-chooser-dialog-new)

;;; --- End of file gtk.file-chooser-dialog.lisp -------------------------------
