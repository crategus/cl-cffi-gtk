;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser.lisp
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
;;; GtkFileChooser
;;;
;;;     File chooser interface used by GtkFileChooserWidget and
;;;     GtkFileChooserDialog
;;;
;;; Types and Values
;;;
;;;     GtkFileChooser
;;;     GtkFileChooserAction
;;;     GtkFileChooserConfirmation
;;;     GTK_FILE_CHOOSER_ERROR
;;;     GtkFileChooserError
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_set_action                        Accessor
;;;     gtk_file_chooser_get_action                        Accessor
;;;     gtk_file_chooser_set_local_only                    Accessor
;;;     gtk_file_chooser_get_local_only                    Accessor
;;;     gtk_file_chooser_set_select_multiple               Accessor
;;;     gtk_file_chooser_get_select_multiple               Accessor
;;;     gtk_file_chooser_set_show_hidden                   Accessor
;;;     gtk_file_chooser_get_show_hidden                   Accessor
;;;     gtk_file_chooser_set_do_overwrite_confirmation     Accessor
;;;     gtk_file_chooser_get_do_overwrite_confirmation     Accessor
;;;     gtk_file_chooser_set_create_folders                Accessor
;;;     gtk_file_chooser_get_create_folders                Accessor
;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name
;;;     gtk_file_chooser_get_filename
;;;     gtk_file_chooserset_filename
;;;     gtk_file_chooser_select_filename
;;;     gtk_file_chooser_unselect_filename
;;;     gtk_file_chooser_select_all
;;;     gtk_file_chooser_unselect_all
;;;     gtk_file_chooser_get_filenames
;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder
;;;     gtk_file_chooser_get_uri
;;;     gtk_file_chooser_set_uri
;;;     gtk_file_chooser_select_uri
;;;     gtk_file_chooser_unselect_uri
;;;     gtk_file_chooser_get_uris
;;;     gtk_file_chooser_set_current_folder_uri
;;;     gtk_file_chooser_get_current_folder_uri
;;;     gtk_file_chooser_set_preview_widget                Accessor
;;;     gtk_file_chooser_get_preview_widget                Accessor
;;;     gtk_file_chooser_set_preview_widget_active         Accessor
;;;     gtk_file_chooser_get_preview_widget_active         Accessor
;;;     gtk_file_chooser_set_use_preview_label             Accessor
;;;     gtk_file_chooser_get_use_preview_label             Accessor
;;;     gtk_file_chooser_get_preview_filename
;;;     gtk_file_chooser_get_preview_uri
;;;     gtk_file_chooser_set_extra_widget                  Accessor
;;;     gtk_file_chooser_get_extra_widget                  Accessor
;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_list_filters
;;;     gtk_file_chooser_set_filter                        Accessor
;;;     gtk_file_chooser_get_filter                        Accessor
;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_list_shortcut_folders
;;;     gtk_file_chooser_add_shortcut_folder_uri
;;;     gtk_file_chooser_remove_shortcut_folder_uri
;;;     gtk_file_chooser_list_shortcut_folder_uris
;;;     gtk_file_chooser_get_current_folder_file
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_get_preview_file
;;;     gtk_file_chooser_select_file
;;;     gtk_file_chooser_set_current_folder_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_unselect_file
;;;
;;; Properties
;;;
;;:       GtkFileChooserAction    action                       Read / Write
;;;                   gboolean    create-folders               Read / Write
;;;                   gboolean    do-overwrite-confirmation    Read / Write
;;;                  GtkWidget*   extra-widget                 Read / Write
;;;              GtkFileFilter*   filter                       Read / Write
;;;                   gboolean    local-only                   Read / Write
;;;                  GtkWidget*   preview-widget               Read / Write
;;;                   gboolean    preview-widget-active        Read / Write
;;;                   gboolean    select-multiple              Read / Write
;;;                   gboolean    show-hidden                  Read / Write
;;;                   gboolean    use-preview-label            Read / Write
;;;
;;; Signals
;;;
;;; GtkFileChooserConfirmation    confirm-overwrite            Run Last
;;;                       void    current-folder-changed       Run Last
;;;                       void    file-activated               Run Last
;;;                       void    selection-changed            Run Last
;;;                       void    update-preview               Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserAction
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserAction" gtk-file-chooser-action
  (:export t
   :type-initializer "gtk_file_chooser_action_get_type")
  (:open 0)
  (:save 1)
  (:select-folder 2)
  (:create-folder 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-action atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-file-chooser-action atdoc:*external-symbols*)
 "@version{2020-5-23}
  @begin{short}
    Describes whether a @class{gtk-file-chooser} interface is being used to open
    existing files or to save to a possibly new file.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkFileChooserAction\" gtk-file-chooser-action
  (:export t
   :type-initializer \"gtk_file_chooser_action_get_type\")
  (:open 0)
  (:save 1)
  (:select-folder 2)
  (:create-folder 3))
  @end{pre}
  @begin[code]{table}
    @entry[:open]{Indicates open mode. The file chooser will only let the user
      pick an existing file.}
    @entry[:save]{Indicates save mode. The file chooser will let the user pick
      an existing file, or type in a new filename.}
    @entry[:select-folder]{Indicates an Open mode for selecting folders. The
      file chooser will let the user pick an existing folder.}
    @entry[:create-folder]{Indicates a mode for creating a new folder. The file
      chooser will let the user name an existing or new folder.}
  @end{table}
  @see-class{gtk-file-chooser}")

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserConfirmation
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserConfirmation" gtk-file-chooser-confirmation
  (:export t
   :type-initializer "gtk_file_chooser_confirmation_get_type")
  (:confirm 0)
  (:accept-filename 1)
  (:select-again 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-confirmation atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-file-chooser-confirmation atdoc:*external-symbols*)
 "@version{2020-5-23}
  @begin{short}
    Used as a return value of handlers for the \"confirm-overwrite\" signal of a
    @class{gtk-file-chooser} interface. This value determines whether the file
    chooser will present the stock confirmation dialog, accept the user's choice
    of a filename, or let the user choose another filename.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkFileChooserConfirmation\" gtk-file-chooser-confirmation
  (:export t
   :type-initializer \"gtk_file_chooser_confirmation_get_type\")
  (:confirm 0)
  (:accept-filename 1)
  (:select-again 2))
  @end{pre}
  @begin[code]{table}
    @entry[:confirm]{The file chooser will present its stock dialog to confirm
      about overwriting an existing file.}
    @entry[:accept-filename]{The file chooser will terminate and accept the
      user's choice of a file name.}
    @entry[:select-again]{The file chooser will continue running, so as to let
      the user select another file name.}
  @end{table}
  @class{gtk-file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GTK_FILE_CHOOSER_ERROR
;;;
;;; #define GTK_FILE_CHOOSER_ERROR (gtk_file_chooser_error_quark ())
;;;
;;; Used to get the GError quark for GtkFileChooser errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkFileChooserError
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFileChooserError" gtk-file-chooser-error
  (:export t
   :type-initializer "gtk_file_chooser_error_get_type")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-error atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-file-chooser-error atdoc:*external-symbols*)
 "@version{2020-5-23}
  @begin{short}
    These identify the various errors that can occur while calling
    @sym{gtk-file-chooser} interface functions.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkFileChooserError\" gtk-file-chooser-error
  (:export t
   :type-initializer \"gtk_file_chooser_error_get_type\")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))
  @end{pre}
  @begin[code]{table}
    @entry[:nonexistent]{Indicates that a file does not exist.}
    @entry[:bad-filename]{Indicates a malformed filename.}
    @entry[:already-exists]{Indicates a duplicate path, e. g. when adding a
      bookmark.}
    @entry[:incomplete-hostname]{Indicates an incomplete hostname, e. g.
      \"http://foo\" without a slash after that.}
  @end{table}
  @class{gtk-file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFileChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkFileChooser" gtk-file-chooser
  (:export t
   :type-initializer "gtk_file_chooser_get_type")
  (action
   gtk-file-chooser-action
   "action" "GtkFileChooserAction" t t)
  (create-folders
   gtk-file-chooser-create-folders
   "create-folders" "gboolean" t t)
  (do-overwrite-confirmation
   gtk-file-chooser-do-overwrite-confirmation
   "do-overwrite-confirmation" "gboolean" t t)
  (extra-widget
   gtk-file-chooser-extra-widget
   "extra-widget" "GtkWidget" t t)
  (filter
   gtk-file-chooser-filter
   "filter" "GtkFileFilter" t t)
  (local-only
   gtk-file-chooser-local-only
   "local-only" "gboolean" t t)
  (preview-widget
   gtk-file-chooser-preview-widget
   "preview-widget" "GtkWidget" t t)
  (preview-widget-active
   gtk-file-chooser-preview-widget-active
   "preview-widget-active" "gboolean" t t)
  (select-multiple
   gtk-file-chooser-select-multiple
   "select-multiple" "gboolean" t t)
  (show-hidden
   gtk-file-chooser-show-hidden
   "show-hidden" "gboolean" t t)
  (use-preview-label
   gtk-file-chooser-use-preview-label
   "use-preview-label" "gboolean" t t))
;  #+win32
;  (:cffi filename
;         gtk-file-tename
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_filename_utf8"
;         "gtk_file_chooser_set_filename_utf8")
;  #-win32
;  (:cffi filename
;         gtk-file-chooser-filename
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_filename"
;         "gtk_file_chooser_set_filename")
;  #+win32
;  (:cffi current-folder
;         gtk-file-chooser-current-folder
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_current_folder_utf8"
;         "gtk_file_chooser_set_current_folder_utf8")
;  #-win32
;  (:cffi current-folder
;         gtk-file-chooser-current-folder
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_current_folder"
;         "gtk_file_chooser_set_current_folder")
;  (:cffi uri
;         gtk-file-chooser-uri
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_uri" "gtk_file_chooser_set_uri")
;  (:cffi current-folder-uri
;         gtk-file-chooser-current-folder-uri
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_current_folder_uri"
;         "gtk_file_chooser_set_current_folder_uri")
;  #+win32
;  (:cffi preview-filename
;         gtk-file-chooser-preview-filename
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_preview_filename_utf8" nil)
;  #-win32
;  (:cffi preview-filename
;         gtk-file-chooser-preview-filename
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_preview_filename" nil)
;  (:cffi preview-uri
;         gtk-file-chooser-preview-uri
;         (g-string :free-from-foreign t :free-to-foreign t)
;         "gtk_file_chooser_get_preview_uri" nil))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-file-chooser 'type)
 "@version{2020-5-23}
  @begin{short}
    @sym{gtk-file-chooser} is an interface that can be implemented by file
    selection widgets.
  @end{short}
  In GTK+, the main objects that implement this interface are
  @class{gtk-file-chooser-widget}, @class{gtk-file-chooser-dialog}, and
  @class{gtk-file-chooser-button}. You do not need to write an object that
  implements the @sym{gtk-file-chooser} interface unless you are trying to
  adapt an existing file selector to expose a standard programming interface.

  @sym{gtk-file-chooser} allows for shortcuts to various places in the
  filesystem. In the default implementation these are displayed in the left
  pane. It may be a bit confusing at first that these shortcuts come from
  various sources and in various flavours, so lets explain the terminology
  here:
  @begin{table}
    @begin[Bookmarks]{entry}
      are created by the user, by dragging folders from the right pane to the
      left pane, or by using the \"Add\". Bookmarks can be renamed and deleted
      by the user.
    @end{entry}
    @begin[Shortcuts]{entry}
      can be provided by the application or by the underlying filesystem
      abstraction, e. g. both the gnome-vfs and the Windows filesystems provide
      \"Desktop\" shortcuts. Shortcuts cannot be modified by the user.
    @end{entry}
    @begin[Volumes]{entry}
      are provided by the underlying filesystem abstraction. They are the
      \"roots\" of the filesystem.
    @end{entry}
  @end{table}
  @subheading{File Names and Encodings}
    When the user is finished selecting files in a @sym{gtk-file-chooser}, your
    program can get the selected names either as filenames or as URIs. For URIs,
    the normal escaping rules are applied if the URI contains non-ASCII
    characters. However, filenames are always returned in the character set
    specified by the @code{G_FILENAME_ENCODING} environment variable. Please see
    the GLib documentation for more details about this variable.

    This means that while you can pass the result of the
    @fun{gtk-file-chooser-get-filename} function to @code{open()} or
    @code{fopen()}, you may not be able to directly set it as the text of a
    @class{gtk-label} widget unless you convert it first to UTF-8, which all
    GTK+ widgets expect. You should use the @fun{g-filename-to-utf8} function
    to convert filenames into strings that can be passed to GTK+ widgets.

  @subheading{Adding a Preview Widget}
    You can add a custom preview widget to a file chooser and then get
    notification about when the preview needs to be updated. To install a
    preview widget, use the @fun{gtk-file-chooser-preview-widget} slot access
    function. Then, connect to the \"update-preview\" signal to get notified
    when you need to update the contents of the preview.

    Your callback should use the @fun{gtk-file-chooser-get-preview-filename}
    function to see what needs previewing. Once you have generated the preview
    for the corresponding file, you must call the
    @fun{gtk-file-chooser-preview-widget-active} slot access function with a
    boolean flag that indicates whether your callback could successfully
    generate a preview.

    @b{Example:} Sample Usage
    @begin{pre}
   {
     GtkImage *preview;

     ...

     preview = gtk_image_new ();

     gtk_file_chooser_set_preview_widget (my_file_chooser, preview);
     g_signal_connect (my_file_chooser, \"update-preview\",
               G_CALLBACK (update_preview_cb), preview);
   @}

   static void
   update_preview_cb (GtkFileChooser *file_chooser, gpointer data)
   {
     GtkWidget *preview;
     char *filename;
     GdkPixbuf *pixbuf;
     gboolean have_preview;

     preview = GTK_WIDGET (data);
     filename = gtk_file_chooser_get_preview_filename (file_chooser);

     pixbuf = gdk_pixbuf_new_from_file_at_size (filename, 128, 128, NULL);
     have_preview = (pixbuf != NULL);
     g_free (filename);

     gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
     if (pixbuf)
       g_object_unref (pixbuf);

     gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
   @}
    @end{pre}
  @subheading{Adding Extra Widgets}
    You can add extra widgets to a file chooser to provide options that are not
    present in the default design. For example, you can add a toggle button to
    give the user the option to open a file in read-only mode. You can use the
    @fun{gtk-file-chooser-set-extra-widget} function to insert additional
    widgets in a file chooser.

    @b{Example:} Sample Usage
    @begin{pre}
   GtkWidget *toggle;

   ...

   toggle = gtk_check_button_new_with_label (\"Open file read-only\");
   gtk_widget_show (toggle);
   gtk_file_chooser_set_extra_widget (my_file_chooser, toggle);
   @}
    @end{pre}
    If you want to set more than one extra widget in the file chooser, you can a
    container such as a @class{gtk-box} or a @class{gtk-grid} and include your
    widgets in it. Then, set the container as the whole extra widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"confirm-overwrite\" signal}
      @begin{pre}
 lambda (chooser)    : Run Last
      @end{pre}
      This signal gets emitted whenever it is appropriate to present a
      confirmation dialog when the user has selected a file name that already
      exists. The signal only gets emitted when the file chooser is in
      @code{:action-save} mode.

      Most applications just need to turn on the
      @code{do-overwrite-confirmation} property (or call the
      @fun{gtk-file-chooser-do-overwrite-confirmation} function), and they will
      automatically get a stock confirmation dialog. Applications which need to
      customize this behavior should do that, and also connect to the
      \"confirm-overwrite\" signal.

      A signal handler for this signal must return a
      @symbol{gtk-file-chooser-confirmation} value, which indicates the action
      to take. If the handler determines that the user wants to select a
      different filename, it should return @code{:select-again}. If it
      determines that the user is satisfied with his choice of file name, it
      should return @code{:accept-filename}. On the other hand, if it
      determines that the stock confirmation dialog should be used, it should
      return @code{:confirm}. The following example illustrates this.

      @b{Example:} Custom confirmation
      @begin{pre}
   static GtkFileChooserConfirmation
   confirm_overwrite_callback (GtkFileChooser *chooser, gpointer data)
   {
     char *uri;
     uri = gtk_file_chooser_get_uri (chooser);

     if (is_uri_read_only (uri))
       {
         if (user_wants_to_replace_read_only_file (uri))
           return GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME;
         else
           return GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN;
       @} else
         // fall back to the default dialog
         return GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM;
   @}

   ...

   chooser = gtk_file_chooser_dialog_new (...);

   gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog),
                                                   TRUE);
   g_signal_connect (chooser, \"confirm-overwrite\",
                     G_CALLBACK (confirm_overwrite_callback), NULL);

   if (gtk_dialog_run (chooser) == GTK_RESPONSE_ACCEPT)
           save_to_file (gtk_file_chooser_get_filename
                                                (GTK_FILE_CHOOSER (chooser));

   gtk_widget_destroy (chooser);
      @end{pre}
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} object which received the
          signal.}
        @entry[Returns]{A @symbol{gtk-file-chooser-confirmation} value that
          indicates which action to take after emitting the signal.}
      @end{table}
    @subheading{The \"current-folder-changed\" signal}
      @begin{pre}
 lambda (chooser)    : Run Last
      @end{pre}
      This signal is emitted when the current folder in a file chooser changes.
      This can happen due to the user performing some action that changes
      folders, such as selecting a bookmark or visiting a folder on the file
      list. It can also happen as a result of calling a function to explicitly
      change the current folder in a file chooser.

      Normally you do not need to connect to this signal, unless you need to
      keep track of which folder a file chooser is showing.

      See also the functions @fun{gtk-file-chooser-current-folder} and
      @fun{gtk-file-chooser-current-folder-uri}.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} object which received the
          signal.}
      @end{table}
    @subheading{The \"file-activated\" signal}
      @begin{pre}
 lambda (chooser)    : Run Last
      @end{pre}
      This signal is emitted when the user \"activates\" a file in the file
      chooser. This can happen by double-clicking on a file in the file list,
      or by pressing Enter.

      Normally you do not need to connect to this signal. It is used internally
      by @class{gtk-file-chooser-dialog} to know when to activate the default
      button in the dialog.

      See also the functions @fun{gtk-file-chooser-filename},
      @fun{gtk-file-chooser-filenames}, @fun{gtk-file-chooser-uri}, and
      @fun{gtk-file-chooser-uris}.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} object which received the
          signal.}
      @end{table}
    @subheading{The \"selection-changed\" signal}
      @begin{pre}
 lambda (chooser)    : Run Last
      @end{pre}
      This signal is emitted when there is a change in the set of selected files
      in a file chooser. This can happen when the user modifies the selection
      with the mouse or the keyboard, or when explicitly calling functions to
      change the selection.

      Normally you do not need to connect to this signal, as it is easier to
      wait for the file chooser to finish running, and then to get the list of
      selected files using the functions mentioned below.

      See also the functions @fun{gtk-file-chooser-select-filename},
      @fun{gtk-file-chooser-unselect-filename}, @fun{gtk-file-chooser-filename},
      @fun{gtk-file-chooser-filenames}, @fun{gtk-file-chooser-select-uri},
      @fun{gtk-file-chooser-unselect-uri}, @fun{gtk-file-chooser-uri},
      @fun{gtk-file-chooser-uris}.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} object which received the
          signal.}
      @end{table}
    @subheading{The \"update-preview\" signal}
      @begin{pre}
 lambda (chooser)    : Run Last
      @end{pre}
      This signal is emitted when the preview in a file chooser should be
      regenerated. For example, this can happen when the currently selected file
      changes. You should use this signal if you want your file chooser to have
      a preview widget.

      Once you have installed a preview widget with the function
      @fun{gtk-file-chooser-preview-widget}, you should update it
      when this signal is emitted. You can use the functions
      @fun{gtk-file-chooser-preview-filename} or
      @fun{gtk-file-chooser-preview-uri} to get the name of the file to
      preview. Your widget may not be able to preview all kinds of files. Your
      callback must call the function
      @fun{gtk-file-chooser-preview-widget-active} to inform the file chooser
      about whether the preview was generated successfully or not.

      Please see the example code in the section called \"Adding a Preview
      Widget\".

      See also the functions @fun{gtk-file-chooser-preview-widget},
      @fun{gtk-file-chooser-preview-widget-active},
      @fun{gtk-file-chooser-use-preview-label},
      @fun{gtk-file-chooser-preview-filename},
      @fun{gtk-file-chooser-preview-uri}.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-file-chooser-action}
  @see-slot{gtk-file-chooser-create-folders}
  @see-slot{gtk-file-chooser-do-overwrite-confirmation}
  @see-slot{gtk-file-chooser-extra-widget}
  @see-slot{gtk-file-chooser-filter}
  @see-slot{gtk-file-chooser-local-only}
  @see-slot{gtk-file-chooser-preview-widget}
  @see-slot{gtk-file-chooser-preview-widget-active}
  @see-slot{gtk-file-chooser-select-multiple}
  @see-slot{gtk-file-chooser-show-hidden}
  @see-slot{gtk-file-chooser-use-preview-label}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;: --- gtk-file-chooser-action ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action" 'gtk-file-chooser) 't)
 "The @code{action} property of type @symbol{gtk-file-chooser-action}
  (Read / Write) @br{}
  The type of operation that the file selector is performing. @br{}
  Default value: @code{:action-open}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-action atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-action 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-action object) => action}
  @syntax[]{(setf (gtk-file-chooser-action object) action)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[action]{the action of type @symbol{gtk-file-chooser-action} that
    the file selector is performing}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{action} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-action} gets the type of
  operation that the file chooser is performing. The slot access function
  @sym{(setf gtk-file-chooser-action)} sets the type of operation that the
  chooser is performing. The user interface is adapted to suit the selected
  action.

  For example, an option to create a new folder might be shown if the action is
  @code{:save} but not if the action is @code{:open}.
  @see-class{gtk-file-chooser}
  @see-symbol{gtk-file-chooser-action}")

;;; --- gtk-file-chooser-create-folders ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "create-folders"
                                               'gtk-file-chooser) 't)
 "The @code{create-folders} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether a file chooser not in @code{:action-open} mode will offer the user to
  create new folders. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-create-folders atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-create-folders 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-create-folders object) => create-folders}
  @syntax[]{(setf (gtk-file-chooser-create-folders object) create-folders)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[create-folders]{@em{true} if the New Folder button should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{create-folders} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-create-folders} gets whether
  file choser will offer to create new folders. The slot access function
  @sym{(setf gtk-file-chooser-create-folders)} sets whether file choser will
  offer to create new folders.

  This is only relevant if the action is not set to be @code{:open}.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-do-overwrite-confirmation -----------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "do-overwrite-confirmation"
                                               'gtk-file-chooser) 't)
 "The @code{do-overwrite-confirmation} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a file chooser in @code{:action-save} mode will present an overwrite
  confirmation dialog if the user selects a file name that already exists. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-do-overwrite-confirmation
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-do-overwrite-confirmation 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-do-overwrite-confirmation object) => confirmation}
  @syntax[]{(setf (gtk-file-chooser-do-overwrite-confirmation object) confirmation)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[confirmation]{a boolean whether to confirm overwriting in save mode}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{do-overwrite-confirmation} slot of
    the @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-do-overwrite-confirmation}
  queries whether a file chooser is set to confirm for overwriting when the
  user types a file name that already exists. The slot access function
  @sym{(setf gtk-file-chooser-do-overwrite-confirmation)} sets whether a file
  chooser in @code{:save} mode will present a confirmation dialog if the user
  types a file name that already exists. This is @em{false} by default.

  Regardless of this setting, the chooser will emit the \"confirm-overwrite\"
  signal when appropriate.

  If all you need is the stock confirmation dialog, set this property to
  @em{true}. You can override the way confirmation is done by actually handling
  the \"confirm-overwrite\" signal. Please refer to its documentation for the
  details.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-extra-widget ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "extra-widget"
                                               'gtk-file-chooser) 't)
 "The @code{extra-widget} property of type @class{gtk-widget} (Read / Write)
  @br{}
  Application supplied widget for extra options.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-extra-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-extra-widget 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-extra-widget object) => extra-widget}
  @syntax[]{(setf (gtk-file-chooser-extra-widget object) extra-widget)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[extra-widget]{a @class{gtk-widget} for extra options}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{extra-widget} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-extra-widget} sets the current
  preview widget. The slot access function
  @sym{(setf gtk-file-chooser-extra-widget)} sets an application-supplied
  widget to provide extra options to the user.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-filter ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "filter" 'gtk-file-chooser) 't)
 "The @code{filter} property of type @class{gtk-file-filter} (Read / Write)
  @br{}
  The current filter for selecting which files are displayed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-filter atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-filter 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-filter object) => filter}
  @syntax[]{(setf (gtk-file-chooser-filter object) filter)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{filter} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-filter} gets the current
  filter. The slot access function @sym{(setf gtk-file-chooser-filter)} sets
  the current filter. Only the files that pass the filter will be displayed.

  If the user-selectable list of filters is non-empty, then the filter should be
  one of the filters in that list. Setting the current filter when the list of
  filters is empty is useful if you want to restrict the displayed set of files
  without letting the user change it.
  @see-class{gtk-file-chooser}
  @see-class{gtk-file-filter}")

;;; --- gtk-file-chooser-local-only --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "local-only"
                                               'gtk-file-chooser) 't)
 "The @code{local-only} property of type @code{:boolean} (Read / Write) @br{}
  Whether the selected file(s) should be limited to local file URLs. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-local-only atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-local-only 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-local-only object) => local-only}
  @syntax[]{(setf (gtk-file-chooser-local-only object) local-only)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[local-only]{@em{true} if only local files can be selected}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{local-only} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-local-only} gets whether only
  local files can be selected in the file selector. The slot access function
  @sym{(setf gtk-file-chooser-local-only)} sets whether only local files can be
  selected in the file selector.

  If @arg{local-only} is @em{true}, the default, then the selected files are
  guaranteed to be accessible through the operating systems native file file
  system and therefore the application only needs to worry about the filename
  functions in @class{gtk-file-chooser}, like the function
  @fun{gtk-file-chooser-filename}, rather than the URI functions like the
  function @fun{gtk-file-chooser-uri}.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-preview-widget ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-widget"
                                               'gtk-file-chooser) 't)
 "The @code{preview-widget} property of type @class{gtk-widget} (Read / Write)
  @br{}
  Application supplied widget for custom previews.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-preview-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-preview-widget 'function)
 "@version{2019-5-13}
  @syntax[]{(gtk-file-chooser-preview-widget object) => preview-widget}
  @syntax[]{(setf (gtk-file-chooser-preview-widget object) preview-widget)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[preview-widget]{a @class{gtk-widget} for displaying preview}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{preview-widget} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-preview-widget} gets the
  current preview widget. The slot access function
  @sym{(setf gtk-file-chooser-preview-widget)} sets an application-supplied
  widget to use to display a custom preview of the currently selected file.

  To implement a preview, after setting the preview widget, you connect to the
  \"update-preview\" signal, and call the functions
  @fun{gtk-file-chooser-preview-filename} or @fun{gtk-file-chooser-preview-uri}
  on each change. If you can display a preview of the new file, update your
  widget and set the preview active using the function
  @fun{gtk-file-chooser-preview-widget-active}. Otherwise, set the preview
  inactive.

  When there is no application-supplied preview widget, or the
  application-supplied preview widget is not active, the file chooser may
  display an internally generated preview of the current file or it may
  display no preview at all.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-preview-widget-active ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-widget-active"
                                               'gtk-file-chooser) 't)
 "The @code{preview-widget-active} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the application supplied widget for custom previews should be
  shown. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-preview-widget-active
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-preview-widget-active 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-preview-widget-active object) => active}
  @syntax[]{(setf (gtk-file-chooser-preview-widget-active object) active)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[active]{a boolean whether to display the user-specified preview
    widget}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{preview-widget-active} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-preview-widget-active} gets
  whether the preview widget should be shown for the current filename. The
  slot access function @sym{(setf gtk-file-chooser-preview-widget-active)} sets
  whether the preview widget should be shown for the current filename.

  When @arg{active} is set to @em{false}, the file chooser may display an
  internally generated preview of the current file or it may display no preview
  at all. See the function @fun{gtk-file-chooser-preview-widget} for more
  details.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-preview-widget}")

;;; --- gtk-file-chooser-select-multiple ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "select-multiple"
                                               'gtk-file-chooser) 't)
 "The @code{select-multiple} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to allow multiple files to be selected. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-select-multiple atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-select-multiple 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-select-multiple object) => select-multiple}
  @syntax[]{(setf (gtk-file-chooser-select-multiple object) select-multiple)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[select-multiple]{@em{true} if multiple files can be selected}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{select-multiple} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-select-multiple} gets whether
  multiple files can be selected in the file selector. The slot access function
  @sym{(setf gtk-file-chooser-select-multiple)} sets whether multiple files can
  be selected in the file selector.

  This is only relevant if the action is set to be @code{:open} or
  @code{:select-folder}.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-show-hidden -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-hidden"
                                               'gtk-file-chooser) 't)
 "The @code{show-hidden} property of type @code{:boolean} (Read / Write) @br{}
  Whether the hidden files and folders should be displayed. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-show-hidden atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-show-hidden 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-show-hidden object) => show-hidden}
  @syntax[]{(setf (gtk-file-chooser-show-hidden object) show-hidden)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[show-hidden]{@em{true} if hidden files and folders should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{show-hidden} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-show-hidden} gets whether
  hidden files and folders are displayed in the file selector. The slot access
  function @sym{(setf gtk-file-chooser-show-hidden)} sets whether hidden files
  and folders are displayed in the file selector.
  @see-class{gtk-file-chooser}")

;;; --- gtk-file-chooser-use-preview-label -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-preview-label"
                                               'gtk-file-chooser) 't)
 "The @code{use-preview-label} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to display a stock label with the name of the previewed file. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-use-preview-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-use-preview-label 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-use-preview-label object) => use-label}
  @syntax[]{(setf (gtk-file-chooser-use-preview-label object) use-label)}
  @argument[object]{a @class{gtk-file-chooser} object}
  @argument[use-label]{a boolean whether to display a stock label with the name
    of the previewed file}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{use-preview-label} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-use-preview-label} gets whether
  a stock label should be drawn with the name of the previewed file. The slot
  access function @sym{(setf gtk-file-chooser-use-preview-label)} sets whether
  the file chooser should display a stock label with the name of the file that
  is being previewed. The default is @em{true}.

  Applications that want to draw the whole preview area themselves should set
  this to @em{false} and display the name themselves in their preview widget.

  See also the function @fun{gtk-file-chooser-preview-widget}.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-preview-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_name ()
;;; gtk_file_chooser_set_current_name () -> gtk-file-chooser-current-name
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-current-name) (name chooser)
  (foreign-funcall "gtk_file_chooser_set_current_name"
                   (g-object gtk-file-chooser) chooser
                   (:string :free-to-foreign t :encoding :utf-8) name
                   :void)
  name)

(defcfun ("gtk_file_chooser_get_current_name" gtk-file-chooser-current-name)
    (:string :free-from-foreign t :encoding :utf-8)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-current-name chooser) => name}
  @syntax[]{(setf (gtk-file-chooser-current-name chooser) name)}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[name]{a string with the filename to use, as a UTF-8 string}
  @begin{short}
    Accessor of the current name of a file chooser object.
  @end{short}

  The function @sym{gtk-file-chooser-current-name} gets the current name in the
  file selector, as entered by the user in the text entry for \"Name\". The slot
  access functions @sym{(gtk-file-chooser-current-name)} sets the current name
  in the file selector, as if entered by the user.

  This is meant to be used in save dialogs, to get the currently typed filename
  when the file itself does not exist yet. For example, an application that adds
  a custom extra widget to the file chooser for \"file format\" may want to
  change the extension of the typed filename based on the chosen format, say,
  from \".jpg\" to \".png\".

  Note that the name passed in here is a UTF-8 string rather than a filename.
  This function is meant for such uses as a suggested name in a \"Save As...\"
  dialog. You can pass \"Untitled.doc\" or a similarly suitable suggestion for
  the name.

  If you want to preselect a particular existing file, you should use the
  functions @fun{gtk-file-chooser-filename} or @fun{gtk-file-chooser-uri}
  instead. Please see the documentation for those functions for an
  example of using the function @sym{gtk-file-chooser-current-name} as well.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-filename}
  @see-function{gtk-file-chooser-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-current-name)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filename ()
;;; gtk_file_chooser_set_filename () -> gtk-file-chooser-filename
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-filename) (filename chooser)
  (foreign-funcall "gtk_file_chooser_set_filename"
                   (g-object gtk-file-chooser) chooser
                   :string filename
                   :boolean)
  filename)

(defcfun ("gtk_file_chooser_get_filename" gtk-file-chooser-filename)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-filename chooser) => filename}
  @syntax[]{(setf (gtk-file-chooser-filename chooser) filename)}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[filename]{a string with the filename to set as current}
  @begin{short}
    Accessor of the filename of a file chooser widget.
  @end{short}

  The function @sym{gtk-file-chooser-filename} gets the filename for the
  currently selected file in the file selector. If multiple files are selected,
  one of the filenames will be returned at random. If the file chooser is in
  folder mode, this function returns the selected folder.

  The function @sym{(gtk-file-chooser-filename)} sets @arg{filename} as the
  current filename for the file chooser, by changing to the file's parent folder
  and actually selecting the file in the list; all other files will be
  unselected.

  If the chooser is in @code{:save} mode, the file's base name will also appear
  in the dialog's file name entry.

  Note that the file must exist, or nothing will be done except for the
  directory change.

  You should use this function only when implementing a \"File/Save As...\"
  dialog for which you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does
  \"File/Save As...\" on it to save a copy or a modified version. If you do not
  have a file name already - for example, if the user just created a new file
  and is saving it for the first time, do not call this function. Instead, use
  something similar to this:
  @begin{pre}
 if (document_is_new)
   {
     /* the user just created a new document */
     gtk_file_chooser_set_current_name (chooser, \"Untitled document\");
   @}
  else
   {
     /* the user edited an existing document */
     gtk_file_chooser_set_filename (chooser, existing_filename);
   @}
  @end{pre}
  In the first case, the file chooser will present the user with useful
  suggestions as to where to save his new file. In the second case, the file's
  existing location is already known, so the file chooser will use it.
  @see-class{gtk-file-chooser}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_select_filename" gtk-file-chooser-select-filename)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[filename]{a string with the filename to select}
  @begin{return}
    Not useful. See also the @fun{gtk-file-chooser-filename} function.
  @end{return}
  @begin{short}
    Selects a filename.
  @end{short}
  If the file name is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  filename.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-filename}
  @see-function{gtk-file-chooser-unselect-filename}"
  (chooser (g-object gtk-file-chooser))
  (filename :string))

(export 'gtk-file-chooser-select-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_unselect_filename"
           gtk-file-chooser-unselect-filename) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[filename]{a string with the filename to unselect}
  @begin{short}
    Unselects a currently selected filename.
  @end{short}
  If the filename is not in the current directory, does not exist, or is
  otherwise not currently selected, does nothing.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-select-filename}"
  (chooser (g-object gtk-file-chooser))
  (filenname :string))

(export 'gtk-file-chooser-unselect-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_select_all" gtk-file-chooser-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{short}
    Selects all the files in the current folder of a file chooser.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-unselect-all}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_unselect_all" gtk-file-chooser-unselect-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{short}
    Unselects all the files in the current folder of a file chooser.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-select-all}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filenames () -> gtk-file-chooser-filenames
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_filenames" gtk-file-chooser-filenames)
    (g-slist :string)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    A list containing the filenames of all selected files and subfolders
    in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser.
  @end{short}
  The returned names are full absolute paths. If files in the current folder
  cannot be represented as local filenames they will be ignored. See the
  @fun{gtk-file-chooser-uris} function.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-uris}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-filenames)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder ()
;;; gtk_file_chooser_set_current_folder () -> gtk-file-chooser-current-folder
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-current-folder) (filename chooser)
  (foreign-funcall "gtk_file_chooser_set_current_folder"
                   (g-object gtk-file-chooser) chooser
                   :string filename
                   :boolean)
  filename)

(defcfun ("gtk_file_chooser_get_current_folder" gtk-file-chooser-current-folder)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @syntax[]{(gtk-file-chooser-current-folder chooser) => filename}
  @syntax[]{(setf (gtk-file-chooser-current-folder chooser) filename)}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[filename]{a string with the full path of the new current folder}
  @begin{short}
    Accessor of the current folder of the file chooser.
  @end{short}

  The function @sym{gtk-file-chooser-current-folder} gets the current folder of
  the file chooser as a local filename. The function
  @sym{(setf gtk-file-chooser-current-folder)} sets the current folder for
  the file chooser from a local filename.

  The user will be shown the full contents of the current folder, plus user
  interface elements for navigating to other folders.

  Note that this is the folder that the file chooser is currently displaying,
  e. g. \"/home/username/Documents\", which is not the same as the
  currently-selected folder if the chooser is in @code{:select-folder} mode,
  e. g. \"/home/username/Documents/selected-folder/\". To get the
  currently-selected folder in that mode, use the function
  @fun{gtk-file-chooser-uri} as the usual way to get the selection.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-current-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_uri" gtk-file-chooser-get-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    The currently selected URI, or @code{nil} if no file is selected.
  @end{return}
  @begin{short}
    Gets the URI for the currently selected file in the file selector.
  @end{short}
  If multiple files are selected, one of the filenames will be returned at
  random.

  If the file chooser is in folder mode, this function returns the selected
  folder.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-set-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-get-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_set_uri" gtk-file-chooser-set-uri) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[uri]{the URI to set as current}
  @return{Not useful.}
  @begin{short}
    Sets the file referred to by uri as the current file for the file chooser,
    by changing to the URI's parent folder and actually selecting the URI in
    the list.
  @end{short}
  If the chooser is in @code{:save} mode, the URI's base name will also appear
  in the dialog's file name entry.

  Note that the URI must exist, or nothing will be done except for the
  directory change.

  You should use this function only when implementing a File/Save As... dialog
  for which you already have a file name to which the user may save. For
  example, whenthe user opens an existing file and then does File/Save As...
  on it to save a copy or a modified version. If you do not have a file name
  already - for example, if the user just created a new file and is saving it
  for the first time, do not call this function. Instead, use something
  similar to this:
  @begin{pre}
   if (document_is_new)
     {
       /* the user just created a new document */
       gtk_file_chooser_set_current_name (chooser, \"Untitled document\");
     @}
   else
     {
       /* the user edited an existing document */
       gtk_file_chooser_set_uri (chooser, existing_uri);
     @}
  @end{pre}
  In the first case, the file chooser will present the user with useful
  suggestions as to where to save his new file. In the second case, the file's
  existing location is already known, so the file chooser will use it.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-get-uri}"
  (chooser (g-object gtk-file-chooser))
  (uri :string))

(export 'gtk-file-chooser-set-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_select_uri" gtk-file-chooser-select-uri) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[uri]{the URI to select}
  @return{Not useful.}
  @begin{short}
    Selects the file to by @arg{uri}.
  @end{short}
  If the URI does not refer to a file in the current folder of @arg{chooser},
  then the current folder of chooser will be changed to the folder containing
  filename.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-unselect-uri}"
  (chooser (g-object gtk-file-chooser))
  (uri :string))

(export 'gtk-file-chooser-select-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_unselect_uri" gtk-file-chooser-unselect-uri) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[uri]{the URI to unselect}
  @begin{short}
    Unselects the file referred to by @arg{uri}.
  @end{short}
  If the file is not in the current directory, does not exist, or is otherwise
  not currently selected, does nothing.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-select-uri}"
  (chooser (g-object gtk-file-chooser))
  (uri :string))

(export 'gtk-file-chooser-unselect-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_uris ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_uris" gtk-file-chooser-get-uris)
    (g-slist :string)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    A list containing the URIs of all selected files and subfolders in the
    current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    @arg{chooser}. The returned names are full absolute URIs.
  @end{short}
  @see-class{gtk-file-chooser}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-get-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_set_current_folder_uri"
           gtk-file-chooser-set-current-folder-uri) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[uri]{the URI for the new current folder}
  @begin{return}
    @em{true} if the folder could be changed successfully, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Sets the current folder for chooser from an URI.
  @end{short}
  The user will be shown the full contents of the current folder, plus user
  interface elements for navigating to other folders.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-get-current-folde-uri}"
  (chooser (g-object gtk-file-chooser))
  (uri :string))

(export 'gtk-file-chooser-set-current-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_current_folder_uri"
           gtk-file-chooser-get-current-folder-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    The URI for the current folder. This function will also return @code{nil}
    if the file chooser was unable to load the last folder that was requested
    from it; for example, as would be for calling the function
    @fun{gtk-file-chooser-set-current-folder-uri} on a nonexistent folder.
  @end{return}
  @begin{short}
    Gets the current folder of @arg{chooser} as an URI.
  @end{short}
  See the @fun{gtk-file-chooser-set-current-folder-uri} function.

  Note that this is the folder that the file chooser is currently displaying,
  e. g. \"file:///home/username/Documents\", which is not the same as the
  currently-selected folder if the chooser is in @code{:select-folder}, e. g.
  \"file:///home/username/Documents/selected-folder/\". To get the
  currently-selected folder in that mode, use the function
  @fun{gtk-file-chooser-get-uri} as the usual way to get the selection.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-get-uri}
  @see-function{gtk-file-chooser-set-current-folder-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-get-current-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_preview_filename"
           gtk-file-chooser-get-preview-filename) (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    The filename to preview, or @code{nil} if no file is selected, or if the
    selected file cannot be represented as a local filename.
  @end{return}
  @begin{short}
    Gets the filename that should be previewed in a custom preview widget.
  @end{short}
  See the @fun{gtk-file-chooser-preview-widget} slot access function.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-set-preview-widget}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-get-preview-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_preview_uri" gtk-file-chooser-get-preview-uri)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    The URI for the file to preview, or @code{nil} if no file is selected.
  @end{return}
  @begin{short}
    Gets the URI that should be previewed in a custom preview widget.
  @end{short}
  See the @fun{gtk-file-chooser-preview-widget} function.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-get-preview-widget}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-get-preview-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_add_filter" gtk-file-chooser-add-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Adds filter to the list of filters that the user can select between.
  @end{short}
  When a filter is selected, only files that are passed by that filter are
  displayed.

  Note that the chooser takes ownership of the filter, so you have to ref and
  sink it if you want to keep a reference.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-remove-filter}"
  (chooser (g-object gtk-file-chooser))
  (filter (g-object gtk-file-filter)))

(export 'gtk-file-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_remove_filter" gtk-file-chooser-remove-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Removes filter from the list of filters that the user can select between.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-filter}"
  (chooser (g-object gtk-file-chooser))
  (filter (g-object gtk-file-filter)))

(export 'gtk-file-chooser-remove-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_filters ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_list_filters" gtk-file-chooser-list-filters)
    (g-slist (g-object gtk-file-filter))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    A list containing the current set of user selectable filters.
  @end{return}
  @begin{short}
    Lists the current set of user-selectable filters.
  @end{short}
  See the @fun{gtk-file-chooser-add-filter} and
  @fun{gtk-file-chooser-remove-filter} functions.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-filter}
  @see-function{gtk-file-chooser-remove-filter}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-list-filters)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_add_shortcut_folder"
          %gtk-file-chooser-add-shortcut-folder) :boolean
  (chooser (g-object gtk-file-chooser))
  (folder :string)
  (error :pointer))

(defun gtk-file-chooser-add-shortcut-folder (chooser folder)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[folder]{filename of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @code{nil} otherwise.
    In the latter case, the error will be set as appropriate.
  @end{return}
  @begin{short}
    Adds a folder to be displayed with the shortcut folders in a file chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  \"/usr/share/mydrawprogram/Clipart\" folder to the volume list.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-remove-shortcut-folder}"
  (with-g-error (err)
    (%gtk-file-chooser-add-shortcut-folder chooser folder err)))

(export 'gtk-file-chooser-add-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_remove_shortcut_folder"
          %gtk-file-chooser-remove-shortcut-folder) :boolean
  (chooser (g-object gtk-file-chooser))
  (folder :string)
  (error :pointer))

(defun gtk-file-chooser-remove-shortcut-folder (chooser folder)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[folder]{filename of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @code{nil} otherwise. In the latter
    case, the error will be set as appropriate. See also the function
    @fun{gtk-file-chooser-add-shortcut-folder}.
  @end{return}
  @begin{short}
    Removes a folder from a file chooser's list of shortcut folders.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-shortcut-folder}"
  (with-g-error (err)
    (%gtk-file-chooser-remove-shortcut-folder chooser folder err)))

(export 'gtk-file-chooser-remove-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_shortcut_folders ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_list_shortcut_folders"
           gtk-file-chooser-list-shortcut-folders) (g-slist :string)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    A list of folder filenames, or @code{nil} if there are no shortcut folders.
  @end{return}
  @begin{short}
    Queries the list of shortcut folders in the file chooser, as set by the
    @fun{gtk-file-chooser-add-shortcut-folder} function.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-shortcut-folder}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-list-shortcut-folders)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_add_shortcut_folder_uri"
          %gtk-file-chooser-add-shortcut-folder-uri) :boolean
  (chooser (g-object gtk-file-chooser))
  (uri :string)
  (error :pointer))

(defun gtk-file-chooser-add-shortcut-folder-uri (chooser uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[uri]{URI of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @code{nil} otherwise.
    In the latter case, the error will be set as appropriate.
  @end{return}
  @begin{short}
    Adds a folder URI to be displayed with the shortcut folders in a file
    chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  \"file:///usr/share/mydrawprogram/Clipart\" folder to the volume list.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-remove-shortcut-folder}"
  (with-g-error (err)
    (%gtk-file-chooser-add-shortcut-folder-uri chooser uri err)))

(export 'gtk-file-chooser-add-shortcut-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_remove_shortcut_folder_uri"
          %gtk-file-chooser-remove-shortcut-folder-uri) :boolean
  (chooser (g-object gtk-file-chooser))
  (uri :string)
  (error :pointer))

(defun gtk-file-chooser-remove-shortcut-folder-uri (chooser uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @argument[uri]{URI of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @code{nil} otherwise. In the latter
    case, the error will be set as appropriate. See also the function
    @fun{gtk-file-chooser-add-shortcut-folder-uri}.
  @end{return}
  @begin{short}
    Removes a folder URI from a file chooser's list of shortcut folders.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-shortcut-folder-uri}"
  (with-g-error (err)
    (%gtk-file-chooser-remove-shortcut-folder-uri chooser uri err)))

(export 'gtk-file-chooser-remove-shortcut-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_shortcut_folder_uris ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_list_shortcut_folder_uris"
           gtk-file-chooser-list-shortcut-folder-uris) (g-slist :string)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-24}
  @argument[chooser]{a @class{gtk-file-chooser} object}
  @begin{return}
    A list of folder URIs, or @code{nil} if there are no shortcut folders.
  @end{return}
  @begin{short}
    Queries the list of shortcut folders in the file chooser, as set by the
    @fun{gtk-file-chooser-add-shortcut-folder-uri} function.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-shortcut-folder-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-list-shortcut-folder-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder_file ()
;;;
;;; GFile * gtk_file_chooser_get_current_folder_file (GtkFileChooser *chooser);
;;;
;;; Gets the current folder of chooser as GFile. See
;;; gtk_file_chooser_get_current_folder_uri().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; Returns :
;;;     the GFile for the current folder
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_file ()
;;;
;;; GFile * gtk_file_chooser_get_file (GtkFileChooser *chooser);
;;;
;;; Gets the GFile for the currently selected file in the file selector. If
;;; multiple files are selected, one of the files will be returned at random.
;;;
;;; If the file chooser is in folder mode, this function returns the selected
;;; folder.
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; Returns :
;;;     a selected GFile. You own the returned file; use g_object_unref() to
;;;     release it
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_files ()
;;;
;;; GSList * gtk_file_chooser_get_files (GtkFileChooser *chooser);
;;;
;;; Lists all the selected files and subfolders in the current folder of chooser
;;; as GFile. An internal function, see gtk_file_chooser_get_uris().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; Returns :
;;;     a GSList containing a GFile for each selected file and subfolder in the
;;;     current folder. Free the returned list with g_slist_free(), and the
;;;     files with g_object_unref().
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_file ()
;;;
;;; GFile * gtk_file_chooser_get_preview_file (GtkFileChooser *chooser);
;;;
;;; Gets the GFile that should be previewed in a custom preview Internal
;;; function, see gtk_file_chooser_get_preview_uri().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; Returns :
;;;     the GFile for the file to preview, or NULL if no file is selected. Free
;;;     with g_object_unref().
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_file ()
;;;
;;; gboolean gtk_file_chooser_select_file (GtkFileChooser *chooser,
;;;                                        GFile *file,
;;;                                        GError **error);
;;;
;;; Selects the file referred to by file. An internal function. See
;;; gtk_file_chooser_select_uri().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; file :
;;;     the file to select
;;;
;;; error :
;;;     location to store error, or NULL
;;;
;;; Returns :
;;;     Not useful.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder_file ()
;;;
;;; gboolean gtk_file_chooser_set_current_folder_file (GtkFileChooser *chooser,
;;;                                                    GFile *file,
;;;                                                    GError **error);
;;;
;;; Sets the current folder for chooser from a GFile. Internal function, see
;;; gtk_file_chooser_set_current_folder_uri().
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; file :
;;;     the GFile for the new folder
;;;
;;; error :
;;;     location to store error, or NULL
;;;
;;; Returns :
;;;     TRUE if the folder could be changed successfully, FALSE otherwise.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_file ()
;;;
;;; gboolean gtk_file_chooser_set_file (GtkFileChooser *chooser,
;;;                                     GFile *file,
;;;                                     GError **error);
;;;
;;; Sets file as the current filename for the file chooser, by changing to the
;;; file's parent folder and actually selecting the file in list. If the chooser
;;; is in GTK_FILE_CHOOSER_ACTION_SAVE mode, the file's base name will also
;;; appear in the dialog's file name entry.
;;;
;;; If the file name isn't in the current folder of chooser, then the current
;;; folder of chooser will be changed to the folder containing filename. This is
;;; equivalent to a sequence of gtk_file_chooser_unselect_all() followed by
;;; gtk_file_chooser_select_filename().
;;;
;;; Note that the file must exist, or nothing will be done except for the
;;; directory change.
;;;
;;; If you are implementing a File/Save As... dialog, you should use this
;;; function if you already have a file name to which the user may save; for
;;; example, when the user opens an existing file and then does File/Save As...
;;; on it. If you don't have a file name already - for example, if the user just
;;; created a new file and is saving it for the first time, do not call this
;;; function. Instead, use something similar to this:
;;;
;;;   if (document_is_new)
;;;     {
;;;       /* the user just created a new document */
;;;       gtk_file_chooser_set_current_folder_file (chooser,
;;;                                                 default_file_for_saving);
;;;       gtk_file_chooser_set_current_name (chooser, "Untitled document");
;;;     }
;;;   else
;;;     {
;;;       /* the user edited an existing document */
;;;       gtk_file_chooser_set_file (chooser, existing_file);
;;;     }
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; file :
;;;     the GFile to set as current
;;;
;;; error :
;;;     location to store the error, or NULL to ignore errors. [allow-none]
;;;
;;; Returns :
;;;     Not useful.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_file ()
;;;
;;; void gtk_file_chooser_unselect_file (GtkFileChooser *chooser,
;;;                                      GFile *file);
;;;
;;; Unselects the file referred to by file. If the file is not in the current
;;; directory, does not exist, or is otherwise not currently selected, does
;;; nothing.
;;;
;;; chooser :
;;;     a GtkFileChooser
;;;
;;; file :
;;;     a GFile
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.file-chooser.lisp --------------------------------------
