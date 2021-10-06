;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     gtk_file_chooser_set_filename
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
(setf (gethash 'gtk-file-chooser-action atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-file-chooser-action atdoc:*external-symbols*)
 "@version{2021-1-28}
  @begin{short}
    Describes whether a @class{gtk-file-chooser} widget is being used to
    open existing files or to save to a possibly new file.
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
    @entry[:open]{Indicates Open mode. The file chooser will only let the user
      pick an existing file.}
    @entry[:save]{Indicates Save mode. The file chooser will let the user pick
      an existing file, or type in a new filename.}
    @entry[:select-folder]{Indicates an Open mode for selecting folders. The
      file chooser will let the user pick an existing folder.}
    @entry[:create-folder]{Indicates a mode for creating a new folder. The
      file chooser will let the user name an existing or new folder.}
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
(setf (gethash 'gtk-file-chooser-confirmation atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-file-chooser-confirmation atdoc:*external-symbols*)
 "@version{2021-1-28}
  @begin{short}
    Used as a return value of handlers for the \"confirm-overwrite\" signal of
    a @class{gtk-file-chooser} widget.
  @end{short}
  This value determines whether the file chooser will present the stock
  confirmation dialog, accept the user's choice of a filename, or let the user
  choose another filename.
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

;; GtkFileChooserError is not exported

(define-g-enum "GtkFileChooserError" gtk-file-chooser-error
  (:export nil
   :type-initializer "gtk_file_chooser_error_get_type")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-error atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-file-chooser-error atdoc:*external-symbols*)
 "@version{2028-1-28}
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
    @entry[:already-exists]{Indicates a duplicate path, e.g. when adding a
      bookmark.}
    @entry[:incomplete-hostname]{Indicates an incomplete hostname, e.g.
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
;         gtk-file-chooser-filename
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
(setf (gethash 'gtk-file-chooser atdoc:*class-name-alias*)
      "Interface"
      (documentation 'gtk-file-chooser 'type)
 "@version{2021-2-4}
  @begin{short}
    The @sym{gtk-file-chooser} interface is an interface that can be
    implemented by file selection widgets.
  @end{short}
  The main widgets that implement this interface are
  @class{gtk-file-chooser-widget}, @class{gtk-file-chooser-dialog}, and
  @class{gtk-file-chooser-button}. You do not need to write a widget that
  implements the @sym{gtk-file-chooser} interface unless you are trying to
  adapt an existing file selector to expose a standard programming interface.

  The @sym{gtk-file-chooser} interface allows for shortcuts to various places
  in the filesystem. In the default implementation these are displayed in the
  left pane. It may be a bit confusing at first that these shortcuts come from
  various sources and in various flavours, so lets explain the terminology here:
  @begin{table}
    @begin[Bookmarks]{entry}
      are created by the user, by dragging folders from the right pane to the
      left pane, or by using the \"Add\". Bookmarks can be renamed and deleted
      by the user.
    @end{entry}
    @begin[Shortcuts]{entry}
      can be provided by the application or by the underlying filesystem
      abstraction, e.g. both the gnome-vfs and the Windows filesystems provide
      \"Desktop\" shortcuts. Shortcuts cannot be modified by the user.
    @end{entry}
    @begin[Volumes]{entry}
      are provided by the underlying filesystem abstraction. Volumes are the
      \"roots\" of the filesystem.
    @end{entry}
  @end{table}
  @subheading{File Names and Encodings}
    When the user is finished selecting files in a @sym{gtk-file-chooser}
    widget, the program can get the selected names either as filenames or as
    URIs. For URIs, the normal escaping rules are applied if the URI contains
    non-ASCII characters. However, filenames are always returned in the
    character set specified by the @code{G_FILENAME_ENCODING} environment
    variable. Please see the GLib documentation for more details about this
    variable.

    This means that while you can pass the result of the the function
    @fun{gtk-file-chooser-filename} to the functions @code{open()} or
    @code{fopen()}, you may not be able to directly set it as the text of a
    @class{gtk-label} widget unless you convert it first to UTF-8, which all
    GTK widgets expect. You should use the function @fun{g-filename-to-utf8}
    to convert filenames into strings that can be passed to GTK widgets.

  @subheading{Adding a Preview Widget}
    You can add a custom preview widget to a file chooser and then get
    notification about when the preview needs to be updated. To install a
    preview widget, use the function @fun{gtk-file-chooser-preview-widget}.
    Then, connect to the \"update-preview\" signal to get notified
    when you need to update the contents of the preview.

    Your callback should use the function
    @fun{gtk-file-chooser-preview-filename} to see what needs previewing. Once
    you have generated the preview for the corresponding file, you must call
    the function @fun{gtk-file-chooser-preview-widget-active} with a boolean
    flag that indicates whether your callback could successfully generate a
    preview.

    @b{Example:} Sample Usage
    @begin{pre}
(defun create-file-chooser-preview ()
  (let ((response nil)
        (preview-width 256)
        (preview-height 256)
        (chooser (gtk-file-chooser-dialog-new \"Example File Chooser Preview\"
                                              nil
                                              :open
                                              \"gtk-open\" :accept
                                              \"gtk-cancel\" :cancel))
        (preview (make-instance 'gtk-image
                                :margin 24)))
    ;; Handler for the signal \"upadate-preview\"
    (g-signal-connect chooser \"update-preview\"
        (lambda (chooser)
          (let* ((filename (gtk-file-chooser-preview-filename chooser))
                 (pixbuf (when filename
                           (gdk-pixbuf-new-from-file-at-size filename
                                                             preview-width
                                                             preview-height))))
            (if pixbuf
                (progn
                  (gtk-image-set-from-pixbuf preview pixbuf)
                  (setf (gtk-file-chooser-preview-widget-active chooser) t))
                (setf (gtk-file-chooser-preview-widget-active chooser) nil)))))
    ;; Set the preview widget
    (setf (gtk-file-chooser-preview-widget chooser) preview)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk-dialog-run chooser)))
      (format t \"Save to file ~A~%\"
                (gtk-file-chooser-filename chooser)))
    (gtk-widget-destroy chooser)
    response))
    @end{pre}
  @subheading{Adding Extra Widgets}
    You can add extra widgets to a file chooser to provide options that are not
    present in the default design. For example, you can add a toggle button to
    give the user the option to open a file in read-only mode. You can use the
    function @fun{gtk-file-chooser-extra-widget} to insert additional widgets
    in a file chooser.

    @b{Example:} Sample Usage
    @begin{pre}
(defun create-file-chooser-widget ()
  (let ((response nil)
        (chooser (gtk-file-chooser-dialog-new \"Example File Chooser Widget\"
                                              nil
                                              :open
                                              \"gtk-open\" :accept
                                              \"gtk-cancel\" :cancel))
        (extra-widget (make-instance 'gtk-box
                                     :orientation :horizontal
                                     :spacing 12))
        (local-only (gtk-check-button-new-with-label \"Local only\"))
        (select-multiple (gtk-check-button-new-with-label \"Select Multiple\"))
        (show-hidden (gtk-check-button-new-with-label \"Show hidden\")))
    ;; Connect signal handlers to the toggle buttons
    (g-signal-connect local-only \"toggled\"
                      (lambda (button)
                        (setf (gtk-file-chooser-local-only chooser)
                              (gtk-toggle-button-active button))))
    (g-signal-connect select-multiple \"toggled\"
                      (lambda (button)
                        (setf (gtk-file-chooser-select-multiple chooser)
                              (gtk-toggle-button-active button))))
    (g-signal-connect show-hidden \"toggled\"
                      (lambda (button)
                        (setf (gtk-file-chooser-show-hidden chooser)
                              (gtk-toggle-button-active button))))
    ;; Put the extra widgets in a box
    (gtk-box-pack-start extra-widget local-only)
    (setf (gtk-toggle-button-active local-only) t) ; default is true
    (gtk-box-pack-start extra-widget select-multiple)
    (gtk-box-pack-start extra-widget show-hidden)
    (setf (gtk-file-chooser-extra-widget chooser) extra-widget)
    ;; Show the extra widgets
    (gtk-widget-show-all extra-widget)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk-dialog-run chooser)))
      (format t \"Open file ~A~%\"
                (gtk-file-chooser-filename chooser)))
    (gtk-widget-destroy chooser)
    response))
    @end{pre}
    If you want to set more than one extra widget in the file chooser, you can
    add a container such as a @class{gtk-box} or a @class{gtk-grid} widget and
    include your widgets in it. Then, set the container as the whole extra
    widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"confirm-overwrite\" signal}
      @begin{pre}
 lambda (chooser)    :run-last
      @end{pre}
      The signal gets emitted whenever it is appropriate to present a
      confirmation dialog when the user has selected a file name that already
      exists. The signal only gets emitted when the file chooser is in
      @code{:save} mode.

      Most applications just need to turn on the
      @code{do-overwrite-confirmation} property, and they will automatically
      get a stock confirmation dialog. Applications which need to customize
      this behavior should do that, and also connect to the
      \"confirm-overwrite\" signal.

      A signal handler for this signal must return a value of the
      @symbol{gtk-file-chooser-confirmation} enumeration, which indicates the
      action to take. If the handler determines that the user wants to select
      a different filename, it should return the @code{:select-again} value. If
      it determines that the user is satisfied with his choice of file name, it
      should return the @code{:accept-filename} value. On the other hand, if it
      determines that the stock confirmation dialog should be used, it should
      return the @code{:confirm} value. The following example illustrates this.

      @b{Example:} Custom confirmation
      @begin{pre}
(defun confirm-overwrite (chooser)
  (let ((uri (gtk-file-chooser-uri chooser)))
    ;; Check for read-only file
    (if (is-uri-read-only uri)
        (if (user-wants-to-replace-read-only-file uri)
            ;; User accepts overwriting
            :accept-filename
            ;; User rejects overwriting
            :select-again)
         ;; Fall back to the default dialog
         :confirm)))
   ...
  (let ((chooser (gtk-file-choose-dialog-new ...)))
    ...
    (setf (gtk-file-chooser-do-overwrite-confirmation chooser) t)
    (g-signal-connect chooser \"confirm-overwrite\" #'confirm-overwrite)
    ... )
      @end{pre}
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} widget which received the
          signal.}
        @entry[Returns]{A @symbol{gtk-file-chooser-confirmation} value that
          indicates which action to take after emitting the signal.}
      @end{table}
    @subheading{The \"current-folder-changed\" signal}
      @begin{pre}
 lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the current folder in a file chooser changes.
      This can happen due to the user performing some action that changes
      folders, such as selecting a bookmark or visiting a folder on the file
      list. It can also happen as a result of calling a function to explicitly
      change the current folder in a file chooser. Normally you do not need to
      connect to this signal, unless you need to keep track of which folder a
      file chooser is showing.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} widget which received the
          signal.}
      @end{table}
    @subheading{The \"file-activated\" signal}
      @begin{pre}
 lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the user \"activates\" a file in the file
      chooser. This can happen by double-clicking on a file in the file list,
      or by pressing the @kbd{Enter} key. Normally you do not need to connect to
      the signal. It is used internally by the @class{gtk-file-chooser-dialog}
      class to know when to activate the default button in the dialog.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} widget which received the
          signal.}
      @end{table}
    @subheading{The \"selection-changed\" signal}
      @begin{pre}
 lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when there is a change in the set of selected files
      in a file chooser. This can happen when the user modifies the selection
      with the mouse or the keyboard, or when explicitly calling functions to
      change the selection. Normally you do not need to connect to the signal,
      as it is easier to wait for the file chooser to finish running.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} widget which received the
          signal.}
      @end{table}
    @subheading{The \"update-preview\" signal}
      @begin{pre}
 lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the preview in a file chooser should be
      regenerated. For example, this can happen when the currently selected
      file changes. You should use this signal if you want your file chooser
      to have a preview widget. Once you have installed a preview widget with
      the @fun{gtk-file-chooser-preview-widget} function, you should update it
      when the signal is emitted. You can use the
      @fun{gtk-file-chooser-preview-filename} or
      @fun{gtk-file-chooser-preview-uri} functions to get the name of the file
      to preview. Your widget may not be able to preview all kinds of files.
      Your callback function must call the
      @fun{gtk-file-chooser-preview-widget-active} function to inform the file
      chooser about whether the preview was generated successfully or not.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-file-chooser} widget which received the
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
  @see-slot{gtk-file-chooser-use-preview-label}
  @see-class{gtk-file-chooser-button}
  @see-class{gtk-file-chooser-dialog}
  @see-class{gtk-file-chooser-widget}")

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
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-action object) => action}
  @syntax[]{(setf (gtk-file-chooser-action object) action)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[action]{a value of the @symbol{gtk-file-chooser-action} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{action} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-action} gets the type of
  operation that the file chooser is performing. The slot access function
  @sym{(setf gtk-file-chooser-action)} sets the type of operation. The user
  interface is adapted to suit the selected action. For example, an option to
  create a new folder might be shown if the action is @code{:save} but not if
  the action is @code{:open}.
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
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-create-folders object) => create-folders}
  @syntax[]{(setf (gtk-file-chooser-create-folders object) create-folders)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[create-folders]{@em{true} if the New Folder button should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{create-folders} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-create-folders} gets whether
  the file choser will offer to create new folders. The slot access function
  @sym{(setf gtk-file-chooser-create-folders)} sets whether the file choser
  will offer to create new folders.

  This is only relevant if the action of the file chooser is not set to be
  @code{:open}.
  @see-class{gtk-file-chooser}
  @see-symbol{gtk-file-chooser-action}")

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
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-do-overwrite-confirmation object) => confirm}
  @syntax[]{(setf (gtk-file-chooser-do-overwrite-confirmation object) confirm)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[confirm]{a boolean whether to confirm overwriting in @code{:save}
    mode}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{do-overwrite-confirmation} slot of
    the @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-do-overwrite-confirmation}
  queries whether the file chooser in @code{:save} mode is set to confirm for
  overwriting when the user types a file name that already exists. The slot
  access function @sym{(setf gtk-file-chooser-do-overwrite-confirmation)} sets
  whether the file chooser will present a confirmation dialog. This is
  @em{false} by default.

  Regardless of this setting, the chooser will emit the \"confirm-overwrite\"
  signal when appropriate.

  If all you need is the stock confirmation dialog, set this property to
  @em{true}. You can override the way confirmation is done by actually handling
  the \"confirm-overwrite\" signal. Please refer to its documentation for the
  details.
  @see-class{gtk-file-chooser}
  @see-symbol{gtk-file-chooser-action}")

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
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-extra-widget object) => extra-widget}
  @syntax[]{(setf (gtk-file-chooser-extra-widget object) extra-widget)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[extra-widget]{a @class{gtk-widget} widget for extra options}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{extra-widget} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-extra-widget} gets the
  application-supplied widget to provide extra options to the user. The slot
  access function @sym{(setf gtk-file-chooser-extra-widget)} sets the widget.
  @see-class{gtk-file-chooser}
  @see-class{gtk-widget}")

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
 "@version{2021-1-28}
  @syntax[]{(gtk-file-chooser-filter object) => filter}
  @syntax[]{(setf (gtk-file-chooser-filter object) filter)}
  @argument[object]{a @class{gtk-file-chooser} widget}
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
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-local-only object) => local-only}
  @syntax[]{(setf (gtk-file-chooser-local-only object) local-only)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[local-only]{@em{true} if only local files can be selected}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{local-only} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-local-only} gets whether only
  local files can be selected in the file selector. The slot access function
  @sym{(setf gtk-file-chooser-local-only)} sets whether only local files can be
  selected.

  If @arg{local-only} is @em{true}, the default, then the selected files are
  guaranteed to be accessible through the operating systems native file
  system and therefore the application only needs to worry about the filename
  functions in the file chooser, like the function
  @fun{gtk-file-chooser-filename}, rather than the URI functions like the
  function @fun{gtk-file-chooser-uri}.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-filename}
  @see-function{gtk-file-chooser-uri}")

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
 "@version{*2021-2-4}
  @syntax[]{(gtk-file-chooser-preview-widget object) => preview-widget}
  @syntax[]{(setf (gtk-file-chooser-preview-widget object) preview-widget)}
  @argument[object]{a @class{gtk-file-chooser} widget}
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
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-preview-filename}
  @see-function{gtk-file-chooser-preview-uri}
  @see-function{gtk-file-chooser-preview-widget-active}")

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
 "@version{*2021-2-4}
  @syntax[]{(gtk-file-chooser-preview-widget-active object) => active}
  @syntax[]{(setf (gtk-file-chooser-preview-widget-active object) active)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[active]{a boolean whether to display the user-specified preview
    widget}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{preview-widget-active} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-preview-widget-active} gets
  whether the preview widget should be shown for the current filename. The
  slot access function @sym{(setf gtk-file-chooser-preview-widget-active)} sets
  whether the preview widget should be shown.

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
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-select-multiple object) => select-multiple}
  @syntax[]{(setf (gtk-file-chooser-select-multiple object) select-multiple)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[select-multiple]{@em{true} if multiple files can be selected}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{select-multiple} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-select-multiple} gets whether
  multiple files can be selected in the file selector. The slot access function
  @sym{(setf gtk-file-chooser-select-multiple)} sets whether multiple files can
  be selected.

  This is only relevant if the action of the file chooser is set to be
  @code{:open} or @code{:select-folder}.
  @see-class{gtk-file-chooser}
  @see-symbol{gtk-file-chooser-action}")

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
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-show-hidden object) => show-hidden}
  @syntax[]{(setf (gtk-file-chooser-show-hidden object) show-hidden)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[show-hidden]{@em{true} if hidden files and folders should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{show-hidden} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-show-hidden} gets whether
  hidden files and folders are displayed in the file selector. The slot access
  function @sym{(setf gtk-file-chooser-show-hidden)} sets whether hidden files
  and folders are displayed.
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
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-use-preview-label object) => use-label}
  @syntax[]{(setf (gtk-file-chooser-use-preview-label object) use-label)}
  @argument[object]{a @class{gtk-file-chooser} widget}
  @argument[use-label]{a boolean whether to display a stock label with the name
    of the previewed file}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser]{use-preview-label} slot of the
    @class{gtk-file-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-file-chooser-use-preview-label} gets whether
  a stock label should be drawn with the name of the previewed file. The slot
  access function @sym{(setf gtk-file-chooser-use-preview-label)} sets whether
  the file chooser should display a stock label. The default is @em{true}.

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
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-current-name chooser) => name}
  @syntax[]{(setf (gtk-file-chooser-current-name chooser) name)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[name]{a string with the filename to use, as a UTF-8 string}
  @begin{short}
    Accessor of the current name of a file chooser widget.
  @end{short}

  The function @sym{gtk-file-chooser-current-name} gets the current name in the
  file selector, as entered by the user in the text entry for \"Name\". The slot
  access functions @sym{(gtk-file-chooser-current-name)} sets the current name.

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
 "@version{*2021-2-4}
  @syntax[]{(gtk-file-chooser-filename chooser) => filename}
  @syntax[]{(setf (gtk-file-chooser-filename chooser) filename)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[filename]{a string with the filename to set as current}
  @begin{short}
    Accessor of the filename of a file chooser widget.
  @end{short}

  The function @sym{gtk-file-chooser-filename} gets the filename for the
  currently selected file in the file selector. If multiple files are selected,
  one of the filenames will be returned at random. If the file chooser is in
  folder mode, this function returns the selected folder.

  The function @sym{(gtk-file-chooser-filename)} sets @arg{filename} as the
  current filename for the file chooser, by changing to the file's parent
  folder and actually selecting the file in the list. All other files will be
  unselected. If the chooser is in @code{:save} mode, the file's base name will
  also appear in the dialog's file name entry. Note that the file must exist,
  or nothing will be done except for the directory change.

  You should use this function only when implementing a \"File/Save As...\"
  dialog for which you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does
  \"File/Save As...\" on it to save a copy or a modified version. If you do not
  have a file name already - for example, if the user just created a new file
  and is saving it for the first time, do not call this function. Instead, use
  something similar to this:
  @begin{pre}
(if (document-is-new)
    ;; the user just created a new document
    (setf (gtk-file-chooser-current-name chooser) \"Untitled document\")
    ;; the user edited an existing document
    (setf (gtk-file-chooser-filename chooser) existing-filename))
  @end{pre}
  In the first case, the file chooser will present the user with useful
  suggestions as to where to save his new file. In the second case, the file's
  existing location is already known, so the file chooser will use it.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-current-name}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_select_filename" gtk-file-chooser-select-filename)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[filename]{a string with the filename to select}
  @begin{short}
    Selects a filename in the file chooser.
  @end{short}
  If the filename is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  @arg{filename}.
  @see-class{gtk-file-chooser}
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
 "@version{2021-1-28}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
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
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{short}
    Selects all files in the current folder of the file chooser.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-unselect-all}
  @see-function{gtk-file-chooser-select-filename}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_unselect_all" gtk-file-chooser-unselect-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{short}
    Unselects all files in the current folder of the file chooser.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-select-all}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filenames () -> gtk-file-chooser-filenames
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_filenames" gtk-file-chooser-filenames)
    (g-slist :string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-28}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A list containing the filenames of all selected files and subfolders
    in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser.
  @end{short}
  The returned names are full absolute paths. If the files in the current
  folder cannot be represented as local filenames they will be ignored. See
  the function @fun{gtk-file-chooser-uris}.
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
                   :string (if filename filename (null-pointer))
                   :boolean)
  filename)

(defcfun ("gtk_file_chooser_get_current_folder" gtk-file-chooser-current-folder)
    :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-current-folder chooser) => filename}
  @syntax[]{(setf (gtk-file-chooser-current-folder chooser) filename)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[filename]{a string with the full path of the new current folder}
  @begin{short}
    Accessor of the current folder of the file chooser.
  @end{short}

  The function @sym{gtk-file-chooser-current-folder} gets the current folder
  of the file chooser as a local filename. The function
  @sym{(setf gtk-file-chooser-current-folder)} sets the current folder.

  The user will be shown the full contents of the current folder, plus user
  interface elements for navigating to other folders.

  Note that this is the folder that the file chooser is currently displaying,
  e.g. \"/home/username/Documents\", which is not the same as the
  currently-selected folder if the chooser is in @code{:select-folder} mode,
  e.g. \"/home/username/Documents/selected-folder/\". To get the
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
;;; gtk_file_chooser_set_uri () -> gtk-file-chooser-uri
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-uri) (uri chooser)
  (foreign-funcall "gtk_file_chooser_set_uri"
                   (g-object gtk-file-chooser) chooser
                   :string uri
                   :boolean)
  uri)

(defcfun ("gtk_file_chooser_get_uri" gtk-file-chooser-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-uri chooser) => uri}
  @syntax[]{(setf (gtk-file-chooser-uri chooser) uri)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[uri]{a string with the URI to set as current}
  @begin{short}
    Accessor of the currently selected URI.
  @end{short}

  The function @sym{gtk-file-chooser-uri} gets the URI for the currently
  selected file in the file selector. If multiple files are selected, one of
  the filenames will be returned at random. If the file chooser is in folder
  mode, this function returns the selected folder.

  The function @sym{(setf gtk-file-chooser-uri)} sets the file referred to by
  @arg{uri} as the current file for the file chooser, by changing to the URI's
  parent folder and actually selecting the URI in the list. If the chooser is
  in @code{:save} mode, the URI's base name will also appear in the dialog's
  file name entry.

  Note that the URI must exist, or nothing will be done except for the
  directory change.

  You should use this function only when implementing a File/Save As... dialog
  for which you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does File/Save As...
  on it to save a copy or a modified version. If you do not have a file name
  already - for example, if the user just created a new file and is saving it
  for the first time, do not call this function. Instead, use something
  similar to this:
  @begin{pre}
(if (document-is-new)
    ;; the user just created a new document
    (setf (gtk-file-chooser-current-name chooser) \"Untitled document\")
    ;; the user edited an existing document
    (setf (gtk-file-chooser-uri chooser) existing-uri))
  @end{pre}
  In the first case, the file chooser will present the user with useful
  suggestions as to where to save his new file. In the second case, the file's
  existing location is already known, so the file chooser will use it.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-current-name}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_select_uri" gtk-file-chooser-select-uri) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[uri]{a string with the URI to select}
  @begin{short}
    Selects the file by @arg{uri}.
  @end{short}
  If the URI does not refer to a file in the current folder of the file chooser,
  then the current folder of the file chooser will be changed to the folder
  containing @arg{uri}.
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
 "@version{2021-1-28}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[uri]{a string with the URI to unselect}
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
;;; gtk_file_chooser_get_uris () -> gtk-file-chooser-uris
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_uris" gtk-file-chooser-uris)
    (g-slist :string)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A list containing strings with the URIs of all selected files and subfolders
    in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of the
    file chooser.
  @end{short}
  The returned names are full absolute URIs.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-filenames}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder_uri ()
;;; gtk_file_chooser_get_current_folder_uri ()
;;;     -> gtk-file-chooser-current-folder-uri
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-current-folder-uri) (uri chooser)
  (when (foreign-funcall "gtk_file_chooser_set_current_folder_uri"
                   (g-object gtk-file-chooser) chooser
                   :string uri
                   :boolean)
    uri))

(defcfun ("gtk_file_chooser_get_current_folder_uri"
           gtk-file-chooser-current-folder-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-5}
  @syntax[]{(gtk-file-chooser-current-folder-uri chooser) => uri}
  @syntax[]{(setf (gtk-file-chooser-current-folder-uri chooser) uri)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[uri]{a string with the URI for the current folder}
  @begin{short}
    Accessor of the URI for the current folder of the file chooser.
  @end{short}

  The function @sym{gtk-file-chooser-current-folder-uri} gets the current
  folder of the file chooser as an URI. This function will also return
  @code{nil} if the file chooser was unable to load the last folder that was
  requested from it. For example, as would be for calling this function on a
  nonexistent folder.

  Note that this is the folder that the file chooser is currently displaying,
  e.g. \"file:///home/username/Documents\", which is not the same as the
  currently-selected folder if the chooser is in @code{:select-folder}, e.g.
  \"file:///home/username/Documents/selected-folder/\". To get the
  currently-selected folder in that mode, use the function
  @fun{gtk-file-chooser-uri} as the usual way to get the selection.

  The function @sym{(setf gtk-file-chooser-current-folder-uri)} sets the
  current folder for the file chooser from an URI. The user will be shown the
  full contents of the current folder, plus user interface elements for
  navigating to other folders.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-current-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_filename ()
;;;     -> gtk-file-chooser-preview-filename
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_preview_filename"
           gtk-file-chooser-preview-filename) (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A string with the filename to preview, or @code{nil} if no file is
    selected, or if the selected file cannot be represented as a local filename.
  @end{return}
  @begin{short}
    Gets the filename that should be previewed in a custom preview widget.
  @end{short}
  See the function @fun{gtk-file-chooser-preview-widget}.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-preview-widget}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-preview-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_uri () -> gtk-file-chooser-preview-uri
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_preview_uri" gtk-file-chooser-preview-uri)
    :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A string with the URI for the file to preview, or @code{nil} if no file is
    selected.
  @end{return}
  @begin{short}
    Gets the URI that should be previewed in a custom preview widget.
  @end{short}
  See the function @fun{gtk-file-chooser-preview-widget}.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-preview-widget}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-preview-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_add_filter" gtk-file-chooser-add-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Adds a filter to the list of filters that the user can select between.
  @end{short}
  When a filter is selected, only files that are passed by that filter are
  displayed.
  @see-class{gtk-file-chooser}
  @see-class{gtk-file-filter}
  @see-function{gtk-file-chooser-remove-filter}"
  (chooser (g-object gtk-file-chooser))
  (filter (g-object gtk-file-filter)))

(export 'gtk-file-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_remove_filter" gtk-file-chooser-remove-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Removes a filter from the list of filters that the user can select between.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-class{gtk-file-filter}
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
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A list containing the current set of user selectable @class{gtk-file-filter}
    objects.
  @end{return}
  @begin{short}
    Lists the current set of user-selectable filters.
  @end{short}
  See the functions @fun{gtk-file-chooser-add-filter} and
  @fun{gtk-file-chooser-remove-filter}.
  @see-class{gtk-file-chooser}
  @see-class{gtk-file-filter}
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
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[folder]{a string with a filename of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @code{false} otherwise.
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
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[folder]{a string with the filename of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @code{false} otherwise.
  @end{return}
  @begin{short}
    Removes a folder from a file chooser's list of shortcut folders.
  @end{short}
  See also the function @fun{gtk-file-chooser-add-shortcut-folder}.
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
 "@version{2021-1-28}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A list of strings with the folder filenames, or @code{nil} if there are no
    shortcut folders.
  @end{return}
  @begin{short}
    Queries the list of shortcut folders in the file chooser, as set by the
    function @fun{gtk-file-chooser-add-shortcut-folder}.
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
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[uri]{a string with the URI of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @code{false} otherwise.
  @end{return}
  @begin{short}
    Adds a folder URI to be displayed with the shortcut folders in a file
    chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  \"file:///usr/share/mydrawprogram/Clipart\" folder to the volume list.
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-remove-shortcut-folder-uri}
  @see-function{gtk-file-chooser-add-shortcut-folder}"
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
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[uri]{a string with the URI of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @code{false} otherwise.
  @end{return}
  @begin{short}
    Removes a folder URI from a file chooser's list of shortcut folders.
  @end{short}
  See also the function @fun{gtk-file-chooser-add-shortcut-folder-uri}.
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
 "@version{2021-1-28}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A list of strings with the folder URIs, or @code{nil} if there are no
    shortcut folders.
  @end{return}
  @begin{short}
    Queries the list of shortcut folders in the file chooser, as set by the
    function @fun{gtk-file-chooser-add-shortcut-folder-uri}.
  @end{short}
  @see-class{gtk-file-chooser}
  @see-function{gtk-file-chooser-add-shortcut-folder-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-list-shortcut-folder-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder_file ()
;;; gtk_file_chooser_set_current_folder_file ()
;;;   -> gtk-file-chooser-current-folder-file
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-current-folder-file) (file chooser)
  (with-g-error (err)
    (when (foreign-funcall "gtk_file_chooser_set_current_folder_file"
                           (g-object gtk-file-chooser) chooser
                           (g-object g-file) file
                           :pointer err
                           :boolean)
      file)))

(defcfun ("gtk_file_chooser_get_current_folder_file"
           gtk-file-chooser-current-folder-file) (g-object g-file)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-current-folder-file chooser) => file}
  @syntax[]{(setf (gtk-file-chooser-current-folder-file chooser) file)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[file]{a @class{g-file} object for the folder}
  @begin{short}
    Accessor of the current folder file of the file chooser.
  @end{short}

  The function @sym{gtk-file-chooser-current-folder-file} gets the current
  folder of the file chooser as a @class{g-file} object. The function
  @sym{(setf gtk-file-chooser-current-folder-file)} sets the current folder.
  This function returns @code{nil}, if the folder could not be changed
  successfully.

  Internal function, see the function @fun{gtk-file-chooser-current-folder-uri}.
  @see-class{gtk-file-chooser}
  @see-class{g-file}
  @see-function{gtk-file-chooser-current-folder-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-current-folder-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_file ()
;;; gtk_file_chooser_set_file () -> gtk-file-chooser-file
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-chooser-file) (file chooser)
  (with-g-error (err)
    (foreign-funcall "gtk_file_chooser_set_file"
                     (g-object gtk-file-chooser) chooser
                     (g-object g-file) file
                     :pointer err
                     :boolean)
     file))

(defcfun ("gtk_file_chooser_get_file" gtk-file-chooser-file) (g-object g-file)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @syntax[]{(gtk-file-chooser-file chooser) => file}
  @syntax[]{(setf (gtk-file-chooser-file chooser) file)}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[file]{a @class{g-file} object for the file}
  @begin{short}
    Accessor of the file of the file chooser.
  @end{short}

  The function @sym{gtk-file-choose-file} gets the @class{g-file} object for
  the currently selected file in the file selector. If multiple files are
  selected, one of the files will be returned at random. If the file chooser
  is in folder mode, this function returns the selected folder.

  The function @sym{gtk-file-chooser-file} sets @arg{file} as the current
  filename for the file chooser, by changing to the file's parent folder and
  actually selecting the file in list. If the chooser is in @code{:save} mode,
  the file's base name will also appear in the dialog's file name entry.

  If the file name is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  @arg{file}. This is equivalent to a sequence of the function
  @fun{gtk-file-chooser-unselect-all} followed by the function
  @fun{gtk-file-chooser-select-filename}.

  Note that the file must exist, or nothing will be done except for the
  directory change.

  If you are implementing a File/Save As... dialog, you should use this
  function if you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does File/Save As...
  on it. If you do not have a file name already - for example, if the user just
  created a new file and is saving it for the first time, do not call this
  function. Instead, use something similar to this:
  @begin{pre}
(if document-is-new
    (progn
      ;; the user just created a new document
      (setf (gtk-file-chooser-current-folder-file chooser)
            default-file-for-saving)
      (setf (gtk-file-chooser-current-name chooser \"Untitled document\")))
      (progn
        ;; the user edited an existing document
        (setf (gtk-file-chooser-file chooser) existing-file)))
  @end{pre}
  @see-class{gtk-file-chooser}
  @see-class{g-file}
  @see-function{gtk-file-chooser-unselect-all}
  @see-function{gtk-file-chooser-select-filename}
  @see-function{gtk-file-chooser-current-folder-file}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_files () -> gtk-file-chooser-files
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_files" gtk-file-chooser-files)
    (g-slist (g-object g-file :free-from-foreign t) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    A list containing a @class{g-file} object for each selected file and
    subfolder in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser as a list of @class{g-file} objects.
  @end{short}
  This is an internal function, see the function @fun{gtk-file-chooser-uris}.
  @see-class{gtk-file-chooser}
  @see-class{g-file}
  @see-function{gtk-file-chooser-uris}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-files)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_file () -> gtk-file-chooser-preview-file
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_get_preview_file" gtk-file-chooser-preview-file)
    (g-object g-file)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-28}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @begin{return}
    The @class{g-file} object for the file to preview, or @code{nil} if no file
    is selected.
  @end{return}
  @begin{short}
    Gets the @class{g-file} object that should be previewed in a custom preview.
  @end{short}
  Internal function, see @fun{gtk-file-chooser-preview-uri}.
  @see-class{gtk-file-chooser}
  @see-class{g-file}
  @see-function{gtk-file-chooser-preview-uri}"
  (chooser (g-object gtk-file-chooser)))

(export 'gtk-file-chooser-preview-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_select_file" %gtk-file-chooser-select-file) :boolean
  (chooser (g-object gtk-file-chooser))
  (file (g-object g-file))
  (err :pointer))

(defun gtk-file-chooser-select-file (chooser file)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[file]{a @class{g-file} object to select}
  @begin{short}
    Selects the file referred to by @arg{file}.
  @end{short}
  An internal function. See the function @fun{gtk-file-chooser-select-uri}.
  @see-class{gtk-file-chooser}
  @see-class{g-file}
  @see-function{gtk-file-chooser-select-uri}"
  (with-g-error (err)
    (%gtk-file-chooser-select-file chooser file err)))

(export 'gtk-file-chooser-select-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_chooser_unselect_file" gtk-file-chooser-unselect-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-5}
  @argument[chooser]{a @class{gtk-file-chooser} widget}
  @argument[file]{a @class{g-file} object}
  @begin{short}
    Unselects the file referred to by @arg{file}.
  @end{short}
  If the file is not in the current directory, does not exist, or is otherwise
  not currently selected, does nothing.
  @see-class{gtk-file-chooser}
  @see-class{g-file}
  @see-function{gkt-file-chooser-select-file}"
  (chooser (g-object gtk-file-chooser))
  (file (g-object g-file)))

(export 'gtk-file-chooser-unselect-file)

;;; --- End of file gtk.file-chooser.lisp --------------------------------------
