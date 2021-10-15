(def-suite gtk-file-chooser :in gtk-suite)
(in-suite gtk-file-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooser
;;;     GtkFileChooserAction
;;;     GtkFileChooserConfirmation
;;;     GTK_FILE_CHOOSER_ERROR
;;;     GtkFileChooserError

;;; --- Properties -------------------------------------------------------------

(test gtk-file-chooser-properties
  (let ((chooser (make-instance 'gtk-file-chooser-widget)))
    (is (eq :open (gtk-file-chooser-action chooser)))
    (is-true (gtk-file-chooser-create-folders chooser))
    (is-false (gtk-file-chooser-do-overwrite-confirmation chooser))
    (is-false (gtk-file-chooser-extra-widget chooser))
    (is-false (gtk-file-chooser-filter chooser))
    (is-true (gtk-file-chooser-local-only chooser))
    (is-false (gtk-file-chooser-preview-widget chooser))
    (is-true (gtk-file-chooser-preview-widget-active chooser))
    (is-false (gtk-file-chooser-show-hidden chooser))
    (is-true (gtk-file-chooser-use-preview-label chooser))))

;;; --- Signals ----------------------------------------------------------------

;;; GtkFileChooserConfirmation    confirm-overwrite            Run Last
;;;                       void    current-folder-changed       Run Last
;;;                       void    file-activated               Run Last
;;;                       void    selection-changed            Run Last
;;;                       void    update-preview               Run Last

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name

(test gtk-file-chooser-current-name
  (let ((chooser (make-instance 'gtk-file-chooser-widget
                                :action :create-folder)))
    (is (string= "" (gtk-file-chooser-current-name chooser)))
    (is (string= "Untitled"
                 (setf (gtk-file-chooser-current-name chooser) "Untitled")))
    (is (string= "Untitled" (gtk-file-chooser-current-name chooser)))))

;;;     gtk_file_chooser_get_filename
;;;     gtk_file_chooser_set_filename

(test gtk-file-chooser-filename
  (let ((filename "/home/dieter/Lisp/lisp-projects/cl-gtk/test/ducky.png")
        (chooser (make-instance 'gtk-file-chooser-widget
                                :action :save)))

    (is-false (gtk-file-chooser-filename chooser))
    (is (string= filename
                 (setf (gtk-file-chooser-filename chooser) filename)))
    (is-false (gtk-file-chooser-filename chooser))

))

;;;     gtk_file_chooser_select_filename
;;;     gtk_file_chooser_unselect_filename
;;;     gtk_file_chooser_select_all
;;;     gtk_file_chooser_unselect_all
;;;     gtk_file_chooser_get_filenames

;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder

(test gtk-file-chooser-current-folder
  (let ((filename "/home/dieter/Lisp/lisp-projects/cl-gtk/test")
        (chooser (make-instance 'gtk-file-chooser-widget
                                :action :save)))

    (is-false (gtk-file-chooser-current-folder chooser))
    (is (string= filename
                 (setf (gtk-file-chooser-current-folder chooser) filename)))
    (is-false (gtk-file-chooser-current-folder chooser))

))

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

(test gtk-file-chooser-shortcut-folder
  (let ((chooser (make-instance 'gtk-file-chooser-widget)))
    (is (equal '() (gtk-file-chooser-list-shortcut-folders chooser)))
    (is-true (gtk-file-chooser-add-shortcut-folder chooser "unknown"))
    (is (every #'stringp
               (gtk-file-chooser-list-shortcut-folders chooser)))
    (is (= 1 (length (gtk-file-chooser-list-shortcut-folders chooser))))
    (is-true (gtk-file-chooser-remove-shortcut-folder chooser "unknown"))
    (is (equal '() (gtk-file-chooser-list-shortcut-folders chooser)))))

;;;     gtk_file_chooser_add_shortcut_folder_uri
;;;     gtk_file_chooser_remove_shortcut_folder_uri
;;;     gtk_file_chooser_list_shortcut_folder_uris

(test gtk-file-chooser-shortcut-folder-uri
  (let ((chooser (make-instance 'gtk-file-chooser-widget)))

    (is (equal '() (gtk-file-chooser-list-shortcut-folder-uris chooser)))

    (is-true (gtk-file-chooser-add-shortcut-folder-uri chooser "unknown"))
    (is (equal '("unknown")
               (gtk-file-chooser-list-shortcut-folder-uris chooser)))

    (is-true (gtk-file-chooser-remove-shortcut-folder-uri chooser "unknown"))
    (is (equal '() (gtk-file-chooser-list-shortcut-folder-uris chooser)))

))

;;;     gtk_file_chooser_get_current_folder_file
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_get_preview_file
;;;     gtk_file_chooser_select_file
;;;     gtk_file_chooser_set_current_folder_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_unselect_file

;;; 2021-10-14
