;;;; File Chooser Custom Filter

(in-package #:gtk-demo)

(defun custom-file-filter (filter-info)
  ;; Select files with upcase characters in the display name
  (let ((display-name (gtk-file-filter-info-display-name filter-info)))
    (string= display-name
             (string-upcase display-name))))

(defun create-file-chooser-custom-filter ()
  (let ((filter-custom (gtk-file-filter-new))
        (filter-all (gtk-file-filter-new))
        (dialog (gtk-file-chooser-dialog-new "Example File Chooser Custom Filter"
                                             nil
                                             :open
                                             "gtk-save" :accept
                                             "gtk-cancel" :cancel)))
    ;; Add a custom file filter
    (setf (gtk-file-filter-name filter-custom) "Custom Filter")
    (gtk-file-filter-add-custom filter-custom
                                :display-name
                                #'custom-file-filter)
    (gtk-file-chooser-add-filter dialog filter-custom)
    ;; Add a second file filter
    (setf (gtk-file-filter-name filter-all) "All Files")
    (gtk-file-filter-add-pattern filter-all "*")
    (gtk-file-chooser-add-filter dialog filter-all)
    ;; Run the dialog
    (when (eq :accept (gtk-dialog-run dialog))
      (format t "Open the file ~A~%"
                (gtk-file-chooser-filename dialog)))
    (gtk-widget-destroy dialog)))

;;; 2021-2-11
