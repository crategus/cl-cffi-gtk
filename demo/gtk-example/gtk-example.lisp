(defpackage :gtk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :split-sequence :common-lisp)
  (:export #:create-page-setup-dialog
           #:create-print-dialog
           #:do-print-operation
           #:example-clipboard
           #:example-custom-window
           #:example-combo-box
           #:example-combo-box-text
           #:example-dialog
           #:example-dialog-new
           #:example-dialog-new-with-buttons
           #:example-dialog-ui
           #:example-drag-and-drop
           #:example-drag-and-drop-simple
           #:example-drawing-area
           #:example-file-chooser-custom-filter
           #:example-file-chooser-dialog
           #:example-file-chooser-preview
           #:example-file-chooser-widget
           #:example-grab
           #:example-grid-packing
           #:example-image-button-press
           #:example-image-menu-item
           #:example-info-bar
           #:example-level-bar
           #:example-menu-builder
           #:example-message-dialog-get-message-area
           #:example-message-dialog-new
           #:example-message-dialog-new-with-markup
           #:example-message-dialog-set-image
           #:example-message-dialog-set-markup
           #:example-message-dialog-ui
           #:example-pointer-device
           #:example-print-dialog
           #:example-print-operation
           #:example-print-run-page-setup-dialog
           #:example-print-run-page-setup-dialog-async
           #:example-radio-button
           #:example-scrolled-window
           #:example-show-about-dialog
           #:example-simple-list-store
           #:example-tool-palette
           #:example-widget-pointer
           #:simple-application
           ))

(in-package :gtk-example)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-example)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;;; 2021-3-26
