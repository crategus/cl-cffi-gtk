(defpackage :gtk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :split-sequence :common-lisp)
  (:export #:application-simple
           #:create-page-setup-dialog
           #:create-print-dialog
           #:do-print-operation
           #:example-box-packing                       ; Packing widgets
           #:example-box-simple                        ; Packing widgets
           #:example-button-box
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
           #:example-drawing-area-input                ; Getting started
           #:example-file-chooser-custom-filter
           #:example-file-chooser-dialog
           #:example-file-chooser-preview
           #:example-file-chooser-widget
           #:example-getting-started                   ; Getting started
           #:example-grab
           #:example-grid-packing
           #:example-grid-simple                       ; Packing widgets
           #:example-grid-spacing                      ; Packing widgets
           #:example-hello-world                       ; Getting started
           #:example-hello-world-upgraded              ; Getting started
           #:example-hello-world-upgraded-2            ; Getting started
           #:example-image-button-press
           #:example-image-menu-item
           #:example-info-bar
           #:example-layout
           #:example-level-bar
           #:example-list-box
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
           #:example-progress-bar
           #:example-query-settings
           #:example-radio-button
           #:example-revealer
           #:example-revealer-icon
           #:example-scale-button
           #:example-scrolled-window
           #:example-show-about-dialog
           #:example-simple-list-store
           #:example-tool-palette
           #:example-widget-pointer
           #:example-window-simple                     ; Getting started
           ))

(in-package :gtk-example)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-example)))
    (princ-to-string (merge-pathnames filename system-path))))

;; Rename rel-path to sys-path
(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-example)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;;; 2021-5-14
