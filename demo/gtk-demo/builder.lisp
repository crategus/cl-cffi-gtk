;;;; Glade and GtkBuilder

(in-package #:gtk-demo)

(defun destroy (widget)
  (declare (ignore widget))
  (leave-gtk-main))

(defun pressed (widget)
  (declare (ignore widget))
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type :info
                               :buttons :ok
                               :text "Hello, World!")))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))

(defun example-builder ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder :from-file (rel-path "builder.ui"))))
      (gtk-builder-connect-signals-auto builder #.(find-package '#:gtk-demo))
      (gtk-widget-show-all (gtk-builder-get-object builder "window")))))
