;;;; Glade and GtkBuilder

(in-package #:gtk-demo)

(defun destroy (widget)
  (declare (ignore widget))
  (leave-gtk-main))

(defun pressed (widget)
  (let ((dialog (make-instance 'gtk-message-dialog
                               :transient-for (gtk-widget-parent widget)
                               :message-type :info
                               :buttons :ok
                               :text "Hello, World!")))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))

(defun example-builder ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-objects-from-file builder (rel-path "builder.ui") '("window"))
      (gtk-builder-connect-signals-auto builder #.(find-package '#:gtk-demo))
      (gtk-widget-show-all (gtk-builder-get-object builder "window")))))
