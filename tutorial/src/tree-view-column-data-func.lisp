;;;; Tree View Column Data Func

(in-package :gtk-tutorial)

(defun age-cell-data-func (col renderer model iter)
  (declare (ignore col))
  (let* ((value (gtk-tree-model-value model iter 1))
         (text (format nil "~,2f" value)))
    (setf (gtk-cell-renderer-text-text renderer) text)))

(defun create-and-fill-model-4 ()
  (let ((model (make-instance 'gtk-list-store
                              :column-types '("gchararray" "gfloat"))))
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Klaus-Dieter Mustermann" 51)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Ulrike Langhals" 23)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Marius Kalinowski" 42)
    model))

(defun create-view-and-model-4 ()
  (let* ((model (create-and-fill-model))
         (view (make-instance 'gtk-tree-view
                              :model model)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Name"
                                                             renderer
                                                             "text" 0)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Age"
                                                             renderer
                                                             "text" 1)))
      (gtk-tree-view-append-column view column)
      (gtk-tree-view-column-set-cell-data-func column
                                               renderer
                                               #'age-cell-data-func))
    view))

(defun example-tree-view-column-data-func ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Simple Tree View"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 200))
          (view (create-view-and-model-4)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))
