;;;; Example Simple Tree View

(in-package #:gtk-demo)

(defun simple-create-and-fill-model ()
  (let ((model (make-instance 'gtk-list-store
                              :column-types '("gchararray" "guint"))))
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Klaus-Dieter Mustermann" 51)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Ulrike Langhals" 23)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Marius Kalinowski" 91)
    model))

(defun simple-create-view-and-model ()
  (let* ((model (simple-create-and-fill-model))
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
      (gtk-tree-view-append-column view column))
    view))

(defun example-simple-tree-view ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Simple Tree View"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 200))
          (view (simple-create-view-and-model)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

