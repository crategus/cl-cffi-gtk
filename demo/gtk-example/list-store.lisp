(in-package :gtk-example)

(defun create-and-fill-list-store ()
  (let ((list-data '("Name1" "Name2" "Name3" "Name4" "Name5"))
        ;; Create a new list store with three columns
        (list-store (make-instance 'gtk-list-store
                                   :column-types
                                   '("gint" "gchararray" "gboolean"))))
    ;; Fill in some data
    (loop for data in list-data
          for i from 0 do
          ;; Add a new row to the model
          (gtk-list-store-set list-store
                              (gtk-list-store-append list-store)
                              i
                              data
                              nil))
    ;; Modify a particular row
    (let ((path (gtk-tree-path-new-from-string "2")))
      (gtk-list-store-set-value list-store
                                (gtk-tree-model-iter list-store path)
                                2
                                t))
    ;; Return the new list store
    list-store))

(defun create-view-and-list-store ()
  (let* ((list-store (create-and-fill-list-store))
         (view (make-instance 'gtk-tree-view
                              :model list-store)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Number"
                                                             renderer
                                                             "text" 0)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Name"
                                                             renderer
                                                             "text" 1)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Bool"
                                                             renderer
                                                             "text" 2)))
      (gtk-tree-view-append-column view column))
    view))

(defun example-simple-list-store ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Simple List Store"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 200))
          (view (create-view-and-list-store)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

