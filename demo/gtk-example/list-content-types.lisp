;;;; List of content types - 2021-11-5

(in-package :gtk-example)

(defun create-and-fill-model-for-content-types ()
  (let ((types (sort (g-content-types-registered) #'string-lessp))
        (model (make-instance 'gtk-list-store
                              :column-types
                              '("GIcon"
                                "gchararray" "gchararray" "gchararray"))))
    (dolist (type types)
      (let ((icon (g-content-type-icon type))
            (name (g-content-type-generic-icon-name type))
            (desc (g-content-type-description type)))
        (gtk-list-store-set model
                            (gtk-list-store-append model)
                            icon
                            type
                            (if name name "")
                            (if desc desc ""))))
    model))

(defun create-view-and-model-for-content-types ()
  (let* ((model (create-and-fill-model-for-content-types))
         (view (make-instance 'gtk-tree-view
                              :model model)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk-cell-renderer-pixbuf-new))
           (column (gtk-tree-view-column-new-with-attributes "Icon"
                                                             renderer
                                                             "gicon" 0)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Content Type"
                                                             renderer
                                                             "text" 1)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Icon Name"
                                                             renderer
                                                             "text" 2)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Description"
                                                             renderer
                                                             "text" 3)))
      (gtk-tree-view-append-column view column))
    view))

(defun list-content-types ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "List Content Types"
                                 :type :toplevel
                                 :default-width 300
                                 :default-height 200))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always))
          (view (create-view-and-model-for-content-types)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-scrolled-window-add-with-viewport scrolled view)
      (gtk-container-add window scrolled)
      (gtk-widget-show-all window))))
