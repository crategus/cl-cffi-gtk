(in-package :gtk-example)

(let ((col-setting 0) (col-type 1) (col-value 2) (col-desc 3))

  (defun create-and-fill-list-store (data)
    (flet ((mklist (obj)
             (if (listp obj)
                 obj
                 (list obj))))
    (let ((model (apply #'gtk-list-store-new (mklist (first data)))))
      (dolist (entry (rest data))
        (let ((iter (gtk-list-store-append model)))
          (apply #'gtk-list-store-set model iter (mklist entry))))
      model)))

  (defun create-and-fill-model-settings ()
    (let ((data nil)
          (settings (sort (g-object-class-list-properties "GtkSettings")
                          #'string< :key #'g-param-spec-name)))
      (setf settings (rest settings)) ; Remove the first entry, it is color-hash
      (setf data
            (loop for setting in settings
                  for value = (parse-g-value
                                  (g-param-spec-default-value setting))
                  collect (list (g-param-spec-name setting)
                                (g-type-name (g-param-spec-value-type setting))
                                (format nil "~a" value)
                                (g-param-spec-nick setting))))
      (push (list "gchararray" "gchararray" "gchararray" "gchararray") data)
      (create-and-fill-list-store data)))

  (defun create-view-and-model-settings ()
    (let* ((model (create-and-fill-model-settings))
           (view (gtk-tree-view-new-with-model model)))
      ;; Create renderer for Setting column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Setting"
                                                               renderer
                                                               "text"
                                                                col-setting)))
        (gtk-tree-view-append-column view column))
      ;; Create renderer for Type column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Type"
                                                               renderer
                                                               "text"
                                                               col-type)))
        (gtk-tree-view-append-column view column))
      ;; Create renderer for Value column
      (let* ((renderer (make-instance 'gtk-cell-renderer-text
                                      :ellipsize :end))
             (column (gtk-tree-view-column-new-with-attributes "Value"
                                                               renderer
                                                               "text"
                                                               col-value)))
        (setf (gtk-tree-view-column-resizable column) t)
        (gtk-tree-view-append-column view column))
      ;; Create renderer for Description column
      (let* ((renderer (make-instance 'gtk-cell-renderer-text
                                      :ellipsize :end))
             (column (gtk-tree-view-column-new-with-attributes "Description"
                                                               renderer
                                                               "text"
                                                               col-desc)))
        (setf (gtk-tree-view-column-resizable column) t)
        (gtk-tree-view-append-column view column))
      view))

  (defun example-query-settings ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Query Settings"
                                   :type :toplevel
                                   :default-width 600
                                   :default-height 420))
            (scrolled (make-instance 'gtk-scrolled-window
                                     :margin-start 6
                                     :margin-end 6
                                     :margin-bottom 6))
            (view (create-view-and-model-settings)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Pack and show the widgets
        (gtk-container-add scrolled view)
        (gtk-container-add window scrolled)
        (gtk-widget-show-all window)))))

;;; 2021-4-21
