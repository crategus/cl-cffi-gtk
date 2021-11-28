;;;; Example Tree View Sorting (2021-6-4)

(in-package :gtk-example)

(let ((col-firstname 0) (col-lastname 1) (col-yearborn 2))

  (defun age-cell-data-sortable (column renderer model iter)
    (declare (ignore column))
    (let ((year (sixth (multiple-value-list (get-decoded-time))))
          (text nil)
          (value (gtk-tree-model-value model iter col-yearborn)))
      (if (and (< value year) (> value 0))
          (progn
            (setf text (format nil "~a  years old" (- year value)))
            (setf (gtk-cell-renderer-text-foreground-set renderer) nil))
          (progn
            (setf text "age unknown")
            (setf (gtk-cell-renderer-text-foreground renderer) "Red")
            (setf (gtk-cell-renderer-text-foreground-set renderer) t)))
      (setf (gtk-cell-renderer-text-text renderer) text)))

  (defun create-and-fill-model-sortable ()
    (let ((model (gtk-list-store-new "gchararray" "gchararray" "guint")))
      ;; Append a row and fill in some data
      (gtk-list-store-set model (gtk-list-store-append model)
                                "Hans" "Müller" 1961)
      ;; Append another row and fill in some data
      (gtk-list-store-set model (gtk-list-store-append model)
                                "Barbara" "Schmidt" 1998)
      ;; Append a third row
      (gtk-list-store-set model (gtk-list-store-append model)
                                "Peter" "Schneider" 1982)
      ;; Append a third row
      (gtk-list-store-set model (gtk-list-store-append model)
                                "Ursula" "Fischer" 2009)
      ;; Append a third row
      (gtk-list-store-set model (gtk-list-store-append model)
                                "Wolfgang" "Weber" 2002)

      (gtk-tree-sortable-set-sort-func model col-yearborn
          (lambda (sortable iter1 iter2)
            (- (gtk-tree-model-value sortable iter2 col-yearborn)
               (gtk-tree-model-value sortable iter1 col-yearborn))))

      (setf (gtk-tree-sortable-sort-column-id model)
            (list col-yearborn :descending))

      model))

  (defun create-view-and-model-sortable ()
    (let* ((model (create-and-fill-model-sortable))
           (view (gtk-tree-view-new-with-model model)))
      ;; Create renderer for first column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "First Name"
                                                               renderer
                                                               "text"
                                                                col-firstname)))
        (gtk-tree-view-append-column view column))

      ;; Create renderer for second column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Last Name"
                                                               renderer
                                                               "text"
                                                                col-lastname)))
        ;; Set weight property to bold
        (setf (gtk-cell-renderer-text-weight renderer) 700)
        (setf (gtk-cell-renderer-text-weight-set renderer) t)
        (gtk-tree-view-append-column view column))

      ;; Create renderer for third column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Age"
                                                               renderer
                                                               "text"
                                                               col-yearborn)))
        (gtk-tree-view-column-set-cell-data-func column
                                                 renderer
                                                 #'age-cell-data-sortable)

        (setf (gtk-tree-view-column-sort-column-id column) col-yearborn)
        (setf (gtk-tree-view-column-sort-indicator column) t)

        (gtk-tree-view-append-column view column))

      (setf (gtk-tree-selection-mode (gtk-tree-view-selection view)) :none)

      view))

  (defun example-tree-view-sortable ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Tree View Sortable"
                                   :type :toplevel
                                   :default-width 350
                                   :default-height 200))
            (toolbar (make-instance 'gtk-toolbar))
            (button (make-instance 'gtk-tool-button
                                   :label "Add year"))
            (vbox (make-instance 'gtk-box :orientation :vertical))
            (view (create-view-and-model-sortable)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Print the name when double click a row
        (g-signal-connect view "row-activated"
            (lambda (view path column)
              (declare (ignore column))
              (let* ((model (gtk-tree-view-model view))
                     ;; Lookup iter from path
                     (iter (gtk-tree-model-iter model path)))
                (when iter
                  (format t "Double click on name ~a~%"
                            (gtk-tree-model-value model iter col-lastname))))))
        ;; Add one year to the column year born
        (g-signal-connect button "clicked"
            (lambda (button)
              (declare (ignore button))
              (do* ((model (gtk-tree-view-model view))
                    (iter (gtk-tree-model-iter-first model)
                          (gtk-tree-model-iter-next model iter)))
                   ((not iter))
                   (let ((value (gtk-tree-model-value model iter col-yearborn)))
                     (gtk-list-store-set-value model iter
                                               col-yearborn
                                               (1+ value))))))
        ;; Pack and show the widgets
        (gtk-container-add toolbar button)
        (gtk-box-pack-start vbox toolbar :expand nil)
        (gtk-box-pack-start vbox view)
        (gtk-container-add window vbox)
        (gtk-widget-show-all window)))))
