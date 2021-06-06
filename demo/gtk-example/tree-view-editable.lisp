;;;; Example Tree View Editable (2021-6-4)

(in-package :gtk-example)

(defvar *shoppinglist*
        '(("gboolean" "guint" "gchararray" "gchararray")
          (nil 500 "ml"    "Milk")
          (nil 300 "g"     "Sugar")
          (nil  10 "Piece" "Apples")
          (nil   2 "Glass" "Honey")
          (nil 500 "g"     "Oatmeal")
          (nil   2 "Piece" "Lamb's Lettuce")
          (nil  12 "Piece" "Tomatoes")
          (nil   1 "Glass" "Cucumbers")))

(let ((col-done 0) (col-quantity 1) (col-unit 2) (col-product 3))

  (defun mklist (obj)
    (if (listp obj)
        obj
        (list obj)))

  (defun create-and-fill-list-store (data)
    (let ((model (apply #'gtk-list-store-new (mklist (first data)))))
      (dolist (entry (rest data))
        (let ((iter (gtk-list-store-append model)))
          (apply #'gtk-list-store-set model iter (mklist entry))))
      model))

  (defun get-values-from-list-store (model column)
    (let ((values nil))
      (do ((iter (gtk-tree-model-iter-first model)
                 (gtk-tree-model-iter-next model iter)))
          ((not iter))
          (let ((value (gtk-tree-model-value model iter column)))
            (when (not (member value values :test #'string=))
              (push value values))))
      (sort values #'string-lessp)))

  (defun create-view-and-model-editable ()
    (let* ((model (create-and-fill-list-store *shoppinglist*))
           (view (gtk-tree-view-new-with-model model)))

      ;; Create renderer for Done column
      (let* ((renderer (gtk-cell-renderer-toggle-new))
             (column (gtk-tree-view-column-new-with-attributes "Done"
                                                               renderer
                                                               "active"
                                                               col-done)))
        (g-signal-connect renderer "toggled"
            (lambda (cell pathstr)
              (declare (ignore cell))
              (let* ((iter (gtk-tree-model-iter-from-string model pathstr))
                     (value (not (gtk-tree-model-value model iter col-done))))
                (gtk-list-store-set-value model iter col-done value))))
        (gtk-tree-view-append-column view column))

      ;; Create renderer for Quantity column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Quantity"
                                                               renderer
                                                               "text"
                                                                col-quantity)))
        (setf (gtk-cell-renderer-xalign renderer) 1.0)
        (setf (gtk-cell-renderer-text-editable renderer) t)

        (g-signal-connect renderer "edited"
            (lambda (renderer pathstr text)
              (declare (ignore renderer))
              (let ((value (parse-integer text :junk-allowed t))
                    (iter (gtk-tree-model-iter-from-string model pathstr)))
                (when (and value iter)
                  (gtk-list-store-set-value model
                                            iter
                                            col-quantity
                                            value)))))

        (gtk-tree-view-append-column view column))

      ;; Create renderer for Unit column
      (let* ((data '("gchararray" "mg" "g" "ml" "l" "Piece" "Glass"))
             (combo (create-and-fill-list-store data))
             (renderer (gtk-cell-renderer-combo-new))
             (column (gtk-tree-view-column-new-with-attributes "Unit"
                                                               renderer
                                                               "text"
                                                               col-unit)))

        (setf (gtk-cell-renderer-text-editable renderer) t)
        (setf (gtk-cell-renderer-combo-model renderer) combo)
        (setf (gtk-cell-renderer-combo-text-column renderer) 0)

        (g-signal-connect renderer "editing-started"
            (lambda (renderer editable path)
              (declare (ignore renderer editable path))
              (let ((data (get-values-from-list-store model col-unit)))
                (gtk-list-store-clear combo)
                (dolist (entry data)
                  (let ((iter (gtk-list-store-append combo)))
                    (apply #'gtk-list-store-set combo iter (mklist entry)))))))

        (g-signal-connect renderer "edited"
            (lambda (renderer pathstr text)
              (declare (ignore renderer))
              (let ((iter (gtk-tree-model-iter-from-string model pathstr)))

                (gtk-list-store-set-value model
                                          iter
                                          col-unit
                                          text))))

        (gtk-tree-view-append-column view column))

      ;; Create renderer for Product column
      (let* ((data '("gchararray" "Apples" "Bread" "Cucumbers"))
             (completion (create-and-fill-list-store data))
             (entry (gtk-entry-completion-new))
             (renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Product"
                                                               renderer
                                                               "text"
                                                               col-product)))

        (setf (gtk-entry-completion-model entry) completion)
        (setf (gtk-cell-renderer-text-editable renderer) t)

        (g-signal-connect renderer "editing-started"
            (lambda (renderer editable path)
              (declare (ignore renderer path))
              (setf (gtk-entry-completion editable) entry)
              (setf (gtk-entry-completion-text-column entry) 0)
              (setf (gtk-entry-completion-popup-completion entry) nil)
              (setf (gtk-entry-completion-inline-completion entry) t)))

        (g-signal-connect renderer "edited"
            (lambda (renderer pathstr text)
              (declare (ignore renderer))
              (let ((iter (gtk-tree-model-iter-from-string model pathstr)))

                (gtk-list-store-set-value model
                                          iter
                                          col-product
                                          text))))

        (gtk-tree-view-append-column view column))
      view))

  (defun example-tree-view-editable ()
    (within-main-loop
      (let* ((window (make-instance 'gtk-window
                                    :title "Example Tree View Editable"
                                    :type :toplevel
                                    :default-width 400
                                    :default-height 300))
             (vbox (make-instance 'gtk-box :orientation :vertical))
             (toolbar (make-instance 'gtk-toolbar))
             (add-item (make-instance 'gtk-tool-button
                                      :icon-name "list-add"))
             (remove-item (make-instance 'gtk-tool-button
                                         :icon-name "list-remove"))
             (scrolled (make-instance 'gtk-scrolled-window))
             (view (create-view-and-model-editable)))

        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))

        ;; Setup the selection
        (let ((selection (gtk-tree-view-selection view)))
          (setf (gtk-tree-selection-mode selection) :single)
          (g-signal-connect selection "changed"
              (lambda (selection)
                (let* ((view (gtk-tree-selection-tree-view selection))
                       (model (gtk-tree-view-model view))
                       (iter (gtk-tree-selection-selected selection)))
                  (when iter
                    (format t "Selected Product is ~a~%"
                            (gtk-tree-model-value model iter col-product)))))))

        ;; Add item to the shopping list
        (g-signal-connect add-item "clicked"
            (lambda (button)
              (declare (ignore button))
              (let* ((selection (gtk-tree-view-selection view))
                     (model (gtk-tree-view-model view))
                     (iter (gtk-list-store-append model)))
                (gtk-list-store-set model iter nil 0 "" "")
                (gtk-tree-selection-select-iter selection iter))))

        ;; Remove item from the shopping list
        (g-signal-connect remove-item "clicked"
            (lambda (button)
              (declare (ignore button))
              (let* ((selection (gtk-tree-view-selection view))
                     (model (gtk-tree-view-model view))
                     (iter (gtk-tree-selection-selected selection)))
              (when iter
                (gtk-list-store-remove model iter)))))

        ;; Pack and show the widgets
        (gtk-toolbar-insert toolbar add-item -1)
        (gtk-toolbar-insert toolbar remove-item -1)
        (gtk-box-pack-start vbox toolbar :expand nil)
        (gtk-container-add scrolled view)
        (gtk-box-pack-start vbox scrolled)
        (gtk-container-add window vbox)
        (gtk-widget-show-all window)))))
