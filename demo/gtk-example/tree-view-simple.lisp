;;;; Example Tree View Simple (2021-6-4)

(in-package :gtk-example)

(let ((col-title 0) (col-author 1) (col-year 2))

  (defun create-and-fill-model-simple ()
    (let ((model (gtk-tree-store-new "gchararray" "gchararray" "guint")))
      ;; First Book
      (let ((iter (gtk-tree-store-append model nil))) ; Toplevel iterator
        ;; Set the toplevel row
        (gtk-tree-store-set model
                            iter
                            "The Art of Computer Programming"
                            "Donald E. Knuth"
                            2011)
        ;; Append and set child rows
        (gtk-tree-store-set model
                            (gtk-tree-store-append model iter) ; Child iterator
                            "Volume 1: Fundamental Algorithms"
                            ""
                            1997)
        (gtk-tree-store-set model
                            (gtk-tree-store-append model iter) ; Child iterator
                            "Volume 2: Seminumerical Algorithms"
                            ""
                            1998)
        (gtk-tree-store-set model
                            (gtk-tree-store-append model iter) ; Child iterator
                            "Volume 3: Sorting and Searching"
                            ""
                            1998))
      ;; Second Book
      (let ((iter (gtk-tree-store-append model nil))) ; Toplevel iterator
        (gtk-tree-store-set model
                            iter
                            "Let Over Lambda"
                            "Doug Hoyte"
                            2008))
      ;; Third Book
      (let ((iter (gtk-tree-store-append model nil))) ; Toplevel iterator
        (gtk-tree-store-set model
                            iter
                            "On Lisp"
                            "Paul Graham"
                            1993))
      model))

  (defun create-view-and-model-simple ()
    (let* ((model (create-and-fill-model-simple))
           (view (gtk-tree-view-new-with-model model)))
      ;; Create renderer for Title column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Title"
                                                               renderer
                                                               "text"
                                                                col-title)))
        (gtk-tree-view-append-column view column))
      ;; Create renderer for Author column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Author"
                                                               renderer
                                                               "text"
                                                               col-author)))
        (gtk-tree-view-append-column view column))
      ;; Create renderer for Year column
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Year"
                                                               renderer
                                                               "text"
                                                               col-year)))
        (gtk-tree-view-append-column view column))
      view))

  (defun example-tree-view-simple ()
    (within-main-loop
      (let* ((window (make-instance 'gtk-window
                                    :title "Example Simple Tree View"
                                    :type :toplevel
                                    :default-width 350
                                    :default-height 200))
             (view (create-view-and-model-simple)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Setup the selection handler
        (let ((selection (gtk-tree-view-selection view)))
          (setf (gtk-tree-selection-mode selection) :single)
          (g-signal-connect selection "changed"
             (lambda (object)
               (let* ((view (gtk-tree-selection-tree-view object))
                      (model (gtk-tree-view-model view))
                      (iter (gtk-tree-selection-selected object))
                      (title (gtk-tree-model-value model iter col-title)))
                 (format t "Selected title is ~a~%" title)))))
        ;; Pack and show the widgets
        (gtk-container-add window view)
        (gtk-widget-show-all window)))))
