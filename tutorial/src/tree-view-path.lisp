;;;; Example Tree View Path

(in-package :gtk-tutorial)

(defun create-and-fill-model-1 ()
  (let ((model (gtk-tree-store-new "gchararray" "gchararray")))
    (let ((parent (gtk-tree-store-set model
                                      (gtk-tree-store-append model nil)
                                      "0" "Songs")))
      (gtk-tree-store-set model
                          (gtk-tree-store-append model parent)
                          "0:0" "MP3s")
      (gtk-tree-store-set model
                          (gtk-tree-store-append model parent)
                          "0:1" "Oggs"))
    (let ((parent (gtk-tree-store-set model
                                      (gtk-tree-store-append model nil)
                                      "1" "Videos")))
      (let ((parent1 (gtk-tree-store-set model
                                         (gtk-tree-store-append model parent)
                                         "1:0" "Clips")))
        (gtk-tree-store-set model
                            (gtk-tree-store-append model parent1)
                            "1:0:0" "Funny Clips")
        (gtk-tree-store-set model
                            (gtk-tree-store-append model parent1)
                            "1:0:1" "Movie Trailers"))
      (gtk-tree-store-set model
                          (gtk-tree-store-append model parent)
                          "1:1" "Movies"))
    model))

(defun create-view-and-model-1 ()
  (let* ((model (create-and-fill-model-1))
         (view (gtk-tree-view-new-with-model model)))
    ;; Create renderer for the cells
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Path"
                                                             renderer
                                                             "text" 0)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Entry"
                                                             renderer
                                                             "text" 1)))
      (gtk-tree-view-append-column view column))
    view))

(defun example-tree-view-path ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Tree View Path"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 250))
          (view (create-view-and-model-1)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))
