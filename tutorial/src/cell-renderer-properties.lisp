;;;; Cell Renderer Properties

(in-package :gtk-tutorial)

(defun create-and-fill-model-properties ()
  (let ((model (make-instance 'gtk-tree-store
                              :column-types '("gchararray" "gchararray"))))
    ;; Append a top level row and leave it empty
    (gtk-tree-store-append model nil)
    ;; Append a second top level row, and fill it with some data
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Joe" "Average")))
      ;; Append a child to the second top level row, and fill in some data
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Jane" "Average"))
    model))

(defun create-view-and-model-properties ()
  (let* ((model (create-and-fill-model-properties))
         (view (make-instance 'gtk-tree-view
                              :model model)))
  ;; Create the first column
  (let* ((column (make-instance 'gtk-tree-view-column
                                :title "First Name"))
         (renderer (make-instance 'gtk-cell-renderer-text
                                  :text "Booooo")))
    ;; pack tree view column into tree view
    (gtk-tree-view-append-column view column)
    ;; pack cell renderer into tree view column
    (gtk-tree-view-column-pack-start column renderer))

  ;; Create the second column
  (let* ((column (make-instance 'gtk-tree-view-column
                                :title "Last Name"))
         (renderer (make-instance 'gtk-cell-renderer-text
                                  :cell-background "Orange"
                                  :cell-background-set t)))
    ;; pack tree view column into tree view
    (gtk-tree-view-append-column view column)
    ;; pack cell renderer into tree view column
    (gtk-tree-view-column-pack-start column renderer))
  ;; No selection possible
  (setf (gtk-tree-selection-mode (gtk-tree-view-selection view)) :none)
  view))

(defun example-cell-renderer-properties ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Cell Renderer Properties"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 200))
          (view (create-view-and-model-properties)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))
