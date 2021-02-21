;;;; Tree View Dump Model

(in-package :gtk-tutorial)

(defun dump-model (model path iter)
  (let ((first-name (gtk-tree-model-value model iter 0))
        (last-name (gtk-tree-model-value model iter 1))
        (age (gtk-tree-model-value model iter 2))
        (tree-path (gtk-tree-path-to-string path)))
    (format t "Row ~A: ~A ~A, age ~A~%" tree-path first-name last-name age)))

(defun example-tree-view-dump-model ()
  (let ((model (gtk-list-store-new "gchararray" "gchararray" "guint")))
    ;; Fill the model with data
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Klaus-Dieter" "Mustermann" 51)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Ulrike" "Langhals" 23)
    (gtk-list-store-set model (gtk-list-store-append model)
                              "Marius" "Kalinowski" 42)
    ;; Now traverse the list
    (gtk-tree-model-foreach model #'dump-model)))
