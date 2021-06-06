;;;; Example Tree View Dump Model (2021-6-4)

(in-package :gtk-example)

(defun dump-model (model path iter)
  (let ((firstname (gtk-tree-model-value model iter 0))
        (lastname (gtk-tree-model-value model iter 1))
        (yearborn (gtk-tree-model-value model iter 2))
        (path-str (gtk-tree-path-to-string path)))
    (format t "Row ~A: ~A ~A, year ~A~%" path-str firstname lastname yearborn)))

(defun example-tree-view-dump-model ()
  (let ((model (create-and-fill-model-example)))
    ;; Traverse the model and dump it on the console
    (gtk-tree-model-foreach model #'dump-model)))
