;;;; Simple Tree View

(in-package :gtk-tutorial)

(defvar *shoppinglist*
        '((500 "ml"    "Milch")
          (300 "g"     "Zucker")
          ( 10 "Stück" "Äpfel")
          (  2 "Glas"  "Honig")
          (  1 "Tüte"  "Haferflocken")
          (  2 "Stück" "Feldsalat")
          ( 12 "Stück" "Tomaten")
          (  1 "Glas"  "Gurken")))

(defun create-and-fill-model ()
  (let ((model (gtk-list-store-new "guint" "gchararray" "gchararray")))
    (dolist (entry *shoppinglist*)
      (gtk-list-store-set model (gtk-list-store-append model)
                                (first entry)
                                (second entry)
                                (third entry)))
    model))

(defun create-view-and-model ()
  (let* ((model (create-and-fill-model))
         (view (gtk-tree-view-new-with-model model)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Menge"
                                                             renderer
                                                             "text" 0)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Angabe"
                                                             renderer
                                                             "text" 1)))
      (gtk-tree-view-append-column view column))
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Produkt"
                                                             renderer
                                                             "text" 2)))
      (gtk-tree-view-append-column view column))
    view))

(defun example-tree-view-simple ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Simple Tree View"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 200))
          (view (create-view-and-model)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))
