;;;; Combo Box

(in-package :gtk-example)

(defparameter *icon-list*
              '(("gchararray" "gchararray")
                ("dialog-warning" "Warning")
                ("process-stop" "Stop")
                ("document-new" "New")
                ("edit-clear" "Clear")
                ("" "separator")
                ("document-open" "Open")))

(defun create-and-fill-list-store (data)
  (flet ((mklist (obj) (if (listp obj) obj (list obj))))
    (let ((model (apply #'gtk-list-store-new (mklist (first data)))))
      (dolist (entry (rest data))
        (let ((iter (gtk-list-store-append model)))
          (apply #'gtk-list-store-set model iter (mklist entry))))
      model)))

(defun set-sensitivity (layout cell model iter)
  (declare (ignore layout))
  (let* ((path (gtk-tree-model-path model iter))
         (pathstr (gtk-tree-path-to-string path)))
    (if (string= "1" pathstr)
        (setf (gtk-cell-renderer-sensitive cell) nil)
        (setf (gtk-cell-renderer-sensitive cell) t))))

(defun example-combo-box ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                 :border-width 12
                                 :title "Example Combo Box"))
           (vbox1 (make-instance 'gtk-box
                                 :orientation :vertical
                                 :spacing 6))
           (vbox2 (make-instance 'gtk-box
                                 :orientation :vertical
                                 :spacing 6))
           (hbox (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 24))
           (label (make-instance 'gtk-label :label "label"))
           (model (create-and-fill-list-store *icon-list*))
           (combo (make-instance 'gtk-combo-box
                                 :model model)))
      ;; Setup Cell Renderer for icon
      (let ((renderer (gtk-cell-renderer-pixbuf-new)))
        (setf (gtk-cell-renderer-xpad renderer) 6) ; More space between cells
        (gtk-cell-layout-pack-start combo renderer :expand nil)
        (gtk-cell-layout-set-attributes combo renderer "icon-name" 0)
        (gtk-cell-layout-set-cell-data-func combo renderer #'set-sensitivity))
      ;; Setup Cell Renderer for icon-name
      (let ((renderer (gtk-cell-renderer-text-new)))
        (gtk-cell-layout-pack-start combo renderer)
        (gtk-cell-layout-set-attributes combo renderer "text" 1)
        (gtk-cell-layout-set-cell-data-func combo renderer #'set-sensitivity))
      ;; Setup a Row Separator Function
      (gtk-combo-box-set-row-separator-func combo
          (lambda (object iter)
            (let ((value (gtk-tree-model-value object iter 1)))
              (string= value "separator"))))
      ;; Combo box selection has changed
      (g-signal-connect combo "changed"
          (lambda (object)
            (let ((value (gtk-combo-box-active-id object)))
              (gtk-label-set-markup label
                                    (format nil "<tt>~a</tt>" value)))))
      (setf (gtk-combo-box-id-column combo) 1)
      (setf (gtk-combo-box-active combo) 0)
      ;; Pack and show widgets
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0
                                         :label "<b>Select item</b>")
                          :expand nil)
      (gtk-box-pack-start vbox1 combo)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xaling 0
                                         :label "<b>Activated item</b>")
                          :expand nil)
      (gtk-box-pack-start vbox2 label)
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start hbox vbox2)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
