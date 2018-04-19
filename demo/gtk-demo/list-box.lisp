;;;; List Boxes

(in-package #:gtk-demo)

(defun example-list-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-dialog
                                 :type :toplevel
                                 :title "Example Flow Box"
                                 :border-width 0
                                 :width-request 350
                                 :height-request 300))
          (scrolled1 (make-instance 'gtk-scrolled-window
                                    :border-width 12
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :always))
          (scrolled2 (make-instance 'gtk-scrolled-window
                                    :border-width 12
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :always))
          (list-box1 (make-instance 'gtk-list-box :selection-mode :multiple))
          (list-box2 (make-instance 'gtk-list-box :selection-mode :multiple))
          (store (make-instance 'g-list-store :item-type (g-type-from-name "GtkButton"))))
      (setf (gtk-window-transient-for window) *demo-window*)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-box-pack-start (gtk-dialog-get-content-area window) scrolled1)
      (gtk-scrolled-window-add-with-viewport scrolled1 list-box1)

      (let ((labels (make-sequence 'vector 10)))
        (dotimes (i 10)
          (let ((label (format NIL "~D" i))
                (index (1- (- 10 i))))
            (setf (aref labels index) (gtk-button-new-with-label label))))
        (g-list-store-splice store 0 0 labels (length labels)))
      (gtk-list-box-bind-model list-box1 store #'identity)

      (gtk-box-pack-end (gtk-dialog-get-content-area window) scrolled2)
      (gtk-scrolled-window-add-with-viewport scrolled2 list-box2)

      (dotimes (i 10)
        (gtk-list-box-insert list-box2 (gtk-label-new (format nil "~d" i)) i))

      (gtk-list-box-set-filter-func
       list-box2
       (lambda (child)
         (let ((label (gtk-label-label (gtk-bin-get-child child))))
           (eql (mod (parse-integer label) 2) 0))))
      (gtk-list-box-set-sort-func
       list-box2
       (lambda (child1 child2)
         (let ((%a (gtk-label-label (gtk-bin-get-child child1)))
               (%b (gtk-label-label (gtk-bin-get-child child2))))
           (if (string< %a %b)
               -1
               (if (string= %a %b)
                   0
                   1)))))
      (gtk-list-box-set-header-func
       list-box2
       (lambda (row before)
         (declare (ignore before))
         (let ((label (gtk-label-label (gtk-bin-get-child row))))
           (setf (gtk-list-box-row-header row)
                 (gtk-label-new (format NIL "This is the header for ~A!" label))))))

      (gtk-list-box-invalidate-sort list-box2)
      (gtk-list-box-invalidate-filter list-box2)
      (gtk-list-box-invalidate-headers list-box2)

      (let ((button (make-instance 'gtk-button
                                   :label "Close"
                                   :can-default t)))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start (gtk-dialog-get-action-area window) button)
        (gtk-widget-grab-default button))
      (gtk-widget-show-all window))))
