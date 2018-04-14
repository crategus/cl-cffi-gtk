;;;; Flow Boxes

(in-package #:gtk-demo)

(defun example-flow-box ()
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
          (flow-box1 (make-instance 'gtk-flow-box))
          (flow-box2 (make-instance 'gtk-flow-box))
          (store (make-instance 'g-list-store :item-type (g-type-from-name "GtkButton"))))
      (setf (gtk-window-transient-for window) *demo-window*)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-box-pack-start (gtk-dialog-get-content-area window) scrolled1)
      (gtk-scrolled-window-add-with-viewport scrolled1 flow-box1)

      (let ((labels (make-sequence 'vector 100)))
        (dotimes (i 10)
          (dotimes (j 10)
            (let ((label (format NIL "(~D, ~D)" i j))
                  (index (1- (- 100 (+ (* i 10) j)))))
              (setf (aref labels index) (make-instance 'gtk-button :label label)))))
        (g-list-store-splice store 0 0 labels (length labels)))
      (gtk-flow-box-bind-model flow-box1 store #'identity)

      (gtk-box-pack-end (gtk-dialog-get-content-area window) scrolled2)
      (gtk-scrolled-window-add-with-viewport scrolled2 flow-box2)

      (dotimes (i 10)
        (dotimes (j 10)
          (gtk-flow-box-insert flow-box2 (make-instance 'gtk-label :label (format nil "(~d, ~d)" i j)) (+ (* i 10) j))))

      (gtk-flow-box-set-filter-func flow-box2 (lambda (child) (search "0" (gtk-label-label (gtk-bin-get-child child)))))
      (gtk-flow-box-set-sort-func flow-box2 (lambda (child1 child2)
                                              (let ((%a (gtk-label-label (gtk-bin-get-child child1)))
                                                    (%b (gtk-label-label (gtk-bin-get-child child2))))
                                                (if (string< %a %b)
                                                    -1
                                                    (if (string= %a %b)
                                                        0
                                                        1)))))
      (gtk-flow-box-invalidate-sort flow-box2)
      (gtk-flow-box-invalidate-filter flow-box2)

      (g-signal-connect flow-box2 "child-activated"
                        (lambda (flow-box child)
                          (declare (ignore flow-box))
                          (format T "Button ~A activated!~%" (gtk-flow-box-child-get-index child))))

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
