;;;; Paned Window Widgets
;;;; 
;;;; Creating a paned widget with minimum sizes.

(defun demo-paned-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Paned Window"
                                 :border-width 12))
          (paned (make-instance 'gtk-paned
                                :orientation :horizontal
                                :width-request 250
                                :height-request 150))
          (frame1 (make-instance 'gtk-frame :shadow-type :in
                                            :width-request 100))
          (frame2 (make-instance 'gtk-frame :shadow-type :in
                                            :width-request 50)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window paned)
      (gtk-paned-pack1 paned frame1 :resize t :shrink nil)
      (gtk-paned-pack2 paned frame2 :resize nil :shrink nil)
      (gtk-widget-show-all window))))

