;;;; Aspect Frames

(defun example-aspect-frame ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Aspect Frame"
                                 :default-width 300
                                 :default-height 250
                                 :border-width 12))
          (frame (make-instance 'gtk-aspect-frame
                                :label "Ratio 2 x 1"
                                :xalign 0.5
                                :yalign 0.5
                                :ratio 2
                                :obey-child nil))
          (area (make-instance 'gtk-drawing-area
                               :width-request 200
                               :hight-request 200)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window frame)
      (gtk-container-add frame area)
      (gtk-widget-show-all window))))

