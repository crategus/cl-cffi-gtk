;;;; Example Scale Button (2021-4-27)

(in-package :gtk-example)

(defun example-scale-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Scale Button"
                                 :border-width 12
                                 :width-request 360
                                 :height-request 240))
          (button (make-instance 'gtk-scale-button
                                 :margin-left 60
                                 :size 6
                                 :value 9.0
                                 :icons
                                 '("face-crying"     ; lowest value
                                   "face-smile-big"  ; highest value
                                   "face-sad"        ; other value between
                                   "face-worried"
                                   "face-uncertain"
                                   "face-plain"
                                   "face-smile")
                                 :adjustment
                                 (make-instance 'gtk-adjustment
                                                :lower 0.0
                                                :upper 10.0
                                                :step-increment 1.0
                                                :page-increment 2.0))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Pack and show the widgets
      (gtk-container-add window button)
      (gtk-widget-show-all window))))
