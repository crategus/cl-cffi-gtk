;;;; Scrolled Window (2021-3-19)

(in-package #:gtk-example)

(defun example-scrolled-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Scrolled Window"
                                 :width-request 350
                                 :height-request 300))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always))
          (image (gtk-image-new-from-file (sys-path "ducky.png"))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Pack and show the widgets
      (gtk-container-add scrolled image)
      (gtk-container-add window scrolled)
      (gtk-widget-show-all window))))
