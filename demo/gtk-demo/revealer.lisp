;;;; Revealers

(in-package #:gtk-demo)

(defun example-revealer ()
  (within-main-loop
    (let ((window (make-instance 'gtk-dialog
                                 :type :toplevel
                                 :title "Example Revealer"
                                 :border-width 0
                                 :width-request 350
                                 :height-request 300))
          (revealer (make-instance 'gtk-revealer :transition-type :crossfade :transition-duration 1000)))
      (setf (gtk-window-transient-for window) *demo-window*)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-box-pack-start (gtk-dialog-get-content-area window) revealer)

      (gtk-container-add revealer (gtk-button-new-with-label "Hello, World!"))

      (let ((button (gtk-button-new-with-label "Reveal")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (setf (gtk-revealer-reveal-child revealer) (not (gtk-revealer-reveal-child revealer)))))
        (gtk-box-pack-start (gtk-dialog-get-action-area window) button))

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
