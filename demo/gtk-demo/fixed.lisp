;;;; Fixed Container
;;;;
;;;; In this example, three buttons are placed in the fixed container with the
;;;; function gtk-fixed-put. When pressed, the buttons are moved randomly with
;;;; the function gtk-fixed-move.
;;;;
;;;; To get the width and height of the fixed container and the buttons the
;;;; functions gtk-widget-allocated-width and gtk-widget-allocated-height are
;;;; used.

(in-package #:gtk-demo)

(defun move-button (fixed button)
  (let ((width (- (gtk-widget-allocated-width fixed)
                  (gtk-widget-allocated-width button)))
        (height (- (gtk-widget-allocated-height fixed)
                   (gtk-widget-allocated-height button))))
    (gtk-fixed-move fixed button (random width) (random height))))

(defun example-fixed ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Fixed Container"
                                 :default-width 350
                                 :default-height 200
                                 :border-width 12))
          (fixed (make-instance 'gtk-fixed)))
      (g-signal-connect window "destroy"
                        (lambda (window)
                          (declare (ignore window))
                          (leave-gtk-main)))
      (dotimes (i 3)
        (let ((button (gtk-button-new-with-label "Press me")))
          (g-signal-connect button "clicked"
                            (lambda (widget)
                              (move-button fixed widget)))
          (gtk-fixed-put fixed button (random 250) (random 180))))
      (gtk-container-add window fixed)
      (gtk-widget-show-all window))))

;;; 2021-2-9
