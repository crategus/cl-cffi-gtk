;;;; Cairo Drawing Line Caps

(in-package :cairo-example)

(defun drawing-joins (widget cr)
  (let* ((cr (pointer cr))
         (width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (offset (truncate (/ height 5)))
         (border (truncate (/ width 10)))
         (line-width (truncate (/ height 10))))
    ;; Set RGB color.
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    ;; Set the line width
    (cairo-set-line-width cr line-width)

    ;; First angle
    (cairo-set-line-join cr :miter)
    (cairo-move-to cr border (- height border))
    (cairo-line-to cr border border)
    (cairo-line-to cr (- width border) border)
    (cairo-stroke cr)

    ;; Second angle
    (cairo-set-line-join cr :bevel)
    (cairo-move-to cr (+ border offset) (- height border))
    (cairo-line-to cr (+ border offset) (+ border offset))
    (cairo-line-to cr (- width border) (+ border offset))
    (cairo-stroke cr)

    ;; Third angle
    (cairo-set-line-join cr :round)
    (cairo-move-to cr (+ border (* 2 offset)) (- height border))
    (cairo-line-to cr (+ border (* 2 offset)) (+ border (* 2 offset)))
    (cairo-line-to cr (- width border) (+ border (* 2 offset)))
    (cairo-stroke cr)

))


(defun example-drawing-joins ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Drawing Caps"
                                 :default-width 300
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-joins)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-22
