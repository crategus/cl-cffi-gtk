;;;; Cairo Drawing Line Caps

(in-package :cairo-example)

(defun drawing-caps (widget cr)
  (let* ((cr (pointer cr))
         (width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (offset (truncate (/ height 4)))
         (border (truncate (/ width 5)))
         (line-width (truncate (/ height 10))))
    ;; Draw in black ink.
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    ;; Set the line width
    (cairo-set-line-width cr line-width)
    ;; First line with butt caps
    (cairo-set-line-cap cr :butt)
    (cairo-move-to cr border offset)
    (cairo-line-to cr (- width border) offset)
    (cairo-stroke cr)

    ;; Second line with round caps.
    (cairo-set-line-cap cr :round)
    (cairo-move-to cr border (* 2 offset))
    (cairo-line-to cr (- width border) (* 2 offset))
    (cairo-stroke cr)

    ;; Third line with square caps.
    (cairo-set-line-cap cr :square)
    (cairo-move-to cr border (* 3 offset))
    (cairo-line-to cr (- width border) (* 3 offset))
    (cairo-stroke cr)

    ;; Helper lines to show the line length.
    (cairo-set-source-rgb cr 1.0 0.0 0.0)
    (cairo-set-line-width cr 1.0)
    ;; Line on the left side.
    (cairo-move-to cr border (- offset line-width))
    (cairo-line-to cr border (+ (* 3 offset) line-width))
    (cairo-stroke cr)
    ;; Two lines on the right side.
    (cairo-move-to cr (- width border) (- offset line-width))
    (cairo-line-to cr (- width border) (+ (* 3 offset) line-width))
    (cairo-stroke cr)

    (cairo-move-to cr (+ (- width border) (/ line-width 2))
                      (- offset line-width))
    (cairo-line-to cr (+ (- width border) (/ line-width 2))
                      (+ (* 3 offset) line-width))
    (cairo-stroke cr)))

(defun example-drawing-caps ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Drawing Caps"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-caps)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-22
