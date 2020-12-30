;;;; Cairo Drawing Line Dashes
;;;;
;;;; Each line can be drawn with a different pen dash. It defines the style of
;;;; the line. The dash is used by the function cairo-stroke. The dash pattern
;;;; is specified by the function cairo-set-dash. The pattern is set by the
;;;; dash list, which is a list of positive floating point values. They set the
;;;; on and off parts of the dash pattern. If the list is empty, the dashing is
;;;; disabled. If it is 1, a symmetric pattern is asumed with alternating on
;;;; and off portions of the size specified by the single value in dashes.

(in-package :cairo-example)

(defun drawing-dashes (widget cr)
  (let* ((cr (pointer cr))
         (width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (offset (truncate (/ height 4)))
         (border (truncate (/ width 10))))
    ;; Draw in black ink.
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    ;; Set the line width
    (cairo-set-line-width cr 2.5)
    ;; First line.
    (cairo-set-dash cr '(4.0 21.0 2.0) 0)
    (cairo-move-to cr border offset)
    (cairo-line-to cr (- width border) offset)
    (cairo-stroke cr)
    ;; Second line.
    (cairo-set-dash cr '(14.0 6.0) 1)
    (cairo-move-to cr border (* 2 offset))
    (cairo-line-to cr (- width border) (* 2 offset))
    (cairo-stroke cr)
    ;; Third line.
    (cairo-set-dash cr '(2.0) 0)
    (cairo-move-to cr border (* 3 offset))
    (cairo-line-to cr (- width border) (* 3 offset))
    (cairo-stroke cr)))

(defun example-drawing-dashes ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Drawing Dashes"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-dashes)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-22
