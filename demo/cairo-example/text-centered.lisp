(in-package :cairo-example)

(defun drawing-text-centered (widget cr)
  (let ((width (gtk-widget-allocated-width widget))
        (height (gtk-widget-allocated-height widget))
        (cr (pointer cr)))
    ;; Set the color.
    (cairo-set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo-select-font-face cr "Courier" :normal :bold)
    ;; Specify the font size
    (cairo-set-font-size cr 60)
    (let* ((extents (cairo-text-extents cr "Crategus"))
           (text-width (cairo-text-extents-width extents)))
      ;; Display text on the drawing area
      (cairo-move-to cr (- (/ width 2) (/ text-width 2)) (/ height 2))
      (cairo-show-text cr "Crategus"))))

(defun example-text-centered ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Text Centered"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-text-centered)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-28
