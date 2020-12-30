(in-package :cairo-example)

(defun drawing-text-glyph (widget cr)
  (let* ((cr (pointer cr))
         (width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (font-size (min (truncate (- (/ height 20) 5))
                         (truncate (- (/ width 36) 5))))
         (glyphs nil)
         (extents nil))
    ;; Set the color.
    (cairo-set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo-select-font-face cr "Purisa" :normal :normal)
    ;; Specify the font size
    (cairo-set-font-size cr font-size)

    ;; Make a list of 20 * 35 glyphs
    (loop for y from 0 below 20
          do (loop for x from 0 below 35
                   do (push (list (+ x (* y 20))
                                  (+ font-size (* x (+ 5 font-size)))
                                  (+ font-size (* y (+ 5 font-size))))
                            glyphs)))
    (setf glyphs (reverse glyphs))

    ;; Print the glyps extents
    (setf extents (cairo-glyph-extents cr glyphs))
    (format t "~&bearing ~a ~a~%" (cairo-text-extents-x-bearing extents)
                                  (cairo-text-extents-y-bearing extents))
    (format t "~&size ~a ~a~%"    (cairo-text-extents-width extents)
                                  (cairo-text-extents-height extents))
    (format t "~&advance ~a ~a~%" (cairo-text-extents-x-advance extents)
                                  (cairo-text-extents-y-advance extents))
    ;; Show the list of glyphs
    (cairo-show-glyphs cr glyphs)))

(defun example-text-glyph ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Text Glyph"
                                 :default-width 800
                                 :default-height 480))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-text-glyph)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-29
