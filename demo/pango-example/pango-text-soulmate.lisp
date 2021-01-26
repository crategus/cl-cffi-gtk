(in-package :pango-example)

(defparameter text
"Most relationships seem so transitory
They're all good but not the permanent one
Who doesn't long for someone to hold
Who knows how to love you without being told
Somebody tell me why I'm on my own
If there's a soulmate for everyone")

(defun drawing-text-soulmate (widget cr)
  (let* ((width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (cr (pointer cr))
         (layout (pango-cairo-create-layout cr)))
    ;; Set the color.
    (cairo-set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo-select-font-face cr "Purisa" :weight :bold)
    ;; Specify the font size
    (cairo-set-font-size cr 13)
    ;; Set the font
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string "Purisa Normal 12"))
    ;; Set more line spacing
    (setf (pango-layout-line-spacing layout) 1.5)
    ;; Set the text
    (setf (pango-layout-text layout) text)
    ;; Extents of the layout in pixel Units
    (multiple-value-bind (text-width text-height)
        (pango-layout-pixel-size layout)
      ;; Move to the start position of the text
      (cairo-move-to cr (- (/ width 2) (/ text-width 2))
                        (- (/ height 2) (/ text-height 2)))
      ;; Print the text on the Cario context
      (pango-cairo-show-layout cr layout))
    (cairo-destroy cr)))

(defun example-text-soulmate ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Pango Text Soulmate"
                                 :default-width 450
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-text-soulmate)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2021-1-16
