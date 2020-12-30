(in-package :cairo-example)

(defun drawing-text-soulmate (widget cr)
  (declare (ignore widget))
  (let ((cr (pointer cr)))
    ;; Set the color.
    (cairo-set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo-select-font-face cr "Purisa" :normal :bold)
    ;; Specify the font size
    (cairo-set-font-size cr 13)
    ;; Display text on the drawing area
    (cairo-move-to cr 20 30)
    (cairo-show-text cr "Most relationships seem so transitory")
    (cairo-move-to cr 20 60)
    (cairo-show-text cr "They're all good but not the permanent one")

    (cairo-move-to cr 20 120)
    (cairo-show-text cr "Who doesn't long for someone to hold")

    (cairo-move-to cr 20 150)
    (cairo-show-text cr "Who knows how to love you without being told")
    (cairo-move-to cr 20 180)
    (cairo-show-text cr "Somebody tell me why I'm on my own")
    (cairo-move-to cr 20 210)
    (cairo-show-text cr "If there's a soulmate for everyone")))

(defun example-text-soulmate ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Text Soulmate"
                                 :default-width 400
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

;;; 2020-12-28
