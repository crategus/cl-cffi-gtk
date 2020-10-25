(defpackage :demo-pango
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :common-lisp)
  (:export #:demo-pango))

(in-package :demo-pango)

(defun demo-pango ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Using Pango with Cairo"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400))
          (circle 100)
          (n-words 12)
          (font "Sans Bold 16"))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let* ((cr (pointer cr))
                  ;; Get the GdkWindow for the widget
                  (window (gtk-widget-window widget))
                  (width (gdk-window-width window))
                  (height (gdk-window-height window))
                  (radius (- (/ (min width height) 2) 20)))
             ;; Set up a transformation matrix so that the user space
             ;; coordinates for where we are drawing are [-RADIUS, RADIUS],
             ;; [-RADIUS, RADIUS] We first center, then change the scale
             (cairo-translate cr
                              (+ radius (/ (- width (* 2 radius)) 2))
                              (+ radius (/ (- height (* 2 radius)) 2)))
             (cairo-scale cr (/ radius circle) (/ radius circle))

           ;; Clear surface
           (cairo-set-source-rgb cr 1 1 1)
           (cairo-paint cr)

           ;; Create a PangoLayout, set the font and text
           (let* ((screen (gdk-window-screen window))
                  (context (gdk-pango-context-for-screen screen))
                  (layout (pango-layout-new context))
                  (desc (pango-font-description-from-string font)))
             (setf (pango-layout-text layout) "Text")
             (setf (pango-layout-font-description layout) desc)

             ;; Draw the layout n-words times in a circle
             (do* ((i 0 (+ i 1))
                   (angle 0 (/ (* 360 i) n-words))
                   ;; Gradient from red to blue
                   (red (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)
                        (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)))
                  ((>= i n-words))
               (cairo-save cr)
               (cairo-set-source-rgb cr red 0 (- 1 red))
               (cairo-rotate cr (/ (* angle pi) 180))

               ;; Inform Pango to re-layout the text with the new
               ;; transformation matrix
               (pango-cairo-update-layout cr layout)

               (multiple-value-bind (width height)
                   (pango-layout-size layout)
                 (declare (ignore height))
                 (cairo-move-to cr (- (/ width 2 +pango-scale+)) (- circle)))
               (pango-cairo-show-layout cr layout)
               (cairo-restore cr)))
           (cairo-destroy cr)
           t)))
      (gtk-widget-show-all window))))

