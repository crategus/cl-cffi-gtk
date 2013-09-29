;;;; A demo for GtkFrame
;;;;
;;;; This demo allows to change interactively the appearance of the frame.

(defun demo-pixbuf-scale ()
  (within-main-loop
    (let* (;; Create a toplevel window.
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo GtkFrame"
                                  :border-width 12))
           ;; A horizontal Box for the content of the window.
           (content (make-instance 'gtk-grid
                                   :orientation :horizontal
                                   :column-spacing 24))
           ;; A vertical Grid for the actions.
           (action (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :row-spacing 6))
           ;; A drawing area
           (area (make-instance 'gtk-drawing-area
                                :width-request 300
                                :height-request 300))
;           (pixbuf (gdk-pixbuf-new-from-file "background.jpg"))
)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g-signal-connect area "draw"
         (lambda (widget cr)
           (let ((width (gtk-widget-get-allocated-width widget))
                 (height (gtk-widget-get-allocated-height widget)))
             (cairo-arc (pointer cr)
                        (/ width 2.0d0)
                        (/ height 2.0d0)
                        (/ (min width height) 2.0d0)
                        0.0d0
                        (* 2.0d0 3.14))
             (gdk-cairo-set-source-rgba
               (pointer cr)
               (gtk-style-context-get-color
                 (gtk-widget-get-style-context widget) :normal))
             (cairo-fill (pointer cr))
             (cairo-paint (pointer cr))
             nil)))

      ;; A Quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit"
                                   :margin-top 12)))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-widget-destroy window)))
        (gtk-container-add action button))

      ;; Add frame, content, and action to the window.
      (gtk-container-add content area)
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))

