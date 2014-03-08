;;;; A demo for GtkGrid
;;;;
;;;; This demo allows to change interactively the appearance of a GtkGrid.

(defun demo-grid ()
  (within-main-loop
    (let* (;; Create a toplevel window.
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo GtkGrid"
                                  :border-width 12))
           ;; A horizontal Box for the content of the window.
           (content (make-instance 'gtk-grid
                                   :orientation :horizontal
                                   :column-spacing 24))
           ;; A vertical Grid for the actions.
           (action (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :row-spacing 6))
           ;; A GtkGrid.
           (grid (make-instance 'gtk-grid)))

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Put nine buttons into a 3 x 3 Grid.
      (dotimes (i 3)
        (dotimes (j 3)
          (let ((button (make-instance 'gtk-button
                                       :label (format nil "(~A ~A)" i j))))
            (gtk-grid-attach grid button i j 1 1))))

      ;; Set Grid spacing.
      (let ((hbox (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-homogenous t
                                  :column-spacing 6))
            (x-spin (make-instance 'gtk-spin-button
                                   :adjustment
                                   (make-instance 'gtk-adjustment
                                                  :value
                                                  (gtk-grid-column-spacing grid)
                                                  :lower 0.0
                                                  :upper 50.0
                                                  :step-increment 1.0
                                                  :page-increment 1.0
                                                  :page-size 0.0)
                                   :climb-rate 0
                                   :digits 0
                                   :wrap t))
            (y-spin (make-instance 'gtk-spin-button
                                   :adjustment
                                   (make-instance 'gtk-adjustment
                                                  :value
                                                  (gtk-grid-row-spacing grid)
                                                  :lower 0.0
                                                  :upper 50.0
                                                  :step-increment 1.0
                                                  :page-increment 1.0
                                                  :page-size 0.0)
                                   :climb-rate 0
                                   :digits 0
                                   :wrap t)))
        (g-signal-connect x-spin "value-changed"
           (lambda (spin)
             (setf (gtk-grid-column-spacing grid)
                   (gtk-spin-button-get-value-as-int spin))))
        (g-signal-connect y-spin "value-changed"
           (lambda (spin)
             (setf (gtk-grid-row-spacing grid)
                   (gtk-spin-button-get-value-as-int spin))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Set Grid Spacing</b>"))
        (gtk-container-add hbox x-spin)
        (gtk-container-add hbox y-spin)
        (gtk-container-add action hbox))

      ;; Insert Grid lines
      (let ((toggle (gtk-check-button-new-with-label "Grid Lines")))

        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Insert Grid Lines</b>"))
        (gtk-container-add action toggle))




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
      (gtk-container-add content grid)
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))

