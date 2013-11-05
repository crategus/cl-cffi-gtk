;;;; Size management
;;;;
;;;; 

(defun demo-size-management ()
  (within-main-loop
    (let* (;; Create a toplevel window.
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo GtkFrame"
                                  :border-width 12))
           ;; A horizontal Grid for the content of the window.
           (content (make-instance 'gtk-grid
                                   :orientation :horizontal
                                   :column-spacing 24))
           ;; A vertical Grid for the actions.
           (action (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :row-spacing 6))
           ;; A Frame as Container
           (frame (make-instance 'gtk-frame
                                 :width-request 200
                                 :height-request 200))
           ;; A Button to put into the Container
           (button (make-instance 'gtk-button
                                  :label "Button")))

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Put the button into the Frame
      (gtk-container-add frame button)

      ;; Information about the GtkRequisition of button
      (multiple-value-bind (minimum-size natural-size)
          (gtk-widget-get-preferred-size button)
        (gtk-container-add action (make-instance 'gtk-label
                                                 :label "GtkRequistion"))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "minmum width: ~A"
                                                  (gtk-requisition-width minimum-size))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "minmum height: ~A"
                                                  (gtk-requisition-height minimum-size))))

        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "natural width: ~A"
                                                  (gtk-requisition-width natural-size))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "natural height: ~A"
                                                  (gtk-requisition-height natural-size))))
        (multiple-value-bind (minimum-width natural-width)
            (gtk-widget-get-preferred-width button)
          (gtk-container-add action
                             (make-instance 'gtk-label
                                            :label
                                            (format nil "preferred width: ~A, ~A"
                                                    minimum-width natural-width))))
        (multiple-value-bind (minimum-height natural-height)
            (gtk-widget-get-preferred-height button)
          (gtk-container-add action
                             (make-instance 'gtk-label
                                            :label
                                            (format nil "preferred height: ~A, ~A"
                                                    minimum-height natural-height))))
                                                 

      )



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
      (gtk-container-add content frame)
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))
