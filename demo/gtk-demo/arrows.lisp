;;;; Arrows

(in-package #:gtk-demo)

(defun create-arrow-button (arrow-type shadow-type)
  (let (;; Create a button
        (button (make-instance 'gtk-button
                               ;; Add a small margin around the button
                               :margin 3
                               ;; Make big buttons of size 75 x 75
                               :width-request 75
                               :height-request 75)))
    ;; Add an arrow to the button
    (gtk-container-add button
                       (make-instance 'gtk-arrow
                                      :arrow-type arrow-type
                                      :shadow-type shadow-type))
    ;; Add a tooltip to the button
    (setf (gtk-widget-tooltip-text button)
          (format nil "Arrow of type ~A" (symbol-name arrow-type)))
    button))

(defun example-arrows ()
  (within-main-loop
    (let ((;; Create the main window
           window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Arrow Buttons"
                                 :default-width 275
                                 :default-height 125
                                 :border-width 12))
          ;; Create a grid for the buttons
          (grid (make-instance 'gtk-grid
                               :orientation :horizontal
                               :column-homogeneous t)))
      ;; Connect a signal handler to the window
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create buttons with an arrow and add the buttons to the grid
      (gtk-container-add grid (create-arrow-button :up :in))
      (gtk-container-add grid (create-arrow-button :down :out))
      (gtk-container-add grid (create-arrow-button :left :etched-in))
      (gtk-container-add grid (create-arrow-button :right :etched-out))
      ;; Add the grid to the window
      (gtk-container-add window grid)
      ;; Show the window
      (gtk-widget-show-all window))))

