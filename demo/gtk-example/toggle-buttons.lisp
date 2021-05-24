;;;; Example Toggle Buttons (2021-5-20)

(in-package :gtk-example)

(defun example-toggle-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Toggle Buttons"
                                 :type :toplevel
                                 :border-width 18))
          (grid (make-instance 'gtk-grid
                               :halign :center
                               :valign :center
                               :column-spacing 24
                               :row-spacing 12)))

      ;; Handler for the signal "destroy"
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Create three radio buttons and put the buttons into the grid
      (let ((button (gtk-radio-button-new-with-label nil "Radio Button 1")))
        (gtk-grid-attach grid button 0 0 1 1)
        (setq button
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group button)
                                          "Radio Button 2"))
        ;; Make this button active
        (setf (gtk-toggle-button-active button) t)
        (gtk-grid-attach grid button 0 1 1 1)
        (setq button
              (gtk-radio-button-new-with-mnemonic
                                          (gtk-radio-button-get-group button)
                                          "Radio Button _3"))
        (gtk-grid-attach grid button 0 2 1 1))

      ;; Create three check buttons and put the buttons into the grid
      (gtk-grid-attach grid
                       (gtk-check-button-new-with-label "Check Button 1")
                       1 0 1 1)
      (gtk-grid-attach grid
                       (gtk-check-button-new-with-label "Check Button 2")
                       1 1 1 1)
      (gtk-grid-attach grid
                       (gtk-check-button-new-with-label "Check Button 3")
                       1 2 1 1)
      ;; Make the first check button active
      (setf (gtk-toggle-button-active (gtk-grid-child-at grid 1 0)) t)

      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
