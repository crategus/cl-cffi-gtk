;;;; Toggle Buttons

(in-package #:gtk-demo)

(defun example-toggle-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Toggle Buttons"
                                 :type :toplevel))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal)))
      ;; Handler for the signal "destroy"
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create three radio buttons and put the buttons in a vbox
      (let ((vbox (make-instance 'gtk-box
                                 :orientation :vertical
                                 :spacing 12
                                 :border-width 12))
            (button (gtk-radio-button-new-with-label nil "Radio Button 1")))
        (gtk-box-pack-start vbox button)
        (setq button
              (gtk-radio-button-new-with-label
                                          (gtk-radio-button-get-group button)
                                          "Radio Button 2"))
        (setf (gtk-toggle-button-active button) t)
        (gtk-box-pack-start vbox button)
        (setq button
              (gtk-radio-button-new-with-mnemonic
                                          (gtk-radio-button-get-group button)
                                          "_Radio Button 3"))
        (gtk-box-pack-start vbox button)
        ;; Put the vbox with the radio buttons in a hbox
        (gtk-box-pack-start hbox vbox :expand nil :fill nil))
      ;; Create three check buttons and put the buttons in a vbox
      (let ((vbox (make-instance 'gtk-box
                                 :orientation :vertical
                                 :homogeneous nil
                                 :spacing 12
                                 :border-width 12)))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 1"))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 2"))
        (gtk-box-pack-start vbox
                            (gtk-check-button-new-with-label "Check Button 3"))
        ;; Put the vbox with the buttons in a hbox
        (gtk-box-pack-start hbox vbox :expand nil :fill nil))
      ;; Put the hbox in a vbox
      (gtk-box-pack-start vbox hbox :expand nil :fill nil)
      ;; Add a separator to the vbox
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-separator
                                         :orientation :horizontal)
                          :expand nil :fill nil)
      ;; Add a quit button to the vbox
      (let ((vbox-quit (make-instance 'gtk-box
                                      :orientation :vertical
                                      :spacing 12
                                      :border-width 12))
            (button (make-instance 'gtk-button :label "Close")))
        (gtk-box-pack-start vbox-quit button :expand nil :fill nil)
        (gtk-box-pack-start vbox vbox-quit :expand nil)
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (declare (ignore button))
                            (gtk-widget-destroy window))))
      ;; Put the vbox in the window widget
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
