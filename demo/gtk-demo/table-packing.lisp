;;;; Table Packing Example

(defun example-table-packing ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Table Packing"
                                 :border-width 12
                                 :default-width 300))
          (table (make-instance 'gtk-table
                                :n-columns 2
                                :n-rows 2
                                :homogeneous t))
          (button1 (make-instance 'gtk-button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk-button
                                  :label "Button 2"))
          (quit (make-instance 'gtk-button
                               :label "Quit")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-table-attach table button1 0 1 0 1)
      (gtk-table-attach table button2 1 2 0 1)
      (gtk-table-attach table quit    0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show-all window))))

