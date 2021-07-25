;;;; Example Table packing with more spacing (2021-7-9)

(in-package :gtk-example)

(defun example-table-packing-2 ()
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
          (button1 (make-instance 'gtk-toggle-button
                                  :label "More Row Spacing"))
          (button2 (make-instance 'gtk-toggle-button
                                  :label "More Col Spacing"))
          (quit (make-instance 'gtk-button
                               :label "Quit")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect button1 "toggled"
         (lambda (widget)
           (if (gtk-toggle-button-active widget)
               (progn
                 (setf (gtk-table-row-spacing table) 12)
                 (setf (gtk-button-label widget) "Less Row Spacing"))
               (progn
                 (setf (gtk-table-row-spacing table) 0)
                 (setf (gtk-button-label widget) "More Row Spacing")))))
      (g-signal-connect button2 "toggled"
         (lambda (widget)
           (if (gtk-toggle-button-active widget)
               (progn
                 (setf (gtk-table-column-spacing table) 12)
                 (setf (gtk-button-label widget) "Less Col Spacing"))
               (progn
                 (setf (gtk-table-column-spacing table) 0)
                 (setf (gtk-button-label widget) "More Col Spacing")))))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-table-attach table button1 0 1 0 1)
      (gtk-table-attach table button2 1 2 0 1)
      (gtk-table-attach table quit    0 2 1 2)
      (gtk-container-add window table)
      (gtk-widget-show-all window))))
