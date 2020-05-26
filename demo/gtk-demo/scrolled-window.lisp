;;;; Scrolled Windows

(in-package #:gtk-demo)

(defun example-scrolled-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-dialog
                                 :type :toplevel
                                 :title "Example Scrolled Window"
                                 :border-width 0
                                 :width-request 350
                                 :height-request 300))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :border-width 12
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always))
          (table (make-instance 'gtk-table
                                :n-rows 10
                                :n-columns 10
                                :row-spacing 10
                                :column-spacing 10
                                :homogeneous nil)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-box-pack-start (gtk-dialog-content-area window) scrolled)
      (gtk-scrolled-window-add-with-viewport scrolled table)
      (dotimes (i 10)
        (dotimes (j 10)
          (gtk-table-attach table
                            (make-instance 'gtk-button
                                           :label
                                           (format nil "(~d, ~d)" i j))
                            i (+ i 1) j (+ j 1))))
      (let ((button (make-instance 'gtk-button
                                   :label "Close"
                                   :can-default t)))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start (gtk-dialog-action-area window) button)
        (gtk-widget-grab-default button))
      (gtk-widget-show-all window))))

