;;;; Example Text Entry (2021-6-11)

(in-package :gtk-example)

(defun example-text-entry ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text Entry"
                                  :default-width 250
                                  :default-height 120))
           (vbox (make-instance 'gtk-box :orientation :vertical))
           (hbox (make-instance 'gtk-box :orientation :horizontal))
           (entry (make-instance 'gtk-entry
                                 :text "Hello"
                                 :max-length 50))
           (pos (gtk-entry-text-length entry)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect entry "activate"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Entry contents: ~A"
                                  (gtk-entry-text entry))))
      (gtk-editable-insert-text entry " world" pos)
      (gtk-editable-select-region entry 0 (gtk-entry-text-length entry))
      (gtk-box-pack-start vbox entry :expand t :fill t :padding 0)
      (let ((check (gtk-check-button-new-with-label "Editable")))
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (setf (gtk-editable-editable entry)
                   (gtk-toggle-button-active check))))
        (gtk-box-pack-start hbox check))
      (let ((check (gtk-check-button-new-with-label "Visible")))
        (setf (gtk-toggle-button-active check) t)
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (setf (gtk-entry-visibility entry)
                   (gtk-toggle-button-active check))))
        (gtk-box-pack-start hbox check))
      (gtk-box-pack-start vbox hbox)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
