;;;; Text Entry

(defun example-text-entry ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text Entry"
                                  :default-width 250))
           (vbox (make-instance 'gtk-vbox))
           (hbox (make-instance 'gtk-hbox))
           (entry (make-instance 'gtk-entry
                                 :text "Hello"
                                 :max-length 50))
           (pos (gtk-entry-get-text-length entry)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect entry "activate"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Entry contents: ~A"
                                  (gtk-entry-get-text entry))))
      (gtk-editable-insert-text entry " world" pos)
      (gtk-editable-select-region entry 0 (gtk-entry-get-text-length entry))
      (gtk-box-pack-start vbox entry :expand t :fill t :padding 0)
      (let ((check (gtk-check-button-new-with-label "Editable")))
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-editable-set-editable entry
                                        (gtk-toggle-button-get-active check))))
        (gtk-box-pack-start hbox check))
      (let ((check (gtk-check-button-new-with-label "Visible")))
        (gtk-toggle-button-set-active check t)
        (g-signal-connect check "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-entry-set-visibility entry
                                       (gtk-toggle-button-get-active check))))
        (gtk-box-pack-start hbox check))
      (gtk-box-pack-start vbox hbox)
      (let ((button (gtk-button-new-from-stock "gtk-close")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

