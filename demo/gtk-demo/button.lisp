;;;; Normal Buttons

(defun image-label-box (filename text)
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
                            :border-width 3))
        (label (make-instance 'gtk-label
                              :label text))
        (image (gtk-image-new-from-file filename)))
    (gtk-box-pack-start box image :expand nil :fill nil :padding 3)
    (gtk-box-pack-start box label :expand nil :fill nil :padding 3)
    box))

(defun example-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Cool Button"
                                 :type :toplevel
                                 :border-width 12))
          (button (make-instance 'gtk-button))
          (box (image-label-box "save.png" "Save to File")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add button box)
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

