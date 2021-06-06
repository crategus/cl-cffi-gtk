;;;; Example Notebook (2021-6-4)

(in-package :gtk-example)

(defun example-notebook ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Notebook"
                                 :type :toplevel
                                 :default-width 300
                                 :default-height 210))
          (notebook (make-instance 'gtk-notebook
                                   :enable-popup t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (dotimes (i 3)
        (let ((page (make-instance 'gtk-label
                                   :label
                                   (format nil
                                           "Text for page ~a" (1+ i))))
              (tab-label (make-instance 'gtk-label
                                        :label (format nil "Page ~a" (1+ i))))
              (tab-button (make-instance 'gtk-button
                                         :image
                                         (make-instance 'gtk-image
                                                        :icon-name
                                                        "gtk-close"
                                                        :icon-size 1)
                                         :relief :none)))
          (g-signal-connect tab-button "clicked"
             (let ((page page))
               (lambda (button)
                 (declare (ignore button))
                 (format t "Removing page ~a~%" page)
                 (gtk-notebook-remove-page notebook page))))
          (let ((tab-hbox (make-instance 'gtk-box
                                         :orientation :horizontal)))
            (gtk-box-pack-start tab-hbox tab-label)
            (gtk-box-pack-start tab-hbox tab-button)
            (gtk-widget-show-all tab-hbox)
            (gtk-notebook-add-page notebook page tab-hbox))))
      (gtk-container-add window notebook)
      (gtk-widget-show-all window))))
