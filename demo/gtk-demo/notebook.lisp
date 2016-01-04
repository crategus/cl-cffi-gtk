;;;; Example of GtkNotebook

(in-package #:gtk-demo)

(defun example-notebook ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Notebook"
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 200))
          (expander (make-instance 'gtk-expander
                                   :expanded t
                                   :label "Notebook"))
          (notebook (make-instance 'gtk-notebook
                                   :enable-popup t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (dotimes (i 5)
        (let ((page (make-instance 'gtk-label
                                   :label
                                   (format nil
                                           "Text for page ~A" i)))
              (tab-label (make-instance 'gtk-label
                                        :label (format nil "Tab ~A" i)))
              (tab-button (make-instance 'gtk-button
                                         :image
                                         (make-instance 'gtk-image
                                                        :stock
                                                        "gtk-close"
                                                        :icon-size 1)
                                         :relief :none)))
          (g-signal-connect tab-button "clicked"
             (let ((page page))
               (lambda (button)
                 (declare (ignore button))
                 (format t "Removing page ~A~%" page)
                 (gtk-notebook-remove-page notebook page))))
          (let ((tab-hbox (make-instance 'gtk-box
                                         :orientation :horizontal)))
            (gtk-box-pack-start tab-hbox tab-label)
            (gtk-box-pack-start tab-hbox tab-button)
            (gtk-widget-show-all tab-hbox)
            (gtk-notebook-add-page notebook page tab-hbox))))
      (gtk-container-add expander notebook)
      (gtk-container-add window expander)
      (gtk-widget-show-all window))))

