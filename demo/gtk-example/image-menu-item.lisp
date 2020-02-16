(in-package :gtk-example)

(defun create-image-menu-item ()
    (let ((box (make-instance 'gtk-box :orientation :horizontal
                                       :spacing 6))
          (icon (make-instance 'gtk-image :icon-name "folder-music-symbolic"
                                          :icon-size 1))
          (label (make-instance 'gtk-label :label "Music"))
          (menu-item (make-instance 'gtk-menu-item)))
      (gtk-container-add box icon)
      (gtk-container-add box label)
      (gtk-container-add menu-item box)
      menu-item))

(defun create-image-menu-item-with-accel ()
    (let ((box (make-instance 'gtk-box :orientation :horizontal
                                       :spacing 6))
          (icon (make-instance 'gtk-image :icon-name "folder-music-symbolic"
                                          :icon-size 1))
          (label (make-instance 'gtk-accel-label :label "Music"
                                                 :use-underline t
                                                 :xalign 0.0))
          (menu-item (make-instance 'gtk-menu-item))
          (accel-group (make-instance 'gtk-accel-group)))

      (gtk-widget-add-accelerator menu-item
                                  "activate"
                                  accel-group
                                  (gdk-keyval-from-name "o")
                                  :control-mask
                                  :visible)

      (setf (gtk-accel-label-accel-widget label) menu-item)

      (gtk-container-add box icon)
      (gtk-box-pack-end box label :expand t :fill t :padding 0)
      (gtk-container-add menu-item box)
      menu-item))

(defun example-image-menu-item ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window :type :toplevel
                                             :title "Example Image Menu Item"
                                             :default-height 200
                                             :default-width 350
                                             :border-width 12))
          (box (make-instance 'gtk-box :orientation :vertical :spacing 12)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add box (create-image-menu-item))
      (gtk-container-add box (create-image-menu-item-with-accel))
      (gtk-container-add window box)
      ;; Show the window.
      (gtk-widget-show-all window))))
