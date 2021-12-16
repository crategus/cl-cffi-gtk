;;;; Stack Sidebar
;;;;
;;;; GtkStackSidebar provides an automatic sidebar widget to control navigation
;;;; of a GtkStack object. This widget automatically updates it content based on
;;;; what is presently available in the GtkStack object, and using the "title"
;;;; child property to set the display labels.

(in-package :gtk-example)

(defun example-stack-sidebar (&optional (application nil))
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Example Stack Sidebar"
                                  :type :toplevel
                                  :application application
                                  :width-request 500
                                  :height-request 450))
           (header (make-instance 'gtk-header-bar
                                  :title "Example Stack Sidebar"
                                  :show-close-button t))
           (box (make-instance 'gtk-box
                               :orientation :horizontal))
           (stack (make-instance 'gtk-stack
                                 :transition-type :slide-up-down))
           (sidebar (make-instance 'gtk-stack-sidebar
                                   :stack stack)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (setf (gtk-window-titlebar window) header)
      (gtk-box-pack-start box (make-instance 'gtk-separator
                                             :orientation :vertical)
                              :expand nil)
      (dolist (name '("Welcome to GTK"
                      "GtkStackSidebar Widget"
                      "Automatic navigation"
                      "Consistent appearance"
                      "Scrolling"
                      "Page 6"
                      "Page 7"
                      "Page 8"
                      "Page 9"))
        (if (string= "Welcome to GTK" name)
            (let ((image (gtk-image-new-from-icon-name "help-about" :menu)))
              (setf (gtk-image-pixel-size image) 256)
              (gtk-stack-add-named stack image name)
              (setf (gtk-stack-child-title stack image) name))
            (let ((label (make-instance 'gtk-label
                                        :label name)))
              (gtk-stack-add-named stack label name)
              ;; Workaround for GTK-STACK-CHILD-TITLE
              (setf (gtk-stack-child-title stack label) name))))
      (gtk-box-pack-start box sidebar :expand nil)
      (gtk-box-pack-start box stack :expand t :fill t)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
