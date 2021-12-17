;;;; Example Header Bar - 2021-12-7
;;;;
;;;; GtkHeaderBar is a container that is suitable for implementing window
;;;; titlebars. One of its features is that it can position a title, and
;;;; optional subtitle, centered with regard to the full width, regardless of
;;;; variable-width content at the left or right. It is commonly used with the
;;;; gtk-window-titlebar function.

(in-package :gtk-example)

(defun example-header-bar (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Header Bar"
                                 :type :toplevel
                                 :application application
                                 :default-width 600
                                 :default-height 400))
          (header (make-instance 'gtk-header-bar
                                 :title "Welcome to GTK"
                                 :subtitle "Lisp programming with GTK"
                                 :decoration-layout ":close"
                                 :show-close-button t)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (let* ((button (make-instance 'gtk-button))
             (icon (g-themed-icon-new "mail-send-receive"))
             (image (gtk-image-new-from-gicon icon :button)))
        (gtk-container-add button image)
        (gtk-header-bar-pack-end header button))
      (let* ((box (make-instance 'gtk-box
                                 :orientation :horizontal))
             (button (make-instance 'gtk-button)))
        (gtk-style-context-add-class (gtk-widget-style-context box) "linked")
        (gtk-container-add button
                           (gtk-image-new-from-icon-name "pan-start" :button))
        (gtk-container-add box button)
        (setf button (make-instance 'gtk-button))
        (gtk-container-add button
                           (gtk-image-new-from-icon-name "pan-end" :button))
        (gtk-container-add box button)
        (gtk-header-bar-pack-start header box))

      (setf (gtk-window-titlebar window) header)
      (gtk-widget-show-all window))))
