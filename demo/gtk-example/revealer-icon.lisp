;;;; Example Revealer Icon - 2021-12-4
;;;;
;;;; GtkRevealer is a container that animates showing and hiding
;;;; of its sole child with nice transitions.

(in-package :gtk-example)

(defun example-revealer-icon (&optional (application nil))
  (within-main-loop
    (let* ((count 0)
           (timeout 0)
           (builder (gtk-builder-new-from-file (sys-path "revealer-icon.ui")))
           (window (gtk-builder-object builder "window")))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (when (not (= timeout 0))
                                   (g-source-remove timeout)
                                   (setf timeout 0))
                                 (leave-gtk-main)))
      (setf (gtk-window-application window) application)
      (setf timeout
            (g-timeout-add 690
                (lambda ()
                  (let* ((name (format nil "revealer~d" count))
                         (revealer (gtk-builder-object builder name)))
                    (setf (gtk-revealer-reveal-child revealer) t)
                    (g-signal-connect revealer "notify::child-revealed"
                        (lambda (widget pspec)
                          (declare (ignore pspec))
                          (when (gtk-widget-mapped widget)
                            (setf (gtk-revealer-reveal-child widget)
                                  (not (gtk-revealer-child-revealed widget))))))
                    (setf count (+ count 1))
                    (if (>= count 9)
                        (progn
                          (setf timeout 0)
                          nil)
                        t)))))
      (gtk-widget-show-all window))))
