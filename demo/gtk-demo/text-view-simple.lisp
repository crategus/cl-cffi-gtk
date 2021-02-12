;;;; Simple Text View

(in-package #:gtk-demo)

(defun example-text-view-simple ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Simple Text View"
                                  :default-width 350))
           (view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-buffer view)))
      (g-signal-connect window "destroy"
          (lambda (widget)
            (declare (ignore widget))
            (let ((start (gtk-text-buffer-start-iter buffer))
                  (end (gtk-text-buffer-end-iter buffer))
                  (include-hidden-chars t))
              (print (gtk-text-buffer-get-text buffer
                                               start
                                               end
                                               include-hidden-chars))
              (terpri)
              (leave-gtk-main))))
      (setf (gtk-text-buffer-text buffer) "Some text for the text view.")
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

;;; 2021-2-12
