;;;; Simple Text View (2021-6-4)

(in-package :gtk-example)

(defun example-text-view-simple ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Simple Text View"
                                  :default-width 350
                                  :default-height 200))
           (textview (make-instance 'gtk-text-view
                                    :wrap-mode :word
                                    :top-margin 6
                                    :left-margin 6
                                    :right-margin 6))
           (buffer (gtk-text-view-buffer textview)))
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
      (setf (gtk-text-buffer-text buffer) *lorem-ipsum-short*)
      (gtk-container-add window textview)
      (gtk-widget-show-all window))))
