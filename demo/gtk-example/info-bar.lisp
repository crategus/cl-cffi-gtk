(in-package :gtk-example)

(defun create-info-bar ()
  (let* ((info-bar (make-instance 'gtk-info-bar))
         (message (make-instance 'gtk-label :label "Some text"))
         (content (gtk-info-bar-content-area info-bar)))
    ;; Hide the info by default
    (setf (gtk-widget-no-show-all info-bar) t)
    ;; Add a label for the message to the content of the info bar
    (gtk-container-add content message)
    (gtk-widget-show message)
    ;; Add buttons to the info bar
    (gtk-info-bar-add-buttons info-bar "gtk-ok" 1 "gtk-cancel" 2)
    ;; Connect a signal handler to the info bar
    (g-signal-connect info-bar "response"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk-widget-hide widget)))
    info-bar))

(defun show-error-message (info-bar message type)
  (let* ((content (gtk-info-bar-content-area info-bar))
         (label (first (gtk-container-get-children content))))
    (setf (gtk-label-label label) message)
    (setf (gtk-info-bar-message-type info-bar) type)
    (gtk-widget-show info-bar)))

(defun example-info-bar ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Info bar"
                                 :border-width 12
                                 :default-width 450))
          (grid (make-instance 'gtk-grid))
          (info-bar (create-info-bar)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Add the info bar to the grid and the grid to the window
      (gtk-grid-attach grid info-bar 0 2 1 1)
      (gtk-container-add window grid)
      ;; Show an error message in the info bar
      (show-error-message info-bar "This is an error message." :error)
      ;; Show the window.
      (gtk-widget-show-all window))))

