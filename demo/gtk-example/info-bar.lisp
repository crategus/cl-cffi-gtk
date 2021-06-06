;;;; Example Info Bar (2021-5-24)

(in-package :gtk-example)

(defun example-info-bar ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Info bar"
                                  :border-width 12
                                  :default-width 250))
           (grid (make-instance 'gtk-grid
                                :orientation :vertical))
           (info-bar (make-instance 'gtk-info-bar
                                    :no-show-all t))
           (message (make-instance 'gtk-label
                                   :label ""))
           (content (gtk-info-bar-content-area info-bar)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show message)
      ;; Add a label to the content area of the info bar
      (gtk-container-add content message)
      ;; Add a button OK to the action area
      (gtk-info-bar-add-button info-bar "gtk-ok" 1)
      ;; Add two more buttons to the action area
      (gtk-info-bar-add-buttons info-bar "gtk-cancel" 2
                                         "gtk-no" 3)
      ;; Connect a handler for the "response" signal of the info bar
      (g-signal-connect info-bar "response"
         (lambda (widget response-id)
           (declare (ignore widget))
           (format t "response-id is ~A~%" response-id)
           (gtk-widget-hide info-bar)))
      (gtk-container-add grid info-bar)
      ;; Show the info bar
      (setf (gtk-label-text message) "An Info Message in the content area.")
      (setf (gtk-info-bar-message-type info-bar) :info)
      (gtk-widget-show info-bar)
      ;; Add the container grid to the window and show all
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
