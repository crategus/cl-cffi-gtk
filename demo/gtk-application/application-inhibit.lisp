;;;; Application Inhibit - 2021-10-11

;; TODO: This examples does not seem to work as expected. Try to work out an
;; example that demonstrates the gtk-application-inhibit function.

(in-package :gtk-application)

(defun application-inhibit (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk-application
                            :application-id "com.crategus.application-inhibit"
                            :flags :none)))
    ;; Connect signal "activate" to the applicaton
    (g-signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let ((cookie 0)
                (flags '(:logout :switch :suspend :idle))
                (message "Application is inhibited.")
                (window (make-instance 'gtk-application-window
                                       :application application
                                       :title "Application Inhibit"
                                       :default-width 480
                                       :default-height 300))
                (toggle (make-instance 'gtk-toggle-button
                                       :label "Inhibit application")))
            ;; Connect signal "destroy" to the application window
            (g-signal-connect window "destroy"
                              (lambda (widget)
                                (declare (ignore widget))
                                ;; Quit the application
                              ))
            (g-signal-connect toggle "toggled"
                (lambda (widget)
                  (if (gtk-toggle-button-active widget)
                      (progn
                        (setf (gtk-button-label widget) "Unhibit Application")
                        (setf cookie
                              (gtk-application-inhibit app nil flags message))
                        (format t "Application is inhibited: ~a, ~a~%"
                                  (gtk-application-is-inhibited app flags)
                                  cookie))
                      (progn
                        (setf (gtk-button-label widget) "Inhibit Application")
                        (gtk-application-uninhibit app cookie)
                        (setf cookie 0)
                        (format t "Application is unhibited: ~a, ~a~%"
                                  (gtk-application-is-inhibited app flags)
                                  cookie)))))
            ;; Show the application window
            (gtk-container-add window toggle)
            (gtk-widget-show-all window))))
    ;; Connect signal "shutdown" to the application
    (g-signal-connect app "shutdown"
        (lambda (application)
          (declare (ignore application))
          ;; Leave the main loop on shutdown of the application
    ))
    ;; Run the application
    (g-application-run app argv)))
