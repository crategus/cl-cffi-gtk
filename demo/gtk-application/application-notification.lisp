;;;; Application Notification - 2021-10-12

;; TODO: This examples does not seem to work as expected. Try to work out an
;; example that demonstrates the usage of the g-notification class.

(in-package :gtk-application)

(defun application-notification (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk-application
                            :application-id
                            "com.crategus.application-notification"
                            :flags :none)))
    ;; Connect signal "startup" to the application
    (g-signal-connect app "startup"
        (lambda (application)
          ;; Add action "clear-all" to the application
          (let ((action (g-simple-action-new "clear-all" nil)))
            (g-signal-connect action "activate"
                (lambda (action parameter)
                  (declare (ignore action parameter))
                  (format t "in action CLEAR-ALL~%")))
            (g-action-map-add-action application action))))
    ;; Connect signal "activate" to the application
    (g-signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let ((window (make-instance 'gtk-application-window
                                       :application application
                                       :title "Application Notification"
                                       :default-width 480
                                       :default-height 300))
                (msg (g-notification-new "Three lines of text")))
            ;; Add more information to the notification
            (g-notification-set-body msg "Keep up the good work.")
            (g-notification-add-button msg "Start over " "app.clear-all")
            ;; Send the notification to the application
            (g-application-send-notification application "three-lines" msg)
            ;; Show the application window
            (gtk-widget-show-all window))))
    ;; Run the application
    (g-application-run app argv)))
