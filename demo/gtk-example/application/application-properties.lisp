;;;; Simple Application (2021-8-17)

(in-package :gtk-application)

(defun application-print-properties (app)
  (format t "     application-id : ~a~%" (g-application-application-id app))
  (format t "              flags : ~a~%" (g-application-flags app))
  (format t " inactivity-timeout : ~a~%" (g-application-inactivity-timeout app))
  (format t "            is-busy : ~a~%" (g-application-is-busy app))
  (format t "      is-registered : ~a~%" (g-application-is-registered app))
  (format t "          is-remote : ~a~%" (g-application-is-remote app))
  (format t " resource-base-path : ~a~%" (g-application-resource-base-path app))
  (format t "~%")
  (format t "      active-window : ~a~%" (gtk-application-active-window app))
  (format t "           app-menu : ~a~%" (gtk-application-app-menu app))
  (format t "            menubar : ~a~%" (gtk-application-menubar app))
  (format t "   register-session : ~a~%" (gtk-application-register-session app))
  (format t " screensaver-active : ~a~%" (gtk-application-screensaver-active app))
)

(defun application-properties (&optional (argv nil))
  (within-main-loop
    (let (;; Create an application
          (app (make-instance 'gtk-application
                              :application-id
                              "com.crategus.application-properties"
                              :register-session t
                              :inactivity-timeout 10000
                              :flags :send-enviroment)))
      ;; Connect signal "startup" to the application
      (g-signal-connect app "startup"
          (lambda (application)
            (format t "~%in signal STARTUP~%")
            (application-print-properties application)

      ))

      ;; Connect signal "activate" to the applicaton
      (g-signal-connect app "activate"
          (lambda (application)
            ;; Create an application window
            (let ((window (make-instance 'gtk-application-window
                                         :application application
                                         :title "Application Properties"
                                         :default-width 500
                                         :default-height 300)))
              (format t "~%in signal ACTIVATE~%")
              (application-print-properties application)

              ;; Connect signal "destroy" to the application window
              (g-signal-connect window "destroy"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  ;; Quit the application
                                  (g-application-quit app)))
              ;; Show the application window
              (gtk-widget-show-all window))))
      ;; Connect signal "shutdown" to the application
      (g-signal-connect app "shutdown"
          (lambda (application)
            (format t "~%in signal SHUTDOWN~%")
            (application-print-properties application)
            ;; Leave the main loop on shutdown of the application
            (leave-gtk-main)))
      ;; Run the application
      (g-application-run app argv)))
  (join-gtk-main))
