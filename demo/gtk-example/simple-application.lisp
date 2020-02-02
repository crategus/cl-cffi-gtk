;;;; Simple Application

(in-package #:gtk-example)

(defun simple-application (&optional (argv nil))
  (within-main-loop
    (let (;; Create an application
          (app (make-instance 'gtk-application
                              :application-id "com.crategus.simple-application"
                              :flags :none)))

      ;; Connect signal "activate" to the applicaton
      (g-signal-connect app "activate"
          (lambda (application)
            ;; Create an application window
            (let ((window (make-instance 'gtk-application-window
                                         :application application
                                         :title "Simple Application"
                                         :default-width 500
                                         :default-height 300)))
              ;; Connect signal "destroy" to the application window
              (g-signal-connect window "destroy"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  ;; Leave the main loop
                                  (leave-gtk-main)
                                  ;; Quit the application
                                  (g-application-quit app)))
              ;; Show the application window
              (gtk-widget-show-all window))))

      ;; Run the application
      (g-application-run app argv))))
