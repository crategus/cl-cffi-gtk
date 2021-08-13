;;;; Simple Application (2021-8-12)

(in-package :gtk-application)

(defun application-simple (&optional (argv nil))
  (within-main-loop
    (let (;; Create an application
          (app (make-instance 'gtk-application
                              :application-id "com.crategus.application-simple"
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
                                  ;; Quit the application
                                  (g-application-quit app)))
              ;; Show the application window
              (gtk-widget-show-all window))))
      ;; Connect signal "shutdown" to the application
      (g-signal-connect app "shutdown"
          (lambda (application)
            (declare (ignore application))
            ;; Leave the main loop on shutdown of the application
            (leave-gtk-main)))
      ;; Run the application
      (g-application-run app argv)))
  (join-gtk-main))
