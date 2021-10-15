;;;; Simple Application - 2021-10-7

(in-package :gtk-application)

(defun application-simple (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk-application
                            :application-id "com.crategus.application-simple"
                            :flags :none)))
    ;; Connect signal "activate" to the application
    (g-signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (let ((window (make-instance 'gtk-application-window
                                       :application application
                                       :title "Simple Application"
                                       :default-width 480
                                       :default-height 300)))
            ;; Show the application window
            (gtk-widget-show-all window))))
    ;; Run the application
    (g-application-run app argv)))
