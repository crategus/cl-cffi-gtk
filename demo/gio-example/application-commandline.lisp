;;;; Example Application Commandline (2021-5-7)

(in-package :gio-example)

(defun application-commandline (&optional (argv nil))
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id
                              "com.crategus.application-commandline"
                              :inactivity-timeout 10000
                              :flags
                              '(:send-enviroment :handles-command-line))))

      ;; Signal handler "startup"
      (g-signal-connect app "startup"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "Signal handler 'startup'~%")))

      ;; Signal handler "command-line"
      (g-signal-connect app "command-line"
          (lambda (application cmdline)
            (declare (ignore application))
            (let ((args (g-application-command-line-get-arguments cmdline))
                  (data (g-application-command-line-get-platform-data cmdline)))
              (format t "Signal handler 'command-line'~%")
              (format t "     arguments : ~a~%" args)
              (format t " platform-data : ~a~%" data)
              ;; Return the exit status
              0)))

      ;; Signal handler "shutdown"
      (g-signal-connect app "shutdown"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "Signal handler 'shutdown'~%")
                          ;; Stop the main loop
                          (leave-gtk-main)))

      ;; Start the application
      (g-application-run app argv))))
