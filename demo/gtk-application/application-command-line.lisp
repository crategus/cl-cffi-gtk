;;;; Application Command Line (2021-9-19)

(in-package :gtk-application)

(defun application-cmdline (&rest argv)
  (within-main-loop
    (let (;; Get the command line arguments and cons the programm name on the
          ;; list of arguments.
          (argv (cons "application-cmdline"
                      (if argv argv (uiop:command-line-arguments))))
          (options '(("version" #\v :none :none nil "Print version and exit" nil)
                     ("small" #\Nul :none :none nil "Small game" nil)
                     ("medium" #\Nul :none :none nil "Medium game" nil)
                     ("big" #\Nul :none :none nil "Big game" nil)))
          ;; Create an application
          (app (make-instance 'gtk-application
                              :application-id "com.crategus.application-cmdline"
                              :flags '(:handles-command-line))))
      ;; Connect signal "activate" to the applicaton
      (g-signal-connect app "activate"
          (lambda (application)
            (format t "in signal handler ACTIVATE~%")
            ;; Create an application window
            (let ((window (make-instance 'gtk-application-window
                                         :application application
                                         :title "Application Command Line"
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
      ;; Connect signal "handle-local-options"
      (g-signal-connect app "handle-local-options"
          (lambda (application options)
            (declare (ignore application))
            (format t "in signal handler HANDLE-LOCAL-OPTIONS~%")
            (format t "        prgname : ~a~%" (g-prgname))
            (format t "        options : ~a~%" options)
            (let ((status -1)) ; Default is to continue processing
              (cond ((g-variant-dict-contains options "version")
                     (format t "~%~a~%" (cl-cffi-gtk-build-info))
                     ;; Return zero to exit the application
                     ;; TODO: Does not work, the application is not shut down.
                     (setf status 0)
                     ;; We have to stop the main loop
                     (leave-gtk-main))
                    (t
                     (when (g-variant-dict-contains options "small")
                       (format t "  found option small~%"))
                     (when (g-variant-dict-contains options "medium")
                       (format t "  found option medium~%"))
                     (when (g-variant-dict-contains options "big")
                       (format t "  found option big~%"))))
              ;; Return status
              status)))
      ;; Connect signal "command-line"
      (g-signal-connect app "command-line"
          (lambda (application cmdline)
            (declare (ignore application))
            (let ((args (g-application-command-line-get-arguments cmdline))
                  (data (g-application-command-line-get-platform-data cmdline))
                  (cwd (g-application-command-line-cwd cmdline))
                  (environ (g-application-command-line-environ cmdline)))
              (format t "in signal handler COMMAND-LINE~%")
              (format t "      arguments : ~a~%" args)
              (format t "  platform-data : ~a~%" data)
              (format t "            cwd : ~a~%" cwd)
              (format t "        environ : ~a ...~%" (first environ))
              ;; Activate the application
              (g-application-activate app)
              ;; Exit the application with status 0
              0)))
      ;; Connect signal "shutdown"
      (g-signal-connect app "shutdown"
          (lambda (application)
            (declare (ignore application))
            (format t "in signal handler SHUTDOWN~%")
            ;; Leave the main loop on shutdown of the application
            (leave-gtk-main)))
      ;; Run the application
      (format t "COMMAND-LINE-ARGUMENTS : ~a~%" (uiop:command-line-arguments))
      ;; Add the option entries to the application
      (g-application-add-main-option-entries app options)
      ;; Run the application
      (g-application-run app argv)))
  (join-gtk-main))
