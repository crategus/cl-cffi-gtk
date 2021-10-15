;;;; Application Command Line - 2021-10-10

(in-package :gtk-application)

(defun application-cmdline (&rest argv)
  (let (;; Get the command line arguments, cons the program name on the list
        (argv (cons "application-cmdline"
                    (if argv argv (uiop:command-line-arguments))))
        (options '(("version" #\v :none :none nil "Print version and exit" nil)
                   ("small" #\Nul :none :none nil "Small game" nil)
                   ("medium" #\Nul :none :none nil "Medium game" nil)
                   ("big" #\Nul :none :none nil "Big game" nil)))
        ;; Create an application
        (app (make-instance 'gtk-application
                            :application-id "com.crategus.application-cmdline"
                            :flags :handles-command-line)))
    ;; Print the command line arguments
    (format t "COMMAND-LINE-ARGUMENTS : ~a~%" argv)
    ;; Connect signal "activate" to the applicaton
    (g-signal-connect app "activate"
        (lambda (application)
          (format t "in signal handler ACTIVATE~%")
          ;; Create an application window
          (let ((window (make-instance 'gtk-application-window
                                       :application application
                                       :title "Application Command Line"
                                       :default-width 480
                                       :default-height 300)))
            ;; Show the application window
            (gtk-widget-show-all window))))
    ;; Connect signal "handle-local-options"
    (g-signal-connect app "handle-local-options"
        (lambda (application options)
          (declare (ignore application))
          (format t "in signal handler HANDLE-LOCAL-OPTIONS~%")
          (format t "        prgname : ~a~%" (g-prgname))
          (format t "        options : ~a~%" options)
          (let ((status -1)) ; Default to continue processing
            (cond ((g-variant-dict-contains options "version")
                   (format t "~%~a~%" (cl-cffi-gtk-build-info))
                   ;; Set status to zero to exit the application
                   (setf status 0))
                  (t
                   (when (g-variant-dict-contains options "small")
                     (format t "found option --SMALL~%"))
                   (when (g-variant-dict-contains options "medium")
                     (format t "found option --MEDIUM~%"))
                   (when (g-variant-dict-contains options "big")
                     (format t "found option --BIG~%"))))
            ;; Return the status
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
    ;; Add the option entries to the application
    (g-application-add-main-option-entries app options)
    ;; Run the application
    (g-application-run app argv)))
