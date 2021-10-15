;;;; Example Application Open - 2021-10-9

(in-package :gio-example)

(defun application-open (&rest argv)
  (let ((app (make-instance 'g-application
                            :application-id "com.crategus.application-open"
                            :flags :handles-open))
        (argv (if argv argv (uiop:command-line-arguments))))
    ;; Print information about the application
    (format t "Start application~%")
    (format t "      arg : ~a~%" argv)
    (format t "  prgname : ~a~%" (g-prgname))
    ;; Signal handler "startup"
    (g-signal-connect app "startup"
                      (lambda (application)
                        (declare (ignore application))
                        (format t "The application is in STARTUP~%")))
    ;; Signal handler "activate"
    (g-signal-connect app "activate"
                      (lambda (application)
                        (declare (ignore application))
                        (format t "The application is in ACTIVATE~%")
                        ;; Note: when doing a longer-lasting action here that
                        ;; returns to the main loop, you should use
                        ;; g-application-hold and g-application-release to
                        ;; keep the application alive until the action is
                        ;; completed.
                      ))
    ;; Signal handler "open"
    (g-signal-connect app "open"
                      (lambda (application files n-files hint)
                        (declare (ignore application))
                          (format t "The application is in OPEN~%")
                          (format t "  n-files : ~A~%" n-files)
                          (format t "     hint : ~A~%" hint)
                          ;; The argument FILES is a C pointer to an array of
                          ;; GFile objects. We list the pathnames of the files.
                          (dotimes (i n-files)
                            (let ((file (mem-aref files '(g-object g-file) i)))
                              (format t " ~a~%" (g-file-path file))))))
    ;; Signal handler "shutdown"
    (g-signal-connect app "shutdown"
                      (lambda (application)
                        (declare (ignore application))
                        (format t "The application is in SHUTDOWN~%")))
    ;; Run the application
    (g-application-run app argv)))
