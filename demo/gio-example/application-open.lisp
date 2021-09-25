;;;; Example Application Open (2021-9-19)

(in-package :gio-example)

(defun application-open (&rest argv)
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id "com.crategus.application-open"
                              :inactivity-timeout 10000
                              :flags :handles-open))
          (argv (if argv argv #+sbcl sb-ext:*posix-argv*)))
      (format t "Start Application.~%")
      (format t "    ARGV : ~a~%" argv)
      (format t " prgname : ~a~%" (g-prgname))
      ;; Signal handler "startup"
      (g-signal-connect app "startup"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "The application is in startup.~%")))
      ;; Signal handler "activate"
      (g-signal-connect app "activate"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "The application is in activate.~%")
                          ;; Note: when doing a longer-lasting action here that
                          ;; returns to the mainloop, you should use
                          ;; g-application-hold and g-application-release to
                          ;; keep the application alive until the action is
                          ;; completed.
                        ))
      ;; Signal handler "open"
      (g-signal-connect app "open"
                        (lambda (application files n-files hint)
                          (declare (ignore application))
                          (format t "The application is in open.~%")
                          (format t " n-files : ~A~%" n-files)
                          (format t "    hint : ~A~%" hint)
                          ;; The argument FILES is a C pointer to an array of
                          ;; GFile objects. We list the pathnames of the files.
                          (dotimes (i n-files)
                            (let ((file (mem-aref files '(g-object g-file) i)))
                              (format t " ~a~%" (g-file-path file))))))
      ;; Signal handler "shutdown"
      (g-signal-connect app "shutdown"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "The application is in shutdown.~%")
                          ;; Stop the main loop
                          (leave-gtk-main)))
      ;; Start the application
      (g-application-run app argv)))
  (join-gtk-main))
