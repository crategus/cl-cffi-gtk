;;; An application which can be inhibited

(in-package :gtk-example)

(defun application-inhibit (&optional (argv nil))
  (within-main-loop
    (let ((inhibit-flags nil)
          (app (make-instance 'gtk-application
                              :application-id "com.crategus.application-inhibit"
                              :register-session t
                              :flags :none)))

      ;; Connect signal "activate" to the applicaton
      (g-signal-connect app "activate"
          (lambda (application)
            ;; Create an application window
            (let ((window (make-instance 'gtk-application-window
                                         :application application
                                         :title "Inhibit Application"
                                         :default-width 500
                                         :default-height 300)))
              ;; Connect signal "destroy" to the application window
              (g-signal-connect window "destroy"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  ;; Quit the application
                                  (g-application-quit app)))

              (let ((box (make-instance 'gtk-box :orientation :horizontal))
                    (view (make-instance 'gtk-text-view))
                    (control (make-instance 'gtk-box :orientation :vertical))
                    (status (make-instance 'gtk-statusbar))
                    (status-id 0))

                (setf status-id (gtk-statusbar-context-id status "flags"))
                (gtk-statusbar-push status status-id (format nil "~A" inhibit-flags))

                (gtk-box-pack-start control
                                    (make-instance 'gtk-label
                                                   :label "Inhibit Application"))

                (let ((check-button (gtk-check-button-new-with-label "Logout")))
                  (g-signal-connect check-button "toggled"
                                    (lambda (button)
                                      (if (gtk-toggle-button-active button)
                                          (push :logout inhibit-flags)
                                          (setf inhibit-flags (remove :logout inhibit-flags)))
                                      (gtk-statusbar-push status status-id (format nil "~a" inhibit-flags))
                                      (format t "in toggled ~A~%" inhibit-flags)))
                  (gtk-box-pack-start control check-button))

                (let ((check-button (gtk-check-button-new-with-label "Switch")))
                  (g-signal-connect check-button "toggled"
                                    (lambda (button)
                                      (if (gtk-toggle-button-active button)
                                          (push :switch inhibit-flags)
                                          (setf inhibit-flags (remove :switch inhibit-flags)))
                                      (gtk-statusbar-push status status-id (format nil "~a" inhibit-flags))
                                      (format t "in toggled ~A~%" inhibit-flags)))
                  (gtk-box-pack-start control check-button))

                (let ((check-button (gtk-check-button-new-with-label "Suspend")))
                  (g-signal-connect check-button "toggled"
                                    (lambda (button)
                                      (if (gtk-toggle-button-active button)
                                          (push :suspend inhibit-flags)
                                          (setf inhibit-flags (remove :suspend inhibit-flags)))
                                      (gtk-statusbar-push status status-id (format nil "~a" inhibit-flags))
                                      (format t "in toggled ~A~%" inhibit-flags)))
                  (gtk-box-pack-start control check-button))

                (let ((check-button (gtk-check-button-new-with-label "Idle")))
                  (g-signal-connect check-button "toggled"
                                    (lambda (button)
                                      (if (gtk-toggle-button-active button)
                                          (push :idle inhibit-flags)
                                          (setf inhibit-flags (remove :idle inhibit-flags)))
                                      (gtk-statusbar-push status status-id (format nil "~a" inhibit-flags))
                                      (format t "in toggled ~A~%" inhibit-flags)))
                  (gtk-box-pack-start control check-button))

                (let ((cookie 0)
                      (toggle-button (gtk-toggle-button-new-with-label "Inhibit")))
                  (g-signal-connect toggle-button "toggled"
                                    (lambda (button)
                                      (if (gtk-toggle-button-active button)
                                          (progn
                                            (setf (gtk-button-label button) "Uninhibit")
                                            (format t "call inihibit with ~a~%" inhibit-flags)
                                            (setf cookie
                                                  (gtk-application-inhibit application
                                                                           window
                                                                           inhibit-flags
                                                                           "Application is inhibited")))
                                          (progn
                                            (setf (gtk-button-label button) "Inhibit")
                                            (format t "call unhibit~%")
                                            (gtk-application-uninhibit application cookie)))))
                  (gtk-box-pack-start control toggle-button))

                (gtk-box-pack-start control status)
                (gtk-box-pack-start box view)
                (gtk-box-pack-start box control)
                (gtk-container-add window box))

              ;; Show the application window
              (gtk-widget-show-all window))))

      ;; Connect signal "shutdown" to the application
      (g-signal-connect app "shutdown"
          (lambda (application)
            (declare (ignore application))
            ;; Leave the main loop on shutdown
            (leave-gtk-main)))

      ;; Run the application
      (g-application-run app argv)))
      (join-gtk-main))

