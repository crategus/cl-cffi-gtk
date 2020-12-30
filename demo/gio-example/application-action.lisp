;;;; Example Application Action

(in-package :gio-example)

(defun activate-action (app name)
  (let ((param-type (g-action-group-get-action-parameter-type app name))
        (state (g-action-group-get-action-state app name))
        (enabled (g-action-group-get-action-enabled app name)))
    ;; Print information about the action
    (format t "     action name : ~A~%" name)
    (format t "  parameter type : ~A~%" param-type)
    (unless (null-pointer-p state)
      (format t "      state type : ~A~%" (g-variant-type-string state)))
    (format t "           state : ~A~%" state)
    (format t "         enabled : ~A~%~%" enabled)
    ;; Activate the action
    (g-action-group-activate-action app name state)))

(defun example-application-action (&optional (argv nil))
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id "com.crategus.application-action"
                              :inactivity-timeout 10000
                              :flags :none)))

      ;; Create the action "simple-action"
      (let ((action (g-simple-action-new "simple-action" nil)))
        ;; Connect a handler to the signal activate
        (g-signal-connect action "activate"
            (lambda (action parameter)
              (declare (ignore parameter))
              (format t "Action ~A is activated.~%~%" (g-action-name action))))
        ;; Add the action to the action map of the application
        (g-action-map-add-action app action))

      ;; Create the action "toggle-action"
      (let ((action (g-simple-action-new-stateful "toggle-action"
                                                  (g-variant-type-new "b")
                                                  (g-variant-new-boolean nil))))
        ;; Connect a handler to the signal activate
        (g-signal-connect action "activate"
            (lambda (action parameter)
              (declare (ignore parameter))
              (format t "Action ~A is activated.~%" (g-action-name action))
              (let ((state (g-variant-boolean (g-action-state action))))
                (if state
                    (setf (g-simple-action-state action)
                          (g-variant-new-boolean nil))
                    (setf (g-simple-action-state action)
                          (g-variant-new-boolean t)))
                (format t "The state changed from ~A to ~A.~%~%"
                          state
                          (not state)))))
        ;; Add the action to the action map of the application
        (g-action-map-add-action app action))

      ;; Signal handler "activate"
      (g-signal-connect app "activate"
                        (lambda (application)
                          (format t "The application is in activate.~%~%")
                          ;; Activate the actions and print information
                          (activate-action application "simple-action")
                          (activate-action application "toggle-action")))

      ;; Signal handler "shutdown"
      (g-signal-connect app "shutdown"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "The application is in shutdown.~%")
                          ;; Stop the main loop
                          (leave-gtk-main)))

      ;; Start the application
      (g-application-run app argv))))

;;; 2020-12-10
