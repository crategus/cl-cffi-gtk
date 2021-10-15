;;;; Example Application Action - 2021-10-7

(in-package :gio-example)

(defun activate-action (app name)
  (let ((ptype (g-action-group-action-parameter-type app name))
        (state (g-action-group-action-state app name))
        (enabled (g-action-group-action-enabled app name)))
    ;; Print information about the action
    (format t "     action name : ~A~%" name)
    (format t "  parameter type : ~A~%" ptype)
    (unless (null-pointer-p state)
      (format t "      state type : ~A~%" (g-variant-type-string state)))
    (format t "           state : ~A~%" state)
    (format t "         enabled : ~A~%" enabled)
    ;; Activate the action
    (g-action-group-activate-action app name state)))

(defun application-action (&rest argv)
  (let ((app (make-instance 'g-application
                            :application-id "com.crategus.application-action"
                            :flags :none)))
    ;; Create the "simple-action" action
    (let ((action (g-simple-action-new "simple-action" nil)))
      ;; Connect a handler to the "activate" signal
      (g-signal-connect action "activate"
          (lambda (action parameter)
            (declare (ignore parameter))
            (format t "Action ~A is activated.~%" (g-action-name action))))
      ;; Add the action to the action map of the application
      (g-action-map-add-action app action))
    ;; Create the "toggle-action" action
    (let ((action (g-simple-action-new-stateful "toggle-action"
                                                "b"
                                                (g-variant-new-boolean nil))))
      ;; Connect a handler to the "activate" signal
      (g-signal-connect action "activate"
          (lambda (action parameter)
            (declare (ignore parameter))
            (format t "Action ~A is activated.~%" (g-action-name action))
            (let ((state (g-variant-boolean (g-action-state action))))
              (if state
                  (setf (g-action-state action) (g-variant-new-boolean nil))
                  (setf (g-action-state action) (g-variant-new-boolean t)))
              (format t "The state changed from ~A to ~A.~%"
                        state
                        (not state)))))
      ;; Add the action to the action map of the application
      (g-action-map-add-action app action))
    ;; Signal handler "activate"
    (g-signal-connect app "activate"
                      (lambda (application)
                        (format t "The application is in activate.~%")
                        ;; Activate the actions and print information
                        (activate-action application "simple-action")
                        (activate-action application "toggle-action")))
    ;; Run the application
    (g-application-run app argv)))
