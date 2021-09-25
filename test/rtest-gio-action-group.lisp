(def-suite g-action-group :in gio-suite)
(in-suite g-action-group)

(defparameter *verbose-g-action-group* t)

(defun change-state (action parameter)
  (when *verbose-g-action-group*
    (format t "~%in CHANGE-STATE~%")
    (format t "     action : ~a~%" (g-action-name action))
    (format t "  parameter : ~a~%" (g-variant-boolean parameter)))
  (setf (g-action-state action) parameter))

(defun change-radio-state (action parameter)
  (when *verbose-g-action-group*
    (format t "~%in CHANGE-RADIO-STATE~%")
    (format t "     action : ~a~%" (g-action-name action))
    (format t "  parameter : ~a~%" (g-variant-string parameter)))
  (setf (g-action-state action) parameter))

(defparameter *action-entries*
              (list (list"paste" nil nil nil nil)
                    (list "copy" nil nil nil nil)
                    (list "toolbar" nil "b" "true" #'change-state)
                    (list "statusbar" nil"b" "false" #'change-state)
                    (list "sources" nil "s" "'vala'" #'change-radio-state)
                    (list "markup" nil "s" "'html'" #'change-radio-state)))

;;; --- Types and Values -------------------------------------------------------

;;;     GActionGroup

(test g-action-group-interface
  ;; Type check
  (is (g-type-is-interface "GActionGroup"))
  ;; Check the registered name
  (is (eq 'g-action-group
          (registered-object-type-by-name "GActionGroup")))
  ;; Check the type initializer
  (is (eq (gtype "GActionGroup")
          (gtype (foreign-funcall "g_action_group_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GActionGroup"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GActionGroup"
                                  G-ACTION-GROUP
                                  (:EXPORT T))
             (get-g-type-definition "GActionGroup"))))

;;; --- Signals ----------------------------------------------------------------

;;;     void    action-added              Has Details
;;;     void    action-enabled-changed    Has Details
;;;     void    action-removed            Has Details
;;;     void    action-state-changed      Has Details

;;; --- Functions --------------------------------------------------------------

;;;     g_action_group_list_actions

(test g-action-group-list-actions
  (let ((group (g-simple-action-group-new)))
    (is (equal '() (g-action-group-list-actions group)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    (is (equal '("markup" "paste" "statusbar" "sources" "copy" "toolbar")
               (g-action-group-list-actions group)))))

;;;     g_action_group_query_action

;; not implemented

;;;     g_action_group_has_action

(test g-action-group-has-action
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    (is-true (g-action-group-has-action group "copy"))
    (is-true (g-action-group-has-action group "paste"))
    (is-false (g-action-group-has-action group "unknonw"))))

;;;     g_action_group_get_action_enabled

(test g-action-group-action-enabled
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    (is-true (g-action-group-action-enabled group "copy"))
    (is-true (g-action-group-action-enabled group "paste"))
    (is-false (setf (g-action-enabled (g-action-map-lookup-action group "copy"))
              nil))
    (is-false (g-action-group-action-enabled group "copy"))))

;;;     g_action_group_get_action_parameter_type

(test g-action-group-action-parameter-type
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    ;; FIXME: Does not return nil, null-pointer or a valid g-variant-type value
    (is (typep (g-action-group-action-parameter-type group "copy") 'g-variant-type))
    ;; But this code works as expected, the return value is nil
    (let ((action (g-action-map-lookup-action group "copy")))
      (is-false (g-action-parameter-type action)))
    ;; This is the expected case.
    (is (typep (g-action-group-action-parameter-type group "markup")
               'g-variant-type))
    (is (string= "s"
                 (g-variant-type-dup-string
                   (g-action-group-action-parameter-type group "markup"))))))

;;;     g_action_group_get_action_state_type

(test g-action-group-action-state-type
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    ;; FIXME: Does not return nil, null-pointer or a valid g-variant-type value
    (is (typep (g-action-group-action-state-type group "copy") 'g-variant-type))
    ;; But this code works as expected, the return value is nil
    (let ((action (g-action-map-lookup-action group "copy")))
      (is-false (g-action-state-type action)))
    ;; This is the expected case.
    (is (typep (g-action-group-action-state-type group "toolbar")
               'g-variant-type))
    (is (string= "b"
                 (g-variant-type-dup-string
                   (g-action-group-action-parameter-type group "toolbar"))))))

;;;     g_action_group_get_action_state_hint

;; TODO: Create an example for using a state hint

(test g-action-group-action-state-hint
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    ;; We get a null-pointer
    (is (null-pointer-p (g-action-group-action-state-hint group "copy")))
    (let ((action (g-action-map-lookup-action group "copy")))
      (is (null-pointer-p (g-action-state-hint action))))
    ;; We get a null-pointer
    (is (null-pointer-p (g-action-group-action-state-hint group "toolbar")))
    (let ((action (g-action-map-lookup-action group "toolbar")))
      (is (null-pointer-p (g-action-state-hint action))))
    ;; We get a null-pointer
    (is (null-pointer-p (g-action-group-action-state-hint group "sources")))
    (let ((action (g-action-map-lookup-action group "sources")))
      (is (null-pointer-p (g-action-state-hint action))))))

;;;     g_action_group_get_action_state

(test g-action-group-action-state
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    (is (null-pointer-p (g-action-group-action-state group "copy")))
    (is (null-pointer-p (g-action-group-action-state group "paste")))
    (is-true (g-variant-boolean (g-action-group-action-state group "toolbar")))
    (is-false (g-variant-boolean (g-action-group-action-state group "statusbar")))
    (is (string= "vala"
                 (g-variant-string (g-action-group-action-state group "sources"))))
    (is (string= "html"
                 (g-variant-string (g-action-group-action-state group "markup"))))))

;;;     g_action_group_change_action_state

(test g-action-group-action-change-action-state
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))
    ;; Change a boolean state
    (is-true (g-variant-boolean (g-action-group-action-state group "toolbar")))
    (is-false (g-action-group-change-action-state group "toolbar"
                                                  (g-variant-new-boolean nil)))
    (is-false (g-variant-boolean (g-action-group-action-state group "toolbar")))
    ;; Change a string state
    (is (string= "vala"
                 (g-variant-string (g-action-group-action-state group "sources"))))
    (is-false (g-action-group-change-action-state group "sources"
                                                  (g-variant-new-string "new value")))
    (is (string= "new value"
                 (g-variant-string (g-action-group-action-state group "sources"))))))

;;;     g_action_group_activate_action

(test g-action-group-activate-action
  (let ((group (g-simple-action-group-new)))
    (is-false (g-action-map-add-action-entries group *action-entries*))

    (is-false (g-action-group-activate-action group "copy" (null-pointer)))
    (is-false (g-action-group-activate-action group "toolbar"
                                                    (g-variant-new-boolean t)))
    (is-false (g-action-group-activate-action group "sources"
                                              (g-variant-new-string "new value")))))

;;;     g_action_group_action_added
;;;     g_action_group_action_removed
;;;     g_action_group_action_enabled_changed
;;;     g_action_group_action_state_changed

;;; 2021-9-8
