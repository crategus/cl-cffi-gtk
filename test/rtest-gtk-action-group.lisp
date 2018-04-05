(in-package :gtk-testsuite)

(def-suite gtk-action-group :in gtk-suite)
(in-suite gtk-action-group)

(defvar *verbose-gtk-action-group* nil)

;;;   GtkActionGroup

;;;   gtk_action_group_new

(test gtk-action-group-new
  (is (eq 'gtk-action-group
          (type-of (gtk-action-group-new "AppWindowActions")))))

;;;   gtk_action_group_get_name

(test gtk-action-group-get-name
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is (equal "AppWindowActions" (gtk-action-group-get-name group)))))

;;;   gtk_action_group_get_sensitive

(test gtk-action-group-get-sensitive
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-true (gtk-action-group-get-sensitive group))))

;;;   gtk_action_group_set_sensitive

(test gtk-action-group-set-sensitive
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-false (gtk-action-group-set-sensitive group nil))
    (is-false (gtk-action-group-get-sensitive group))))

;;;   gtk_action_group_get_visible

(test gtk-action-group-get-visible
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-true (gtk-action-group-get-visible group))))

;;;   gtk_action_group_set_visible

(test gtk-action-group-set-visible
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-false (gtk-action-group-set-visible group nil))
    (is-false (gtk-action-group-get-visible group))))

;;;   gtk_action_group_get_accel_group

(test gtk-action-group-get-accel-group
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-false (gtk-action-group-get-accel-group group))))

;;;   gtk_action_group_set_accel_group

(test gtk-action-group-set-accel-group
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is (eq 'gtk-accel-group
            (type-of (gtk-action-group-set-accel-group group
                                                       (gtk-accel-group-new)))))
    (is (eq 'gtk-accel-group
            (type-of (gtk-action-group-get-accel-group group))))))

;;;   gtk_action_group_get_action

(test gtk-action-group-get-action
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"
                                                :label "_Open"
                                                :tooltip "Open a file"
                                                :stock-id "gtk-stock-open"))
    (is (eq 'gtk-action
             (type-of (gtk-action-group-get-action group "Open"))))
    (is (equal "Open"
               (gtk-action-get-name (gtk-action-group-get-action group
                                                                 "Open"))))))

;;;   gtk_action_group_list_actions

(test gtk-action-group-list-actions
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Save"))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Close"))
    (is (equal '("Save" "Open" "Close")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))))

;;;   gtk_action_group_add_action
;;;   gtk_action_group_add_action_with_accel

(test gtk-action-group-add-action.1
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"
                                                :label "_Open"
                                                :tooltip "Open a file"
                                                :stock-id "gtk-stock-open"))
    (is (eq 'gtk-action
             (type-of (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-action.2
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"
                                                :label "_Open"
                                                :tooltip "Open a file"
                                                :stock-id "gtk-stock-open")
                                 "<ctrl>o")
    (is (eq 'gtk-action
             (type-of (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-action.3
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"
                                                :label "_Open"
                                                :tooltip "Open a file"
                                                :stock-id "gtk-stock-open")
                                 nil)
    (is (eq 'gtk-action
             (type-of (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-action.4
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"
                                                :label "_Open"
                                                :tooltip "Open a file"
                                                :stock-id "gtk-stock-open")
                                 (null-pointer))
    (is (eq 'gtk-action
             (type-of (gtk-action-group-get-action group "Open"))))))

;;;   gtk_action_group_remove_action

(test gtk-action-group-remove-action
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Open"))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Save"))
    (gtk-action-group-add-action group
                                 (make-instance 'gtk-action
                                                :name "Close"))
    (is (equal '("Save" "Open" "Close")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (gtk-action-group-remove-action group 
                                    (gtk-action-group-get-action group "Open"))
    (is (equal '("Save" "Close")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))))

;;;    GtkActionEntry

;;;    gtk_action_group_add_actions
;;;    gtk_action_group_add_actions_full

(test gtk-action-group-add-actions.1
  (let ((group (gtk-action-group-new "AppWindowActions"))
        (actions '(("Open"            ; name
                    "gtk-stock-open"  ; stock-id
                    "_Open"           ; label
                    "<ctrl>o"         ; accelerator
                    "Open a file"     ; tooltip
                    nil               ; callback function
                  ))))
    (gtk-action-group-add-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-actions.2
  (let ((group (gtk-action-group-new "AppWindowActions"))
        (actions '(("Open"            ; name
                    nil               ; stock-id
                    nil               ; label
                    nil               ; accelerator
                    nil               ; tooltip
                    nil               ; callback function
                  ))))
    (gtk-action-group-add-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-actions.3
  (let* ((message nil)
         (group (gtk-action-group-new "AppWindowActions"))
         (func #'(lambda (action)
                   (setf message "in Signal activate")
                   (when *verbose-gtk-action-group*
                     (format t "in Signal activate for ~A~%" action))))
         (actions `(("Open"            ; name
                     nil               ; stock-id
                     nil               ; label
                     nil               ; accelerator
                     nil               ; tooltip
                     ,func             ; callback function
                   ))))
    (gtk-action-group-add-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (setf message nil)
    (is-false (gtk-action-activate (gtk-action-group-get-action group "Open")))
    (is (equal "in Signal activate" message))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-actions.4
  (let* ((message nil)
         (group (gtk-action-group-new "AppWindowActions"))
         (actions (list (list "Open"            ; name
                              nil               ; stock-id
                              nil               ; label
                              nil               ; accelerator
                              nil               ; tooltip
                              ;; callback function
                              (lambda (action)
                                (setf message "in Signal activate")
                                (when *verbose-gtk-action-group*
                                  (format t "in Signal activate for ~A~%"
                                            action)))))))
    (gtk-action-group-add-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (setf message nil)
    (is-false (gtk-action-activate (gtk-action-group-get-action group "Open")))
    (is (equal "in Signal activate" message))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

;;;    GtkToggleActionEntry
;;;
;;;    gtk_action_group_add_toggle_actions
;;;    gtk_action_group_add_toggle_actions_full

(test gtk-action-group-add-toggle-actions.1
  (let ((group (gtk-action-group-new "AppWindowActions"))
        (actions '(("Open"            ; name
                    "gtk-stock-open"  ; stock-id
                    "_Open"           ; label
                    "<ctrl>o"         ; accelerator
                    "Open a file"     ; tooltip
                    nil               ; callback function
                    t                 ; is-active
                  ))))
    (gtk-action-group-add-toggle-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-toggle-actions.2
  (let ((group (gtk-action-group-new "AppWindowActions"))
        (actions '(("Open"            ; name
                    nil               ; stock-id
                    nil               ; label
                    nil               ; accelerator
                    nil               ; tooltip
                    nil               ; callback function
                  ))))
    (gtk-action-group-add-toggle-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-toggle-actions.3
  (let* ((message nil)
         (group (gtk-action-group-new "AppWindowActions"))
         (func #'(lambda (action)
                   (setf message "in Signal activate")
                   (when *verbose-gtk-action-group*
                     (format t "in Signal activate for ~A~%" action))))
         (actions `(("Open"            ; name
                     nil               ; stock-id
                     nil               ; label
                     nil               ; accelerator
                     nil               ; tooltip
                     ,func             ; callback function
                   ))))
    (gtk-action-group-add-toggle-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (setf message nil)
    (is-false (gtk-action-activate (gtk-action-group-get-action group "Open")))
    (is (equal "in Signal activate" message))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

(test gtk-action-group-add-toggle-actions.4
  (let* ((message nil)
         (group (gtk-action-group-new "AppWindowActions"))
         (actions (list (list "Open"            ; name
                              nil               ; stock-id
                              nil               ; label
                              nil               ; accelerator
                              nil               ; tooltip
                              ;; callback function
                              (lambda (action)
                                (setf message "in Signal activate")
                                (when *verbose-gtk-action-group*
                                  (format t "in Signal activate for ~A~%"
                                            action)))
                   ))))
    (gtk-action-group-add-toggle-actions group actions)
    (is (equal '("Open")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (setf message nil)
    (is-false (gtk-action-activate (gtk-action-group-get-action group "Open")))
    (is (equal "in Signal activate" message))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

;;;    GtkRadioActionEntry
;;;
;;;    gtk_action_group_add_radio_actions
;;;    gtk_action_group_add_radio_actions_full


(test gtk-action-group-add-radio-actions.1
  (let ((group (gtk-action-group-new "AppWindowActions"))
        (actions (list
                   (list "Red" nil                      ; name, stock id
                         "_Red" "<control>R"            ; label, accelerator
                         "Blood" 0)                     ; tooltip, value
                   (list "Green" nil                    ; name, stock id
                         "_Green" "<control>G"          ; label, accelerator
                         "Grass" 1)                     ; tooltip, value
                   (list "Blue" nil                     ; name, stock id
                         "_Blue" "<control>B"           ; label, accelerator
                         "Sky" 2))))
    (gtk-action-group-add-radio-actions group actions 0 nil)
    (is (equal '( "Blue" "Red" "Green")
               (mapcar #'gtk-action-get-name
                       (gtk-action-group-list-actions group))))
    (is (equal "<Actions>/AppWindowActions/Red"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Red"))))))

;;;    gtk_action_group_set_translate_func
;;;    gtk_action_group_set_translation_domain
;;;    gtk_action_group_translate_string

