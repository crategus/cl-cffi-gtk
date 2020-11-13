(def-suite gtk-action-group :in gtk-suite)
(in-suite gtk-action-group)

(defvar *verbose-gtk-action-group* nil)

;;;   GtkActionGroup

(test gtk-action-group-class
  ;; Type check
  (is (g-type-is-object "GtkActionGroup"))
  ;; Check the registered name
  (is (eq 'gtk-action-group
          (registered-object-type-by-name "GtkActionGroup")))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GtkActionGroup")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkActionGroup"))))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkActionGroup"))))
  ;; Check the class properties
  (is (equal '("accel-group" "name" "sensitive" "visible")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkActionGroup"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkActionGroup" GTK-ACTION-GROUP
                       (:SUPERCLASS G-OBJECT
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_action_group_get_type")
                       ((ACCEL-GROUP GTK-ACTION-GROUP-ACCEL-GROUP "accel-group"
                         "GtkAccelGroup" T T)
                        (NAME GTK-ACTION-GROUP-NAME "name" "gchararray" T NIL)
                        (SENSITIVE GTK-ACTION-GROUP-SENSITIVE "sensitive"
                         "gboolean" T T)
                        (VISIBLE GTK-ACTION-GROUP-VISIBLE "visible" "gboolean"
                         T T)))
             (get-g-type-definition "GtkActionGroup"))))

;;; --- Access the properties --------------------------------------------------

;;;   gtk-action-group-accel-group

(test gtk-action-group-accel-group
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-false (gtk-action-group-accel-group group))
    (setf (gtk-action-group-accel-group group) (gtk-accel-group-new))
    (is (eq 'gtk-accel-group
            (type-of (gtk-action-group-accel-group group))))))

;;;   gtk-action-group-name

(test gtk-action-group-name
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is (equal "AppWindowActions" (gtk-action-group-name group)))))

;;;   gtk-action-group-sensitive

(test gtk-action-group-sensitive
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-true (gtk-action-group-sensitive group))
    (setf (gtk-action-group-sensitive group) nil)
    (is-false (gtk-action-group-sensitive group))))

;;;   gtk-action-group-visible

(test gtk-action-group-visible
  (let ((group (gtk-action-group-new "AppWindowActions")))
    (is-true (gtk-action-group-visible group))
    (setf (gtk-action-group-visible group) nil)
    (is-false (gtk-action-group-visible group))))

;;; --- Check functions --------------------------------------------------------

;;;   gtk-action-group-new

(test gtk-action-group-new
  (is (eq 'gtk-action-group
          (type-of (gtk-action-group-new "AppWindowActions")))))

;;;   gtk-action-group-get-action

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
               (gtk-action-name (gtk-action-group-get-action group "Open"))))))

;;;   gtk-action-group-list-actions

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
    (is (equal '("Close" "Save" "Open")
               (mapcar #'gtk-action-name
                       (gtk-action-group-list-actions group))))))

;;;   gtk-action-group-add-action
;;;   gtk-action-group-add-action-with-accel

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

;;;   gtk-action-group-remove-action

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
    (is (equal '("Close" "Save" "Open")
               (mapcar #'gtk-action-name
                       (gtk-action-group-list-actions group))))
    (gtk-action-group-remove-action group
                                    (gtk-action-group-get-action group "Open"))
    (is (equal '("Close" "Save")
               (mapcar #'gtk-action-name
                       (gtk-action-group-list-actions group))))))

;;;    GtkActionEntry

;;;    gtk-action-group-add-actions
;;;    gtk-action-group-add-actions-full

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
               (mapcar #'gtk-action-name
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
               (mapcar #'gtk-action-name
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
               (mapcar #'gtk-action-name
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
               (mapcar #'gtk-action-name
                       (gtk-action-group-list-actions group))))
    (setf message nil)
    (is-false (gtk-action-activate (gtk-action-group-get-action group "Open")))
    (is (equal "in Signal activate" message))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

;;;    GtkToggleActionEntry
;;;
;;;    gtk-action-group-add-toggle-actions
;;;    gtk-action-group-add-toggle-actions-full

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
               (mapcar #'gtk-action-name
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
               (mapcar #'gtk-action-name
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
               (mapcar #'gtk-action-name
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
               (mapcar #'gtk-action-name
                       (gtk-action-group-list-actions group))))
    (setf message nil)
    (is-false (gtk-action-activate (gtk-action-group-get-action group "Open")))
    (is (equal "in Signal activate" message))
    (is (equal "<Actions>/AppWindowActions/Open"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Open"))))))

;;;    GtkRadioActionEntry
;;;
;;;    gtk-action-group-add-radio-actions
;;;    gtk-action-group-add-radio-actions-full

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
    (is (equal '("Red" "Blue" "Green")
               (mapcar #'gtk-action-name
                       (gtk-action-group-list-actions group))))
    (is (equal "<Actions>/AppWindowActions/Red"
               (gtk-action-get-accel-path
                 (gtk-action-group-get-action group "Red"))))))

;;;    gtk_action_group_set_translate_func
;;;    gtk_action_group_set_translation_domain
;;;    gtk_action_group_translate_string

