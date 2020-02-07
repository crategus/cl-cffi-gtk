(def-suite g-action-map :in gio-suite)
(in-suite g-action-map)

;;; --- Types and Values -------------------------------------------------------

;;;     GActionMap

(test g-action-map-class
  ;; Type check
  (is-true (g-type-is-interface "GActionMap"))
  ;; Check the registered name
  (is (eq 'g-action-map
          (registered-object-type-by-name "GActionMap")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'param-spec-name
                     (g-object-interface-list-properties "GActionMap"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GActionMap"
    G-ACTION-MAP
    (:EXPORT T))
             (get-g-type-definition "GActionMap"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_action_map_lookup_action
;;;     g_action_map_add_action_entries

;; Example in the documentation of g-action-map-add-action-entries

(defun activate-quit (action parameter)
  (declare (ignore action parameter)))

(defun activate-print (action parameter)
  (declare (ignore action parameter)))

(defun create-action-group ()
  (let ((entries (list (list "quit"
                             #'activate-quit)
                       (list "print"
                             #'activate-print
                             "s")))
        (group (g-simple-action-group-new)))
    (g-action-map-add-action-entries group entries)
    group))

(test g-action-map-add-action-entries.2
  (let* ((group (create-action-group))
         (action-quit (g-action-map-lookup-action group "quit"))
         (action-print (g-action-map-lookup-action group "print")))
    (is (eq 'g-simple-action (type-of action-quit)))
    (is (string= "quit" (g-action-name action-quit)))
    ;; Slot parameter-type is not initialized
    (signals (error) (eq 'g-variant-type (type-of (g-action-parameter-type action-quit))))
    (is (eq 'g-simple-action (type-of action-print)))
    (is (string= "print" (g-action-name action-print)))
    ;; Slot parameter-type is initialized with type "s"
    (is (eq 'g-variant-type (type-of (g-action-parameter-type action-print))))))

;;;     g_action_map_add_action
;;;     g_action_map_remove_action

(test g-action-map-add-action
  (let ((group (g-simple-action-group-new)))
    (g-action-map-add-action group (g-simple-action-new "quit" nil))
    (is (string= "quit" (g-action-name (g-action-map-lookup-action group "quit"))))
    (g-action-map-add-action group (g-simple-action-new "close" nil))
    (is (string= "close" (g-action-name (g-action-map-lookup-action group "close"))))
    (g-action-map-remove-action group "quit")
    (is-false (g-action-map-lookup-action group "quit"))))
