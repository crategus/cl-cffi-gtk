(def-suite g-simple-action-group :in gio-suite)
(in-suite g-simple-action-group)

;;;   GSimpleActionGroup

(test g-simple-action-group-class
  ;; Type check
  (is (g-type-is-object "GSimpleActionGroup"))
  ;; Check the registered name
  (is (eq 'g-simple-action-group
          (registered-object-type-by-name "GSimpleActionGroup")))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GSimpleActionGroup")))
  ;; Check the children
;  TODO: In a second run we have the child "GApplicationExportedActions
;  (is (equal '()
;             (mapcar #'g-type-name (g-type-children "GSimpleActionGroup"))))
  ;; Check the interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (mapcar #'g-type-name (g-type-interfaces "GSimpleActionGroup"))))
  ;; Check the class properties
  (is (equal '()
              (list-class-property-names "GSimpleActionGroup")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GSimpleActionGroup" G-SIMPLE-ACTION-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GActionGroup" "GActionMap"))
                       NIL)
             (get-g-type-definition "GSimpleActionGroup"))))

;;; --- Functions --------------------------------------------------------------

;;;   g_simple_action_group_new

(test g-simple-action-group-new
  (is (eq 'g-simple-action-group (type-of (g-simple-action-group-new)))))

;;;   g_simple_action_group_lookup

(test g-simple-action-group-lookup
  (let ((action-group (g-simple-action-group-new)))
    (is-false (g-simple-action-group-lookup action-group "action"))))

;;;   g_simple_action_group_insert

(test g-simple-action-group-insert
  (let ((group (g-simple-action-group-new)))
    (g-simple-action-group-insert group
                                  (g-simple-action-new "simple"
                                                       (g-variant-type-new "b")))
    (is (eq 'g-simple-action
            (type-of (g-simple-action-group-lookup group "simple"))))))

;;;   g_simple_action_group_remove

(test g-simple-action-group-insert
  (let ((group (g-simple-action-group-new)))
    (g-simple-action-group-insert group
                                  (g-simple-action-new "simple"
                                                       (g-variant-type-new "b")))
    (is (eq 'g-simple-action
            (type-of (g-simple-action-group-lookup group "simple"))))
    (g-simple-action-group-remove group "simple")
    (is-false (g-simple-action-group-lookup group "simple"))))

;;;   g_simple_action_group_add_entries

(test g-simple-action-group-add-entries
  (let ((group (g-simple-action-group-new))
        (entries '(("copy"       ; name
                    nil          ; activate callback
                    nil          ; g-variant-type string
                    nil          ; g-variant
                    nil          ; change-state callback
                   )
                   ("paste" nil nil nil nil))))
    (g-simple-action-group-add-entries group entries)
    (is (typep (g-simple-action-group-lookup group "copy") 'g-simple-action))
    (is (typep (g-simple-action-group-lookup group "paste") 'g-simple-action))))

;;; 2021-10-18
