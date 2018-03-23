(in-package :gtk-testsuite)

(def-suite gio-action :in gio-suite)
(in-suite gio-action)

;;;   GAction

(test g-action
  ;; Type checks
  (is-false (g-type-is-object "GAction"))
  (is-false (g-type-is-abstract "GAction"))
  (is-true  (g-type-is-derived "GAction"))
  (is-false (g-type-is-fundamental "GAction"))
  (is-true  (g-type-is-value-type "GAction"))
  (is-true  (g-type-has-value-table "GAction"))
  (is-false (g-type-is-classed "GAction"))
  (is-false (g-type-is-instantiatable "GAction"))
  (is-true  (g-type-is-derivable "GAction"))
  (is-false (g-type-is-deep-derivable "GAction"))
  (is-true  (g-type-is-interface "GAction"))

  ;; Check the registered name
  (is (eq 'g-action
          (registered-object-type-by-name "GAction")))

  ;; Check infos about the C interface implementation
  (let ((class (g-type-default-interface-ref (gtype "GAction"))))
    (is (equal (gtype "GAction") (g-type-from-interface class)))
    (g-type-default-interface-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'g-action)))
    ;; Check the class name and type of the class
    (is (eq 'g-action (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GAction" (gobject-class-g-type-name class)))
    (is (equal "GAction" (gobject-class-direct-g-type-name class)))
    (is (equal "g_action_get_type"
               (gobject-class-g-type-initializer class)))
    (is-true (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GInterface") (g-type-parent "GAction")))
  (is (= 2 (g-type-depth "GAction")))
  (is (equal (gtype "GAction")
             (g-type-next-base "GAction" "GInterface")))
  (is-true  (g-type-is-a "GAction" "GInterface"))
  (is-false (g-type-is-a "GAction" "GtkWidget"))
  (is-false (g-type-is-a "GAction" "gboolean"))
  (is-false (g-type-is-a "GAction" "GtkWindow"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GAction"))))

  ;; Get the names of the interface properties.
  (is (equal '("enabled" "name" "parameter-type" "state" "state-type")
             (mapcar #'param-spec-name (g-object-interface-list-properties "GAction"))))

  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GAction" G-ACTION
                (:EXPORT T)
                (ENABLED G-ACTION-ENABLED "enabled" "gboolean" T NIL)
                (NAME G-ACTION-NAME "name" "gchararray" T NIL)
                (PARAMETER-TYPE G-ACTION-PARAMETER-TYPE "parameter-type" "GVariantType" T NIL)
                (STATE G-ACTION-STATE "state" "GVariant" T NIL)
                (STATE-TYPE G-ACTION-STATE-TYPE "state-type" "GVariantType" T NIL))
                (get-g-type-definition "GAction"))))

;;;   GActionInterface

;;;   This function are checked in rtest-gio-simple-action.lisp

;;;   g_action_get_name
;;;   g_action_get_parameter_type
;;;   g_action_get_state_type
;;;   g_action_get_state_hint
;;;
;;;   g_action_get_enabled
;;;   g_action_get_state
;;;
;;;   g_action_change_state
;;;   g_action_activate

