(def-suite g-property-action :in gio-suite)
(in-suite g-property-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GPropertyAction

(test g-property-action-class
  ;; Type check
  (is (g-type-is-object "GPropertyAction"))
  ;; Check the registered name
  (is (eq 'g-property-action
          (registered-object-type-by-name "GPropertyAction")))
  ;; Check the type initializer
  (is (eq (gtype "GPropertyAction")
          (gtype (foreign-funcall "g_property_action_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GPropertyAction")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GPropertyAction"))))
  ;; Check the interfaces
  (is (equal '("GAction")
             (mapcar #'g-type-name (g-type-interfaces "GPropertyAction"))))
  ;; Check the class properties
  (is (equal '("enabled" "invert-boolean" "name" "object" "parameter-type"
               "property-name" "state" "state-type")
             (list-class-property-names "GPropertyAction")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GPropertyAction" G-PROPERTY-ACTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES ("GAction"))
                       ((ENABLED G-PROPERTY-ACTION-ENABLED "enabled" "gboolean"
                         T NIL)
                        (INVERT-BOOLEAN G-PROPERTY-ACTION-INVERT-BOOLEAN
                         "invert-boolean" "gboolean" T NIL)
                        (NAME G-PROPERTY-ACTION-NAME "name" "gchararray" T NIL)
                        (OBJECT G-PROPERTY-ACTION-OBJECT "object" "GObject" NIL
                         NIL)
                        (PARAMETER-TYPE G-PROPERTY-ACTION-PARAMETER-TYPE
                         "parameter-type" "GVariantType" T NIL)
                        (PROPERTY-NAME G-PROPERTY-ACTION-PROPERTY-NAME
                         "property-name" "gchararray" NIL NIL)
                        (STATE G-PROPERTY-ACTION-STATE "state" "GVariant" T
                         NIL)
                        (STATE-TYPE G-PROPERTY-ACTION-STATE-TYPE "state-type"
                         "GVariantType" T NIL)))
             (get-g-type-definition "GPropertyAction"))))

;;; --- Properties -------------------------------------------------------------

(test g-property-action-properties
  (let* ((button (make-instance 'gtk-button))
         (action (g-property-action-new "action" button "visible")))
    ;; enabled
    (is-true (g-property-action-enabled action))
    (signals (error) (setf (g-property-action-enabled action) nil))
    ;; invert-boolean
    (is-false (g-property-action-invert-boolean action))
    ;; name
    (is (string= "action" (g-property-action-name action)))
    ;;object is not readable
    (signals (error) (g-property-action-object action))

    ;; parameter-type
    ;; property-name is not readable
    ;; state
    ;; state-type is null-pointer, that causes an error
))

;;; --- Functions --------------------------------------------------------------

;;;     g_property_action_new

(test g-property-action-new
  (let ((button (make-instance 'gtk-button)))
    (is (typep (g-property-action-new "action" button "visible")
                'g-property-action))))

;;; 2021-10-18
