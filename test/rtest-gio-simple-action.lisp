(in-package :gtk-testsuite)

(def-suite gio-simple-action :in gio-suite)
(in-suite gio-simple-action)

(defvar *gio-simple-action-verbose* nil)

;;;   GSimpleAction

(test g-simple-action-class
  ;; Type checks
  (is-true  (g-type-is-object "GSimpleAction"))
  (is-false (g-type-is-abstract "GSimpleAction"))
  (is-true  (g-type-is-derived "GSimpleAction"))
  (is-false (g-type-is-fundamental "GSimpleAction"))
  (is-true  (g-type-is-value-type "GSimpleAction"))
  (is-true  (g-type-has-value-table "GSimpleAction"))
  (is-true  (g-type-is-classed "GSimpleAction"))
  (is-true  (g-type-is-instantiatable "GSimpleAction"))
  (is-true  (g-type-is-derivable "GSimpleAction"))
  (is-true  (g-type-is-deep-derivable "GSimpleAction"))
  (is-false (g-type-is-interface "GSimpleAction"))

  ;; Check the registered name
  (is (eq 'g-simple-action
          (registered-object-type-by-name "GSimpleAction")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GSimpleAction"))))
    (is (equal (gtype "GSimpleAction") (g-type-from-class class)))
    (is (equal (gtype "GSimpleAction") (g-object-class-type class)))
    (is (equal "GSimpleAction" (g-object-class-name class)))
    (is (equal (gtype "GSimpleAction") (g-type-from-class  (g-type-class-peek "GSimpleAction"))))
    (is (equal (gtype "GSimpleAction") (g-type-from-class  (g-type-class-peek-static "GSimpleAction"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'g-simple-action)))
    ;; Check the class name and type of the class
    (is (eq 'g-simple-action (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GSimpleAction" (gobject-class-g-type-name class)))
    (is (equal "GSimpleAction" (gobject-class-direct-g-type-name class)))
    (is (equal "g_simple_action_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GSimpleAction")))
  (is (= 2 (g-type-depth "GSimpleAction")))
  (is (equal (gtype "GSimpleAction")
             (g-type-next-base "GSimpleAction" "GObject")))
  (is-true  (g-type-is-a "GSimpleAction" "GObject"))
  (is-false (g-type-is-a "GSimpleAction" "GtkWidget"))
  (is-false (g-type-is-a "GSimpleAction" "gboolean"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GSimpleAction"))))
  ;; Check the interfaces
  (is (equal '("GAction")
             (mapcar #'gtype-name (g-type-interfaces "GSimpleAction"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GSimpleAction" query)
    (is (equal (gtype "GSimpleAction")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GSimpleAction"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 136
           (foreign-slot-value query '(:struct g-type-query) :class-size))))

  ;; Get the names of the class properties.
  (is (equal '("name" "parameter-type" "enabled" "state-type" "state")
             (mapcar #'param-spec-name (g-object-class-list-properties "GSimpleAction"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GSimpleAction" G-SIMPLE-ACTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES ("GAction"))
                       ((ENABLED G-SIMPLE-ACTION-ENABLED "enabled" "gboolean" T
                         T)
                        (NAME G-SIMPLE-ACTION-NAME "name" "gchararray" T NIL)
                        (PARAMETER-TYPE G-SIMPLE-ACTION-PARAMETER-TYPE
                         "parameter-type" "GVariantType" T NIL)
                        (STATE G-SIMPLE-ACTION-STATE "state" "GVariant" T T)
                        (STATE-TYPE G-SIMPLE-ACTION-STATE-TYPE "state-type"
                         "GVariantType" T NIL)))
             (get-g-type-definition "GSimpleAction"))))

;;; --- Call functions ---------------------------------------------------------

;;;   g_simple_action_new

(test g-simple-action-new
  (let ((action (g-simple-action-new "simple" (g-variant-type-new "b"))))
    (is (eq 'g-simple-action (type-of action)))
    (is (equal "simple" (g-action-get-name action)))
    (is (eq 'g-variant-type (type-of (g-action-get-parameter-type action))))
    (is (equal "b" (g-variant-type-peek-string (g-action-get-parameter-type action))))))

;;;   g_simple_action_new_stateful

(test g-simple-action-new-stateful
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    (is (eq 'g-simple-action (type-of action)))
    (is (equal "simple" (g-action-get-name action)))
    (is (eq 'g-variant-type (type-of (g-action-get-parameter-type action))))
    (is (equal "b" (g-variant-type-peek-string (g-action-get-parameter-type action))))
    (is-true (g-variant-get-boolean (g-action-get-state action)))))

;;; --- Functions from the interface -------------------------------------------

(test g-simple-action-interface-functions
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;; g-action-get-name
    (is (equal "simple" (g-action-get-name action)))
    ;; g-action-get-parameter-type
    (is (eq 'g-variant-type (type-of (g-action-get-parameter-type action))))
    ;; g-action-get-state-type
    (is (eq 'g-variant-type (type-of (g-action-get-state-type action))))
    ;; We have to initialize the state with a g-variant-array to set a hint
    (is-true (null-pointer-p (g-action-get-state-hint action)))
    ;; g-action-get-enabled
    (is-true (g-action-get-enabled action))
    ;; g-action-get-state
    (is-true (g-variant-get-boolean (g-action-get-state action)))))

;;;   g_action_change_state
;;;   g_action_activate

(test g-simple-action-signals
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (when *gio-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'activate' occured.~%")
           (format t "~&    action    : ~A~%" action)
           (format t "~&    name      : ~A~%" (g-action-get-name action))
           (format t "~&    parameter : ~A~%" parameter))))
    (g-signal-connect action "change-state"
       (lambda (action value)
         (g-simple-action-set-state action value)
         (when *gio-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'change-state' occured.~%")
           (format t "~&    action : ~A~%" action)
           (format t "~&    name   : ~A~%" (g-action-get-name action))
           (format t "~&    value  : ~A~%" value))))
    ;; g-action-change-state
    (g-action-change-state action (g-variant-new-boolean nil))
    (is-false (g-variant-get-boolean (g-action-get-state action)))
    ;; g-action-activate
    (g-action-activate action (g-variant-new-boolean t))
))

;;;   g_simple_action_set_enabled

(test g-simple-action-set-enabled
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (when *gio-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'activate' occured.~%")
           (format t "~&    action    : ~A~%" action)
           (format t "~&    name      : ~A~%" (g-action-get-name action))
           (format t "~&    parameter : ~A~%" parameter))))
    (g-signal-connect action "change-state"
       (lambda (action value)
         (g-simple-action-set-state action value)
         (when *gio-simple-action-verbose*
           (format t "~&GSimpleAction: signal 'change-state' occured.~%")
           (format t "~&    action : ~A~%" action)
           (format t "~&    name   : ~A~%" (g-action-get-name action))
           (format t "~&    value  : ~A~%" value))))
    
    (is-true (g-simple-action-set-enabled action  t))
    (g-signal-emit action "activate" (g-variant-new-boolean nil))
    ;; TODO: This seems not to work. We can activate the action.
    (is-false (g-simple-action-set-enabled action nil))
    (is-false (g-action-get-enabled action))
    (g-signal-emit action "activate" (g-variant-new-boolean nil))
))

;;;   g_simple_action_set_state

(test g-simple-action-set-state
  (let ((action (g-simple-action-new-stateful "simple"
                                              (g-variant-type-new "b")
                                              (g-variant-new-boolean t))))
    ;;g-simple-action-set-state
    (g-simple-action-set-state action (g-variant-new-boolean nil))
    (is-false (g-variant-get-boolean (g-action-get-state action)))
    (g-simple-action-set-state action (g-variant-new-boolean t))
    (is-true (g-variant-get-boolean (g-action-get-state action)))))

;;;   Example from the API documentation

(test change-volume-state
  (let ((action (g-simple-action-new-stateful "volume"
                                              (g-variant-type-new "i") ; int32
                                              (g-variant-new-int32 0))))
    (g-signal-connect action "change-state"
                      (lambda (simple-action value)
                        (let ((requested (g-variant-get-int32 value)))
                          ;; Volume only goes from 0 to 10
                          (when (and (>= requested 0) (<= requested 10))
                            (g-simple-action-set-state simple-action value)))))
    ;; Emit the "change-state" signal on action
    (g-action-change-state action (g-variant-new-int32 5))
    (is (= 5 (g-variant-get-int32 (g-action-get-state action))))
    ;; The value is > 10.
    (g-action-change-state action (g-variant-new-int32 20))
    ;; The state has not changed.
    (is (= 5 (g-variant-get-int32 (g-action-get-state action))))
))


#|
   change_volume_state (GSimpleAction *action,
                        GVariant      *value,
                        gpointer       user_data)
   {
     gint requested;
     requested = g_variant_get_int32 (value);

     // Volume only goes from 0 to 10
     if (0 <= requested && requested <= 10)
       g_simple_action_set_state (action, value);
   @}
|#



