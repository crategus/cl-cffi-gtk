(def-suite gobject-binding :in gobject-suite)
(in-suite gobject-binding)

;;; --- Types and Values -------------------------------------------------------

;;;     g-binding-flags

(test gtk-binding-flags
  ;; Check the type
  (is (g-type-is-flags "GBindingFlags"))
  ;; Check the registered name
  (is (eq 'g-binding-flags
          (gobject::registered-flags-type "GBindingFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GBindingFlags")
          (gtype (foreign-funcall "g_binding_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("G_BINDING_DEFAULT" "G_BINDING_BIDIRECTIONAL"
               "G_BINDING_SYNC_CREATE" "G_BINDING_INVERT_BOOLEAN")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GBindingFlags"))))
  ;; Check the values
  (is (equal '(0 1 2 4)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GBindingFlags"))))
  ;; Check the nick names
  (is (equal '("default" "bidirectional" "sync-create" "invert-boolean")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GBindingFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GBindingFlags"
                              G-BINDING-FLAGS
                              (:EXPORT T)
                              (:DEFAULT 0)
                              (:BIDIRECTIONAL 1)
                              (:SYNC-CREATE 2)
                              (:INVERT-BOOLEAN 4))
             (gobject::get-g-type-definition "GBindingFlags"))))

;;;     g-binding

(test g-binding-class
  ;; Type check
  (is (g-type-is-object "GBinding"))
  ;; Check the registered name
  (is (eq 'g-binding
          (registered-object-type-by-name "GBinding")))
  ;; Check the type initializer
  (is (eq (gtype "GBinding")
          (gtype (foreign-funcall "g_binding_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GBinding")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GBinding"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GBinding"))))
  ;; Check the class properties
  (is (equal '("flags" "source" "source-property" "target" "target-property")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GBinding"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GBinding" G-BINDING
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                       ((FLAGS G-BINDING-FLAGS "flags" "GBindingFlags" T NIL)
                        (SOURCE G-BINDING-SOURCE "source" "GObject" T NIL)
                        (SOURCE-PROPERTY G-BINDING-SOURCE-PROPERTY
                         "source-property" "gchararray" T NIL)
                        (TARGET G-BINDING-TARGET "target" "GObject" T NIL)
                        (TARGET-PROPERTY G-BINDING-TARGET-PROPERTY
                         "target-property" "gchararray" T NIL)))
             (get-g-type-definition "GBinding"))))

;;; --- Properties -------------------------------------------------------------

(test g-binding-properties
  (let* ((toggle (make-instance 'gtk-toggle-button))
         (revealer (make-instance 'gtk-revealer))
         (binding (g-object-bind-property toggle "active"
                                          revealer "reveal-child"
                                          :default)))
    (is (equal '() (g-binding-flags binding)))
    (is (eq (gtype "GtkToggleButton") (g-object-type (g-binding-source binding))))
    (is (string= "active" (g-binding-source-property binding)))
    (is (eq (gtype "GtkRevealer") (g-object-type (g-binding-target binding))))
    (is (string= "reveal-child" (g-binding-target-property binding)))))

;;; --- Functions --------------------------------------------------------------

;;;     g-binding-unbind
;;;     g-object-bind-property

(test g-object-bind-property
  (let* ((toggle (make-instance 'gtk-toggle-button))
         (revealer (make-instance 'gtk-revealer))
         (binding (g-object-bind-property toggle "active"
                                          revealer "reveal-child"
                                          :sync-create)))
    (is (eq (gtype "GBinding") (g-object-type binding)))
    (is (equal '(:sync-create) (g-binding-flags binding)))
    ;; Default values are false
    (is-false (gtk-toggle-button-active toggle))
    (is-false (gtk-revealer-reveal-child revealer))
    ;; Set toogle active true
    (is-true (setf (gtk-toggle-button-active toggle) t))
    ;; reveal-child is true
    (is-true (gtk-revealer-reveal-child revealer))
    ;; Unbind the binding
    (is-false (g-binding-unbind binding))))

;;;     GBindingTransformFunc
;;;     g_object_bind_property_full
;;;     g_object_bind_property_with_closures

;;; 2020-11-7
