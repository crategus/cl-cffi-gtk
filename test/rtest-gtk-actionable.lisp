(def-suite gtk-actionable :in gtk-suite)
(in-suite gtk-actionable)

;;; --- GtkActionable ----------------------------------------------------------

(test gtk-actionable-interface
  ;; Type checks
  (is-false (g-type-is-object "GtkActionable"))
  (is-false (g-type-is-abstract "GtkActionable"))
  (is-true  (g-type-is-derived "GtkActionable"))
  (is-false (g-type-is-fundamental "GtkActionable"))
  (is-true  (g-type-is-value-type "GtkActionable"))
  (is-true  (g-type-has-value-table "GtkActionable"))
  (is-false (g-type-is-classed "GtkActionable"))
  (is-false (g-type-is-instantiatable "GtkActionable"))
  (is-true  (g-type-is-derivable "GtkActionable"))
  (is-false (g-type-is-deep-derivable "GtkActionable"))
  (is-true  (g-type-is-interface "GtkActionable"))

  ;; Check the registered name
  (is (eq 'gtk-actionable
          (registered-object-type-by-name "GtkActionable")))

  ;; Check infos about the C class implementation
  ;; An interace has no C class implementation

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-actionable)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-actionable (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkActionable" (gobject-class-g-type-name class)))
    (is (equal "GtkActionable" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_actionable_get_type"
               (gobject-class-g-type-initializer class)))
    (is-true  (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (eq (gtype "GInterface") (g-type-parent "GtkActionable")))
  (is (= 2 (g-type-depth "GtkActionable")))
  (is (eq (gtype "GtkActionable")
             (g-type-next-base "GtkActionable" "GInterface")))
  (is-true  (g-type-is-a "GtkActionable" "GObject"))
  (is-true  (g-type-is-a "GtkActionable" "GInterface"))
  (is-true  (g-type-is-a "GtkActionable" "GInitiallyUnowned"))
  (is-false (g-type-is-a "GtkActionable" "gboolean"))
  (is-false (g-type-is-a "GtkActionable" "GtkWindow"))

  ;; Query infos about the class
  ;; No query infos for an interface

  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkActionable"))))

  ;; Check the interfaces
  ;; No interfaces

  ;; Check the interface properties
  (is (equal '("action-name" "action-target")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-interface-list-properties "GtkActionable"))
                          #'string-lessp)))

  ;; Check the style properties
  ;; No style properties

  ;; Check the names of the child properties
  ;; No child properties

  ;; Check the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkActionable"
    GTK-ACTIONABLE
    (:EXPORT T :TYPE-INITIALIZER "gtk_actionable_get_type")
  (ACTION-NAME GTK-ACTIONABLE-ACTION-NAME "action-name" "gchararray" T T)
  (ACTION-TARGET GTK-ACTIONABLE-ACTION-TARGET "action-target" "GVariant" T T))
             (get-g-type-definition "GtkActionable")))
)

;;; ----------------------------------------------------------------------------
;;; Properties and Accessors
;;; ----------------------------------------------------------------------------

(test gtk-actionable-properties
  (let ((button (make-instance 'gtk-button)))
    ;; Default is false
    (is-false (gtk-actionable-action-name button))
    ;; Default is null-pointer
    (is-true (null-pointer-p (gtk-actionable-action-target button)))
    ;; Set the name and the target
    (gtk-actionable-set-detailed-action-name button "app::save")
    (is (equal "app"  (gtk-actionable-action-name button)))
    (is (equal "save" (g-variant-string (gtk-actionable-action-target button))))))

;;; ----------------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------------

;;;    gtk_actionable_set_action_target

;;; --- gtk_actionable_set_detailed_action_name --------------------------------

;;; See gtk-actionable-properties for an example
