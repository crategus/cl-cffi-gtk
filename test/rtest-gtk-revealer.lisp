(def-suite gtk-revealer :in gtk-suite)
(in-suite gtk-revealer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRevealerClass
;;;     GtkRevealerTransitionType

;;;     GtkRevealer

(test gtk-revealer-class
  ;; Type check
  (is (g-type-is-object "GtkRevealer"))
  ;; Check the registered name
  (is (eq 'gtk-revealer
          (registered-object-type-by-name "GtkRevealer")))
  ;; Check the type initializer
  (is (eq (gtype "GtkRevealer")
          (gtype (foreign-funcall "gtk_revealer_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBin")
          (g-type-parent "GtkRevealer")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkRevealer"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkRevealer"))))
  ;; Check the class properties
  (is (equal '("child-revealed" "reveal-child" "transition-duration"
               "transition-type")
             (list-class-property-names "GtkRevealer")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-class-style-property-names "GtkRevealer")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-class-child-property-names "GtkRevealer")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkRevealer" GTK-REVEALER
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_revealer_get_type")
                       ((CHILD-REVEALED GTK-REVEALER-CHILD-REVEALED
                         "child-revealed" "gboolean" T NIL)
                        (REVEAL-CHILD GTK-REVEALER-REVEAL-CHILD "reveal-child"
                         "gboolean" T T)
                        (TRANSITION-DURATION GTK-REVEALER-TRANSITION-DURATION
                         "transition-duration" "guint" T T)
                        (TRANSITION-TYPE GTK-REVEALER-TRANSITION-TYPE
                         "transition-type" "GtkRevealerTransitionType" T T)))
             (get-g-type-definition "GtkRevealer"))))

;;; --- Properties -------------------------------------------------------------

;;;     child-revealed
;;;     reveal-child
;;;     transition-duration
;;;     transition-type

;;; --- Functions --------------------------------------------------------------

;;;     gtk_revealer_new
;;;     gtk_revealer_get_reveal_child                      Accessor
;;;     gtk_revealer_set_reveal_child                      Accessor
;;;     gtk_revealer_get_child_revealed                    Accessor
;;;     gtk_revealer_get_transition_duration               Accessor
;;;     gtk_revealer_set_transition_duration               Accessor
;;;     gtk_revealer_get_transition_type                   Accessor
;;;     gtk_revealer_set_transition_type                   Accessor

;;; 2021-10-18
