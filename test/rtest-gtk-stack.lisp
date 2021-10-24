(def-suite gtk-stack :in gtk-suite)
(in-suite gtk-stack)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackTransitionType

;;;     GtkStack

(test gtk-stack-class
  ;; Type check
  (is (g-type-is-object "GtkStack"))
  ;; Check the registered name
  (is (eq 'gtk-stack
          (registered-object-type-by-name "GtkStack")))
  ;; Check the type initializer
  (is (eq (gtype "GtkStack")
          (gtype (foreign-funcall "gtk_stack_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkContainer")
          (g-type-parent "GtkStack")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkStack"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkStack"))))
  ;; Check the class properties
  (is (equal '("hhomogeneous" "homogeneous" "interpolate-size"
               "transition-duration" "transition-running" "transition-type"
               "vhomogeneous" "visible-child" "visible-child-name")
             (list-class-property-names "GtkStack")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-class-style-property-names "GtkStack")))
  ;; Get the names of the child properties
  (is (equal '("icon-name" "name" "needs-attention" "position" "title")
             (list-class-child-property-names "GtkStack")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkStack" GTK-STACK
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_stack_get_type")
                       ((HHOMOGENEOUS GTK-STACK-HHOMOGENEOUS "hhomogeneous"
                         "gboolean" T T)
                        (HOMOGENEOUS GTK-STACK-HOMOGENEOUS "homogeneous"
                         "gboolean" T T)
                        (INTERPOLATE-SIZE GTK-STACK-INTERPOLATE-SIZE
                         "interpolate-size" "gboolean" T T)
                        (TRANSITION-DURATION GTK-STACK-TRANSITION-DURATION
                         "transition-duration" "guint" T T)
                        (TRANSITION-RUNNING GTK-STACK-TRANSITION-RUNNING
                         "transition-running" "gboolean" T NIL)
                        (TRANSITION-TYPE GTK-STACK-TRANSITION-TYPE
                         "transition-type" "GtkStackTransitionType" T T)
                        (VHOMOGENEOUS GTK-STACK-VHOMOGENEOUS "vhomogeneous"
                         "gboolean" T T)
                        (VISIBLE-CHILD GTK-STACK-VISIBLE-CHILD "visible-child"
                         "GtkWidget" T T)
                        (VISIBLE-CHILD-NAME GTK-STACK-VISIBLE-CHILD-NAME
                         "visible-child-name" "gchararray" T T)))
             (get-g-type-definition "GtkStack"))))

;;; --- Properties -------------------------------------------------------------
;;;
;;;                   gboolean  hhomogeneous         Read / Write
;;;                   gboolean  homogeneous          Read / Write
;;;                   gboolean  interpolate-size     Read / Write
;;;                      guint  transition-duration  Read / Write
;;;                   gboolean  transition-running   Read
;;;     GtkStackTransitionType  transition-type      Read / Write
;;;                   gboolean  vhomogeneous         Read / Write
;;;                  GtkWidget  visible-child        Read / Write
;;;                      gchar  visible-child-name   Read / Write

;;; --- Child Properties -------------------------------------------------------

;;;                      gchar  icon-name            Read / Write
;;;                      gchar  name                 Read / Write
;;;                   gboolean  needs-attention      Read / Write
;;;                       gint  position             Read / Write
;;;                      gchar  title                Read / Write

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_new
;;;     gtk_stack_add_named
;;;     gtk_stack_add_titled
;;;     gtk_stack_get_child_by_name
;;;     gtk_stack_set_visible_child                        Accessor
;;;     gtk_stack_get_visible_child                        Accessor
;;;     gtk_stack_set_visible_child_name                   Accessor
;;;     gtk_stack_get_visible_child_name                   Accessor
;;;     gtk_stack_set_visible_child_full
;;;     gtk_stack_set_homogeneous                          Accessor
;;;     gtk_stack_get_homogeneous                          Accessor
;;;     gtk_stack_set_hhomogeneous                         Accessor
;;;     gtk_stack_get_hhomogeneous                         Accessor
;;;     gtk_stack_set_vhomogeneous                         Accessor
;;;     gtk_stack_get_vhomogeneous                         Accessor
;;;     gtk_stack_set_transition_duration                  Accessor
;;;     gtk_stack_get_transition_duration                  Accessor
;;;     gtk_stack_set_transition_type                      Accessor
;;;     gtk_stack_get_transition_type                      Accessor
;;;     gtk_stack_get_transition_running                   Accessor
;;;     gtk_stack_get_interpolate_size                     Accessor
;;;     gtk_stack_set_interpolate_size                     Accessor

;;; 2021-10-20
