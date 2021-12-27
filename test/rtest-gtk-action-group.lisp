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
             (sort (mapcar #'g-param-spec-name
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
    (is (typep (gtk-action-group-accel-group group) 'gtk-accel-group))))

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
;;;   gtk_action_group_get_action
;;;   gtk-action-group-list-actions
;;;   gtk_action_group_add_action
;;;   gtk_action_group_add_action_with_accel
;;;   gtk-action-group-remove-action

;;;    GtkActionEntry

;;;    gtk-action-group-add-actions
;;;    gtk-action-group-add-actions-full

;;;    GtkToggleActionEntry

;;;    gtk-action-group-add-toggle-actions
;;;    gtk-action-group-add-toggle-actions-full

;;;    GtkRadioActionEntry

;;;    gtk-action-group-add-radio-actions
;;;    gtk-action-group-add-radio-actions-full
;;;    gtk_action_group_set_translate_func
;;;    gtk_action_group_set_translation_domain
;;;    gtk_action_group_translate_string

;;; 2021-12-24
