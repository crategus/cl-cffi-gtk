(def-suite gtk-actionable :in gtk-suite)
(in-suite gtk-actionable)

;;; --- GtkActionable ----------------------------------------------------------

(test gtk-actionable-interface
  ;; Type check
  (is (g-type-is-interface "GtkActionable"))
  ;; Check the registered name
  (is (eq 'gtk-actionable
          (registered-object-type-by-name "GtkActionable")))
  ;; Check the type initializer
  (is (eq (gtype "GtkActionable")
          (gtype (foreign-funcall "gtk_actionable_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '("action-name" "action-target")
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GtkActionable"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkActionable" GTK-ACTIONABLE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_actionable_get_type")
                       (ACTION-NAME GTK-ACTIONABLE-ACTION-NAME
                        "action-name" "gchararray" T T)
                       (ACTION-TARGET GTK-ACTIONABLE-ACTION-TARGET
                        "action-target" "GVariant" T T))
             (get-g-type-definition "GtkActionable"))))

;;; --- Properties and Accessors -----------------------------------------------

(test gtk-actionable-properties
  (let ((button (make-instance 'gtk-button)))
    ;; Default is false
    (is-false (gtk-actionable-action-name button))
    ;; Default is null-pointer
    (is-true (null-pointer-p (gtk-actionable-action-target button)))
    ;; Set the name and the target
    (gtk-actionable-set-detailed-action-name button "win.justify::left")
    (is (string= "win.justify"
                 (gtk-actionable-action-name button)))
    (is (string= "left"
                 (g-variant-string (gtk-actionable-action-target button))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_actionable_set_action_target

(test gtk-actionable-action-target
  (let ((button (make-instance 'gtk-button)))
    (is (null-pointer-p (gtk-actionable-action-target button)))
    (setf (gtk-actionable-action-target button) (g-variant-new-int16 128))
    (is (= 128 (g-variant-int16 (gtk-actionable-action-target button))))
    (setf (gtk-actionable-action-target button) (null-pointer))
    (is (null-pointer-p (gtk-actionable-action-target button)))))

;;;     gtk_actionable_set_detailed_action_name

;;; See gtk-actionable-properties for an example

;;; 2021-9-8
