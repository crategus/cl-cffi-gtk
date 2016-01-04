
(def-suite gtk-app-chooser :in gtk-suite)
(in-suite gtk-app-chooser)

;;;   GtkAppChooser

(test gtk-app-chooser-interface
  ;; Type checks
  (is-false (g-type-is-object "GtkAppChooser"))
  (is-false (g-type-is-abstract "GtkAppChooser"))
  (is-true  (g-type-is-derived "GtkAppChooser"))
  (is-false (g-type-is-fundamental "GtkAppChooser"))
  (is-true  (g-type-is-value-type "GtkAppChooser"))
  (is-true  (g-type-has-value-table "GtkAppChooser"))
  (is-false (g-type-is-classed "GtkAppChooser"))
  (is-false (g-type-is-instantiatable "GtkAppChooser"))
  (is-true  (g-type-is-derivable "GtkAppChooser"))
  (is-false (g-type-is-deep-derivable "GtkAppChooser"))
  (is-true  (g-type-is-interface "GtkAppChooser"))

  ;; Check the registered name
  (is (eq 'gtk-app-chooser
          (registered-object-type-by-name "GtkAppChooser")))

  ;; Check infos about the C interface implementation
  (let ((class (g-type-default-interface-ref (gtype "GtkAppChooser"))))
    (is (equal (gtype "GtkAppChooser") (g-type-from-interface class)))
    (g-type-default-interface-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-app-chooser)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-app-chooser (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkAppChooser" (gobject-class-g-type-name class)))
    (is (equal "GtkAppChooser" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_app_chooser_get_type"
               (gobject-class-g-type-initializer class)))
    (is-true (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GInterface") (g-type-parent "GtkAppChooser")))
  (is (= 2 (g-type-depth "GtkAppChooser")))
  (is (equal (gtype "GtkAppChooser")
             (g-type-next-base "GtkAppChooser" "GInterface")))
  (is-true  (g-type-is-a "GtkAppChooser" "GInterface"))
  ;; FIXME: This returns T, this is wrong!?
;  (is-false (g-type-is-a "GtkAppChooser" "GtkWidget"))
  (is-false (g-type-is-a "GtkAppChooser" "gboolean"))
  (is-false (g-type-is-a "GtkAppChooser" "GtkWindow"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkAppChooser"))))

  ;; Get the names of the interface properties.
  (is (equal '("content-type")
             (mapcar #'param-spec-name
                     (g-object-interface-list-properties "GtkAppChooser"))))

  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkAppChooser" GTK-APP-CHOOSER
                 (:EXPORT T
                  :TYPE-INITIALIZER "gtk_app_chooser_get_type")
                 (CONTENT-TYPE
                  GTK-APP-CHOOSER-CONTENT-TYPE
                  "content-type" "gchararray" T NIL))
                (get-g-type-definition "GtkAppChooser"))))

;;;     gtk_app_chooser_get_app_info
;;;     gtk_app_chooser_get_content_type
;;;     gtk_app_chooser_refresh

