(def-suite gtk-application :in gtk-suite)
(in-suite gtk-application)

;;; --- GtkApplicationInhibitFlags ---------------------------------------------

(test gtk-application-inhibit-flags
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkApplicationInhibitFlags"
    GTK-APPLICATION-INHIBIT-FLAGS
    (:EXPORT T :TYPE-INITIALIZER "gtk_application_inhibit_flags_get_type")
  (:LOGOUT 1)
  (:SWITCH 2)
  (:SUSPEND 4)
  (:IDLE 8))
             (gobject::get-g-type-definition "GtkApplicationInhibitFlags")))
  ;; Check the names
  (is (equal '("GTK_APPLICATION_INHIBIT_LOGOUT" "GTK_APPLICATION_INHIBIT_SWITCH"
 "GTK_APPLICATION_INHIBIT_SUSPEND" "GTK_APPLICATION_INHIBIT_IDLE")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GtkApplicationInhibitFlags"))))
  ;; Check the values
  (is (equal '(1 2 4 8)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GtkApplicationInhibitFlags"))))
  ;; Check the nick names
  (is (equal '("logout" "switch" "suspend" "idle")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GtkApplicationInhibitFlags"))))
)

;;; --- GtkApplication ---------------------------------------------------------

(test gtk-application-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkApplication"))
  (is-false (g-type-is-abstract "GtkApplication"))
  (is-true  (g-type-is-derived "GtkApplication"))
  (is-false (g-type-is-fundamental "GtkApplication"))
  (is-true  (g-type-is-value-type "GtkApplication"))
  (is-true  (g-type-has-value-table "GtkApplication"))
  (is-true  (g-type-is-classed "GtkApplication"))
  (is-true  (g-type-is-instantiatable "GtkApplication"))
  (is-true  (g-type-is-derivable "GtkApplication"))
  (is-true  (g-type-is-deep-derivable "GtkApplication"))
  (is-false (g-type-is-interface "GtkApplication"))

  ;; Check the registered name
  (is (eq 'gtk-application
          (registered-object-type-by-name "GtkApplication")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkApplication"))))
    (is (equal (gtype "GtkApplication") (g-type-from-class class)))
    (is (equal (gtype "GtkApplication") (g-object-class-type class)))
    (is (equal "GtkApplication" (g-object-class-name class)))
    (is (equal (gtype "GtkApplication")
               (g-type-from-class  (g-type-class-peek "GtkApplication"))))
    (is (equal (gtype "GtkApplication")
               (g-type-from-class  (g-type-class-peek-static "GtkApplication"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-application)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-application (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkApplication" (gobject-class-g-type-name class)))
    (is (equal "GtkApplication" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_application_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GApplication") (g-type-parent "GtkApplication")))
  (is (= 3 (g-type-depth "GtkApplication")))
  (is (equal (gtype "GApplication")
             (g-type-next-base "GtkApplication" "GObject")))
  (is-true  (g-type-is-a "GtkApplication" "GObject"))
  (is-false (g-type-is-a "GtkApplication" "GInitiallyUnowned"))
  (is-false (g-type-is-a "GtkApplication" "gboolean"))
  (is-false (g-type-is-a "GtkApplication" "GtkWindow"))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkApplication" query)
    (is (equal (gtype "GtkApplication")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkApplication"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 424
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 40
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkApplication"))))
             
  ;; Check the interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (mapcar #'gtype-name (g-type-interfaces "GtkApplication"))))

  ;; Check the class properties
  (is (equal '("action-group" "active-window" "app-menu" "application-id" "flags"
 "inactivity-timeout" "is-busy" "is-registered" "is-remote" "menubar"
 "register-session" "resource-base-path" "screensaver-active")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkApplication"))
                          #'string-lessp)))

  ;; Check the style properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkApplication"))))

  ;; Check the names of the child properties
  ;; GtkApplication is not a GtkContainer
;  (is (equal '()
;             (mapcar #'param-spec-name
;                     (gtk-container-class-list-child-properties "GtkApplication"))))

  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkApplication" GTK-APPLICATION
                       (:SUPERCLASS G-APPLICATION :EXPORT T :INTERFACES
                        ("GActionGroup" "GActionMap") :TYPE-INITIALIZER
                        "gtk_application_get_type")
                       ((ACTIVE-WINDOW GTK-APPLICATION-ACTIVE-WINDOW
                         "active-window" "GtkWindow" T NIL)
                        (APP-MENU GTK-APPLICATION-APP-MENU "app-menu"
                         "GMenuModel" T T)
                        (MENUBAR GTK-APPLICATION-MENUBAR "menubar" "GMenuModel"
                         T T)
                        (REGISTER-SESSION GTK-APPLICATION-REGISTER-SESSION
                         "register-session" "gboolean" T T)
                        (SCREENSAVER-ACTIVE GTK-APPLICATION-SCREENSAVER-ACTIVE
                         "screensaver-active" "gboolean" T NIL)))
             (get-g-type-definition "GtkApplication")))
)

;;; ----------------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------------

;;;     gtk_application_new
;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;;     gtk_application_get_window_by_id
;;;     gtk_application_get_active_window                  Accessor
;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit
;;;     gtk_application_is_inhibited
;;;     gtk_application_prefers_app_menu
;;;     gtk_application_get_app_menu                       Accessor
;;;     gtk_application_set_app_menu                       Accessor
;;;     gtk_application_get_menubar                        Accessor
;;;     gtk_application_set_menubar                        Accessor
;;;     gtk_application_get_menu_by_id
;;;     gtk_application_add_accelerator
;;;     gtk_application_remove_accelerator
;;;     gtk_application_list_action_descriptions
;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action
;;;     gtk_application_get_actions_for_accel

