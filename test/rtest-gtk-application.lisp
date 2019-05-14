(def-suite gtk-application :in gtk-suite)
(in-suite gtk-application)

;;; --- GtkApplicationInhibitFlags ---------------------------------------------

(test gtk-application-inhibit-flags
  ;; Check the type
  (is-true (g-type-is-flags "GtkApplicationInhibitFlags"))
  ;; Check the registered name
  (is (eql 'gtk-application-inhibit-flags
           (gobject::registered-flags-type "GtkApplicationInhibitFlags")))
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
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkApplicationInhibitFlags"
    GTK-APPLICATION-INHIBIT-FLAGS
    (:EXPORT T :TYPE-INITIALIZER "gtk_application_inhibit_flags_get_type")
  (:LOGOUT 1)
  (:SWITCH 2)
  (:SUSPEND 4)
  (:IDLE 8))
             (gobject::get-g-type-definition "GtkApplicationInhibitFlags"))))

;;; --- GtkApplication ---------------------------------------------------------

(test gtk-application-class
  ;; Type check
  (is-true  (g-type-is-object "GtkApplication"))
  ;; Check the registered name
  (is (eq 'gtk-application
          (registered-object-type-by-name "GtkApplication")))
  ;; Check the parent
  (is (equal (gtype "GApplication") (g-type-parent "GtkApplication")))
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
             (get-g-type-definition "GtkApplication"))))

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

(test gtk-applicaton-list-action-descriptions
  (let ((application (make-instance 'gtk-application)))
    (is (equal '()
               (gtk-application-list-action-descriptions application)))
    (gtk-application-set-accels-for-action application "win::close"
                                                       '("<Control>q" "<Shift><Alt>F1" "<Release>z"))
    (is (equal '("win::close")
               (gtk-application-list-action-descriptions application)))))

;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action

(test gtk-application-get-accels-for-action
  (let ((application (make-instance 'gtk-application)))
    (is-false (gtk-application-get-accels-for-action application "win::close"))
    (gtk-application-set-accels-for-action application "win::close"
                                                       '("<Control>q" "<Shift><Alt>F1" "<Release>z"))
    (is (equal '("<Primary>q" "<Shift><Alt>F1" "<Release>z")
               (gtk-application-get-accels-for-action application "win::close")))))

;;;     gtk_application_get_actions_for_accel

(test gtk-application-get-actions-for-accel
  (let ((application (make-instance 'gtk-application)))
    (gtk-application-set-accels-for-action application "win::close"
                                                       '("<Control>q" "<Shift><Alt>F1" "<Release>z"))
    (is (equal '("win::close")
               (gtk-application-get-actions-for-accel application "<Control>q")))
    (gtk-application-set-accels-for-action application "win::save" '("<Control>q"))
    (is (equal '("win::close" "win::save")
               (gtk-application-get-actions-for-accel application "<Control>q")))))

