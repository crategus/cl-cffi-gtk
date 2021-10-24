(def-suite gtk-window :in gtk-suite)
(in-suite gtk-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowType

(test gtk-window-type
  ;; Check the type
  (is (g-type-is-enum "GtkWindowType"))
  ;; Check the type initializer
  (is (eq (gtype "GtkWindowType")
          (gtype (foreign-funcall "gtk_window_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-window-type
          (registered-enum-type "GtkWindowType")))
  ;; Check the names
  (is (equal '("GTK_WINDOW_TOPLEVEL" "GTK_WINDOW_POPUP")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkWindowType"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkWindowType"))))
  ;; Check the nick names
  (is (equal '("toplevel" "popup")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkWindowType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkWindowType"
                             GTK-WINDOW-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_window_type_get_type")
                             (:TOPLEVEL 0)
                             (:POPUP 1))
             (get-g-type-definition "GtkWindowType"))))

;;;     GtkWindowPosition

(test gtk-window-position
  ;; Check the type
  (is (g-type-is-enum "GtkWindowPosition"))
  ;; Check the type initializer
  (is (eq (gtype "GtkWindowPosition")
          (gtype (foreign-funcall "gtk_window_position_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-window-position
          (registered-enum-type "GtkWindowPosition")))
  ;; Check the names
  (is (equal '("GTK_WIN_POS_NONE" "GTK_WIN_POS_CENTER" "GTK_WIN_POS_MOUSE"
               "GTK_WIN_POS_CENTER_ALWAYS" "GTK_WIN_POS_CENTER_ON_PARENT")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkWindowPosition"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkWindowPosition"))))
  ;; Check the nick names
  (is (equal '("none" "center" "mouse" "center-always" "center-on-parent")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkWindowPosition"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkWindowPosition"
                             GTK-WINDOW-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_window_position_get_type")
                             (:NONE 0)
                             (:CENTER 1)
                             (:MOUSE 2)
                             (:CENTER-ALWAYS 3)
                             (:CENTER-ON-PARENT 4))
             (get-g-type-definition "GtkWindowPosition"))))

;;;     GtkWindow

(test gtk-window-class
  ;; Type check
  (is (g-type-is-object "GtkWindow"))
  ;; Check the registered name
  (is (eq 'gtk-window
          (registered-object-type-by-name "GtkWindow")))
  ;; Check the type initializer
  (is (eq (gtype "GtkWindow")
          (gtype (foreign-funcall "gtk_window_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBin") (g-type-parent "GtkWindow")))
  ;; Check the children
  #-windows
  (is (equal '("GtkDialog" "GtkAssistant" "GtkOffscreenWindow" "GtkPlug"
               "GtkShortcutsWindow" "GtkApplicationWindow")
             (mapcar #'g-type-name (g-type-children "GtkWindow"))))
  #+windows
  (is (equal '("GtkDialog" "GtkAssistant" "GtkOffscreenWindow"
               "GtkShortcutsWindow" "GtkApplicationWindow")
             (mapcar #'g-type-name (g-type-children "GtkWindow"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkWindow"))))
  ;; Check the class properties
  (is (equal '("accept-focus" "application" "attached-to" "decorated"
               "default-height" "default-width" "deletable"
               "destroy-with-parent" "focus-on-map" "focus-visible" "gravity"
               "has-resize-grip" "has-toplevel-focus"
               "hide-titlebar-when-maximized" "icon" "icon-name" "is-active"
               "is-maximized" "mnemonics-visible" "modal" "resizable"
               "resize-grip-visible" "role" "screen" "skip-pager-hint"
               "skip-taskbar-hint" "startup-id" "title" "transient-for"
               "type" "type-hint" "urgency-hint" "window-position")
             (list-class-property-names "GtkWindow")))
  ;; Get the names of the style properties
  (is (equal '("decoration-button-layout" "decoration-resize-handle")
             (list-class-style-property-names "GtkWindow")))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkWindow"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWindow" GTK-WINDOW
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_window_get_type")
                       ((ACCEPT-FOCUS GTK-WINDOW-ACCEPT-FOCUS "accept-focus"
                         "gboolean" T T)
                        (APPLICATION GTK-WINDOW-APPLICATION "application"
                         "GtkApplication" T T)
                        (ATTACHED-TO GTK-WINDOW-ATTACHED-TO "attached-to"
                         "GtkWidget" T T)
                        (DECORATED GTK-WINDOW-DECORATED "decorated" "gboolean"
                         T T)
                        (DEFAULT-HEIGHT GTK-WINDOW-DEFAULT-HEIGHT
                         "default-height" "gint" T T)
                        (DEFAULT-WIDTH GTK-WINDOW-DEFAULT-WIDTH "default-width"
                         "gint" T T)
                        (DELETABLE GTK-WINDOW-DELETABLE "deletable" "gboolean"
                         T T)
                        (DESTROY-WITH-PARENT GTK-WINDOW-DESTROY-WITH-PARENT
                         "destroy-with-parent" "gboolean" T T)
                        (FOCUS-ON-MAP GTK-WINDOW-FOCUS-ON-MAP "focus-on-map"
                         "gboolean" T T)
                        (FOCUS-VISIBLE GTK-WINDOW-FOCUS-VISIBLE "focus-visible"
                         "gboolean" T T)
                        (GRAVITY GTK-WINDOW-GRAVITY "gravity" "GdkGravity" T T)
                        (HAS-RESIZE-GRIP GTK-WINDOW-HAS-RESIZE-GRIP
                         "has-resize-grip" "gboolean" T T)
                        (HAS-TOPLEVEL-FOCUS GTK-WINDOW-HAS-TOPLEVEL-FOCUS
                         "has-toplevel-focus" "gboolean" T NIL)
                        (HIDE-TITLEBAR-WHEN-MAXIMIZED
                         GTK-WINDOW-HIDE-TITLEBAR-WHEN-MAXIMIZED
                         "hide-titlebar-when-maximized" "gboolean" T T)
                        (ICON GTK-WINDOW-ICON "icon" "GdkPixbuf" T T)
                        (ICON-NAME GTK-WINDOW-ICON-NAME "icon-name"
                         "gchararray" T T)
                        (IS-ACTIVE GTK-WINDOW-IS-ACTIVE "is-active" "gboolean"
                         T NIL)
                        (IS-MAXIMIZED GTK-WINDOW-IS-MAXIMIZED "is-maximized"
                         "gboolean" T NIL)
                        (MNEMONICS-VISIBLE GTK-WINDOW-MNEMONICS-VISIBLE
                         "mnemonics-visible" "gboolean" T T)
                        (MODAL GTK-WINDOW-MODAL "modal" "gboolean" T T)
                        (RESIZABLE GTK-WINDOW-RESIZABLE "resizable" "gboolean"
                         T T)
                        (RESIZE-GRIP-VISIBLE GTK-WINDOW-RESIZE-GRIP-VISIBLE
                         "resize-grip-visible" "gboolean" T NIL)
                        (ROLE GTK-WINDOW-ROLE "role" "gchararray" T T)
                        (SCREEN GTK-WINDOW-SCREEN "screen" "GdkScreen" T T)
                        (SKIP-PAGER-HINT GTK-WINDOW-SKIP-PAGER-HINT
                         "skip-pager-hint" "gboolean" T T)
                        (SKIP-TASKBAR-HINT GTK-WINDOW-SKIP-TASKBAR-HINT
                         "skip-taskbar-hint" "gboolean" T T)
                        (STARTUP-ID GTK-WINDOW-STARTUP-ID "startup-id"
                         "gchararray" NIL T)
                        (TITLE GTK-WINDOW-TITLE "title" "gchararray" T T)
                        (TRANSIENT-FOR GTK-WINDOW-TRANSIENT-FOR "transient-for"
                         "GtkWindow" T T)
                        (TYPE GTK-WINDOW-TYPE "type" "GtkWindowType" T NIL)
                        (TYPE-HINT GTK-WINDOW-TYPE-HINT "type-hint"
                         "GdkWindowTypeHint" T T)
                        (URGENCY-HINT GTK-WINDOW-URGENCY-HINT "urgency-hint"
                         "gboolean" T T)
                        (WINDOW-POSITION GTK-WINDOW-WINDOW-POSITION
                         "window-position" "GtkWindowPosition" T T)))
             (get-g-type-definition "GtkWindow"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-window-properties
  (let ((window (make-instance 'gtk-window)))
    (is-true  (gtk-window-accept-focus window))
    (is-false (gtk-window-application window))
    (is-false (gtk-window-attached-to window))
    (is-true  (gtk-window-decorated window))
    (is (= -1 (gtk-window-default-height window)))
    (is (= -1 (gtk-window-default-width window)))
    (is-true  (gtk-window-deletable window))
    (is-false (gtk-window-destroy-with-parent window))
    (is-true  (gtk-window-focus-on-map window))
    (is-true  (gtk-window-focus-visible window))
    (is (eq :north-west (gtk-window-gravity window)))
    (is-false (gtk-window-has-resize-grip window))
    (is-false (gtk-window-has-toplevel-focus window))
    (is-false (gtk-window-hide-titlebar-when-maximized window))
    (is-false (gtk-window-icon window))
    (is-false (gtk-window-icon-name window))
    (is-false (gtk-window-is-active window))
    (is-false (gtk-window-is-maximized window))
    (is-true  (gtk-window-mnemonics-visible window))
    (is-false (gtk-window-modal window))
    (is-true  (gtk-window-resizable window))
    (is-false (gtk-window-resize-grip-visible window))
    (is-false (gtk-window-role window))
    (is (typep (gtk-window-screen window) 'gdk-screen))
    (is-false (gtk-window-skip-pager-hint window))
    (is-false (gtk-window-skip-taskbar-hint window))
    ;; startup-id is not readable
    (signals (error) (gtk-window-startup-id window))
    (is-false (gtk-window-title window))
    (is-false (gtk-window-transient-for window))
    (is (eq :toplevel (gtk-window-type window)))
    (is (eq :normal (gtk-window-type-hint window)))
    (is-false (gtk-window-urgency-hint window))
    (is (eq :none (gtk-window-window-position window)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-window-style-properties
  (let ((win (make-instance 'gtk-window)))
    (is (string= "menu:close"
                 (gtk-widget-style-property win "decoration-button-layout")))
    (is (= 20 (gtk-widget-style-property win "decoration-resize-handle")))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-window-signals
  ;; Check the list of signals
  (is (equal '("keys-changed" "set-focus" "activate-focus" "activate-default"
               "enable-debugging")
             (mapcar #'g-signal-name
                     (g-signal-list-ids "GtkWindow"))))
  ;; Query info for "activate-default"
  (let ((query (g-signal-query (g-signal-lookup "activate-default"
                                                "GtkWindow"))))
    (is (string= "activate-default" (g-signal-query-signal-name query)))
    (is (string= "GtkWindow" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g-signal-query-signal-flags query)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for "activate-focus"
  (let ((query (g-signal-query (g-signal-lookup "activate-focus" "GtkWindow"))))
    (is (string= "activate-focus" (g-signal-query-signal-name query)))
    (is (string= "GtkWindow" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g-signal-query-signal-flags query)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for "enable-debugging"
  (let ((query (g-signal-query (g-signal-lookup "enable-debugging"
                                                "GtkWindow"))))
    (is (string= "enable-debugging" (g-signal-query-signal-name query)))
    (is (string= "GtkWindow" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g-signal-query-signal-flags query)))
    (is (string= "gboolean" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("gboolean")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for "keys-changed"
  (let ((query (g-signal-query (g-signal-lookup "keys-changed" "GtkWindow"))))
    (is (string= "keys-changed" (g-signal-query-signal-name query)))
    (is (string= "GtkWindow" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (g-signal-query-signal-flags query)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for "set-focus"
  (let ((query (g-signal-query (g-signal-lookup "set-focus" "GtkWindow"))))
    (is (string= "set-focus" (g-signal-query-signal-name query)))
    (is (string= "GtkWindow" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (g-signal-query-signal-flags query)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GtkWidget")
               (mapcar #'g-type-name (g-signal-query-param-types query))))
    (is-false (g-signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_new

(test gtk-window-new
  (is (typep (gtk-window-new :toplevel) 'gtk-window))
  (is (typep (gtk-window-new :popup) 'gtk-window)))

;;;     gtk_window_set_title                               Accessor
;;;     gtk_window_set_wmclass
;;;     gtk_window_set_resizable                           Accessor
;;;     gtk_window_get_resizable                           Accessor

;;;     gtk_window_add_accel_group
;;;     gtk_window_remove_accel_group

(test gtk-window-add-accel-group
  (let ((window (gtk-window-new :toplevel))
        (group (gtk-accel-group-new)))
    (is-false (gtk-window-add-accel-group window group))
    (is-false (gtk-window-remove-accel-group window group))))

;;;     gtk_window_activate_focus
;;;     gtk_window_activate_default
;;;     gtk_window_set_modal                               Accessor
;;;     gtk_window_set_default_geometry
;;;     gtk-window-set-geometry-hints
;;;     gtk_window_set_gravity                             Accessor
;;;     gtk_window_get_gravity                             Accessor
;;;     gtk_window_set_position
;;;     gtk_window_set_transient_for                       Accessor
;;;     gtk_window_set_attached_to                         Accessor
;;;     gtk_window_set_destroy_with_parent                 Accessor
;;;     gtk_window_set_hide_titlebar_when_maximized        Accessor
;;;     gtk_window_set_screen                              Accessor
;;;     gtk_window_get_screen                              Accessor
;;;     gtk_window_is_active                               Accessor
;;;     gtk_window_is_maximized                            Accessor
;;;     gtk_window_has_toplevel_focus                      Accessor
;;;     gtk_window_list_toplevels

(test gtk-window-list-toplevels
  (is (every (lambda (obj) (typep obj 'gtk-window))
             (gtk-window-list-toplevels))))

;;;     gtk_window_add_mnemonic
;;;     gtk_window_remove_mnemonic
;;;     gtk_window_mnemonic_activate
;;;     gtk_window_activate_key
;;;     gtk_window_propagate_key_event
;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_get_default_widget
;;;     gtk_window_set_default
;;;     gtk_window_present
;;;     gtk_window_present_with_time
;;;     gtk_window_close
;;;     gtk_window_iconify
;;;     gtk_window_deiconify
;;;     gtk_window_stick
;;;     gtk_window_unstick
;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor ()
;;;     gtk_window_unfullscreen
;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below
;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag
;;;     gtk_window_set_decorated                           Accessor
;;;     gtk_window_set_deletable                           Accessor
;;;     gtk_window_set_mnemonic_modifier
;;;     gtk_window_set_type_hint                           Accessor
;;;     gtk_window_set_skip_taskbar_hint                   Accessor
;;;     gtk_window_set_skip_pager_hint                     Accessor
;;;     gtk_window_set_urgency_hint                        Accessor
;;;     gtk_window_set_accept_focus                        Accessor
;;;     gtk_window_set_focus_on_map                        Accessor
;;;     gtk_window_set_startup_id                          Accessor
;;;     gtk_window_set_role                                Accessor
;;;     gtk_window_get_decorated                           Accessor
;;;     gtk_window_get_deletable                           Accessor
;;;     gtk_window_get_default_icon_list
;;;     gtk_window_get_default_icon_name

;;;     gtk-window-default-size

(test gtk-window-default-size
  (let ((window (make-instance 'gtk-window)))
    (is (equal '(-1 -1)
               (multiple-value-list (gtk-window-default-size window))))
    (is (equal '(100 200)
               (multiple-value-list (setf (gtk-window-default-size window)
                                          '(100 200)))))
    (is (equal '(100 200)
               (multiple-value-list (gtk-window-default-size window))))))

;;;     gtk_window_get_destroy_with_parent                 Accessor
;;;     gtk_window_get_hide_titlebar_when_maximized        Accessor
;;;     gtk_window_get_icon                                Accessor
;;;     gtk_window_get_icon_list
;;;     gtk_window_get_icon_name                           Accessor
;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_get_modal                               Accessor
;;;     gtk_window_get_position
;;;     gtk_window_get_role                                Accessor
;;;     gtk_window_get_size
;;;     gtk_window_get_title                               Accessor
;;;     gtk_window_get_transient_for                       Accessor
;;;     gtk_window_get_attached_to                         Accessor
;;;     gtk_window_get_type_hint                           Accessor
;;;     gtk_window_get_skip_taskbar_hint                   Accessor
;;;     gtk_window_get_skip_pager_hint                     Accessor
;;;     gtk_window_get_urgency_hint                        Accessor
;;;     gtk_window_get_accept_focus                        Accessor
;;;     gtk_window_get_focus_on_map                        Accessor
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_get_window_type
;;;     gtk_window_move
;;;     gtk_window_parse_geometry
;;;     gtk_window_reshow_with_initial_size
;;;     gtk_window_resize
;;;     gtk_window_resize_to_geometry
;;;     gtk_window_set_default_icon_list
;;;     gtk_window_set_default_icon
;;;     gtk_window_set_default_icon_from_file
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_set_icon                                Accessor
;;;     gtk_window_set_icon_list
;;;     gtk_window_set_icon_from_file
;;;     gtk_window_set_icon_name                           Accessor
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity
;;;     gtk_window_get_mnemonics_visible                   Accessor
;;;     gtk_window_set_mnemonics_visible                   Accessor
;;;     gtk_window_get_focus_visible                       Accessor
;;;     gtk_window_set_focus_visible                       Accessor
;;;     gtk_window_set_has_resize_grip                     Accessor
;;;     gtk_window_get_has_resize_grip                     Accessor
;;;     gtk_window_resize_grip_is_visible
;;;     gtk_window_get_resize_grip_area
;;;     gtk_window_get_application                         Accessor
;;;     gtk_window_set_application                         Accessor
;;;     gtk_window_set_has_user_ref_count
;;;     gtk_window_set_titlebar
;;;     gtk_window_get_titlebar
;;;     gtk_window_set_interactive_debugging

;;; 2021-10-18
