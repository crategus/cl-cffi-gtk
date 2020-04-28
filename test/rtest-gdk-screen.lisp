(def-suite gdk-screen :in gdk-suite)
(in-suite gdk-screen)

;;;     GdkScreen

(test gdk-screen-class
  ;; Type checks
  (is-true  (g-type-is-object "GdkScreen"))
  (is-false (g-type-is-abstract "GdkScreen"))
  (is-true  (g-type-is-derived "GdkScreen"))
  (is-false (g-type-is-fundamental "GdkScreen"))
  (is-true  (g-type-is-value-type "GdkScreen"))
  (is-true  (g-type-has-value-table "GdkScreen"))
  (is-true  (g-type-is-classed "GdkScreen"))
  (is-true  (g-type-is-instantiatable "GdkScreen"))
  (is-true  (g-type-is-derivable "GdkScreen"))
  (is-true  (g-type-is-deep-derivable "GdkScreen"))
  (is-false (g-type-is-interface "GdkScreen"))

  ;; Check the registered name
  (is (eq 'gdk-screen
          (registered-object-type-by-name "GdkScreen")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkScreen"))))
    (is (equal (gtype "GdkScreen") (g-type-from-class class)))
    (is (equal (gtype "GdkScreen") (g-object-class-type class)))
    (is (equal "GdkScreen" (g-object-class-name class)))
    (is (equal (gtype "GdkScreen")
               (g-type-from-class  (g-type-class-peek "GdkScreen"))))
    (is (equal (gtype "GdkScreen")
               (g-type-from-class (g-type-class-peek-static "GdkScreen"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gdk-screen)))
    ;; Check the class name and type of the class
    (is (eq 'gdk-screen (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GdkScreen" (gobject-class-g-type-name class)))
    (is (equal "GdkScreen" (gobject-class-direct-g-type-name class)))
    (is (equal "gdk_screen_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GdkScreen")))
  (is (= 2 (g-type-depth "GdkScreen")))
  (is (equal (gtype "GdkScreen")
             (g-type-next-base "GdkScreen" "GObject")))
  (is-true  (g-type-is-a "GdkScreen" "GObject"))
  (is-false (g-type-is-a "GdkScreen" "GtkWidget"))
  (is-false (g-type-is-a "GdkScreen" "gboolean"))

  ;; Check the children
  (is (equal '("GdkX11Screen" "GdkBroadwayScreen")
             (mapcar #'gtype-name (g-type-children "GdkScreen"))))

  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GdkScreen"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GdkScreen" query)
    (is (equal (gtype "GdkScreen")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GdkScreen"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 416 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  48 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("font-options" "resolution")
             (mapcar #'param-spec-name
                     (g-object-class-list-properties "GdkScreen"))))

  ;; No style properties
  ;; No child properties

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkScreen" GDK-SCREEN
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_screen_get_type")
                       ((FONT-OPTIONS GDK-SCREEN-FONT-OPTIONS "font-options"
                         "gpointer" T T)
                        (RESOLUTION GDK-SCREEN-RESOLUTION "resolution"
                         "gdouble" T T)))
             (get-g-type-definition "GdkScreen"))))

;;;     gdk_screen_get_font_options                          Accessor
;;;     gdk_screen_set_font_options                          Accessor
;;;     gdk_screen_get_resolution                            Accessor
;;;     gdk_screen_set_resolution                            Accessor

;;;     gdk_screen_get_default
;;;     gdk_screen_get_system_visual
;;;     gdk_screen_get_rgba_visual
;;;     gdk_screen_is_composited
;;;     gdk_screen_get_root_window
;;;     gdk_screen_get_display

;;;     gdk_screen_get_number                              * deprecated
;;;     gdk_screen_get_width                               * deprecated
;;;     gdk_screen_get_height                              * deprecated
;;;     gdk_screen_get_width_mm                            * deprecated
;;;     gdk_screen_get_height_mm                           * deprecated

;;;     gdk_screen_list_visuals
;;;     gdk_screen_get_toplevel_windows

;;;     gdk_screen_make_display_name                       * deprecated
;;;     gdk_screen_get_n_monitors                          * deprecated
;;;     gdk_screen_get_primary_monitor                     * deprecated
;;;     gdk_screen_get_monitor_geometry                    * deprecated
;;;     gdk_screen_get_monitor_workarea                    * deprecated
;;;     gdk_screen_get_monitor_at_point                    * deprecated
;;;     gdk_screen_get_monitor_at_window                   * deprecated
;;;     gdk_screen_get_monitor_height_mm                   * deprecated
;;;     gdk_screen_get_monitor_width_mm                    * deprecated
;;;     gdk_screen_get_monitor_plug_name                   * deprecated
;;;     gdk_screen_get_monitor_scale_factor                * deprecated

;;;     gdk_screen_get_setting

(test gdk-screen-get-setting
  (let ((screen (gdk-display-default-screen (gdk-display-default))))
    (is (= 400 (gdk-screen-get-setting screen "gtk-double-click-time" "gint")))))

;;;     gdk_screen_get_active_window                       * deprecated
;;;     gdk_screen_get_window_stack

