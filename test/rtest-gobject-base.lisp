
(def-suite gobject-base :in gobject-suite)
(in-suite gobject-base)

;;;   GParameter

(test g-parameter
  (is (= 24 (foreign-type-size '(:struct g-parameter))))
  (is (equal '(:name :value)
             (foreign-slot-names '(:struct g-parameter)))))

;;;    GObject
;;;    GObjectClass
;;;    GObjectConstructParam
;;;
;;;    G_TYPE_IS_OBJECT
;;;    G_OBJECT
;;;    G_IS_OBJECT
;;;    G_OBJECT_CLASS
;;;    G_IS_OBJECT_CLASS
;;;    G_OBJECT_GET_CLASS
;;;    G_OBJECT_TYPE
;;;    G_OBJECT_TYPE_NAME
;;;    G_OBJECT_CLASS_TYPE
;;;    G_OBJECT_CLASS_NAME
;;;
;;;    g_object_class_install_property
;;;    g_object_class_install_properties
;;;    g_object_class_find_property

;;;   g_object_class_list_properties

(test g-object-class-list-properties
  (is (equal '("name" "parent" "width-request" "height-request" "visible"
               "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
               "can-default" "has-default" "receives-default" "composite-child"
               "style" "events" "no-show-all" "has-tooltip" "tooltip-markup"
               "tooltip-text" "window" "double-buffered" "halign" "valign"
               "margin-left" "margin-right" "margin-top" "margin-bottom"
               "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set"
               "expand" "xalign" "yalign" "xpad" "ypad" "label" "attributes"
               "use-markup" "use-underline" "justify" "pattern" "wrap"
               "wrap-mode" "selectable" "mnemonic-keyval" "mnemonic-widget"
               "cursor-position" "selection-bound" "ellipsize" "width-chars"
               "single-line-mode" "angle" "max-width-chars"
               "track-visited-links")
             (mapcar #'param-spec-name
                     (g-object-class-list-properties "GtkLabel")))))

;;;    g_object_class_override_property
;;;    g_object_interface_install_property

;;;    g_object_interface_find_property

(test g-object-interface-find-property
  (gobject::with-unwind (interface (g-type-default-interface-ref "GAction")
                        g-type-default-interface-unref)
    (is (equal "enabled"
               (foreign-slot-value (g-object-interface-find-property interface "enabled")
                                   '(:struct g-param-spec) :name)))

  ))

;;;    g_object_interface_list_properties

(test g-object-interface-list-properties
  (is (equal '("orientation")
             (mapcar #'param-spec-name
                     (g-object-interface-list-properties "GtkOrientable"))))
  (is (equal '("enabled" "name" "parameter-type" "state" "state-type")
             (mapcar #'param-spec-name
                     (g-object-interface-list-properties "GAction"))))
  (is (equal '()
             (mapcar #'param-spec-name
                     (g-object-interface-list-properties "GActionGroup"))))
)

;;;    g_object_new
;;;    g_object_newv
;;;

;;;
;;;    g_object_ref
;;;    g_object_unref
;;;    g_object_ref_sink
;;;    g_clear_object
;;;
;;;    GInitiallyUnowned
;;;    GInitiallyUnownedClass
;;;
;;;    G_TYPE_INITIALLY_UNOWNED
;;;
;;;    g_object_is_floating
;;;    g_object_force_floating
;;;    g_object_weak_ref
;;;    g_object_weak_unref
;;;    g_object_add_weak_pointer
;;;    g_object_remove_weak_pointer
;;;    g_object_add_toggle_ref
;;;    g_object_remove_toggle_ref
;;;    g_object_connect
;;;    g_object_disconnect
;;;    g_object_set
;;;    g_object_get
;;;    g_object_notify
;;;    g_object_notify_by_pspec
;;;    g_object_freeze_notify
;;;    g_object_thaw_notify
;;;
;;;    g_object_get_data
;;;    g_object_set_data
;;;    g_object_set_data_full
;;;    g_object_steal_data
;;;    g_object_dup_data
;;;    g_object_replace_data
;;;
;;;    g_object_get_qdata
;;;    g_object_set_qdata
;;;    g_object_set_qdata_full
;;;    g_object_steal_qdata
;;;    g_object_dup_qdata
;;;    g_object_replace-qdata

;;;    g_object_set_property

(test g-object-set-property
  (let ((obj (make-instance 'gtk-label)))
    (gobject::set-gobject-property (pointer obj) "angle" 1.0d0)
    (is (= 1.0d0 (gobject::get-gobject-property (pointer obj) "angle")))
    (gobject::set-gobject-property (pointer obj) "ellipsize" :start)
    (is (eq :start (gobject::get-gobject-property (pointer obj) "ellipsize")))
    (gobject::set-gobject-property (pointer obj) "justify" :fill)
    (is (eq :fill (gobject::get-gobject-property (pointer obj) "justify")))
    (gobject::set-gobject-property (pointer obj) "label" "label")
    (is (equal "label" (gobject::get-gobject-property (pointer obj) "label")))
    (gobject::set-gobject-property (pointer obj) "max-width-chars" 10)
    (is (= 10 (gobject::get-gobject-property (pointer obj) "max-width-chars")))
))

;;;   g_object_get_property

(test g-object-get-property
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 0.0d0 (gobject::get-gobject-property (pointer obj) "angle")))
    (is (eq 'pango-attr-list (type-of (gobject::get-gobject-property (pointer obj) "attributes"))))
    (is (= 0 (gobject::get-gobject-property (pointer obj) "cursor-position")))
    (is (eq :none (gobject::get-gobject-property (pointer obj) "ellipsize")))
    (is (eq :left (gobject::get-gobject-property (pointer obj) "justify")))
    (is (equal "label" (gobject::get-gobject-property (pointer obj) "label")))
    (is (= -1 (gobject::get-gobject-property (pointer obj) "max-width-chars")))
    (is (= 16777215 (gobject::get-gobject-property (pointer obj) "mnemonic-keyval")))
))

;;;    g_object_new_valist
;;;    g_object_set_valist
;;;    g_object_get_valist
;;;    g_object_watch_closure
;;;    g_object_run_dispose
;;;
;;;    G_OBJECT_WARN_INVALID_PROPERTY_ID
;;;
;;;    GWeakRef
;;;
;;;    g_weak_ref_init
;;;    g_weak_ref_clear
;;;    g_weak_ref_get
;;;    g_weak_ref_set
