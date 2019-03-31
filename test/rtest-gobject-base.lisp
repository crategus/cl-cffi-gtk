
(def-suite gobject-base :in gobject-suite)
(in-suite gobject-base)

;;;   GParameter

(test g-parameter
  #-windows
  (is (= 32 (foreign-type-size '(:struct g-parameter))))
  #+windows
  (is (= 32 (foreign-type-size '(:struct g-parameter))))
  (is (equal '(:name :value)
             (foreign-slot-names '(:struct g-parameter)))))

(test g-parameter.1
  (with-foreign-object (parameter '(:struct g-parameter))
    (setf (foreign-slot-value parameter '(:struct g-parameter) :name)
          "name")
    (set-g-value (foreign-slot-pointer parameter '(:struct g-parameter) :value)
                 12
                 "gint"
                 :zero-g-value t)
;    (format t "~a~%~a~%"
;            (foreign-slot-value parameter '(:struct g-parameter) :name)
;            (foreign-slot-value parameter '(:struct g-parameter) :value))
    (is (equal "name" (foreign-slot-value parameter '(:struct g-parameter) :name)))
    (is (= 12 (parse-g-value (foreign-slot-pointer parameter '(:struct g-parameter) :value))))
  ))

(test g-parameter.2
  (with-foreign-object (parameter '(:struct g-parameter))
    (setf (foreign-slot-value parameter '(:struct g-parameter) :name)
          "name")
    (set-g-value (foreign-slot-pointer parameter '(:struct g-parameter) :value)
                 "text"
                 "gchararray"
                 :zero-g-value t)
;    (format t "~a~%~a~%"
;            (foreign-slot-value parameter '(:struct g-parameter) :name)
;            (foreign-slot-value parameter '(:struct g-parameter) :value))
    (is (equal "name" (foreign-slot-value parameter '(:struct g-parameter) :name)))
    (is (equal "text" (parse-g-value (foreign-slot-pointer parameter '(:struct g-parameter) :value))))
  ))

(test g-parameter.3
  (let* ((args-names (list "title" "border-width"))
         (args-values (list "text" 12))
         (args-types (list "gchararray" "gint"))
         (args-count (length args-names)))
    (with-foreign-object (parameters '(:struct g-parameter) args-count)
      (loop
         for i from 0 below args-count
         for arg-name in args-names
         for arg-value in args-values
         for arg-type in args-types
         for arg-g-type = arg-type
         for parameter = (mem-aptr parameters '(:struct g-parameter) i)
         do (setf (foreign-slot-value parameter '(:struct g-parameter) :name)
                  arg-name)
         do (set-g-value (foreign-slot-pointer parameter
                                               '(:struct g-parameter) :value)
                         arg-value
                         arg-g-type
                         :zero-g-value t)
;        do (format t "(~A, ~A)~%"
;                   (foreign-slot-value parameter '(:struct g-parameter) :name)
;                   (parse-g-value (foreign-slot-pointer parameter '(:struct g-parameter) :value)))
      )
      (loop
         for i from 0 below args-count
         for arg-name in args-names
         for arg-value in args-values
         for parameter = (mem-aptr parameters '(:struct g-parameter) i)
         do (is (equal arg-name (foreign-slot-value parameter '(:struct g-parameter) :name)))
         do (is (equal arg-value
                       (parse-g-value (foreign-slot-pointer parameter '(:struct g-parameter) :value))))
         do (foreign-string-free (mem-ref (foreign-slot-pointer parameter
                                                                '(:struct g-parameter)
                                                                :name)
                                          :pointer))
         do (g-value-unset (foreign-slot-pointer parameter
                                                 '(:struct g-parameter)
                                                 :value))))))

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
  (is (equal '("name" "parent" "width-request" "height-request" "visible" "sensitive"
 "app-paintable" "can-focus" "has-focus" "is-focus" "focus-on-click"
 "can-default" "has-default" "receives-default" "composite-child" "style"
 "events" "no-show-all" "has-tooltip" "tooltip-markup" "tooltip-text" "window"
 "opacity" "double-buffered" "halign" "valign" "margin-left" "margin-right"
 "margin-start" "margin-end" "margin-top" "margin-bottom" "margin" "hexpand"
 "vexpand" "hexpand-set" "vexpand-set" "expand" "scale-factor" "xpad" "ypad"
 "label" "attributes" "use-markup" "use-underline" "justify" "pattern" "wrap"
 "wrap-mode" "selectable" "mnemonic-keyval" "mnemonic-widget" "cursor-position"
 "selection-bound" "ellipsize" "width-chars" "single-line-mode" "angle"
 "max-width-chars" "track-visited-links" "lines" "xalign" "yalign")
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

;;;   g_object_set_property

(test g-object-set-property.1
  (let ((obj (make-instance 'gtk-label)))
    (g-object-set-property (pointer obj) "angle" 1.0d0)
    (is (= 1.0d0 (g-object-get-property (pointer obj) "angle")))
    (g-object-set-property (pointer obj) "ellipsize" :start)
    (is (eq :start (g-object-get-property (pointer obj) "ellipsize")))
    (g-object-set-property (pointer obj) "justify" :fill)
    (is (eq :fill (g-object-get-property (pointer obj) "justify")))
    (g-object-set-property (pointer obj) "label" "label")
    (is (equal "label" (g-object-get-property (pointer obj) "label")))
    (g-object-set-property (pointer obj) "max-width-chars" 10)
    (is (= 10 (g-object-get-property (pointer obj) "max-width-chars")))))

(test g-object-set-property.2
  (let ((obj (make-instance 'gtk-label)))
    (g-object-set-property obj "angle" 1.0d0)
    (is (= 1.0d0 (g-object-get-property obj "angle")))
    (g-object-set-property obj "ellipsize" :start)
    (is (eq :start (g-object-get-property obj "ellipsize")))
    (g-object-set-property obj "justify" :fill)
    (is (eq :fill (g-object-get-property obj "justify")))
    (g-object-set-property obj "label" "label")
    (is (equal "label" (g-object-get-property obj "label")))
    (g-object-set-property obj "max-width-chars" 10)
    (is (= 10 (g-object-get-property obj "max-width-chars")))))

;;;   g_object_get_property

(test g-object-get-property.1
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 0.0d0 (g-object-get-property (pointer obj) "angle")))))

;; FIXME: This test causes a critical warning.
;(test g-object-get-property.2
;  (let ((obj (make-instance 'gtk-label :label "label")))
;    (is (eq 'pango-attr-list (type-of (g-object-get-property (pointer obj) "attributes"))))))

(test g-object-get-property.3
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 0 (g-object-get-property (pointer obj) "cursor-position")))))

(test g-object-get-property.4
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (eq :none (g-object-get-property (pointer obj) "ellipsize")))))

(test g-object-get-property.5
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (eq :left (g-object-get-property (pointer obj) "justify")))))

(test g-object-get-property.6
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (equal "label" (g-object-get-property (pointer obj) "label")))))

(test g-object-get-property.7
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= -1 (g-object-get-property (pointer obj) "max-width-chars")))))

(test g-object-get-property.8
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 16777215 (g-object-get-property (pointer obj) "mnemonic-keyval")))))

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
