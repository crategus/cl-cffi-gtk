(def-suite gobject-base :in gobject-suite)
(in-suite gobject-base)

;;;     GObject
;;;     GObjectClass
;;;     GParameter
;;;     GObjectConstructParam
;;;     GInitiallyUnowned
;;;     GInitiallyUnownedClass

;;;     g-type-is-object

(test g-type-is-object
  (is-true  (g-type-is-object "GtkButton"))
  (is-false (g-type-is-object "gboolean"))
  (is-false (g-type-is-object "GtkWindowType")))

;;;     g-is-object

(test g-is-object
  (is-true  (g-is-object (make-instance 'gtk-button))))

;;;     g-is-object-class

(test g-is-object-class
  (is-true  (g-is-object-class (g-type-class-ref "GtkButton"))))

;;;     g-object-class

(test g-object-class
  (is (g-is-object-class (g-object-class (make-instance 'gtk-button)))))

;;;     g-object-type

(test g-object-type
  (is-false (g-object-type nil))
  (is (eq (gtype "GtkButton")
          (g-object-type (make-instance 'gtk-button)))))

;;;     g-object-type-name

(test g-object-type-name
  (is-false (g-object-type-name nil))
  (is (string= "GtkButton" (g-object-type-name (make-instance 'gtk-button)))))

;;;     g-object-class-type

(test g-object-class-type
  (is (eq (gtype "GtkButton")
          (g-object-class-type (g-object-class (make-instance 'gtk-button))))))

;;;     g-object-class-name

(test g-object-class-name
  (is (string= "GtkButton"
               (g-object-class-name (g-object-class (make-instance 'gtk-button))))))

;;;     g_object_class_install_property
;;;     g_object_class_install_properties

;;;     g-object-class-find-property

(test g-object-class-find-property
  (is (g-is-param-spec (g-object-class-find-property "GtkLabel" "label")))
  (is (g-is-param-spec (g-object-class-find-property (gtype-id (gtype "GtkLabel"))
                                                     "label")))
  (is (g-is-param-spec (g-object-class-find-property (gtype "GtkLabel") "label")))
  ;; Unknown property-name returns nil
  (is-false (g-object-class-find-property "GtkLabel" "xxx")))

;;;     g-object-class-list-properties

(test g-object-class-list-properties
  (is (equal '("name" "parent" "width-request" "height-request" "visible"
               "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
               "focus-on-click" "can-default" "has-default" "receives-default"
               "composite-child" "style" "events" "no-show-all" "has-tooltip"
               "tooltip-markup" "tooltip-text" "window" "opacity"
               "double-buffered" "halign" "valign" "margin-left" "margin-right"
               "margin-start" "margin-end" "margin-top" "margin-bottom" "margin"
               "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
               "scale-factor" "xpad" "ypad" "label" "attributes" "use-markup"
               "use-underline" "justify" "pattern" "wrap" "wrap-mode"
               "selectable" "mnemonic-keyval" "mnemonic-widget"
               "cursor-position" "selection-bound" "ellipsize" "width-chars"
               "single-line-mode" "angle" "max-width-chars"
               "track-visited-links" "lines" "xalign" "yalign")
             (mapcar #'g-param-spec-name
                     (g-object-class-list-properties "GtkLabel")))))

;;;     g_object_class_override_property
;;;     g_object_interface_install_property

;;;     g-object-interface-find-property

(test g-object-interface-find-property
  (is (g-is-param-spec (g-object-interface-find-property "GAction" "enabled")))
  (is (g-is-param-spec (g-object-interface-find-property (gtype "GAction") "enabled")))
  (is (g-is-param-spec (g-object-interface-find-property (gtype-id (gtype "GAction")) "enabled")))
  (is-false (g-object-interface-find-property "GAction" "xxx")))

;;;     g-object-interface-list-properties

(test g-object-interface-list-properties
  (is (equal '("orientation")
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GtkOrientable"))))
  (is (equal '("enabled" "name" "parameter-type" "state" "state-type")
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GAction"))))
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GActionGroup")))))

;;;     g-object-new

(test g-object-new
  (is (eq (gtype "GtkButton")
          (g-object-type (g-object-new "GtkButton"))))
  (is (eq (gtype "GtkButton")
          (g-object-type (g-object-new "GtkButton" :label "text" :margin 6)))))

;;;     g_object_new_with_properties
;;;     g_object_newv

;;;     g_object_ref
;;;     g_object_unref
;;;     g_object_ref_sink
;;;     g_clear_object

;;;     GInitiallyUnowned
;;;     GInitiallyUnownedClass

;;;     G_TYPE_INITIALLY_UNOWNED

;;;     g_object_is_floating
;;;     g_object_force_floating
;;;     g_object_weak_ref
;;;     g_object_weak_unref
;;;     g_object_add_weak_pointer
;;;     g_object_remove_weak_pointer
;;;     g_object_add_toggle_ref
;;;     g_object_remove_toggle_ref

;;;     g_object_connect
;;;     g_object_disconnect
;;;     g_object_set
;;;     g_object_get

;;;     g-object-notify

(test g-object-notify
  (let* ((message nil)
         (button (make-instance 'gtk-button))
         (handler-id (g-signal-connect button "notify::can-default"
                       (lambda (widget pspec)
                         (declare (ignore widget))
                         (setf message
                               "Signal notify::can-default in g-object-notify")
                         (is (g-is-param-spec pspec))
                       ))))
    (is (integerp handler-id))
    ;; Notify button
    (is-false (g-object-notify button "can-default"))
    (is (string= "Signal notify::can-default in g-object-notify" message))))

;;;     g_object_notify_by_pspec

;;;     g-object-freeze-notify
;;;     g-object-thaw-notify

(test g-object-freeze-notify.1
  (let* ((message nil)
         (button (make-instance 'gtk-button))
         (handler-id (g-signal-connect button "notify::can-default"
                       (lambda (widget pspec)
                         (declare (ignore widget))
                         (setf message
                               (append message (list "notify::can-default")))
                         (is (g-is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Freeze the notify signal
    (is-false (g-object-freeze-notify button))
    (setf message (append message (list "freeze")))
    ;; Notify button
    (is-false (g-object-notify button "can-default"))
    ;; Thaw the notify signal
    (setf message (append message (list "thaw")))
    (is-false (g-object-thaw-notify button))
    ;; Check the order of the execution
    (is (equal '("freeze" "thaw" "notify::can-default") message))))

;; Counter sample without g-object-freeze-notify

(test g-object-freeze-notify.2
  (let* ((message nil)
         (button (make-instance 'gtk-button))
         (handler-id (g-signal-connect button "notify::can-default"
                       (lambda (widget pspec)
                         (declare (ignore widget))
                         (setf message
                               (append message (list "notify::can-default")))
                         (is (g-is-param-spec pspec))))))
    (is (integerp handler-id))
    ;; Freeze the notify signal
;    (g-object-freeze-notify button)
    (setf message (append message (list "freeze")))
    ;; Notify button
    (g-object-notify button "can-default")
    ;; Thaw the notify signal
    (setf message (append message (list "thaw")))
;    (g-object-thaw-notify button)
    ;; Check the order of the execution
    (is (equal '("freeze" "notify::can-default" "thaw") message))))

;;;     g-object-data

(test g-object-data
  (let ((button (make-instance 'gtk-button)))
    (is (null-pointer-p (g-object-data button "property")))
    (is (= 0 (pointer-address (g-object-data button "property"))))
    (is (pointerp (setf (g-object-data button "property") (make-pointer 100))))
    (is (pointerp (g-object-data button "property")))
    (is (= 100 (pointer-address (g-object-data button "property"))))
    (is (pointerp (setf (g-object-data button "property") (pointer (make-instance 'gtk-label)))))
    (is (pointerp (g-object-data button "property")))
    (is (eq 'gtk-label (type-of (gobject::get-g-object-for-pointer (g-object-data button "property")))))))

;;;     g-object-set-data-full

(defvar *data-full-status* nil)

(defcallback destroy-notify-cb :void ((data :pointer))
  (is (string= "destroy-notify-cb" (setf *data-full-status* "destroy-notify-cb")))
  (is (pointerp data))
  (is (= 100 (pointer-address data))))

(test g-object-set-data-full
  (let ((button (make-instance 'gtk-button)))
    ;; Set data on the object with a destroy callback
    (is-false (g-object-set-data-full button
                                      "property"
                                      (make-pointer 100)
                                      (callback destroy-notify-cb)))
    ;; Clear the status
    (is-false (setf *data-full-status* nil))
    ;; Destroy the data, the callback will be executed
    (is (pointerp (setf (g-object-data button "property") (null-pointer))))
    ;; Check status
    (is (string= "destroy-notify-cb" *data-full-status*))))

;;;     g-object-steal-data

(test g-object-steal-data
  (let ((button (make-instance 'gtk-button)))
    ;; Set the data
    (is (pointerp (setf (g-object-data button "property") (make-pointer 100))))
    (is (= 100 (pointer-address (g-object-data button "property"))))
    ;; Steal the data
    (is (= 100 (pointer-address (g-object-steal-data button "property"))))
    (is (=   0 (pointer-address (g-object-data button "property"))))))

;;;     g_object_dup_data
;;;     g_object_replace_data

;;;     g_object_get_qdata
;;;     g_object_set_qdata
;;;     g_object_set_qdata_full
;;;     g_object_steal_qdata
;;;     g_object_dup_qdata
;;;     g_object_replace-qdata

;;;     g-object-property

(test g-object-property.1
  (let ((obj (make-instance 'gtk-label)))
    (is (= 1.0d0 (setf (g-object-property (pointer obj) "angle") 1.0d0)))
    (is (= 1.0d0 (g-object-property (pointer obj) "angle")))
    (is (eq :start (setf (g-object-property (pointer obj) "ellipsize") :start)))
    (is (eq :start (g-object-property (pointer obj) "ellipsize")))
    (is (eq :fill (setf (g-object-property (pointer obj) "justify") :fill)))
    (is (eq :fill (g-object-property (pointer obj) "justify")))
    (is (string= "label" (setf (g-object-property (pointer obj) "label") "label")))
    (is (string= "label" (g-object-property (pointer obj) "label")))
    (is (= 10 (setf (g-object-property (pointer obj) "max-width-chars") 10)))
    (is (= 10 (g-object-property (pointer obj) "max-width-chars")))))

(test g-object-property.2
  (let ((obj (make-instance 'gtk-label)))
    (is (= 1.0d0 (setf (g-object-property obj "angle") 1.0d0)))
    (is (= 1.0d0 (g-object-property obj "angle")))
    (is (eq :start (setf (g-object-property obj "ellipsize") :start)))
    (is (eq :start (g-object-property obj "ellipsize")))
    (is (eq :fill (setf (g-object-property obj "justify") :fill)))
    (is (eq :fill (g-object-property obj "justify")))
    (is (string= "label" (setf (g-object-property obj "label") "label")))
    (is (string= "label" (g-object-property obj "label")))
    (is (= 10 (setf (g-object-property obj "max-width-chars") 10)))
    (is (= 10 (g-object-property obj "max-width-chars")))))

(test g-object-property.3
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 0.0d0 (g-object-property obj "angle" "gdouble")))
    (is (= 2.0d0 (setf (g-object-property obj "angle" "gdouble") 2.0d0)))
    (is (= 2.0d0 (g-object-property obj "angle")))))

(test g-object-property.4
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (eq 'pango-attr-list (type-of (g-object-property obj "attributes"))))))

(test g-object-property.5
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 0 (g-object-property obj "cursor-position" "gint")))
    ;; cursor-position is not writable
;    (is (= 2 (setf (g-object-property obj "cursor-position" "gint") 2)))
;    (is (= 2 (g-object-property obj "cursor-position")))
  ))

(test g-object-property.6
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (eq :none (g-object-property obj "ellipsize" "PangoEllipsizeMode")))
    (is (eq :start (setf (g-object-property obj "ellipsize" "PangoEllipsizeMode") :start)))
    (is (eq :start (g-object-property obj "ellipsize")))))

(test g-object-property.7
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (eq :left (g-object-property obj "justify")))
    (is (eq :center (setf (g-object-property obj "justify" "GtkJustification") :center)))
    (is (eq :center (g-object-property obj "justify")))))

(test g-object-property.8
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (string= "label" (g-object-property obj "label")))
    (is (string= "text" (setf (g-object-property obj "label" "gchararray") "text")))
    (is (string= "text" (g-object-property obj "label")))))

(test g-object-property.9
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= -1 (g-object-property obj "max-width-chars" "gint")))
    (is (= 10 (setf (g-object-property obj "max-width-chars" "gint") 10)))
    (is (= 10 (g-object-property obj "max-width-chars")))))

(test g-object-property.10
  (let ((obj (make-instance 'gtk-label :label "label")))
    (is (= 16777215 (g-object-property obj "mnemonic-keyval" "guint")))
    ;; mnemonic-keyval is not writable
;    (is (= 10000000 (setf (g-object-property obj "mnemonic-keyval" "guint") 10000000)))
;    (is (= 10000000 (g-object-property obj "mnmonic-keyval")))
  ))

;;;     g_object_new_valist
;;;     g_object_set_valist
;;;     g_object_get_valist
;;;     g_object_watch_closure
;;;     g_object_run_dispose

;;;     G_OBJECT_WARN_INVALID_PROPERTY_ID

;;;     GWeakRef
;;;     g_weak_ref_init
;;;     g_weak_ref_clear
;;;     g_weak_ref_get
;;;     g_weak_ref_set
;;;     g_assert_finalize_object

;;; 2021-1-28
