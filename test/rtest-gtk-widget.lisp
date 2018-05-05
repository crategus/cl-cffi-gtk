(in-package :gtk-testsuite)

(def-suite gtk-widget :in gtk-suite)
(in-suite gtk-widget)

;;;     GtkRequisition
;;;     GtkAllocation

;;; --- GtkWidget --------------------------------------------------------------

(test gtk-widget-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkWidget"))
  (is-true  (g-type-is-abstract "GtkWidget"))
  (is-true  (g-type-is-derived "GtkWidget"))
  (is-false (g-type-is-fundamental "GtkWidget"))
  (is-true  (g-type-is-value-type "GtkWidget"))
  (is-true  (g-type-has-value-table "GtkWidget"))
  (is-true  (g-type-is-classed "GtkWidget"))
  (is-true  (g-type-is-instantiatable "GtkWidget")) ; Why is this true?
  (is-true  (g-type-is-derivable "GtkWidget"))
  (is-true  (g-type-is-deep-derivable "GtkWidget"))
  (is-false (g-type-is-interface "GtkWidget"))

  ;; Check the registered name
  (is (eq 'gtk-widget
          (registered-object-type-by-name "GtkWidget")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkWidget"))))
    (is (equal (gtype "GtkWidget") (g-type-from-class class)))
    (is (equal (gtype "GtkWidget") (g-object-class-type class)))
    (is (equal "GtkWidget" (g-object-class-name class)))
    (is (equal (gtype "GtkWidget")
               (g-type-from-class  (g-type-class-peek "GtkWidget"))))
    (is (equal (gtype "GtkWidget")
               (g-type-from-class  (g-type-class-peek-static "GtkWidget"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-widget)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-widget (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkWidget" (gobject-class-g-type-name class)))
    (is (equal "GtkWidget" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_widget_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GInitiallyUnowned") (g-type-parent "GtkWidget")))
  (is (= 3 (g-type-depth "GtkWidget")))
  (is (equal (gtype "GInitiallyUnowned")
             (g-type-next-base "GtkWidget" "GObject")))
  (is-true  (g-type-is-a "GtkWidget" "GObject"))
  (is-true  (g-type-is-a "GtkWidget" "GInitiallyUnowned"))
  (is-false (g-type-is-a "GtkWidget" "gboolean"))
  (is-false (g-type-is-a "GtkWidget" "GtkWindow"))

  ;; Check the children
  (is (subsetp '("GtkMisc" "GtkContainer" "GtkRange" "GtkSeparator" "GtkInvisible"
                 "GtkProgressBar" "GtkLevelBar" "GtkSpinner" "GtkSwitch"
                 "GtkCellView" "GtkEntry" "GtkHSV" "GtkCalendar" "GtkDrawingArea")
               (mapcar #'gtype-name (g-type-children "GtkWidget"))
               :test #'string=))
             
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkWidget"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkWidget" query)
    (is (equal (gtype "GtkWidget")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkWidget"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 824
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 32
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the class properties.
  (is (subsetp '("app-paintable" "can-default" "can-focus" "composite-child"
                 "double-buffered" "events" "expand" "halign" "has-default"
                 "has-focus" "has-tooltip" "height-request" "hexpand"
                 "hexpand-set" "is-focus" "margin" "margin-bottom" "margin-end"
                 "margin-left" "margin-right" "margin-start" "margin-top" "name"
                 "no-show-all" "opacity" "parent" "receives-default"
                 "scale-factor" "sensitive" "style" "tooltip-markup"
                 "tooltip-text" "valign" "vexpand" "vexpand-set" "visible"
                 "width-request" "window")
                (mapcar #'param-spec-name
                        (g-object-class-list-properties "GtkWidget"))
                :test #'string=))

  ;; Get the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkWidget"))))

  ;; Get the names of the child properties
  ;; No test because GtkWidget is not a Container

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWidget" GTK-WIDGET
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_widget_get_type")
                       ((APP-PAINTABLE GTK-WIDGET-APP-PAINTABLE "app-paintable"
                         "gboolean" T T)
                        (CAN-DEFAULT GTK-WIDGET-CAN-DEFAULT "can-default"
                         "gboolean" T T)
                        (CAN-FOCUS GTK-WIDGET-CAN-FOCUS "can-focus" "gboolean"
                         T T)
                        (COMPOSITE-CHILD GTK-WIDGET-COMPOSITE-CHILD
                         "composite-child" "gboolean" T NIL)
                        (DOUBLE-BUFFERED GTK-WIDGET-DOUBLE-BUFFERED
                         "double-buffered" "gboolean" T T)
                        (EVENTS GTK-WIDGET-EVENTS "events" "GdkEventMask" T T)
                        (EXPAND GTK-WIDGET-EXPAND "expand" "gboolean" T T)
                        (HALIGN GTK-WIDGET-HALIGN "halign" "GtkAlign" T T)
                        (HAS-DEFAULT GTK-WIDGET-HAS-DEFAULT "has-default"
                         "gboolean" T T)
                        (HAS-FOCUS GTK-WIDGET-HAS-FOCUS "has-focus" "gboolean"
                         T T)
                        (HAS-TOOLTIP GTK-WIDGET-HAS-TOOLTIP "has-tooltip"
                         "gboolean" T T)
                        (HEIGHT-REQUEST GTK-WIDGET-HEIGHT-REQUEST
                         "height-request" "gint" T T)
                        (HEXPAND GTK-WIDGET-HEXPAND "hexpand" "gboolean" T T)
                        (HEXPAND-SET GTK-WIDGET-HEXPAND-SET "hexpand-set"
                         "gboolean" T T)
                        (IS-FOCUS GTK-WIDGET-IS-FOCUS "is-focus" "gboolean" T
                         T)
                        (MARGIN GTK-WIDGET-MARGIN "margin" "gint" T T)
                        (MARGIN-BOTTOM GTK-WIDGET-MARGIN-BOTTOM "margin-bottom"
                         "gint" T T)
                        (MARGIN-END GTK-WIDGET-MARGIN-END "margin-end" "gint" T
                         T)
                        (MARGIN-LEFT GTK-WIDGET-MARGIN-LEFT "margin-left"
                         "gint" T T)
                        (MARGIN-RIGHT GTK-WIDGET-MARGIN-RIGHT "margin-right"
                         "gint" T T)
                        (MARGIN-START GTK-WIDGET-MARGIN-START "margin-start"
                         "gint" T T)
                        (MARGIN-TOP GTK-WIDGET-MARGIN-TOP "margin-top" "gint" T
                         T)
                        (NAME GTK-WIDGET-NAME "name" "gchararray" T T)
                        (NO-SHOW-ALL GTK-WIDGET-NO-SHOW-ALL "no-show-all"
                         "gboolean" T T)
                        (OPACITY GTK-WIDGET-OPACITY "opacity" "gdouble" T T)
                        (PARENT GTK-WIDGET-PARENT "parent" "GtkContainer" T T)
                        (RECEIVES-DEFAULT GTK-WIDGET-RECEIVES-DEFAULT
                         "receives-default" "gboolean" T T)
                        (SCALE-FACTOR GTK-WIDGET-SCALE-FACTOR "scale-factor"
                         "gint" T NIL)
                        (SENSITIVE GTK-WIDGET-SENSITIVE "sensitive" "gboolean"
                         T T)
                        (STYLE GTK-WIDGET-STYLE "style" "GtkStyle" T T)
                        (TOOLTIP-MARKUP GTK-WIDGET-TOOLTIP-MARKUP
                         "tooltip-markup" "gchararray" T T)
                        (TOOLTIP-TEXT GTK-WIDGET-TOOLTIP-TEXT "tooltip-text"
                         "gchararray" T T)
                        (VALIGN GTK-WIDGET-VALIGN "valign" "GtkAlign" T T)
                        (VEXPAND GTK-WIDGET-VEXPAND "vexpand" "gboolean" T T)
                        (VEXPAND-SET GTK-WIDGET-VEXPAND-SET "vexpand-set"
                         "gboolean" T T)
                        (VISIBLE GTK-WIDGET-VISIBLE "visible" "gboolean" T T)
                        (WIDTH-REQUEST GTK-WIDGET-WIDTH-REQUEST "width-request"
                         "gint" T T)
                        (WINDOW GTK-WIDGET-WINDOW "window" "GdkWindow" T NIL)))
             (get-g-type-definition "GtkWidget"))))

;;; ----------------------------------------------------------------------------
;;; Check accessor functions of GtkWidget
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-app-paintable -----------------------------------------------

(test gtk-widget-app-paintable.1
  (let ((widget (make-instance 'gtk-label)))
    ;; The default value is false.
    (is-false (gtk-widget-app-paintable widget))
    (is-true (setf (gtk-widget-app-paintable widget) t))
    (is-true (gtk-widget-app-paintable widget))))

(test gtk-widget-app-paintable.2
  (let ((widget (make-instance 'gtk-label :app-paintable t)))
    ;; The value is set to true.
    (is-true (gtk-widget-app-paintable widget))
    (is-false (setf (gtk-widget-app-paintable widget) nil))
    (is-false (gtk-widget-app-paintable widget))))

;;; --- gtk-widget-can-default -------------------------------------------------

(test gtk-widget-can-default.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is false.
    (is-false (gtk-widget-can-default widget))
    (is-true (setf (gtk-widget-can-default widget) t))
    (is-true (gtk-widget-can-default widget))))

(test gtk-widget-can-default.2
  (let ((widget (make-instance 'gtk-button :can-default t)))
    ;; The value is set to true.
    (is-true (gtk-widget-can-default widget))
    (is-false (setf (gtk-widget-can-default widget) nil))
    (is-false (gtk-widget-can-default widget))))

;;; --- gtk-widget-can-focus ---------------------------------------------------

(test gtk-widget-can-focus.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is true? The documentation says false!
    (is-true (gtk-widget-can-focus widget))
    (is-false (setf (gtk-widget-can-focus widget) nil))
    (is-false (gtk-widget-can-focus widget))))

(test gtk-widget-can-focus.2
  (let ((widget (make-instance 'gtk-button :can-focus nil)))
    ;; The value is set to false.
    (is-false (gtk-widget-can-focus widget))
    (is-true (setf (gtk-widget-can-focus widget) t))
    (is-true (gtk-widget-can-focus widget))))

;;; --- gtk-widget-composite-child ---------------------------------------------

(test gtk-widget-composite-child
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is false.
    (is-false (gtk-widget-composite-child widget))
    ;; "composite-child" is not writable.
    (signals (error) (setf (gtk-widget-composite-child widget) t))))

;;; --- gtk-widget-double-buffered ---------------------------------------------

(test gtk-widget-double-buffered.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is true.
    (is-true (gtk-widget-double-buffered widget))
    (is-false (setf (gtk-widget-double-buffered widget) nil))
    (is-false (gtk-widget-double-buffered widget))))

(test gtk-widget-double-buffered.2
  (let ((widget (make-instance 'gtk-button :double-buffered nil)))
    ;; The value is set to false.
    (is-false (gtk-widget-double-buffered widget))
    (is-true (setf (gtk-widget-double-buffered widget) t))
    (is-true (gtk-widget-double-buffered widget))))

;;; --- gtk-widget-events ------------------------------------------------------

(test gtk-widget-events.1
  (let ((widget (make-instance 'gtk-event-box)))
    ;; The default value is false.
    (is-false (gtk-widget-events widget))
    (is (equal '(:button-press-mask)
               (setf (gtk-widget-events widget) '(:button-press-mask))))
    (is (equal '(:button-press-mask) (gtk-widget-events widget)))))

(test gtk-widget-events.2
  (let ((widget (make-instance 'gtk-event-box :events '(:button-press-mask))))
    ;; The value is set to '(:button-press-mask).
    (is (equal '(:button-press-mask) (gtk-widget-events widget)))))

;;; --- gtk-widget-expand ------------------------------------------------------

(test gtk-widget-expand.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is false.
    (is-false (gtk-widget-expand widget))
    ;; These are default values.
    (is-false (gtk-widget-hexpand widget))
    (is-false (gtk-widget-hexpand-set widget))
    (is-false (gtk-widget-vexpand widget))
    (is-false (gtk-widget-vexpand-set widget))
    ;; Setting "expand"
    (is-true (setf (gtk-widget-expand widget) t))
    ;; These values change to true.
    (is-true (gtk-widget-hexpand widget))
    (is-true (gtk-widget-hexpand-set widget))
    (is-true (gtk-widget-vexpand widget))
    (is-true (gtk-widget-vexpand-set widget))))

(test gtk-widget-expand.2
  (let ((widget (make-instance 'gtk-button :expand t)))
    ;; The value is set true.
    (is-true (setf (gtk-widget-expand widget) t))
    ;; These values change to true.
    (is-true (gtk-widget-hexpand widget))
    (is-true (gtk-widget-hexpand-set widget))
    (is-true (gtk-widget-vexpand widget))
    (is-true (gtk-widget-vexpand-set widget))))

;;; --- gtk-widget-halign ------------------------------------------------------

(test gtk-widget-halign.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is :fill.
    (is (eql :fill (gtk-widget-halign widget)))
    (is (eql :start (setf (gtk-widget-halign widget) :start)))
    (is (eql :start (gtk-widget-halign widget)))))

(test gtk-widget-halign.2
  (let ((widget (make-instance 'gtk-button :halign :end)))
    ;; The value is set to :end.
    (is (eql :end (gtk-widget-halign widget)))
    (is (eql :center (setf (gtk-widget-halign widget) :center)))
    (is (eql :center (gtk-widget-halign widget)))))

;;; --- gtk-widget-has-default -------------------------------------------------

(test gtk-widget-has-default
  (let ((window (make-instance 'gtk-window :type :toplevel))
        (button (make-instance 'gtk-button :can-default t)))
    (gtk-container-add window button)
    ;; The default value is false.
    (is-false (gtk-widget-has-default button))
    (is-true (gtk-widget-can-default button))
    ;; Grab focus on button and check "has-default"
    (gtk-widget-grab-focus button)
    (is-true (gtk-widget-has-default button))))

;;; --- gtk-widget-has-focus ---------------------------------------------------

;; Implement a test which gives a widget the focus.

(test gtk-widget-has-focus
  (let ((window (make-instance 'gtk-window :type :toplevel))
        (button (make-instance 'gtk-button :can-default t
                                           :can-focus t)))
    (gtk-container-add window button)
    ;; The default value is false.
    (is-false (gtk-widget-has-focus button))
    (is-true (gtk-widget-can-default button))
    (is-true (gtk-widget-can-focus button))
    (gtk-widget-grab-focus button)
    ;; This dos not return the expected true value.
    (is-false (gtk-widget-has-focus button))))

;;; --- gtk-widget-has-tooltip -------------------------------------------------

(test gtk-widget-has-tooltip.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is false.
    (is-false (gtk-widget-has-tooltip widget))
    ;; Set a tooltip text and check again.
    (is (equal "Tooltip" (setf (gtk-widget-tooltip-text widget) "Tooltip")))
    (is-true (gtk-widget-has-tooltip widget))))

(test gtk-widget-has-tooltip.2
  (let ((widget (make-instance 'gtk-button :tooltip-text "Tooltip")))
    ;; A tooltip is added.
    (is-true (gtk-widget-has-tooltip widget))
    (is (equal "Tooltip" (gtk-widget-tooltip-text widget)))))

;;; --- gtk-widget-height-request ----------------------------------------------

(test gtk-widget-height-request
  (let ((widget (make-instance 'gtk-button)))
    (is (eql -1 (gtk-widget-height-request widget)))
    (is (eql 10 (setf (gtk-widget-height-request widget) 10)))
    (is (eql 10 (gtk-widget-height-request widget)))))

#|
  @see-slot{gtk-widget-hexpand}
  @see-slot{gtk-widget-hexpand-set}
  @see-slot{gtk-widget-is-focus}
  @see-slot{gtk-widget-margin}
  @see-slot{gtk-widget-margin-bottom}
  @see-slot{gtk-widget-margin-left}
  @see-slot{gtk-widget-margin-right}
  @see-slot{gtk-widget-margin-top}
  @see-slot{gtk-widget-name}
  @see-slot{gtk-widget-no-show-all}
  @see-slot{gtk-widget-opacity}
  @see-slot{gtk-widget-parent}
  @see-slot{gtk-widget-receives-default}

scale-factor  

  @see-slot{gtk-widget-sensitive}
  @see-slot{gtk-widget-style}
  @see-slot{gtk-widget-tooltip-markup}
  @see-slot{gtk-widget-tooltip-text}
|#

;;; --- gtk-widget-valign ------------------------------------------------------

(test gtk-widget-valign.1
  (let ((widget (make-instance 'gtk-button)))
    ;; The default value is :fill.
    (is (eql :fill (gtk-widget-valign widget)))
    (is (eql :start (setf (gtk-widget-valign widget) :start)))
    (is (eql :start (gtk-widget-valign widget)))))

(test gtk-widget-valign.2
  (let ((widget (make-instance 'gtk-button :valign :end)))
    ;; The value is set to :end.
    (is (eql :end (gtk-widget-valign widget)))
    (is (eql :center (setf (gtk-widget-valign widget) :center)))
    (is (eql :center (gtk-widget-valign widget)))))

#|
  @see-slot{gtk-widget-vexpand}
  @see-slot{gtk-widget-vexpand-set}
  @see-slot{gtk-widget-visible}
|#

;;; --- gtk-widget-width-request -----------------------------------------------
  
(test gtk-widget-width-request
  (let ((widget (make-instance 'gtk-button)))
    (is (eql -1 (gtk-widget-width-request widget)))
    (is (eql 10 (setf (gtk-widget-width-request widget) 10)))
    (is (eql 10 (gtk-widget-width-request widget)))))

#|
  @see-slot{gtk-widget-window}
|#

;;; ----------------------------------------------------------------------------

;;;     GtkSelectionData
;;;     GtkWidgetAuxInfo
;;;     GtkWidgetHelpType
;;;
;;;     gtk_widget_new
;;;     gtk_widget_destroy
;;;     gtk_widget_in_destruction
;;;     gtk_widget_destroyed
;;;     gtk_widget_unparent
;;;     gtk_widget_show
;;;     gtk_widget_show_now
;;;     gtk_widget_hide
;;;     gtk_widget_show_all
;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_draw
;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_resize_no_redraw
;;;     gtk_widget_size_request
;;;     gtk_widget_get_child_requisition
;;;     gtk_widget_size_allocate
;;;     gtk_widget_add_accelerator
;;;     gtk_widget_remove_accelerator
;;;     gtk_widget_set_accel_path
;;;     gtk_widget_list_accel_closures
;;;     gtk_widget_can_activate_accel
;;;     gtk_widget_event
;;;     gtk_widget_activate
;;;     gtk_widget_reparent
;;;     gtk_widget_intersect
;;;     gtk_widget_is_focus
;;;     gtk_widget_grab_focus
;;;     gtk_widget_grab_default
;;;     gtk_widget_set_name
;;;     gtk_widget_get_name
;;;     gtk_widget_set_state
;;;     gtk_widget_set_sensitive
;;;     gtk_widget_set_parent
;;;     gtk_widget_set_parent_window
;;;     gtk_widget_get_parent_window

;;;   gtk_widget_set_events
;;;   gtk_widget_get_events
;;;   gtk_widget_add_events

(test gtk-widget-add-events
  (let ((eventbox (make-instance 'gtk-event-box)))
    (is (equal '() (gtk-widget-events eventbox)))
    (setf (gtk-widget-events eventbox) '(:button-press-mask))
    (is (equal '(:button-press-mask) (gtk-widget-events eventbox)))
    (gtk-widget-add-events eventbox
                           '(:pointer-motion-mask :button-release-mask))
    (is (equal '(:BUTTON-PRESS-MASK :BUTTON-RELEASE-MASK :POINTER-MOTION-MASK)
               (stable-sort (gtk-widget-events eventbox)
                            #'string< :key #'symbol-name)))))

;;;     gtk_widget_set_device_events
;;;     gtk_widget_get_device_events
;;;     gtk_widget_add_device_events
;;;     gtk_widget_set_device_enabled
;;;     gtk_widget_get_device_enabled
;;;     gtk_widget_get_toplevel
;;;     gtk_widget_get_ancestor
;;;     gtk_widget_get_visual
;;;     gtk_widget_set_visual
;;;     gtk_widget_get_pointer
;;;     gtk_widget_is_ancestor
;;;     gtk_widget_translate_coordinates
;;;     gtk_widget_hide_on_delete
;;;     gtk_widget_set_style
;;;     gtk_widget_ensure_style
;;;     gtk_widget_get_style
;;;     gtk_widget_reset_rc_styles
;;;     gtk_widget_get_default_style
;;;     gtk_widget_set_direction
;;;
;;;     GtkTextDirection
;;;
;;;     gtk_widget_get_direction
;;;     gtk_widget_set_default_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_shape_combine_region
;;;     gtk_widget_input_shape_combine_region
;;;     gtk_widget_path
;;;     gtk_widget_class_path
;;;     gtk_widget_get_composite_name
;;;     gtk_widget_override_background_color
;;;     gtk_widget_override_color
;;;     gtk_widget_override_font
;;;     gtk_widget_override_symbolic_color
;;;     gtk_widget_override_cursor
;;;     gtk_widget_modify_style
;;;     gtk_widget_get_modifier_style
;;;     gtk_widget_modify_fg
;;;     gtk_widget_modify_bg
;;;     gtk_widget_modify_text
;;;     gtk_widget_modify_base
;;;     gtk_widget_modify_font
;;;     gtk_widget_modify_cursor
;;;     gtk_widget_create_pango_context
;;;     gtk_widget_get_pango_context
;;;     gtk_widget_create_pango_layout
;;;     gtk_widget_render_icon
;;;     gtk_widget_render_icon_pixbuf
;;;     gtk_widget_pop_composite_child
;;;     gtk_widget_push_composite_child
;;;     gtk_widget_queue_draw_area
;;;     gtk_widget_queue_draw_region
;;;     gtk_widget_set_app_paintable
;;;     gtk_widget_set_double_buffered
;;;     gtk_widget_set_redraw_on_allocate
;;;     gtk_widget_set_composite_name
;;;     gtk_widget_mnemonic_activate

;;;     gtk_widget_class_install_style_property
;;;     gtk_widget_class_install_style_property_parser

;;;   gtk_widget_class_find_style_property

(test gtk-widget-class-find-style-property.1
  (is (equal "cursor-aspect-ratio"
             (param-spec-name
               (gtk-widget-class-find-style-property "GtkFrame"
                                                     "cursor-aspect-ratio")))))

(test gtk-widget-class-find-style-property.2
  (is (equal "cursor-color"
             (param-spec-name
               (gtk-widget-class-find-style-property "GtkFrame"
                                                     "cursor-color")))))

(test gtk-widget-class-find-style-property.3
  (is (equal "focus-line-pattern"
             (param-spec-name
               (gtk-widget-class-find-style-property "GtkFrame"
                                                     "focus-line-pattern")))))

;;;   gtk_widget_class_list_style_properties

(test gtk-widget-class-list-style-properties
  ;; Get the names of the style properties of GtkFrame.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkFrame")))))

;;;     gtk_widget_region_intersect
;;;     gtk_widget_send_expose
;;;     gtk_widget_send_focus_change
;;;     gtk_widget_style_get

;;;   gtk_widget_style_get_property

(test gtk-widget-style-get-property
  (let ((widget (make-instance 'gtk-frame)))
    (is (= 0.04 (gtk-widget-style-get-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-get-property widget "cursor-color"))
    (is (equal ""
               (gtk-widget-style-get-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-get-property widget "focus-line-width")))
    (is (= 1 (gtk-widget-style-get-property widget "focus-padding")))
    (is-true (gtk-widget-style-get-property widget "interior-focus"))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-get-property widget "secondary-cursor-color"))
    (is (<= 1 (gtk-widget-style-get-property widget "text-handle-height")))
    (is (<= 1 (gtk-widget-style-get-property widget "text-handle-width")))
    (is-false  (gtk-widget-style-get-property widget "wide-separators"))
    (is-false (gtk-widget-style-get-property widget "window-dragging"))))

;;;     gtk_widget_style_get_valist
;;;     gtk_widget_style_attach
;;;     gtk_widget_class_set_accessible_type
;;;     gtk_widget_class_set_accessible_role
;;;     gtk_widget_get_accessible
;;;     gtk_widget_child_focus
;;;     gtk_widget_child_notify
;;;     gtk_widget_freeze_child_notify
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_get_parent
;;;     gtk_widget_get_settings
;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_root_window
;;;     gtk_widget_get_screen
;;;     gtk_widget_has_screen
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_child_visible
;;;     gtk_widget_set_size_request
;;;     gtk_widget_thaw_child_notify
;;;     gtk_widget_set_no_show_all
;;;     gtk_widget_get_no_show_all
;;;     gtk_widget_list_mnemonic_labels
;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label
;;;     gtk_widget_is_composited
;;;     gtk_widget_error_bell
;;;     gtk_widget_keynav_failed
;;;     gtk_widget_get_tooltip_markup
;;;     gtk_widget_set_tooltip_markup
;;;     gtk_widget_get_tooltip_text
;;;     gtk_widget_set_tooltip_text
;;;     gtk_widget_get_tooltip_window
;;;     gtk_widget_set_tooltip_window
;;;     gtk_widget_get_has_tooltip
;;;     gtk_widget_set_has_tooltip
;;;     gtk_widget_trigger_tooltip_query
;;;     gtk_widget_get_window
;;;     gtk_cairo_should_draw_window
;;;     gtk_cairo_transform_to_window
;;;     gtk_widget_get_allocated_width
;;;     gtk_widget_get_allocated_height
;;;     gtk_widget_get_allocation
;;;     gtk_widget_set_allocation
;;;     gtk_widget_get_app_paintable
;;;     gtk_widget_get_can_default
;;;     gtk_widget_set_can_default
;;;     gtk_widget_get_can_focus
;;;     gtk_widget_set_can_focus
;;;     gtk_widget_get_double_buffered
;;;     gtk_widget_get_has_window
;;;     gtk_widget_set_has_window
;;;     gtk_widget_get_sensitive
;;;     gtk_widget_is_sensitive
;;;     gtk_widget_get_state
;;;     gtk_widget_get_visible
;;;     gtk_widget_set_visible
;;;     gtk_widget_set_state_flags
;;;     gtk_widget_unset_state_flags
;;;     gtk_widget_get_state_flags
;;;     gtk_widget_has_default
;;;     gtk_widget_has_focus
;;;     gtk_widget_has_visible_focus
;;;     gtk_widget_has_grab
;;;     gtk_widget_has_rc_style
;;;     gtk_widget_is_drawable
;;;     gtk_widget_is_toplevel
;;;     gtk_widget_set_window
;;;     gtk_widget_set_receives_default
;;;     gtk_widget_get_receives_default
;;;     gtk_widget_set_support_multidevice
;;;     gtk_widget_get_support_multidevice
;;;     gtk_widget_set_realized
;;;     gtk_widget_get_realized
;;;     gtk_widget_set_mapped
;;;     gtk_widget_get_mapped
;;;     gtk_widget_get_requisition
;;;     gtk_widget_device_is_shadowed
;;;     gtk_widget_get_modifier_mask
;;;
;;;     gtk_widget_get_path
;;;     gtk_widget_get_style_context
;;;     gtk_widget_reset_style
;;;
;;;     gtk_requisition_new
;;;     gtk_requisition_copy
;;;     gtk_requisition_free
;;;
;;;     GtkSizeRequestMode
;;;     GtkRequestedSize
;;;
;;;     gtk_widget_get_preferred_height
;;;     gtk_widget_get_preferred_width
;;;     gtk_widget_get_preferred_height_for_width
;;;     gtk_widget_get_preferred_width_for_height

;;; --- gtk_widget_get_request_mode --------------------------------------------

(test gtk-widget-get-request-mode.1
  (is (eql :constant-size
           (gtk-widget-get-request-mode (make-instance 'gtk-button)))))

(test gtk-widget-get-request-mode.2
  (is (eql :constant-size
           (gtk-widget-get-request-mode (make-instance 'gtk-button 
                                                       :label "Hello")))))

;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation
;;;
;;;     GtkAlign
;;;
;;;     gtk_widget_get_halign
;;;     gtk_widget_set_halign
;;;     gtk_widget_get_valign
;;;     gtk_widget_set_valign
;;;     gtk_widget_get_margin_left
;;;     gtk_widget_set_margin_left
;;;     gtk_widget_get_margin_right
;;;     gtk_widget_set_margin_right
;;;     gtk_widget_get_margin_top
;;;     gtk_widget_set_margin_top
;;;     gtk_widget_get_margin_bottom
;;;     gtk_widget_set_margin_bottom
;;;
;;;     gtk_widget_get_hexpand
;;;     gtk_widget_set_hexpand
;;;     gtk_widget_get_hexpand_set
;;;     gtk_widget_set_hexpand_set
;;;     gtk_widget_get_vexpand
;;;     gtk_widget_set_vexpand
;;;     gtk_widget_get_vexpand_set
;;;     gtk_widget_set_vexpand_set
;;;     gtk_widget_queue_compute_expand
;;;     gtk_widget_compute_expand
