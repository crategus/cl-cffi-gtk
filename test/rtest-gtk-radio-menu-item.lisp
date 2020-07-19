(def-suite gtk-radio-menu-item :in gtk-suite)
(in-suite gtk-radio-menu-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioMenuItem

(test gtk-radio-menu-item-class
  ;; Type check
  (is-true  (g-type-is-object "GtkRadioMenuItem"))
  ;; Check the registered name
  (is (eq 'gtk-radio-menu-item
          (registered-object-type-by-name "GtkRadioMenuItem")))
  ;; Check the type initializer
  (is (string= "GtkRadioMenuItem"
               (g-type-name (gtype (foreign-funcall "gtk_radio_menu_item_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkCheckMenuItem") (g-type-parent "GtkRadioMenuItem")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkRadioMenuItem"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable" "GtkActionable")
             (mapcar #'gtype-name (g-type-interfaces "GtkRadioMenuItem"))))
  ;; Check the class properties
  (is (equal '("accel-path" "action-name" "action-target" "active" "app-paintable"
               "border-width" "can-default" "can-focus" "child" "composite-child"
               "double-buffered" "draw-as-radio" "events" "expand" "focus-on-click" "group"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "inconsistent" "is-focus" "label" "margin" "margin-bottom"
               "margin-end" "margin-left" "margin-right" "margin-start" "margin-top" "name"
               "no-show-all" "opacity" "parent" "receives-default" "related-action"
               "resize-mode" "right-justified" "scale-factor" "sensitive" "style" "submenu"
               "tooltip-markup" "tooltip-text" "use-action-appearance" "use-underline"
               "valign" "vexpand" "vexpand-set" "visible" "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkRadioMenuItem"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "arrow-scaling"
               "arrow-spacing" "horizontal-padding" "selected-shadow-type" "toggle-spacing"
               "width-chars" "indicator-size")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkRadioMenuItem"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkRadioMenuItem"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkRadioMenuItem" GTK-RADIO-MENU-ITEM
                       (:SUPERCLASS GTK-CHECK-MENU-ITEM :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_menu_item_get_type")
                       ((GROUP GTK-RADIO-MENU-ITEM-GROUP "group"
                         "GtkRadioMenuItem" NIL T)))
             (get-g-type-definition "GtkRadioMenuItem"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkRadioMenuItem*  group    Write

(test gtk-radio-menu-item-properties
  (let ((menu-item (make-instance 'gtk-radio-menu-item)))
    ;; group is not readable
    (signals (error) (gtk-radio-menu-item-group menu-item))
    ;; group is writeable
    (is-false (setf (gtk-radio-menu-item-group menu-item) nil))))

;;; --- Signals ----------------------------------------------------------------

;;;     void  group-changed    Run First

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_menu_item_new

(test gtk-radio-menu-item-new
  (let ((menu-item (gtk-radio-menu-item-new nil)))

    (is (eq 'gtk-radio-menu-item (type-of menu-item)))
    (is-false (gtk-bin-child menu-item))

    (is (equal menu-item (first (gtk-radio-menu-item-get-group menu-item))))



))

;;;     gtk_radio_menu_item_new_with_label

(test gtk-radio-menu-item-new-with-label
  (let ((menu-item (gtk-radio-menu-item-new-with-label nil "Label")))

    (is (eq 'gtk-radio-menu-item (type-of menu-item)))
    (is (eq 'gtk-accel-label (type-of (gtk-bin-child menu-item))))

    (is (equal menu-item (first (gtk-radio-menu-item-get-group menu-item))))

    (is (string= "Label" (gtk-label-label (gtk-bin-child menu-item))))
    (is (string= "Label" (gtk-label-text (gtk-bin-child menu-item))))

))

;;;     gtk_radio_menu_item_new_with_mnemonic

(test gtk-radio-menu-item-new-with-mnemonic
  (let ((menu-item (gtk-radio-menu-item-new-with-mnemonic nil "_Label")))

    (is (eq 'gtk-radio-menu-item (type-of menu-item)))
    (is (eq 'gtk-accel-label (type-of (gtk-bin-child menu-item))))

    (is (equal menu-item (first (gtk-radio-menu-item-get-group menu-item))))

    (is (string= "_Label" (gtk-label-label (gtk-bin-child menu-item))))
    (is (string= "Label" (gtk-label-text (gtk-bin-child menu-item))))

))

;;;     gtk_radio_menu_item_new_from_widget

(test gtk-radio-menu-item-new-from-widget
  (let ((menu-item (gtk-radio-menu-item-new-from-widget nil)))

    (is (eq 'gtk-radio-menu-item (type-of menu-item)))
    (is-false (gtk-bin-child menu-item))

))

;;;     gtk_radio_menu_item_new_with_label_from_widget
;;;     gtk_radio_menu_item_new_with_mnemonic_from_widget
;;;     gtk_radio_menu_item_set_group

;;;     gtk_radio_menu_item_get_group

(test gtk-radio-menu-item-get-group
  (let ((menu-item (gtk-radio-menu-item-new nil)))

    (is-true (listp (gtk-radio-menu-item-get-group menu-item)))
    (is (eq 'gtk-radio-menu-item (type-of (first (gtk-radio-menu-item-get-group menu-item)))))

))

;;;     gtk_radio_menu_item_join_group

