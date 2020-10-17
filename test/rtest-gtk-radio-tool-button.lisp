(def-suite gtk-radio-tool-button :in gtk-suite)
(in-suite gtk-radio-tool-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioToolButton

(test gtk-radio-tool-button-class
  ;; Type check
  (is-true  (g-type-is-object "GtkRadioToolButton"))
  ;; Check the registered name
  (is (eq 'gtk-radio-tool-button
          (registered-object-type-by-name "GtkRadioToolButton")))
  ;; Check the type initializer
  (is (string= "GtkRadioToolButton"
               (g-type-name (gtype (foreign-funcall "gtk_radio_tool_button_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkToggleToolButton") (g-type-parent "GtkRadioToolButton")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkRadioToolButton"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable" "GtkActionable")
             (mapcar #'gtype-name (g-type-interfaces "GtkRadioToolButton"))))
  ;; Check the class properties
  (is (equal '("action-name" "action-target" "active" "app-paintable" "border-width"
               "can-default" "can-focus" "child" "composite-child" "double-buffered" "events"
               "expand" "focus-on-click" "group" "halign" "has-default" "has-focus"
               "has-tooltip" "height-request" "hexpand" "hexpand-set" "icon-name"
               "icon-widget" "is-focus" "is-important" "label" "label-widget" "margin"
               "margin-bottom" "margin-end" "margin-left" "margin-right" "margin-start"
               "margin-top" "name" "no-show-all" "opacity" "parent" "receives-default"
               "related-action" "resize-mode" "scale-factor" "sensitive" "stock-id" "style"
               "tooltip-markup" "tooltip-text" "use-action-appearance" "use-underline"
               "valign" "vexpand" "vexpand-set" "visible" "visible-horizontal"
               "visible-vertical" "width-request" "window")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkRadioToolButton"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "icon-spacing")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkRadioToolButton"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkRadioToolButton"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkRadioToolButton" GTK-RADIO-TOOL-BUTTON
                       (:SUPERCLASS GTK-TOGGLE-TOOL-BUTTON :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_tool_button_get_type")
                       ((GROUP GTK-RADIO-TOOL-BUTTON-GROUP "group"
                         "GtkRadioToolButton" NIL T)))
             (get-g-type-definition "GtkRadioToolButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkRadioToolButton*   group    Write

(test gtk-radio-tool-button-properties
  (let ((button (make-instance 'gtk-radio-tool-button)))
    ;; group is not readable
    (signals (error) (gtk-radio-tool-button-group button))
    ;; group is writable
    (is-false (setf (gtk-radio-tool-button-group button) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_tool_button_new

(test gtk-radio-tool-button-new
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-tool-button
          (type-of (setf button (gtk-radio-tool-button-new nil)))))
  ;; Second radio button
  (is (eq 'gtk-radio-tool-button
          (type-of (setf button (gtk-radio-tool-button-new (gtk-radio-tool-button-get-group button))))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-tool-button-get-group button))))
  (is (eq 'gtk-radio-button (type-of (first (gtk-radio-tool-button-get-group button)))))
  ;; No bin child
  (is (eq 'gtk-radio-button (type-of (gtk-bin-child button))))))

;;;     gtk_radio_tool_button_new_from_stock

;;;     gtk_radio_tool_button_new_from_widget

(test gtk-radio-tool-button-new-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk-radio-tool-button
          (type-of (setf button (gtk-radio-tool-button-new-from-widget nil)))))
  ;; Second radio button
  (is (eq 'gtk-radio-tool-button
          (type-of (setf button (gtk-radio-tool-button-new-from-widget button)))))
  ;; Check group list
  (is (= 2 (length (gtk-radio-tool-button-get-group button))))
  (is (eq 'gtk-radio-button (type-of (first (gtk-radio-tool-button-get-group button)))))
  ;; No bin child
  (is (eq 'gtk-radio-button (type-of (gtk-bin-child button))))))

;;;     gtk_radio_tool_button_new_with_stock_from_widget

;;;     gtk_radio_tool_button_get_group
;;;     gtk_radio_tool_button_set_group

(test gtk-radio-tool-button-group
  (let (button)
    ;; First radio button
    (is (eq 'gtk-radio-tool-button (type-of (setf button (gtk-radio-tool-button-new nil)))))
    (is (listp (gtk-radio-tool-button-get-group button)))
    (is (= 1 (length (gtk-radio-tool-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-tool-button-get-group button)))))
    ;; Second radio button
    (is (eq 'gtk-radio-tool-button
            (type-of (setf button (gtk-radio-tool-button-new (gtk-radio-tool-button-get-group button))))))
    (is (listp (gtk-radio-tool-button-get-group button)))
    (is (= 2 (length (gtk-radio-tool-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-tool-button-get-group button)))))
    ;; Third radio button
    (is (eq 'gtk-radio-tool-button
            (type-of (setf button (gtk-radio-tool-button-new (gtk-radio-tool-button-get-group button))))))
    (is (listp (gtk-radio-tool-button-get-group button)))
    (is (= 3 (length (gtk-radio-tool-button-get-group button))))
    (is (eq 'gtk-radio-button (type-of (first (gtk-radio-tool-button-get-group button)))))))

