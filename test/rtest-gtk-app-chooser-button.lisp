(def-suite gtk-app-chooser-button :in gtk-suite)
(in-suite gtk-app-chooser-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserButton

(test gtk-app-chooser-button-class
  ;; Type check
  (is-true  (g-type-is-object "GtkAppChooserButton"))
  ;; Check the registered name
  (is (eq 'gtk-app-chooser-button
          (registered-object-type-by-name "GtkAppChooserButton")))
  ;; Check the type initializer
  (is (string= "GtkAppChooserButton"
               (g-type-name (gtype (foreign-funcall "gtk_app_chooser_button_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkComboBox") (g-type-parent "GtkAppChooserButton")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkAppChooserButton"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout" "GtkCellEditable"
               "GtkAppChooser")
             (mapcar #'gtype-name (g-type-interfaces "GtkAppChooserButton"))))
  ;; Check the class properties
  (is (equal '("active" "active-id" "add-tearoffs" "app-paintable" "border-width"
               "button-sensitivity" "can-default" "can-focus" "cell-area" "child"
               "column-span-column" "composite-child" "content-type" "double-buffered"
               "editing-canceled" "entry-text-column" "events" "expand" "focus-on-click"
               "halign" "has-default" "has-entry" "has-focus" "has-frame" "has-tooltip"
               "heading" "height-request" "hexpand" "hexpand-set" "id-column" "is-focus"
               "margin" "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "model" "name" "no-show-all" "opacity" "parent"
               "popup-fixed-width" "popup-shown" "receives-default" "resize-mode"
               "row-span-column" "scale-factor" "sensitive" "show-default-item"
               "show-dialog-item" "style" "tearoff-title" "tooltip-markup" "tooltip-text"
               "valign" "vexpand" "vexpand-set" "visible" "width-request" "window"
               "wrap-width")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkAppChooserButton"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "appears-as-list"
               "arrow-scaling" "arrow-size" "shadow-type")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkAppChooserButton"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkAppChooserButton"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAppChooserButton" GTK-APP-CHOOSER-BUTTON
                       (:SUPERCLASS GTK-COMBO-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable"
                         "GtkCellEditable" "GtkCellLayout")
                        :TYPE-INITIALIZER "gtk_app_chooser_button_get_type")
                       ((HEADING GTK-APP-CHOOSER-BUTTON-HEADING "heading"
                         "gchararray" T T)
                        (SHOW-DEFAULT-ITEM
                         GTK-APP-CHOOSER-BUTTON-SHOW-DEFAULT-ITEM
                         "show-default-item" "gboolean" T T)
                        (SHOW-DIALOG-ITEM
                         GTK-APP-CHOOSER-BUTTON-SHOW-DIALOG-ITEM
                         "show-dialog-item" "gboolean" T T)))
             (get-g-type-definition "GtkAppChooserButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-button-properties
  (let ((chooser (make-instance 'gtk-app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk-app-chooser-button-heading chooser))
    (is-false (gtk-app-chooser-button-show-default-item chooser))
    (is-false (gtk-app-chooser-button-show-dialog-item chooser))))

;;; --- Signals ----------------------------------------------------------------

;;;         void    custom-item-activated     Has Details

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_button_new

(test gtk-app-chooser-button-new
  (is (eq 'gtk-app-chooser-button (type-of (gtk-app-chooser-button-new nil))))
  (is (eq 'gtk-app-chooser-button (type-of (gtk-app-chooser-button-new (null-pointer)))))
  (is (eq 'gtk-app-chooser-button (type-of (gtk-app-chooser-button-new "plain/text")))))

;;;     gtk_app_chooser_button_append_custom_item
;;;     gtk_app_chooser_button_append_separator
;;;     gtk_app_chooser_button_set_active_custom_item

