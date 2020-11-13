(def-suite gtk-dialog :in gtk-suite)
(in-suite gtk-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDialog

(test gtk-dialog-class
  ;; Type check
  (is (g-type-is-object "GtkDialog"))
  ;; Check the registered name
  (is (eq 'gtk-dialog
          (registered-object-type-by-name "GtkDialog")))
  ;; Check the type initializer
  (is (eq (gtype "GtkDialog")
          (gtype (foreign-funcall "gtk_dialog_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWindow") (g-type-parent "GtkDialog")))
  ;; Check the children
  (is (equal '("GtkMessageDialog" "GtkAboutDialog" "GtkColorChooserDialog"
               "GtkColorSelectionDialog" "GtkFileChooserDialog" "GtkFontChooserDialog"
               "GtkFontSelectionDialog" "GtkRecentChooserDialog" "GtkAppChooserDialog"
               "GtkPrintUnixDialog" "GtkPageSetupUnixDialog")
             (mapcar #'g-type-name (g-type-children "GtkDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkDialog"))))
  ;; Check the class properties
  (is (equal '("accept-focus" "app-paintable" "application" "attached-to"
               "border-width" "can-default" "can-focus" "child" "composite-child"
               "decorated" "default-height" "default-width" "deletable"
               "destroy-with-parent" "double-buffered" "events" "expand"
               "focus-on-click" "focus-on-map" "focus-visible" "gravity" "halign"
               "has-default" "has-focus" "has-resize-grip" "has-tooltip"
               "has-toplevel-focus" "height-request" "hexpand" "hexpand-set"
               "hide-titlebar-when-maximized" "icon" "icon-name" "is-active" "is-focus"
               "is-maximized" "margin" "margin-bottom" "margin-end" "margin-left"
               "margin-right" "margin-start" "margin-top" "mnemonics-visible"
               "modal" "name" "no-show-all" "opacity" "parent" "receives-default"
               "resizable" "resize-grip-visible" "resize-mode" "role" "scale-factor"
               "screen" "sensitive" "skip-pager-hint" "skip-taskbar-hint" "startup-id"
               "style" "title" "tooltip-markup" "tooltip-text" "transient-for" "type"
               "type-hint" "urgency-hint" "use-header-bar" "valign" "vexpand"
               "vexpand-set" "visible" "width-request" "window" "window-position")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkDialog"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
               "separator-height" "separator-width" "text-handle-height"
               "text-handle-width" "visited-link-color" "wide-separators"
               "window-dragging" "decoration-button-layout" "decoration-resize-handle"
               "action-area-border" "button-spacing" "content-area-border"
               "content-area-spacing")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkDialog"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkDialog"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkDialog" GTK-DIALOG
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_dialog_get_type")
                       ((USE-HEADER-BAR GTK-DIALOG-USE-HEADER-BAR
                         "use-header-bar" "gint" T NIL)))
             (get-g-type-definition "GtkDialog"))))


(test gtk-dialog-style-properties
  (let ((widget (make-instance 'gtk-dialog)))
    (is (= 0.04 (gtk-widget-style-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-property widget "cursor-color"))
    (is (equal "" (gtk-widget-style-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-property widget "focus-line-width")))
    (is-true (integerp (gtk-widget-style-property widget "focus-padding")))
    (is-true (gtk-widget-style-property widget "interior-focus"))
    (is-false (gtk-widget-style-property widget "link-color"))
    (is (= 16 (gtk-widget-style-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-property widget "secondary-cursor-color"))
    (is-true (integerp (gtk-widget-style-property widget "separator-height")))
    (is-true (integerp (gtk-widget-style-property widget "separator-width")))
    (is (= 24 (gtk-widget-style-property widget "text-handle-height")))
    (is (= 20 (gtk-widget-style-property widget "text-handle-width")))
    (is-false (gtk-widget-style-property widget "visited-link-color"))
    (is-false  (gtk-widget-style-property widget "wide-separators"))
    (is-false (gtk-widget-style-property widget "window-dragging"))
    (is (= 0 (gtk-widget-style-property widget "action-area-border")))
    (is (= 4 (gtk-widget-style-property widget "button-spacing")))
    (is (= 2 (gtk-widget-style-property widget "content-area-border")))
    (is (= 0 (gtk-widget-style-property widget "content-area-spacing")))))

;;;     GtkDialogFlags
;;;     GtkResponseType
;;;
;;;     gtk_dialog_new
;;;     gtk_dialog_new_with_buttons
;;;     gtk_dialog_run
;;;     gtk_dialog_response
;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_add_action_widget
;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_get_action_area
;;;     gtk_dialog_get_content_area
;;;     gtk_alternative_dialog_button_order
;;;     gtk_dialog_set_alternative_button_order
;;;     gtk_dialog_set_alternative_button_order_from_array

