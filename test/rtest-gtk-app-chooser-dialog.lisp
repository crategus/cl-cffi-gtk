(def-suite gtk-app-chooser-dialog :in gtk-suite)
(in-suite gtk-app-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test gtk-app-chooser-dialog-class
  ;; Type check
  (is-true  (g-type-is-object "GtkAppChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk-app-chooser-dialog
          (registered-object-type-by-name "GtkAppChooserDialog")))
  ;; Check the type initializer
  (is (string= "GtkAppChooserDialog"
               (g-type-name (gtype (foreign-funcall "gtk_app_chooser_dialog_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkDialog") (g-type-parent "GtkAppChooserDialog")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkAppChooserDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkAppChooser")
             (mapcar #'gtype-name (g-type-interfaces "GtkAppChooserDialog"))))
  ;; Check the class properties
  (is (equal '("accept-focus" "app-paintable" "application" "attached-to" "border-width"
               "can-default" "can-focus" "child" "composite-child" "content-type" "decorated"
               "default-height" "default-width" "deletable" "destroy-with-parent"
               "double-buffered" "events" "expand" "focus-on-click" "focus-on-map"
               "focus-visible" "gfile" "gravity" "halign" "has-default" "has-focus"
               "has-resize-grip" "has-tooltip" "has-toplevel-focus" "heading"
               "height-request" "hexpand" "hexpand-set" "hide-titlebar-when-maximized" "icon"
               "icon-name" "is-active" "is-focus" "is-maximized" "margin" "margin-bottom"
               "margin-end" "margin-left" "margin-right" "margin-start" "margin-top"
               "mnemonics-visible" "modal" "name" "no-show-all" "opacity" "parent"
               "receives-default" "resizable" "resize-grip-visible" "resize-mode" "role"
               "scale-factor" "screen" "sensitive" "skip-pager-hint" "skip-taskbar-hint"
               "startup-id" "style" "title" "tooltip-markup" "tooltip-text" "transient-for"
               "type" "type-hint" "urgency-hint" "use-header-bar" "valign" "vexpand"
               "vexpand-set" "visible" "width-request" "window" "window-position")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkAppChooserDialog"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging"
               "decoration-button-layout" "decoration-resize-handle" "action-area-border"
               "button-spacing" "content-area-border" "content-area-spacing")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkAppChooserDialog"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkAppChooserDialog"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAppChooserDialog" GTK-APP-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_app_chooser_dialog_get_type")
                       ((GFILE GTK-APP-CHOOSER-DIALOG-GFILE "gfile" "GFile" T
                         NIL)
                        (HEADING GTK-APP-CHOOSER-DIALOG-HEADING "heading"
                         "gchararray" T T)))
             (get-g-type-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     GFile*   gfile      Read / Write / Construct Only
;;;     gchar*   heading    Read / Write

(test gtk-app-chooser-dialog-properties
 (let ((chooser (make-instance 'gtk-app-chooser-dialog)))
    (is-false (gtk-app-chooser-dialog-gfile chooser))
    (is-false (gtk-app-chooser-dialog-heading chooser))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new

;; FIXME: What is the problem?
;; GLib-GIO-CRITICAL: g_file_info_get_content_type: assertion 'G_IS_FILE_INFO (info)' failed

#+nil
(test gtk-app-chooser-dialog-new
  (let ((chooser (gtk-app-chooser-dialog-new nil '(:modal) (g-file-new-for-path "gio.file.lisp"))))

    (is (eq 'gtk-app-chooser-dialog (type-of chooser)))

    (is (eq 'g-object (type-of (gtk-app-chooser-dialog-gfile chooser))))
    (is (string= "/home/dieter/Lisp/lisp-projects/cl-gtk/test/gio.file.lisp"
                 (g-file-get-path (gtk-app-chooser-dialog-gfile chooser))))))

;;;     gtk_app_chooser_dialog_new_for_content_type

(test gtk-app-chooser-dialog-new-for-content-type
  (let ((chooser (gtk-app-chooser-dialog-new-for-content-type nil '(:modal) "plain/text")))
    (is (eq 'gtk-app-chooser-dialog (type-of chooser)))))

;;;     gtk_app_chooser_dialog_get_widget

(test gtk-app-chooser-dialog-widget
  (is (eq 'gtk-app-chooser-widget
          (type-of (gtk-app-chooser-dialog-widget (make-instance 'gtk-app-chooser-dialog))))))

