(def-suite gtk-page-setup-unix-dialog :in gtk-suite)
(in-suite gtk-page-setup-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetupUnixDialog

(test gtk-page-setup-unix-dialog-class
  ;; Type check
  (is-true  (g-type-is-object "GtkPageSetupUnixDialog"))
  ;; Check the registered name
  (is (eq 'gtk-page-setup-unix-dialog
          (registered-object-type-by-name "GtkPageSetupUnixDialog")))
  ;; Check the type initializer
  (is (string= "GtkPageSetupUnixDialog"
               (g-type-name (gtype (foreign-funcall "gtk_page_setup_unix_dialog_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkDialog") (g-type-parent "GtkPageSetupUnixDialog")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkPageSetupUnixDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkPageSetupUnixDialog"))))
  ;; Check the class properties
  (is (equal '("accept-focus" "app-paintable" "application" "attached-to" "border-width"
               "can-default" "can-focus" "child" "composite-child" "decorated"
               "default-height" "default-width" "deletable" "destroy-with-parent"
               "double-buffered" "events" "expand" "focus-on-click" "focus-on-map"
               "focus-visible" "gravity" "halign" "has-default" "has-focus" "has-resize-grip"
               "has-tooltip" "has-toplevel-focus" "height-request" "hexpand" "hexpand-set"
               "hide-titlebar-when-maximized" "icon" "icon-name" "is-active" "is-focus"
               "is-maximized" "margin" "margin-bottom" "margin-end" "margin-left"
               "margin-right" "margin-start" "margin-top" "mnemonics-visible" "modal" "name"
               "no-show-all" "opacity" "parent" "receives-default" "resizable"
               "resize-grip-visible" "resize-mode" "role" "scale-factor" "screen" "sensitive"
               "skip-pager-hint" "skip-taskbar-hint" "startup-id" "style" "title"
               "tooltip-markup" "tooltip-text" "transient-for" "type" "type-hint"
               "urgency-hint" "use-header-bar" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window" "window-position")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkPageSetupUnixDialog"))
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
                     (gtk-widget-class-list-style-properties "GtkPageSetupUnixDialog"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkPageSetupUnixDialog"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPageSetupUnixDialog" GTK-PAGE-SETUP-UNIX-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER
                        "gtk_page_setup_unix_dialog_get_type")
                       NIL)
             (get-g-type-definition "GtkPageSetupUnixDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_unix_dialog_new

(test gtk-page-setup-unix-dialog-new.1
  (let ((window (make-instance 'gtk-window)))
    (is (eq 'gtk-page-setup-unix-dialog (type-of (gtk-page-setup-unix-dialog-new nil nil))))
    (is (eq 'gtk-page-setup-unix-dialog (type-of (gtk-page-setup-unix-dialog-new "title" nil))))
    (is (eq 'gtk-page-setup-unix-dialog (type-of (gtk-page-setup-unix-dialog-new nil window))))
    (is (eq 'gtk-page-setup-unix-dialog (type-of (gtk-page-setup-unix-dialog-new "title" window))))))

(test gtk-page-setup-unix-dialog-new.2
  (let* ((window (make-instance 'gtk-window))
         (dialog (gtk-page-setup-unix-dialog-new "title" window)))

    (is (eq 'gtk-page-setup-unix-dialog (type-of dialog)))
    (is (string= "title" (gtk-window-title dialog)))
    (is (eq window (gtk-window-transient-for dialog)))))

;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup

(test gtk-page-setup-unix-dialog-page-setup
  (let ((dialog (gtk-page-setup-unix-dialog-new "title" nil))
        (page-setup (gtk-page-setup-new)))
    (is (eq 'gtk-page-setup (type-of (gtk-page-setup-unix-dialog-page-setup dialog))))
    (is (eq page-setup (setf (gtk-page-setup-unix-dialog-page-setup dialog) page-setup)))
    ;; TODO: This should be true.
    (is-false (eq page-setup (gtk-page-setup-unix-dialog-page-setup dialog)))))

;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings

(test gtk-page-setup-unix-dialog-print-settings
  (let ((dialog (gtk-page-setup-unix-dialog-new "title" nil))
        (print-settings (gtk-print-settings-new)))
    (is (eq 'gtk-print-settings (type-of print-settings)))
    (is-false (gtk-page-setup-unix-dialog-print-settings dialog))
    (is (eq 'gtk-print-settings
            (type-of (setf (gtk-page-setup-unix-dialog-print-settings dialog) print-settings))))
    ;; TODO: This should be true.
    (is-false (eq 'gtk-print-settings
              (type-of (gtk-page-setup-unix-dialog-print-settings dialog))))))

