(def-suite gtk-print-unix-dialog :in gtk-suite)
(in-suite gtk-print-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintCapabilities

(test gtk-print-capabilities
  ;; Check the type
  (is-true (g-type-is-flags "GtkPrintCapabilities"))
  ;; Check the registered name
  (is (eql 'gtk-print-capabilities
           (gobject::registered-flags-type "GtkPrintCapabilities")))
  ;; Check the type initializer
  (is (string= "GtkPrintCapabilities"
               (g-type-name (gtype (foreign-funcall "gtk_print_capabilities_get_type" :int)))))
  ;; Check the names
  (is (equal '("GTK_PRINT_CAPABILITY_PAGE_SET" "GTK_PRINT_CAPABILITY_COPIES"
               "GTK_PRINT_CAPABILITY_COLLATE" "GTK_PRINT_CAPABILITY_REVERSE"
               "GTK_PRINT_CAPABILITY_SCALE" "GTK_PRINT_CAPABILITY_GENERATE_PDF"
               "GTK_PRINT_CAPABILITY_GENERATE_PS" "GTK_PRINT_CAPABILITY_PREVIEW"
               "GTK_PRINT_CAPABILITY_NUMBER_UP" "GTK_PRINT_CAPABILITY_NUMBER_UP_LAYOUT")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GtkPrintCapabilities"))))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64 128 256 512)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GtkPrintCapabilities"))))
  ;; Check the nick names
  (is (equal '("page-set" "copies" "collate" "reverse" "scale" "generate-pdf" "generate-ps"
               "preview" "number-up" "number-up-layout")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GtkPrintCapabilities"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkPrintCapabilities"
                              GTK-PRINT-CAPABILITIES
                              (:EXPORT T :TYPE-INITIALIZER "gtk_print_capabilities_get_type")
                              (:PAGE-SET 1)
                              (:COPIES 2)
                              (:COLLATE 4)
                              (:REVERSE 8)
                              (:SCALE 16)
                              (:GENERATE-PDF 32)
                              (:GENERATE-PS 64)
                              (:PREVIEW 128)
                              (:NUMBER-UP 256)
                              (:NUMBER-UP-LAYOUT 512))
             (gobject::get-g-type-definition "GtkPrintCapabilities"))))

;;;     GtkPrintUnixDialog

(test gtk-print-unix-dialog-class
  ;; Type check
  (is-true  (g-type-is-object "GtkPrintUnixDialog"))
  ;; Check the registered name
  (is (eq 'gtk-print-unix-dialog
          (registered-object-type-by-name "GtkPrintUnixDialog")))
  ;; Check the type initializer
  (is (string= "GtkPrintUnixDialog"
               (g-type-name (gtype (foreign-funcall "gtk_print_unix_dialog_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkDialog") (g-type-parent "GtkPrintUnixDialog")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkPrintUnixDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkPrintUnixDialog"))))
  ;; Check the class properties
  (is (equal '("accept-focus" "app-paintable" "application" "attached-to" "border-width"
               "can-default" "can-focus" "child" "composite-child" "current-page" "decorated"
               "default-height" "default-width" "deletable" "destroy-with-parent"
               "double-buffered" "embed-page-setup" "events" "expand" "focus-on-click"
               "focus-on-map" "focus-visible" "gravity" "halign" "has-default" "has-focus"
               "has-resize-grip" "has-selection" "has-tooltip" "has-toplevel-focus"
               "height-request" "hexpand" "hexpand-set" "hide-titlebar-when-maximized" "icon"
               "icon-name" "is-active" "is-focus" "is-maximized" "manual-capabilities"
               "margin" "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "mnemonics-visible" "modal" "name" "no-show-all"
               "opacity" "page-setup" "parent" "print-settings" "receives-default"
               "resizable" "resize-grip-visible" "resize-mode" "role" "scale-factor" "screen"
               "selected-printer" "sensitive" "skip-pager-hint" "skip-taskbar-hint"
               "startup-id" "style" "support-selection" "title" "tooltip-markup"
               "tooltip-text" "transient-for" "type" "type-hint" "urgency-hint"
               "use-header-bar" "valign" "vexpand" "vexpand-set" "visible" "width-request"
               "window" "window-position")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkPrintUnixDialog"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging"
               "decoration-button-layout" "decoration-resize-handle" "action-area-border"
               "button-spacing" "content-area-border" "content-area-spacing")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkPrintUnixDialog"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkPrintUnixDialog"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrintUnixDialog" GTK-PRINT-UNIX-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_print_unix_dialog_get_type")
                       ((CURRENT-PAGE GTK-PRINT-UNIX-DIALOG-CURRENT-PAGE
                         "current-page" "gint" T T)
                        (EMBED-PAGE-SETUP
                         GTK-PRINT-UNIX-DIALOG-EMBED-PAGE-SETUP
                         "embed-page-setup" "gboolean" T T)
                        (HAS-SELECTION GTK-PRINT-UNIX-DIALOG-HAS-SELECTION
                         "has-selection" "gboolean" T T)
                        (MANUAL-CAPABILITIES
                         GTK-PRINT-UNIX-DIALOG-MANUAL-CAPABILITIES
                         "manual-capabilities" "GtkPrintCapabilities" T T)
                        (PAGE-SETUP GTK-PRINT-UNIX-DIALOG-PAGE-SETUP
                         "page-setup" "GtkPageSetup" T T)
                        (PRINT-SETTINGS GTK-PRINT-UNIX-DIALOG-PRINT-SETTINGS
                         "print-settings" "GtkPrintSettings" T T)
                        (SELECTED-PRINTER
                         GTK-PRINT-UNIX-DIALOG-SELECTED-PRINTER
                         "selected-printer" "GtkPrinter" T NIL)
                        (SUPPORT-SELECTION
                         GTK-PRINT-UNIX-DIALOG-SUPPORT-SELECTION
                         "support-selection" "gboolean" T T)))
             (get-g-type-definition "GtkPrintUnixDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-print-unix-dialog-properties
  (let ((dialog (make-instance 'gtk-print-unix-dialog)))
    ;; current-page
    (is (= -1 (gtk-print-unix-dialog-current-page dialog)))
    (is (= 10 (setf (gtk-print-unix-dialog-current-page dialog) 10)))
    (is (= 10 (gtk-print-unix-dialog-current-page dialog)))
    ;; embed-page-setup
    (is-false (gtk-print-unix-dialog-embed-page-setup dialog))
    (is-true (setf (gtk-print-unix-dialog-embed-page-setup dialog) t))
    (is-true (gtk-print-unix-dialog-embed-page-setup dialog))
    ;; has-selection
    (is-false (gtk-print-unix-dialog-has-selection dialog))
    (is-true (setf (gtk-print-unix-dialog-has-selection dialog) t))
    (is-true (gtk-print-unix-dialog-has-selection dialog))
    ;; manual-capabilities
    (is-false (gtk-print-unix-dialog-manual-capabilities dialog))
    (is (equal '(:page-set :scale)
               (setf (gtk-print-unix-dialog-manual-capabilities dialog) '(:page-set :scale))))
    (is (equal '(:page-set :scale) (gtk-print-unix-dialog-manual-capabilities dialog)))
    ;; page-setup
    (is (eq 'gtk-page-setup (type-of (gtk-print-unix-dialog-page-setup dialog))))
    (is (eq 'gtk-page-setup
            (type-of (setf (gtk-print-unix-dialog-page-setup dialog)
                           (make-instance 'gtk-page-setup)))))
    (is (eq 'gtk-page-setup (type-of (gtk-print-unix-dialog-page-setup dialog))))
    ;; print-settings
    (is (eq 'gtk-print-settings (type-of (gtk-print-unix-dialog-print-settings dialog))))
    (is (eq 'gtk-print-settings
            (type-of (setf (gtk-print-unix-dialog-print-settings dialog)
                           (make-instance 'gtk-print-settings)))))
    (is (eq 'gtk-print-settings (type-of (gtk-print-unix-dialog-print-settings dialog))))
    ;; selected-printer
    (is-false (gtk-print-unix-dialog-selected-printer dialog))
    ;; selected-printer is not writeable
    (signals (error) (setf (gtk-print-unix-dialog-selected-printer dialog) (make-instance 'gtk-printer)))
    ;; support-selection
    (is-false (gtk-print-unix-dialog-support-selection dialog))
    (is-true (setf (gtk-print-unix-dialog-support-selection dialog) t))
    (is-true (gtk-print-unix-dialog-support-selection dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_unix_dialog_new

(test gtk-print-unix-dialog-new
  (let ((window (make-instance 'gtk-window)))
    (is (eq 'gtk-print-unix-dialog (type-of (gtk-print-unix-dialog-new nil nil))))
    (is (eq 'gtk-print-unix-dialog (type-of (gtk-print-unix-dialog-new "title" window))))
    (is (eq 'gtk-print-unix-dialog (type-of (gtk-print-unix-dialog-new nil window))))
    (is (eq 'gtk-print-unix-dialog (type-of (gtk-print-unix-dialog-new "title" window))))))

;;;     gtk_print_unix_dialog_set_settings
;;;     gtk_print_unix_dialog_get_settings



;;;     gtk_print_unix_dialog_add_custom_tab

;;;     gtk_print_unix_dialog_get_page_setup_set

