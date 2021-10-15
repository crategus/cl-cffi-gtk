(def-suite gtk-dialog :in gtk-suite)
(in-suite gtk-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDialogFlags

(test gtk-dialog-flags
  ;; Check the type
  (is (g-type-is-flags "GtkDialogFlags"))
  ;; Check the registered name
  (is (eq 'gtk-dialog-flags
          (registered-flags-type "GtkDialogFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GtkDialogFlags")
          (gtype (foreign-funcall "gtk_dialog_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("GTK_DIALOG_MODAL" "GTK_DIALOG_DESTROY_WITH_PARENT"
               "GTK_DIALOG_USE_HEADER_BAR")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkDialogFlags"))))
  ;; Check the values
  (is (equal '(1 2 4)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkDialogFlags"))))
  ;; Check the nick names
  (is (equal '("modal" "destroy-with-parent" "use-header-bar")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkDialogFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkDialogFlags"
                              GTK-DIALOG-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_dialog_flags_get_type")
                              (:MODAL 1)
                              (:DESTROY-WITH-PARENT 2)
                              (:USE-HEADER-BAR 4))
             (get-g-type-definition "GtkDialogFlags"))))

;;;     GtkResponseType

(test gtk-response-type
  ;; Check the type
  (is (g-type-is-enum "GtkResponseType"))
  ;; Check the type initializer
  (is (eq (gtype "GtkResponseType")
          (gtype (foreign-funcall "gtk_response_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-response-type
          (registered-enum-type "GtkResponseType")))
  ;; Check the names
  (is (equal '("GTK_RESPONSE_NONE" "GTK_RESPONSE_REJECT" "GTK_RESPONSE_ACCEPT"
               "GTK_RESPONSE_DELETE_EVENT" "GTK_RESPONSE_OK"
               "GTK_RESPONSE_CANCEL" "GTK_RESPONSE_CLOSE" "GTK_RESPONSE_YES"
               "GTK_RESPONSE_NO" "GTK_RESPONSE_APPLY" "GTK_RESPONSE_HELP")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkResponseType"))))
  ;; Check the values
  (is (equal '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkResponseType"))))
  ;; Check the nick names
  (is (equal '("none" "reject" "accept" "delete-event" "ok" "cancel" "close"
               "yes" "no" "apply" "help")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkResponseType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkResponseType"
                              GTK-RESPONSE-TYPE
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_response_type_get_type")
                              (:NONE -1)
                              (:REJECT -2)
                              (:ACCEPT -3)
                              (:DELETE-EVENT -4)
                              (:OK -5)
                              (:CANCEL -6)
                              (:CLOSE -7)
                              (:YES -8)
                              (:NO -9)
                              (:APPLY -10)
                              (:HELP -11))
             (get-g-type-definition "GtkResponseType"))))

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
  (is (eq (gtype "GtkWindow")
          (g-type-parent "GtkDialog")))
  ;; Check the children
  #-windows
  (is (equal '("GtkMessageDialog" "GtkAboutDialog" "GtkColorChooserDialog"
               "GtkColorSelectionDialog" "GtkFileChooserDialog"
               "GtkFontChooserDialog" "GtkFontSelectionDialog"
               "GtkRecentChooserDialog" "GtkAppChooserDialog"
               "GtkPrintUnixDialog" "GtkPageSetupUnixDialog")
             (mapcar #'g-type-name
                     (g-type-children "GtkDialog"))))
  #+windows
  (is (equal '("GtkMessageDialog" "GtkAboutDialog" "GtkColorChooserDialog"
               "GtkColorSelectionDialog" "GtkFileChooserDialog"
               "GtkFontChooserDialog" "GtkFontSelectionDialog"
               "GtkRecentChooserDialog" "GtkAppChooserDialog")
             (mapcar #'g-type-name
                     (g-type-children "GtkDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name
                     (g-type-interfaces "GtkDialog"))))
  ;; Check the class properties
  (is (equal '("accept-focus" "app-paintable" "application" "attached-to"
               "border-width" "can-default" "can-focus" "child"
               "composite-child" "decorated" "default-height" "default-width"
               "deletable" "destroy-with-parent" "double-buffered" "events"
               "expand" "focus-on-click" "focus-on-map" "focus-visible"
               "gravity" "halign" "has-default" "has-focus" "has-resize-grip"
               "has-tooltip" "has-toplevel-focus" "height-request" "hexpand"
               "hexpand-set" "hide-titlebar-when-maximized" "icon" "icon-name"
               "is-active" "is-focus" "is-maximized" "margin" "margin-bottom"
               "margin-end" "margin-left" "margin-right" "margin-start"
               "margin-top" "mnemonics-visible" "modal" "name" "no-show-all"
               "opacity" "parent" "receives-default" "resizable"
               "resize-grip-visible" "resize-mode" "role" "scale-factor"
               "screen" "sensitive" "skip-pager-hint" "skip-taskbar-hint"
               "startup-id" "style" "title" "tooltip-markup" "tooltip-text"
               "transient-for" "type" "type-hint" "urgency-hint"
               "use-header-bar" "valign" "vexpand" "vexpand-set" "visible"
               "width-request" "window" "window-position")
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GtkDialog"))
                   #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging" "decoration-button-layout"
               "decoration-resize-handle" "action-area-border" "button-spacing"
               "content-area-border" "content-area-spacing")
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

;;; --- Properties -------------------------------------------------------------

(test gtk-dialog-properties.1
  (let ((dialog (make-instance 'gtk-dialog)))
    (is (= 0 (gtk-dialog-use-header-bar dialog)))))

(test gtk-dialog-properties.2
  (let ((dialog (make-instance 'gtk-dialog :use-header-bar 1)))
    (is (= 1 (gtk-dialog-use-header-bar dialog)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-dialog-style-properties
  (let ((dialog (make-instance 'gtk-dialog)))
    (is (= 0 (gtk-widget-style-property dialog "action-area-border")))
    (is (= 4 (gtk-widget-style-property dialog "button-spacing")))
    (is (= 2 (gtk-widget-style-property dialog "content-area-border")))
    (is (= 0 (gtk-widget-style-property dialog "content-area-spacing")))))

;;; --- Signals ----------------------------------------------------------------

;;;     void   close                   Action

(test gtk-dialog-signal-close
  ;; Check the list of signals
  (is (member "close"
              (mapcar #'g-signal-name (g-signal-list-ids "GtkDialog"))
              :test #'string=))
  ;; Query info for "close" signal
  (let ((query (g-signal-query (g-signal-lookup "close" "GtkDialog"))))
    (is (string= "close" (g-signal-query-signal-name query)))
    (is (string= "GtkDialog" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g-type-name (g-signal-query-param-types query))
                     #'string<)))
    (is-false (g-signal-query-signal-detail query))))

;;;     void   response                Run Last

(test gtk-dialog-signal-response
  ;; Check the list of signals
  (is (member "response"
              (mapcar #'g-signal-name (g-signal-list-ids "GtkDialog"))
              :test #'string=))
  ;; Query info for "response" signal
  (let ((query (g-signal-query (g-signal-lookup "response" "GtkDialog"))))
    (is (string= "response" (g-signal-query-signal-name query)))
    (is (string= "GtkDialog" (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("gint")
               (sort (mapcar #'g-type-name (g-signal-query-param-types query))
                     #'string<)))
    (is-false (g-signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk-dialog-new
;;;     gtk-dialog-new-with-buttons

(test gtk-dialog-new
  (is (typep (gtk-dialog-new) 'gtk-dialog))
  (is (typep (gtk-dialog-new-with-buttons "title"
                                          nil :modal "OK" 1 "Cancel" 2)
             'gtk-dialog))
  (is (typep (gtk-dialog-new-with-buttons "title"
                                          nil '(:modal) "OK" 1 "Cancel" 2)
             'gtk-dialog)))

;;;     gtk_dialog_run
;;;     gtk_dialog_response

;;;     gtk-dialog-add-button

(test gtk-dialog-add-button
  (let ((dialog (make-instance 'gtk-dialog)))
    ;; No button in the action area
    (is (= 0 (length (gtk-container-children (gtk-dialog-action-area dialog)))))
    (is (equal '() (gtk-container-children (gtk-dialog-action-area dialog))))
    ;; Add first button to the action area
    (is (typep (gtk-dialog-add-button dialog "button1" 1) 'gtk-button))
    (is (= 1 (length (gtk-container-children (gtk-dialog-action-area dialog)))))
    (is (every #'(lambda (x) (typep x 'gtk-button))
               (gtk-container-children (gtk-dialog-action-area dialog))))
    ;; Add another button to the action area
    (is (typep (gtk-dialog-add-button dialog "button2" 2) 'gtk-button))
    (is (= 2 (length (gtk-container-children (gtk-dialog-action-area dialog)))))
    (is (every #'(lambda (x) (typep x 'gtk-button))
               (gtk-container-children (gtk-dialog-action-area dialog))))))

;;;     gtk-dialog-add-buttons

(test gtk-dialog-add-buttons
  (let ((dialog (make-instance 'gtk-dialog)))
    ;; No button in the action area
    (is (= 0 (length (gtk-container-children (gtk-dialog-action-area dialog)))))
    (is (equal '() (gtk-container-children (gtk-dialog-action-area dialog))))
    ;; Add two buttons
    (is-false (gtk-dialog-add-buttons dialog "button1" 1 "button2" 2))
    (is (= 2 (length (gtk-container-children (gtk-dialog-action-area dialog)))))
    (is (every #'(lambda (x) (typep x 'gtk-button))
               (gtk-container-children (gtk-dialog-action-area dialog))))))

;;;     gtk-dialog-add-action-widget

(test gtk-dialog-add-action-widget
  (let ((dialog (make-instance 'gtk-dialog))
        (widget (make-instance 'gtk-button :label "BUTTON")))
    (is-false (gtk-dialog-add-action-widget dialog widget 1))
    (is (member widget
               (gtk-container-children (gtk-dialog-action-area dialog))
               :test #'eq))))

;;;     gtk-dialog-set-default-response

(test gtk-dialog-set-default-response
  (let ((dialog (make-instance 'gtk-dialog)))
    (is-false (gtk-dialog-add-buttons dialog "button1" 1 "button2" 2))
    (is-false (gtk-dialog-set-default-response dialog 2))))

;;;     gtk-dialog-set-response-sensitive

(test gtk-dialog-set-response-sensitive
  (let ((dialog (make-instance 'gtk-dialog)))
    (is-false (gtk-dialog-add-buttons dialog "button1" 1 "button2" 2))
    (is-false (gtk-dialog-set-response-sensitive dialog 1 t))
    (is-false (gtk-dialog-set-response-sensitive dialog 2 nil))
    (let ((buttons (gtk-container-children (gtk-dialog-action-area dialog))))
      (is-true (gtk-widget-sensitive (first buttons)))
      (is-false (gtk-widget-sensitive (second buttons))))))

;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response

;;;     gtk-dialog-action-area

(test gtk-dialog-action-area
  (let ((dialog (gtk-dialog-new-with-buttons "title"
                                             nil :modal "OK" 1 "Cancel" 2)))
    (is (typep (gtk-dialog-action-area dialog) 'gtk-button-box))
    (is (every (lambda (x) (typep x 'gtk-button))
               (gtk-container-children (gtk-dialog-action-area dialog))))))

;;;     gtk-dialog-content-area

(test gtk-dialog-content-area
  (let ((dialog (gtk-dialog-new-with-buttons "title"
                                             nil :modal "OK" 1 "Cancel" 2)))
    (is (typep (gtk-dialog-content-area dialog) 'gtk-box))
    (is (eq :vertical
            (gtk-orientable-orientation (gtk-dialog-content-area dialog))))))

;;;     gtk_alternative_dialog_button_order
;;;     gtk_dialog_set_alternative_button_order
;;;     gtk_dialog_set_alternative_button_order_from_array

;;; 2021-10-15
