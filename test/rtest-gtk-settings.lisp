(def-suite gtk-settings :in gtk-suite)
(in-suite gtk-settings)

;;; --- Types and Values ------------------------------------------------------

;;;     GtkSettingsValue                                   deprecated

;;;     GtkIMPreeditStyle

(test gtk-im-preedit-style
  ;; Check the type
  (is (g-type-is-enum "GtkIMPreeditStyle"))
  ;; Check the type initializer
  (is (eq (gtype "GtkIMPreeditStyle")
          (gtype (foreign-funcall "gtk_im_preedit_style_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-im-preedit-style
          (registered-enum-type "GtkIMPreeditStyle")))
  ;; Check the names
  (is (equal '("GTK_IM_PREEDIT_NOTHING" "GTK_IM_PREEDIT_CALLBACK"
               "GTK_IM_PREEDIT_NONE")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkIMPreeditStyle"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkIMPreeditStyle"))))
  ;; Check the nick names
  (is (equal '("nothing" "callback" "none")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkIMPreeditStyle"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkIMPreeditStyle" GTK-I-M-PREEDIT-STYLE
                             (:EXPORT T)
                             (:NOTHING 0)
                             (:CALLBACK 1)
                             (:NONE 2))
             (get-g-type-definition "GtkIMPreeditStyle"))))

;;;     GtkIMStatusStyle

(test gtk-im-status-style
  ;; Check the type
  (is (g-type-is-enum "GtkIMStatusStyle"))
  ;; Check the type initializer
  (is (eq (gtype "GtkIMStatusStyle")
          (gtype (foreign-funcall "gtk_im_status_style_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-im-status-style
          (registered-enum-type "GtkIMStatusStyle")))
  ;; Check the names
  (is (equal '("GTK_IM_STATUS_NOTHING" "GTK_IM_STATUS_CALLBACK"
               "GTK_IM_STATUS_NONE")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkIMStatusStyle"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkIMStatusStyle"))))
  ;; Check the nick names
  (is (equal '("nothing" "callback" "none")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkIMStatusStyle"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkIMStatusStyle" GTK-I-M-STATUS-STYLE
                             (:EXPORT T)
                             (:NOTHING 0)
                             (:CALLBACK 1)
                             (:NONE 2))
             (get-g-type-definition "GtkIMStatusStyle"))))

;;;     GtkSettings

(test gtk-settings-class
  ;; Type check
  (is (g-type-is-object "GtkSettings"))
  ;; Check the registered name
  (is (eq 'gtk-settings
          (registered-object-type-by-name "GtkSettings")))
  ;; Check the type initializer
  (is (eq (gtype "GtkSettings")
          (gtype (foreign-funcall "gtk_settings_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GtkSettings")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkSettings"))))
  ;; Check the interfaces
  (is (equal '("GtkStyleProvider" "GtkStyleProviderPrivate")
             (mapcar #'g-type-name (g-type-interfaces "GtkSettings"))))
  ;; Check the class properties
  (is (equal '("color-hash" "gtk-alternative-button-order"
               "gtk-alternative-sort-arrows" "gtk-application-prefer-dark-theme"
               "gtk-auto-mnemonics" "gtk-button-images" "gtk-can-change-accels"
               "gtk-color-palette" "gtk-color-scheme" "gtk-cursor-aspect-ratio"
               "gtk-cursor-blink" "gtk-cursor-blink-time"
               "gtk-cursor-blink-timeout" "gtk-cursor-theme-name"
               "gtk-cursor-theme-size" "gtk-decoration-layout"
               "gtk-dialogs-use-header" "gtk-dnd-drag-threshold"
               "gtk-double-click-distance" "gtk-double-click-time"
               "gtk-enable-accels" "gtk-enable-animations"
               "gtk-enable-event-sounds" "gtk-enable-input-feedback-sounds"
               "gtk-enable-mnemonics" "gtk-enable-primary-paste"
               "gtk-enable-tooltips" "gtk-entry-password-hint-timeout"
               "gtk-entry-select-on-focus" "gtk-error-bell"
               "gtk-fallback-icon-theme" "gtk-file-chooser-backend"
               "gtk-font-name" "gtk-fontconfig-timestamp" "gtk-icon-sizes"
               "gtk-icon-theme-name" "gtk-im-module" "gtk-im-preedit-style"
               "gtk-im-status-style" "gtk-key-theme-name"
               "gtk-keynav-cursor-only" "gtk-keynav-use-caret"
               "gtk-keynav-wrap-around" "gtk-label-select-on-focus"
               "gtk-long-press-time" "gtk-menu-bar-accel"
               "gtk-menu-bar-popup-delay" "gtk-menu-images"
               "gtk-menu-popdown-delay" "gtk-menu-popup-delay" "gtk-modules"
               "gtk-overlay-scrolling" "gtk-primary-button-warps-slider"
               "gtk-print-backends" "gtk-print-preview-command"
               "gtk-recent-files-enabled" "gtk-recent-files-limit"
               "gtk-recent-files-max-age" "gtk-scrolled-window-placement"
               "gtk-shell-shows-app-menu" "gtk-shell-shows-desktop"
               "gtk-shell-shows-menubar" "gtk-show-input-method-menu"
               "gtk-show-unicode-menu" "gtk-sound-theme-name" "gtk-split-cursor"
               "gtk-theme-name" "gtk-timeout-expand" "gtk-timeout-initial"
               "gtk-timeout-repeat" "gtk-titlebar-double-click"
               "gtk-titlebar-middle-click" "gtk-titlebar-right-click"
               "gtk-toolbar-icon-size" "gtk-toolbar-style"
               "gtk-tooltip-browse-mode-timeout" "gtk-tooltip-browse-timeout"
               "gtk-tooltip-timeout" "gtk-touchscreen-mode" "gtk-visible-focus"
               "gtk-xft-antialias" "gtk-xft-dpi" "gtk-xft-hinting"
               "gtk-xft-hintstyle" "gtk-xft-rgba")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkSettings"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSettings" GTK-SETTINGS
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkStyleProvider" "GtkStyleProviderPrivate")
                        :TYPE-INITIALIZER "gtk_settings_get_type")
                       ((COLOR-HASH GTK-SETTINGS-COLOR-HASH "color-hash"
                         "GHashTable" T NIL)
                        (GTK-ALTERNATIVE-BUTTON-ORDER
                         GTK-SETTINGS-GTK-ALTERNATIVE-BUTTON-ORDER
                         "gtk-alternative-button-order" "gboolean" T T)
                        (GTK-ALTERNATIVE-SORT-ARROWS
                         GTK-SETTINGS-GTK-ALTERNATIVE-SORT-ARROWS
                         "gtk-alternative-sort-arrows" "gboolean" T T)
                        (GTK-APPLICATION-PREFER-DARK-THEME
                         GTK-SETTINGS-GTK-APPLICATION-PREFER-DARK-THEME
                         "gtk-application-prefer-dark-theme" "gboolean" T T)
                        (GTK-AUTO-MNEMONICS GTK-SETTINGS-GTK-AUTO-MNEMONICS
                         "gtk-auto-mnemonics" "gboolean" T T)
                        (GTK-BUTTON-IMAGES GTK-SETTINGS-GTK-BUTTON-IMAGES
                         "gtk-button-images" "gboolean" T T)
                        (GTK-CAN-CHANGE-ACCELS
                         GTK-SETTINGS-GTK-CAN-CHANGE-ACCELS
                         "gtk-can-change-accels" "gboolean" T T)
                        (GTK-COLOR-PALETTE GTK-SETTINGS-GTK-COLOR-PALETTE
                         "gtk-color-palette" "gchararray" T T)
                        (GTK-COLOR-SCHEME GTK-SETTINGS-GTK-COLOR-SCHEME
                         "gtk-color-scheme" "gchararray" T T)
                        (GTK-CURSOR-ASPECT-RATIO
                         GTK-SETTINGS-GTK-CURSOR-ASPECT-RATIO
                         "gtk-cursor-aspect-ratio" "gfloat" T T)
                        (GTK-CURSOR-BLINK GTK-SETTINGS-GTK-CURSOR-BLINK
                         "gtk-cursor-blink" "gboolean" T T)
                        (GTK-CURSOR-BLINK-TIME
                         GTK-SETTINGS-GTK-CURSOR-BLINK-TIME
                         "gtk-cursor-blink-time" "gint" T T)
                        (GTK-CURSOR-BLINK-TIMEOUT
                         GTK-SETTINGS-GTK-CURSOR-BLINK-TIMEOUT
                         "gtk-cursor-blink-timeout" "gint" T T)
                        (GTK-CURSOR-THEME-NAME
                         GTK-SETTINGS-GTK-CURSOR-THEME-NAME
                         "gtk-cursor-theme-name" "gchararray" T T)
                        (GTK-CURSOR-THEME-SIZE
                         GTK-SETTINGS-GTK-CURSOR-THEME-SIZE
                         "gtk-cursor-theme-size" "gint" T T)
                        (GTK-DECORATION-LAYOUT
                         GTK-SETTINGS-GTK-DECORATION-LAYOUT
                         "gtk-decoration-layout" "gchararray" T T)
                        (GTK-DIALOGS-USE-HEADER
                         GTK-SETTINGS-GTK-DIALOGS-USE-HEADER
                         "gtk-dialogs-use-header" "gboolean" T T)
                        (GTK-DND-DRAG-THRESHOLD
                         GTK-SETTINGS-GTK-DND-DRAG-THRESHOLD
                         "gtk-dnd-drag-threshold" "gint" T T)
                        (GTK-DOUBLE-CLICK-DISTANCE
                         GTK-SETTINGS-GTK-DOUBLE-CLICK-DISTANCE
                         "gtk-double-click-distance" "gint" T T)
                        (GTK-DOUBLE-CLICK-TIME
                         GTK-SETTINGS-GTK-DOUBLE-CLICK-TIME
                         "gtk-double-click-time" "gint" T T)
                        (GTK-ENABLE-ACCELS GTK-SETTINGS-GTK-ENABLE-ACCELS
                         "gtk-enable-accels" "gboolean" T T)
                        (GTK-ENABLE-ANIMATIONS
                         GTK-SETTINGS-GTK-ENABLE-ANIMATIONS
                         "gtk-enable-animations" "gboolean" T T)
                        (GTK-ENABLE-EVENT-SOUNDS
                         GTK-SETTINGS-GTK-ENABLE-EVENT-SOUNDS
                         "gtk-enable-event-sounds" "gboolean" T T)
                        (GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                         GTK-SETTINGS-GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                         "gtk-enable-input-feedback-sounds" "gboolean" T T)
                        (GTK-ENABLE-MNEMONICS GTK-SETTINGS-GTK-ENABLE-MNEMONICS
                         "gtk-enable-mnemonics" "gboolean" T T)
                        (GTK-ENABLE-PRIMARY-PASTE
                         GTK-SETTINGS-GTK-ENABLE-PRIMARY-PASTE
                         "gtk-enable-primary-paste" "gboolean" T T)
                        (GTK-ENABLE-TOOLTIPS GTK-SETTINGS-GTK-ENABLE-TOOLTIPS
                         "gtk-enable-tooltips" "gboolean" T T)
                        (GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                         GTK-SETTINGS-GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                         "gtk-entry-password-hint-timeout" "guint" T T)
                        (GTK-ENTRY-SELECT-ON-FOCUS
                         GTK-SETTINGS-GTK-ENTRY-SELECT-ON-FOCUS
                         "gtk-entry-select-on-focus" "gboolean" T T)
                        (GTK-ERROR-BELL GTK-SETTINGS-GTK-ERROR-BELL
                         "gtk-error-bell" "gboolean" T T)
                        (GTK-FALLBACK-ICON-THEME
                         GTK-SETTINGS-GTK-FALLBACK-ICON-THEME
                         "gtk-fallback-icon-theme" "gchararray" T T)
                        (GTK-FILE-CHOOSER-BACKEND
                         GTK-SETTINGS-GTK-FILE-CHOOSER-BACKEND
                         "gtk-file-chooser-backend" "gchararray" T T)
                        (GTK-FONT-NAME GTK-SETTINGS-GTK-FONT-NAME
                         "gtk-font-name" "gchararray" T T)
                        (GTK-FONTCONFIG-TIMESTAMP
                         GTK-SETTINGS-GTK-FONTCONFIG-TIMESTAMP
                         "gtk-fontconfig-timestamp" "guint" T T)
                        (GTK-ICON-SIZES GTK-SETTINGS-GTK-ICON-SIZES
                         "gtk-icon-sizes" "gchararray" T T)
                        (GTK-ICON-THEME-NAME GTK-SETTINGS-GTK-ICON-THEME-NAME
                         "gtk-icon-theme-name" "gchararray" T T)
                        (GTK-IM-MODULE GTK-SETTINGS-GTK-IM-MODULE
                         "gtk-im-module" "gchararray" T T)
                        (GTK-IM-PREEDIT-STYLE GTK-SETTINGS-GTK-IM-PREEDIT-STYLE
                         "gtk-im-preedit-style" "GtkIMPreeditStyle" T T)
                        (GTK-IM-STATUS-STYLE GTK-SETTINGS-GTK-IM-STATUS-STYLE
                         "gtk-im-status-style" "GtkIMStatusStyle" T T)
                        (GTK-KEY-THEME-NAME GTK-SETTINGS-GTK-KEY-THEME-NAME
                         "gtk-key-theme-name" "gchararray" T T)
                        (GTK-KEYNAV-CURSOR-ONLY
                         GTK-SETTINGS-GTK-KEYNAV-CURSOR-ONLY
                         "gtk-keynav-cursor-only" "gboolean" T T)
                        (GTK-KEYNAV-USE-CARET GTK-SETTINGS-GTK-KEYNAV-USE-CARET
                         "gtk-keynav-use-caret" "gboolean" T T)
                        (GTK-KEYNAV-WRAP-AROUND
                         GTK-SETTINGS-GTK-KEYNAV-WRAP-AROUND
                         "gtk-keynav-wrap-around" "gboolean" T T)
                        (GTK-LABEL-SELECT-ON-FOCUS
                         GTK-SETTINGS-GTK-LABEL-SELECT-ON-FOCUS
                         "gtk-label-select-on-focus" "gboolean" T T)
                        (GTK-LONG-PRESS-TIME GTK-SETTINGS-GTK-LONG-PRESS-TIME
                         "gtk-long-press-time" "guint" T T)
                        (GTK-MENU-BAR-ACCEL GTK-SETTINGS-GTK-MENU-BAR-ACCEL
                         "gtk-menu-bar-accel" "gchararray" T T)
                        (GTK-MENU-BAR-POPUP-DELAY
                         GTK-SETTINGS-GTK-MENU-BAR-POPUP-DELAY
                         "gtk-menu-bar-popup-delay" "gint" T T)
                        (GTK-MENU-IMAGES GTK-SETTINGS-GTK-MENU-IMAGES
                         "gtk-menu-images" "gboolean" T T)
                        (GTK-MENU-POPDOWN-DELAY
                         GTK-SETTINGS-GTK-MENU-POPDOWN-DELAY
                         "gtk-menu-popdown-delay" "gint" T T)
                        (GTK-MENU-POPUP-DELAY GTK-SETTINGS-GTK-MENU-POPUP-DELAY
                         "gtk-menu-popup-delay" "gint" T T)
                        (GTK-MODULES GTK-SETTINGS-GTK-MODULES "gtk-modules"
                         "gchararray" T T)
                        (GTK-OVERLAY-SCROLLING
                         GTK-SETTINGS-GTK-OVERLAY-SCROLLING
                         "gtk-overlay-scrolling" "gboolean" T T)
                        (GTK-PRIMARY-BUTTON-WARPS-SLIDER
                         GTK-SETTINGS-GTK-PRIMARY-BUTTON-WARPS-SLIDER
                         "gtk-primary-button-warps-slider" "gboolean" T T)
                        (GTK-PRINT-BACKENDS GTK-SETTINGS-GTK-PRINT-BACKENDS
                         "gtk-print-backends" "gchararray" T T)
                        (GTK-PRINT-PREVIEW-COMMAND
                         GTK-SETTINGS-GTK-PRINT-PREVIEW-COMMAND
                         "gtk-print-preview-command" "gchararray" T T)
                        (GTK-RECENT-FILES-ENABLED
                         GTK-SETTINGS-GTK-RECENT-FILES-ENABLED
                         "gtk-recent-files-enabled" "gboolean" T T)
                        (GTK-RECENT-FILES-LIMIT
                         GTK-SETTINGS-GTK-RECENT-FILES-LIMIT
                         "gtk-recent-files-limit" "gint" T T)
                        (GTK-RECENT-FILES-MAX-AGE
                         GTK-SETTINGS-GTK-RECENT-FILES-MAX-AGE
                         "gtk-recent-files-max-age" "gint" T T)
                        (GTK-SCROLLED-WINDOW-PLACEMENT
                         GTK-SETTINGS-GTK-SCROLLED-WINDOW-PLACEMENT
                         "gtk-scrolled-window-placement" "GtkCornerType" T T)
                        (GTK-SHELL-SHOWS-APP-MENU
                         GTK-SETTINGS-GTK-SHELL-SHOWS-APP-MENU
                         "gtk-shell-shows-app-menu" "gboolean" T T)
                        (GTK-SHELL-SHOWS-DESKTOP
                         GTK-SETTINGS-GTK-SHELL-SHOWS-DESKTOP
                         "gtk-shell-shows-desktop" "gboolean" T T)
                        (GTK-SHELL-SHOWS-MENUBAR
                         GTK-SETTINGS-GTK-SHELL-SHOWS-MENUBAR
                         "gtk-shell-shows-menubar" "gboolean" T T)
                        (GTK-SHOW-INPUT-METHOD-MENU
                         GTK-SETTINGS-GTK-SHOW-INPUT-METHOD-MENU
                         "gtk-show-input-method-menu" "gboolean" T T)
                        (GTK-SHOW-UNICODE-MENU
                         GTK-SETTINGS-GTK-SHOW-UNICODE-MENU
                         "gtk-show-unicode-menu" "gboolean" T T)
                        (GTK-SOUND-THEME-NAME GTK-SETTINGS-GTK-SOUND-THEME-NAME
                         "gtk-sound-theme-name" "gchararray" T T)
                        (GTK-SPLIT-CURSOR GTK-SETTINGS-GTK-SPLIT-CURSOR
                         "gtk-split-cursor" "gboolean" T T)
                        (GTK-THEME-NAME GTK-SETTINGS-GTK-THEME-NAME
                         "gtk-theme-name" "gchararray" T T)
                        (GTK-TIMEOUT-EXPAND GTK-SETTINGS-GTK-TIMEOUT-EXPAND
                         "gtk-timeout-expand" "gint" T T)
                        (GTK-TIMEOUT-INITIAL GTK-SETTINGS-GTK-TIMEOUT-INITIAL
                         "gtk-timeout-initial" "gint" T T)
                        (GTK-TIMEOUT-REPEAT GTK-SETTINGS-GTK-TIMEOUT-REPEAT
                         "gtk-timeout-repeat" "gint" T T)
                        (GTK-TITLEBAR-DOUBLE-CLICK
                         GTK-SETTINGS-GTK-TITLEBAR-DOUBLE-CLICK
                         "gtk-titlebar-double-click" "gchararray" T T)
                        (GTK-TITLEBAR-MIDDLE-CLICK
                         GTK-SETTINGS-GTK-TITLEBAR-MIDDLE-CLICK
                         "gtk-titlebar-middle-click" "gchararray" T T)
                        (GTK-TITLEBAR-RIGHT-CLICK
                         GTK-SETTINGS-GTK-TITLEBAR-RIGHT-CLICK
                         "gtk-titlebar-right-click" "gchararray" T T)
                        (GTK-TOOLBAR-ICON-SIZE
                         GTK-SETTINGS-GTK-TOOLBAR-ICON-SIZE
                         "gtk-toolbar-icon-size" "GtkIconSize" T T)
                        (GTK-TOOLBAR-STYLE GTK-SETTINGS-GTK-TOOLBAR-STYLE
                         "gtk-toolbar-style" "GtkToolbarStyle" T T)
                        (GTK-TOOLTIP-BROWSE-MODE-TIMEOUT
                         GTK-SETTINGS-GTK-TOOLTIP-BROWSE-MODE-TIMEOUT
                         "gtk-tooltip-browse-mode-timeout" "gint" T T)
                        (GTK-TOOLTIP-BROWSE-TIMEOUT
                         GTK-SETTINGS-GTK-TOOLTIP-BROWSE-TIMEOUT
                         "gtk-tooltip-browse-timeout" "gint" T T)
                        (GTK-TOOLTIP-TIMEOUT GTK-SETTINGS-GTK-TOOLTIP-TIMEOUT
                         "gtk-tooltip-timeout" "gint" T T)
                        (GTK-TOUCHSCREEN-MODE GTK-SETTINGS-GTK-TOUCHSCREEN-MODE
                         "gtk-touchscreen-mode" "gboolean" T T)
                        (GTK-VISIBLE-FOCUS GTK-SETTINGS-GTK-VISIBLE-FOCUS
                         "gtk-visible-focus" "GtkPolicyType" T T)
                        (GTK-XFT-ANTIALIAS GTK-SETTINGS-GTK-XFT-ANTIALIAS
                         "gtk-xft-antialias" "gint" T T)
                        (GTK-XFT-DPI GTK-SETTINGS-GTK-XFT-DPI "gtk-xft-dpi"
                         "gint" T T)
                        (GTK-XFT-HINTING GTK-SETTINGS-GTK-XFT-HINTING
                         "gtk-xft-hinting" "gint" T T)
                        (GTK-XFT-HINTSTYLE GTK-SETTINGS-GTK-XFT-HINTSTYLE
                         "gtk-xft-hintstyle" "gchararray" T T)
                        (GTK-XFT-RGBA GTK-SETTINGS-GTK-XFT-RGBA "gtk-xft-rgba"
                         "gchararray" T T)))
             (get-g-type-definition "GtkSettings"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-settings-properties
  (let ((settings (gtk-settings-default)))
    ;; FIXME: GHashTable is not implemented
    (signals (error) (gtk-settings-color-hash settings))
    #-windows
    (is-false (gtk-settings-gtk-alternative-button-order settings))
    #-windows
    (is-false (gtk-settings-gtk-alternative-sort-arrows settings))
    (is-false (gtk-settings-gtk-application-prefer-dark-theme settings))
    (is-true  (gtk-settings-gtk-auto-mnemonics settings))
    (is-false (gtk-settings-gtk-button-images settings))
    (is-false (gtk-settings-gtk-can-change-accels settings))
    (is (stringp (gtk-settings-gtk-color-palette settings)))
    (is (stringp (gtk-settings-gtk-color-scheme settings)))
    (is-true  (gtk-settings-gtk-cursor-blink settings))
    (is (integerp (gtk-settings-gtk-cursor-blink-time settings)))
    (is (integerp (gtk-settings-gtk-cursor-blink-timeout settings)))
    #-windows
    (is (stringp (gtk-settings-gtk-cursor-theme-name settings)))
    (is (integerp (gtk-settings-gtk-cursor-theme-size settings)))
    (is-true  (gtk-settings-gtk-decoration-layout settings))
    #-windows
    (is-true  (gtk-settings-gtk-dialogs-use-header settings))
    (is (integerp (gtk-settings-gtk-dnd-drag-threshold settings)))
    (is (integerp (gtk-settings-gtk-double-click-distance settings)))
    (is (integerp (gtk-settings-gtk-double-click-time settings)))
    (is-true  (gtk-settings-gtk-enable-accels settings))
    (is-true  (gtk-settings-gtk-enable-animations settings))
    (is-true  (gtk-settings-gtk-enable-event-sounds settings))
    (is-true  (gtk-settings-gtk-enable-input-feedback-sounds settings))
    (is-true  (gtk-settings-gtk-enable-mnemonics settings))
    (is-true  (gtk-settings-gtk-enable-primary-paste settings))
    (is-true  (gtk-settings-gtk-enable-tooltips settings))
    (is-true  (gtk-settings-gtk-entry-password-hint-timeout settings))
    (is-true  (gtk-settings-gtk-entry-select-on-focus settings))
    (is-true  (gtk-settings-gtk-error-bell settings))
    (is-false (gtk-settings-gtk-fallback-icon-theme settings))
    (is-false (gtk-settings-gtk-file-chooser-backend settings))
    (is (stringp (gtk-settings-gtk-font-name settings)))
    (is (integerp (gtk-settings-gtk-fontconfig-timestamp settings)))
    #-windows
    (is (stringp (gtk-settings-gtk-icon-sizes settings)))
    (is (stringp (gtk-settings-gtk-icon-theme-name settings)))
    (is (stringp (gtk-settings-gtk-im-module settings)))
    (is (eq :callback (gtk-settings-gtk-im-preedit-style settings)))
    (is (eq :callback (gtk-settings-gtk-im-status-style settings)))
    #-windows
    (is (stringp (gtk-settings-gtk-key-theme-name settings)))
    (is-false (gtk-settings-gtk-keynav-cursor-only settings))
    (is-false (gtk-settings-gtk-keynav-use-caret settings))
    (is-true  (gtk-settings-gtk-keynav-wrap-around settings))
    (is-true  (gtk-settings-gtk-label-select-on-focus settings))
    (is (integerp (gtk-settings-gtk-long-press-time settings)))
    (is (stringp (gtk-settings-gtk-menu-bar-accel settings)))
    (is (integerp (gtk-settings-gtk-menu-bar-popup-delay settings)))
    (is-false (gtk-settings-gtk-menu-images settings))
    (is (integerp (gtk-settings-gtk-menu-popdown-delay settings)))
    (is (integerp (gtk-settings-gtk-menu-popup-delay settings)))
    #-windows
    (is (stringp (gtk-settings-gtk-modules settings)))
    (is-true  (gtk-settings-gtk-primary-button-warps-slider settings))
    (is (stringp (gtk-settings-gtk-print-backends settings)))
    (is (stringp (gtk-settings-gtk-print-preview-command settings)))
    ;; TODO: Is this a bug?
    (signals (error) (gtk-settings-gtk-recent-files-enabled settings))
    (is (integerp (gtk-settings-gtk-recent-files-limit settings)))
    (is (integerp (gtk-settings-gtk-recent-files-max-age settings)))
    (is-true  (gtk-settings-gtk-scrolled-window-placement settings))
    (is-false (gtk-settings-gtk-shell-shows-app-menu settings))
    (is-true  (gtk-settings-gtk-shell-shows-desktop settings))
    (is-false (gtk-settings-gtk-shell-shows-menubar settings))
    (is-false (gtk-settings-gtk-show-input-method-menu settings))
    (is-false (gtk-settings-gtk-show-unicode-menu settings))
    (is (stringp (gtk-settings-gtk-sound-theme-name settings)))
    #-windows
    (is-true  (gtk-settings-gtk-split-cursor settings))
    (is (stringp (gtk-settings-gtk-theme-name settings)))
    (is (integerp (gtk-settings-gtk-timeout-expand settings)))
    (is (integerp (gtk-settings-gtk-timeout-initial settings)))
    (is (integerp (gtk-settings-gtk-timeout-repeat settings)))
    (is (stringp (gtk-settings-gtk-titlebar-double-click settings)))
    (is (stringp (gtk-settings-gtk-titlebar-middle-click settings)))
    (is (stringp (gtk-settings-gtk-titlebar-right-click settings)))
    (is (eq :large-toolbar (gtk-settings-gtk-toolbar-icon-size settings)))
    (is (eq :both-horiz (gtk-settings-gtk-toolbar-style settings)))
    (is (integerp (gtk-settings-gtk-tooltip-browse-mode-timeout settings)))
    (is (integerp (gtk-settings-gtk-tooltip-browse-timeout settings)))
    (is (integerp (gtk-settings-gtk-tooltip-timeout settings)))
    (is-false (gtk-settings-gtk-touchscreen-mode settings))
    (is (eq :automatic (gtk-settings-gtk-visible-focus settings)))
    (is (integerp (gtk-settings-gtk-xft-antialias settings)))
    (is (integerp (gtk-settings-gtk-xft-dpi settings)))
    (is (integerp (gtk-settings-gtk-xft-hinting settings)))
    (is (stringp (gtk-settings-gtk-xft-hintstyle settings)))
    (is (stringp (gtk-settings-gtk-xft-rgba settings)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_settings_get_default
;;;     gtk_settings_get_for_screen
;;;     gtk_settings_install_property                      deprecated
;;;     gtk_settings_install_property_parser               deprecated
;;;     gtk_rc_property_parse_color
;;;     gtk_rc_property_parse_enum
;;;     gtk_rc_property_parse_flags
;;;     gtk_rc_property_parse_requisition
;;;     gtk_rc_property_parse_border
;;;     gtk_settings_set_property_value                    deprecated
;;;     gtk_settings_set_string_property                   deprecated
;;;     gtk_settings_set_long_property                     deprecated
;;;     gtk_settings_set_double_property                   deprecated
;;;     gtk_settings_reset_property ()


;;; 2021-10-14
