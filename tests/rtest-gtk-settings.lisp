;;; ----------------------------------------------------------------------------
;;; rtest-gtk-settings.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gtk-tests)

;;; ----------------------------------------------------------------------------

(define-test gtk-settings
  ;; Type checks
  (assert-true  (g-type-is-object "GtkSettings"))
  (assert-false (g-type-is-abstract "GtkSettings"))
  (assert-true  (g-type-is-derived "GtkSettings"))
  (assert-false (g-type-is-fundamental "GtkSettings"))
  (assert-true  (g-type-is-value-type "GtkSettings"))
  (assert-true  (g-type-has-value-table "GtkSettings"))
  (assert-true  (g-type-is-classed "GtkSettings"))
  (assert-true  (g-type-is-instantiatable "GtkSettings"))
  (assert-true  (g-type-is-derivable "GtkSettings"))
  (assert-true  (g-type-is-deep-derivable "GtkSettings"))
  (assert-false (g-type-is-interface "GtkSettings"))

  ;; Check the registered name
  (assert-eq 'gtk-settings
             (registered-object-type-by-name "GtkSettings"))
  
  ;; Check infos about the class
  (let ((class (g-type-class-ref (gtype "GtkSettings"))))
    (assert-equal (gtype "GtkSettings")  (g-type-from-class class))
    (assert-equal (gtype "GtkSettings") (g-object-class-type class))
    (assert-equal "GtkSettings" (g-object-class-name class))
    (assert-equal (gtype "GtkSettings")
                  (g-type-from-class (g-type-class-peek "GtkSettings")))
    (assert-equal (gtype "GtkSettings")
                  (g-type-from-class (g-type-class-peek-static "GtkSettings")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-settings)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-settings (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkSettings" (gobject-class-g-type-name class))
    (assert-equal "GtkSettings" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_settings_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GtkSettings"))
  (assert-eql 2 (g-type-depth "GtkSettings"))
  (assert-eql   (gtype "GtkSettings")
                (g-type-next-base "GtkSettings" "GObject"))
  (assert-true  (g-type-is-a "GtkSettings" "GObject"))
  (assert-false (g-type-is-a "GtkSettings" "gboolean"))
  (assert-false (g-type-is-a "GtkSettings" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkSettings")))
  (assert-equal '("GtkStyleProvider" "GtkStyleProviderPrivate")
                (mapcar #'gtype-name (g-type-interfaces "GtkSettings")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkSettings" query)
    (assert-equal (gtype "GtkSettings")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkSettings"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql  84 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  16 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties
  (assert-equal
       '("gtk-double-click-time" "gtk-double-click-distance" "gtk-cursor-blink"
         "gtk-cursor-blink-time" "gtk-cursor-blink-timeout" "gtk-split-cursor"
         "gtk-theme-name" "gtk-icon-theme-name" "gtk-fallback-icon-theme"
         "gtk-key-theme-name" "gtk-menu-bar-accel" "gtk-dnd-drag-threshold"
         "gtk-font-name" "gtk-icon-sizes" "gtk-modules" "gtk-xft-antialias"
         "gtk-xft-hinting" "gtk-xft-hintstyle" "gtk-xft-rgba" "gtk-xft-dpi"
         "gtk-cursor-theme-name" "gtk-cursor-theme-size"
         "gtk-alternative-button-order" "gtk-alternative-sort-arrows"
         "gtk-show-input-method-menu" "gtk-show-unicode-menu"
         "gtk-timeout-initial" "gtk-timeout-repeat" "gtk-timeout-expand"
         "gtk-color-scheme" "gtk-enable-animations" "gtk-touchscreen-mode"
         "gtk-tooltip-timeout" "gtk-tooltip-browse-timeout"
         "gtk-tooltip-browse-mode-timeout" "gtk-keynav-cursor-only"
         "gtk-keynav-wrap-around" "gtk-error-bell" "color-hash"
         "gtk-file-chooser-backend" "gtk-print-backends"
         "gtk-print-preview-command" "gtk-enable-mnemonics" "gtk-enable-accels"
         "gtk-recent-files-limit" "gtk-im-module" "gtk-recent-files-max-age"
         "gtk-fontconfig-timestamp" "gtk-sound-theme-name"
         "gtk-enable-input-feedback-sounds" "gtk-enable-event-sounds"
         "gtk-enable-tooltips" "gtk-toolbar-style" "gtk-toolbar-icon-size"
         "gtk-auto-mnemonics" "gtk-primary-button-warps-slider"
         "gtk-visible-focus" "gtk-application-prefer-dark-theme"
         "gtk-button-images" "gtk-entry-select-on-focus"
         "gtk-entry-password-hint-timeout" "gtk-menu-images"
         "gtk-menu-bar-popup-delay" "gtk-scrolled-window-placement"
         "gtk-can-change-accels" "gtk-menu-popup-delay"
         "gtk-menu-popdown-delay" "gtk-label-select-on-focus"
         "gtk-color-palette" "gtk-im-preedit-style" "gtk-im-status-style"
         "gtk-shell-shows-app-menu" "gtk-shell-shows-menubar"
         "gtk-enable-primary-paste")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkSettings"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GtkSettings" GTK-SETTINGS
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GtkStyleProvider" "GtkStyleProviderPrivate")
                                :TYPE-INITIALIZER "gtk_settings_get_type")
                               ((COLOR-HASH GTK-SETTINGS-COLOR-HASH
                                 "color-hash" "GHashTable" T NIL)
                                (GTK-ALTERNATIVE-BUTTON-ORDER
                                 GTK-SETTINGS-GTK-ALTERNATIVE-BUTTON-ORDER
                                 "gtk-alternative-button-order" "gboolean" T T)
                                (GTK-ALTERNATIVE-SORT-ARROWS
                                 GTK-SETTINGS-GTK-ALTERNATIVE-SORT-ARROWS
                                 "gtk-alternative-sort-arrows" "gboolean" T T)
                                (GTK-APPLICATION-PREFER-DARK-THEME
                                 GTK-SETTINGS-GTK-APPLICATION-PREFER-DARK-THEME
                                 "gtk-application-prefer-dark-theme" "gboolean"
                                 T T)
                                (GTK-AUTO-MNEMONICS
                                 GTK-SETTINGS-GTK-AUTO-MNEMONICS
                                 "gtk-auto-mnemonics" "gboolean" T T)
                                (GTK-BUTTON-IMAGES
                                 GTK-SETTINGS-GTK-BUTTON-IMAGES
                                 "gtk-button-images" "gboolean" T T)
                                (GTK-CAN-CHANGE-ACCELS
                                 GTK-SETTINGS-GTK-CAN-CHANGE-ACCELS
                                 "gtk-can-change-accels" "gboolean" T T)
                                (GTK-COLOR-PALETTE
                                 GTK-SETTINGS-GTK-COLOR-PALETTE
                                 "gtk-color-palette" "gchararray" T T)
                                (GTK-COLOR-SCHEME GTK-SETTINGS-GTK-COLOR-SCHEME
                                 "gtk-color-scheme" "gchararray" T T)
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
                                (GTK-DND-DRAG-THRESHOLD
                                 GTK-SETTINGS-GTK-DND-DRAG-THRESHOLD
                                 "gtk-dnd-drag-threshold" "gint" T T)
                                (GTK-DOUBLE-CLICK-DISTANCE
                                 GTK-SETTINGS-GTK-DOUBLE-CLICK-DISTANCE
                                 "gtk-double-click-distance" "gint" T T)
                                (GTK-DOUBLE-CLICK-TIME
                                 GTK-SETTINGS-GTK-DOUBLE-CLICK-TIME
                                 "gtk-double-click-time" "gint" T T)
                                (GTK-ENABLE-ACCELS
                                 GTK-SETTINGS-GTK-ENABLE-ACCELS
                                 "gtk-enable-accels" "gboolean" T T)
                                (GTK-ENABLE-ANIMATIONS
                                 GTK-SETTINGS-GTK-ENABLE-ANIMATIONS
                                 "gtk-enable-animations" "gboolean" T T)
                                (GTK-ENABLE-EVENT-SOUNDS
                                 GTK-SETTINGS-GTK-ENABLE-EVENT-SOUNDS
                                 "gtk-enable-event-sounds" "gboolean" T T)
                                (GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                                 GTK-SETTINGS-GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                                 "gtk-enable-input-feedback-sounds" "gboolean"
                                 T T)
                                (GTK-ENABLE-MNEMONICS
                                 GTK-SETTINGS-GTK-ENABLE-MNEMONICS
                                 "gtk-enable-mnemonics" "gboolean" T T)
                                (GTK-ENABLE-PRIMARY-PASTE
                                 GTK-SETTINGS-GTK-ENABLE-PRIMARY-PASTE
                                 "gtk-enable-primary-paste" "gboolean" T T)
                                (GTK-ENABLE-TOOLTIPS
                                 GTK-SETTINGS-GTK-ENABLE-TOOLTIPS
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
                                (GTK-ICON-THEME-NAME
                                 GTK-SETTINGS-GTK-ICON-THEME-NAME
                                 "gtk-icon-theme-name" "gchararray" T T)
                                (GTK-IM-MODULE GTK-SETTINGS-GTK-IM-MODULE
                                 "gtk-im-module" "gchararray" T T)
                                (GTK-IM-PREEDIT-STYLE
                                 GTK-SETTINGS-GTK-IM-PREEDIT-STYLE
                                 "gtk-im-preedit-style" "GtkIMPreeditStyle" T
                                 T)
                                (GTK-IM-STATUS-STYLE
                                 GTK-SETTINGS-GTK-IM-STATUS-STYLE
                                 "gtk-im-status-style" "GtkIMStatusStyle" T T)
                                (GTK-KEY-THEME-NAME
                                 GTK-SETTINGS-GTK-KEY-THEME-NAME
                                 "gtk-key-theme-name" "gchararray" T T)
                                (GTK-KEYNAV-CURSOR-ONLY
                                 GTK-SETTINGS-GTK-KEYNAV-CURSOR-ONLY
                                 "gtk-keynav-cursor-only" "gboolean" T T)
                                (GTK-KEYNAV-WRAP-AROUND
                                 GTK-SETTINGS-GTK-KEYNAV-WRAP-AROUND
                                 "gtk-keynav-wrap-around" "gboolean" T T)
                                (GTK-LABEL-SELECT-ON-FOCUS
                                 GTK-SETTINGS-GTK-LABEL-SELECT-ON-FOCUS
                                 "gtk-label-select-on-focus" "gboolean" T T)
                                (GTK-MENU-BAR-ACCEL
                                 GTK-SETTINGS-GTK-MENU-BAR-ACCEL
                                 "gtk-menu-bar-accel" "gchararray" T T)
                                (GTK-MENU-BAR-POPUP-DELAY
                                 GTK-SETTINGS-GTK-MENU-BAR-POPUP-DELAY
                                 "gtk-menu-bar-popup-delay" "gint" T T)
                                (GTK-MENU-IMAGES GTK-SETTINGS-GTK-MENU-IMAGES
                                 "gtk-menu-images" "gboolean" T T)
                                (GTK-MENU-POPDOWN-DELAY
                                 GTK-SETTINGS-GTK-MENU-POPDOWN-DELAY
                                 "gtk-menu-popdown-delay" "gint" T T)
                                (GTK-MENU-POPUP-DELAY
                                 GTK-SETTINGS-GTK-MENU-POPUP-DELAY
                                 "gtk-menu-popup-delay" "gint" T T)
                                (GTK-MODULES GTK-SETTINGS-GTK-MODULES
                                 "gtk-modules" "gchararray" T T)
                                (GTK-PRIMARY-BUTTON-WARPS-SLIDER
                                 GTK-SETTINGS-GTK-PRIMARY-BUTTON-WARPS-SLIDER
                                 "gtk-primary-button-warps-slider" "gboolean" T
                                 T)
                                (GTK-PRINT-BACKENDS
                                 GTK-SETTINGS-GTK-PRINT-BACKENDS
                                 "gtk-print-backends" "gchararray" T T)
                                (GTK-PRINT-PREVIEW-COMMAND
                                 GTK-SETTINGS-GTK-PRINT-PREVIEW-COMMAND
                                 "gtk-print-preview-command" "gchararray" T T)
                                (GTK-RECENT-FILES-LIMIT
                                 GTK-SETTINGS-GTK-RECENT-FILES-LIMIT
                                 "gtk-recent-files-limit" "gint" T T)
                                (GTK-RECENT-FILES-MAX-AGE
                                 GTK-SETTINGS-GTK-RECENT-FILES-MAX-AGE
                                 "gtk-recent-files-max-age" "gint" T T)
                                (GTK-SCROLLED-WINDOW-PLACEMENT
                                 GTK-SETTINGS-GTK-SCROLLED-WINDOW-PLACEMENT
                                 "gtk-scrolled-window-placement"
                                 "GtkCornerType" T T)
                                (GTK-SHELL-SHOWS-APP-MENU
                                 GTK-SETTINGS-GTK-SHELL-SHOWS-APP-MENU
                                 "gtk-shell-shows-app-menu" "gboolean" T T)
                                (GTK-SHELL-SHOWS-MENUBAR
                                 GTK-SETTINGS-GTK-SHELL-SHOWS-MENUBAR
                                 "gtk-shell-shows-menubar" "gboolean" T T)
                                (GTK-SHOW-INPUT-METHOD-MENU
                                 GTK-SETTINGS-GTK-SHOW-INPUT-METHOD-MENU
                                 "gtk-show-input-method-menu" "gboolean" T T)
                                (GTK-SHOW-UNICODE-MENU
                                 GTK-SETTINGS-GTK-SHOW-UNICODE-MENU
                                 "gtk-show-unicode-menu" "gboolean" T T)
                                (GTK-SOUND-THEME-NAME
                                 GTK-SETTINGS-GTK-SOUND-THEME-NAME
                                 "gtk-sound-theme-name" "gchararray" T T)
                                (GTK-SPLIT-CURSOR GTK-SETTINGS-GTK-SPLIT-CURSOR
                                 "gtk-split-cursor" "gboolean" T T)
                                (GTK-THEME-NAME GTK-SETTINGS-GTK-THEME-NAME
                                 "gtk-theme-name" "gchararray" T T)
                                (GTK-TIMEOUT-EXPAND
                                 GTK-SETTINGS-GTK-TIMEOUT-EXPAND
                                 "gtk-timeout-expand" "gint" T T)
                                (GTK-TIMEOUT-INITIAL
                                 GTK-SETTINGS-GTK-TIMEOUT-INITIAL
                                 "gtk-timeout-initial" "gint" T T)
                                (GTK-TIMEOUT-REPEAT
                                 GTK-SETTINGS-GTK-TIMEOUT-REPEAT
                                 "gtk-timeout-repeat" "gint" T T)
                                (GTK-TOOLBAR-ICON-SIZE
                                 GTK-SETTINGS-GTK-TOOLBAR-ICON-SIZE
                                 "gtk-toolbar-icon-size" "GtkIconSize" T T)
                                (GTK-TOOLBAR-STYLE
                                 GTK-SETTINGS-GTK-TOOLBAR-STYLE
                                 "gtk-toolbar-style" "GtkToolbarStyle" T T)
                                (GTK-TOOLTIP-BROWSE-MODE-TIMEOUT
                                 GTK-SETTINGS-GTK-TOOLTIP-BROWSE-MODE-TIMEOUT
                                 "gtk-tooltip-browse-mode-timeout" "gint" T T)
                                (GTK-TOOLTIP-BROWSE-TIMEOUT
                                 GTK-SETTINGS-GTK-TOOLTIP-BROWSE-TIMEOUT
                                 "gtk-tooltip-browse-timeout" "gint" T T)
                                (GTK-TOOLTIP-TIMEOUT
                                 GTK-SETTINGS-GTK-TOOLTIP-TIMEOUT
                                 "gtk-tooltip-timeout" "gint" T T)
                                (GTK-TOUCHSCREEN-MODE
                                 GTK-SETTINGS-GTK-TOUCHSCREEN-MODE
                                 "gtk-touchscreen-mode" "gboolean" T T)
                                (GTK-VISIBLE-FOCUS
                                 GTK-SETTINGS-GTK-VISIBLE-FOCUS
                                 "gtk-visible-focus" "GtkPolicyType" T T)
                                (GTK-XFT-ANTIALIAS
                                 GTK-SETTINGS-GTK-XFT-ANTIALIAS
                                 "gtk-xft-antialias" "gint" T T)
                                (GTK-XFT-DPI GTK-SETTINGS-GTK-XFT-DPI
                                 "gtk-xft-dpi" "gint" T T)
                                (GTK-XFT-HINTING GTK-SETTINGS-GTK-XFT-HINTING
                                 "gtk-xft-hinting" "gint" T T)
                                (GTK-XFT-HINTSTYLE
                                 GTK-SETTINGS-GTK-XFT-HINTSTYLE
                                 "gtk-xft-hintstyle" "gchararray" T T)
                                (GTK-XFT-RGBA GTK-SETTINGS-GTK-XFT-RGBA
                                 "gtk-xft-rgba" "gchararray" T T)))
     (get-g-type-definition (gtype "GtkSettings")))

  ;; Create an instance
  (let ((settings (gtk-settings-get-default)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GtkSettings") (g-object-type settings))
    (assert-equal "GtkSettings" (g-object-type-name settings))
    (assert-true (g-type-is-a "GtkSettings" (g-type-from-instance (pointer settings))))
    ;; Access the properties

;;;   "color-hash" TODO: Type GHashTable is not implemented

    (assert-false (gtk-settings-gtk-alternative-button-order settings))
    (assert-false (gtk-settings-gtk-alternative-sort-arrows settings))
    (assert-false (gtk-settings-gtk-application-prefer-dark-theme settings))
    ;; Documented default value is false
    (assert-true (gtk-settings-gtk-auto-mnemonics settings))
    ;; Documented default value is true
    (assert-false (gtk-settings-gtk-button-images settings))
    (assert-false (gtk-settings-gtk-can-change-accels settings))
    (assert-equal
        "black:white:gray50:red:purple:blue:light blue:green:yellow:orange:lavender:brown:goldenrod4:dodger blue:pink:light green:gray10:gray30:gray75:gray90"
        (gtk-settings-gtk-color-palette settings))
    (assert-equal
        "tooltip_fg_color: #ffffffffffff
link_color: #dddd48481414
text_color: #3c3c3c3c3c3c
selected_fg_color: #ffffffffffff
bg_color_dark: #3c3c3b3b3737
bg_color: #f6f6f4f4f2f2
\"base_color: #ffffffffffff
tooltip_bg_color: #000000000000
selected_bg_color: #f0f077774646
fg_color: #4c4c4c4c4c4c
"
        (gtk-settings-gtk-color-scheme settings))
    (assert-true (gtk-settings-gtk-cursor-blink settings))
    (assert-eql 1200 (gtk-settings-gtk-cursor-blink-time settings))
    ;; Documented default value is 2147483647
    (assert-eql 10 (gtk-settings-gtk-cursor-blink-timeout settings))
    ;; Documented default value is Null
    (assert-equal "DMZ-White" (gtk-settings-gtk-cursor-theme-name settings))
    ;; Documented defalut value is 0
    (assert-eql 24 (gtk-settings-gtk-cursor-theme-size settings))
    (assert-eql 8 (gtk-settings-gtk-dnd-drag-threshold settings))
    (assert-eql 5 (gtk-settings-gtk-double-click-distance settings))
    ;; Documented default value is 250
    (assert-eql 400 (gtk-settings-gtk-double-click-time settings))
    (assert-true (gtk-settings-gtk-enable-accels settings))
    (assert-true (gtk-settings-gtk-enable-animations settings))
    (assert-true (gtk-settings-gtk-enable-event-sounds settings))
    ;; Documented default value is true
    (assert-false (gtk-settings-gtk-enable-input-feedback-sounds settings))
    (assert-true (gtk-settings-gtk-enable-mnemonics settings))
    (assert-true (gtk-settings-gtk-enable-tooltips settings))
    (assert-eql 0 (gtk-settings-gtk-entry-password-hint-timeout settings))
    (assert-true (gtk-settings-gtk-entry-select-on-focus settings))
    (assert-true (gtk-settings-gtk-error-bell settings))
    ;; Documented default value is Null
    (assert-equal "gnome" (gtk-settings-gtk-fallback-icon-theme settings))
    (assert-false (gtk-settings-gtk-file-chooser-backend settings))
    ;; Documented default value is "Sans 10"
    (assert-equal "Ubuntu 11" (gtk-settings-gtk-font-name settings))
    (assert-eql 0 (gtk-settings-gtk-fontconfig-timestamp settings))
    ;; Documented default value is Null
    (assert-equal  "panel-menu-bar=24,24"
                   (gtk-settings-gtk-icon-sizes settings))
    ;; Documented default value is "hicolor"
    (assert-equal "ubuntu-mono-light"
                  (gtk-settings-gtk-icon-theme-name settings))
    (assert-equal "" (gtk-settings-gtk-im-module settings))
    (assert-eq :callback (gtk-settings-gtk-im-preedit-style settings))
    (assert-eq :callback (gtk-settings-gtk-im-status-style settings))
    ;; Documented default value is Null
    (assert-equal "Default" (gtk-settings-gtk-key-theme-name settings))
    (assert-false (gtk-settings-gtk-keynav-cursor-only settings))
    (assert-true (gtk-settings-gtk-keynav-wrap-around settings))
    (assert-true (gtk-settings-gtk-label-select-on-focus settings))
    (assert-equal "F10" (gtk-settings-gtk-menu-bar-accel settings))
    (assert-eql 0 (gtk-settings-gtk-menu-bar-popup-delay settings))
    ;; Documented default value is true
    (assert-false (gtk-settings-gtk-menu-images settings))
    (assert-eql 1000 (gtk-settings-gtk-menu-popdown-delay settings))
    (assert-eql 225 (gtk-settings-gtk-menu-popup-delay settings))
    ;; Documented default value is Null
    (assert-equal "canberra-gtk-module" (gtk-settings-gtk-modules settings))
    (assert-equal "file,cups" (gtk-settings-gtk-print-backends settings))
    (assert-equal "evince --unlink-tempfile --preview --print-settings %s %f"
                  (gtk-settings-gtk-print-preview-command settings))
    (assert-eql 50 (gtk-settings-gtk-recent-files-limit settings))
    (assert-eql 30 (gtk-settings-gtk-recent-files-max-age settings))
    (assert-eq :top-left (gtk-settings-gtk-scrolled-window-placement settings))
    ;; Documented default value is false
    (assert-true (gtk-settings-gtk-shell-shows-app-menu settings))
    ;; Documented default value is false
    (assert-true (gtk-settings-gtk-shell-shows-menubar settings))
    (assert-true (gtk-settings-gtk-show-input-method-menu settings))
    (assert-true (gtk-settings-gtk-show-unicode-menu settings))
    ;; Documented default value "freedesktop"
    (assert-equal "ubuntu" (gtk-settings-gtk-sound-theme-name settings))
    (assert-true (gtk-settings-gtk-split-cursor settings))
    ;; Documented default value "Raleigh"
    (assert-equal "Radiance" (gtk-settings-gtk-theme-name settings))
    (assert-eql 500 (gtk-settings-gtk-timeout-expand settings))
    (assert-eql 200 (gtk-settings-gtk-timeout-initial settings))
    (assert-eql 20 (gtk-settings-gtk-timeout-repeat settings))
    (assert-eq :large-toolbar (gtk-settings-gtk-toolbar-icon-size settings))
    ;; Documented default value GTK_TOOLBAR_BOTH
    (assert-eq :both-horiz (gtk-settings-gtk-toolbar-style settings))
    (assert-eql 500 (gtk-settings-gtk-tooltip-browse-mode-timeout settings))
    (assert-eql 60 (gtk-settings-gtk-tooltip-browse-timeout settings))
    (assert-eql 500 (gtk-settings-gtk-tooltip-timeout settings))
    (assert-false (gtk-settings-gtk-touchscreen-mode settings))
    (assert-eq :always (gtk-settings-gtk-visible-focus settings))
    ;; Documented default value is -1
    (assert-eql 1 (gtk-settings-gtk-xft-antialias settings))
    ;; Documented default value is -1
    (assert-eql 98304 (gtk-settings-gtk-xft-dpi settings))
    ;; Documented default value is -1
    (assert-eql 1 (gtk-settings-gtk-xft-hinting settings))
    ;; Documented default value is Null
    (assert-equal "hintslight" (gtk-settings-gtk-xft-hintstyle settings))
    ;; Documented default value is Null
    (assert-equal "rgb" (gtk-settings-gtk-xft-rgba settings))
   )
)

