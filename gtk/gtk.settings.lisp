;;; ----------------------------------------------------------------------------
;;; gtk.settings.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;
;;; Settings
;;;
;;; Sharing settings between applications
;;;
;;; Synopsis
;;;
;;;     GtkSettings
;;;     GtkSettingsValue
;;;
;;;     gtk_settings_get_default
;;;     gtk_settings_get_for_screen
;;;     gtk_settings_install_property
;;;     gtk_settings_install_property_parser
;;;     gtk_rc_property_parse_color
;;;     gtk_rc_property_parse_enum
;;;     gtk_rc_property_parse_flags
;;;     gtk_rc_property_parse_requisition
;;;     gtk_rc_property_parse_border
;;;     gtk_settings_set_property_value
;;;     gtk_settings_set_string_property
;;;     gtk_settings_set_long_property
;;;     gtk_settings_set_double_property
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkSettings
;;;
;;; Implemented Interfaces
;;;
;;; GtkSettings implements GtkStyleProvider and GtkStyleProviderPrivate.
;;;
;;; Properties
;;;
;;;   "color-hash"                        GHashTable*           : Read
;;;   "gtk-alternative-button-order"      gboolean              : Read / Write
;;;   "gtk-alternative-sort-arrows"       gboolean              : Read / Write
;;;   "gtk-application-prefer-dark-theme" gboolean              : Read / Write
;;;   "gtk-auto-mnemonics"                gboolean              : Read / Write
;;;   "gtk-button-images"                 gboolean              : Read / Write
;;;   "gtk-can-change-accels"             gboolean              : Read / Write
;;;   "gtk-color-palette"                 gchar*                : Read / Write
;;;   "gtk-color-scheme"                  gchar*                : Read / Write
;;;   "gtk-cursor-blink"                  gboolean              : Read / Write
;;;   "gtk-cursor-blink-time"             gint                  : Read / Write
;;;   "gtk-cursor-blink-timeout"          gint                  : Read / Write
;;;   "gtk-cursor-theme-name"             gchar*                : Read / Write
;;;   "gtk-cursor-theme-size"             gint                  : Read / Write
;;;   "gtk-dnd-drag-threshold"            gint                  : Read / Write
;;;   "gtk-double-click-distance"         gint                  : Read / Write
;;;   "gtk-double-click-time"             gint                  : Read / Write
;;;   "gtk-enable-accels"                 gboolean              : Read / Write
;;;   "gtk-enable-animations"             gboolean              : Read / Write
;;;   "gtk-enable-event-sounds"           gboolean              : Read / Write
;;;   "gtk-enable-input-feedback-sounds"  gboolean              : Read / Write
;;;   "gtk-enable-mnemonics"              gboolean              : Read / Write
;;;   "gtk-enable-tooltips"               gboolean              : Read / Write
;;;   "gtk-entry-password-hint-timeout"   guint                 : Read / Write
;;;   "gtk-entry-select-on-focus"         gboolean              : Read / Write
;;;   "gtk-error-bell"                    gboolean              : Read / Write
;;;   "gtk-fallback-icon-theme"           gchar*                : Read / Write
;;;   "gtk-file-chooser-backend"          gchar*                : Read / Write
;;;   "gtk-font-name"                     gchar*                : Read / Write
;;;   "gtk-fontconfig-timestamp"          guint                 : Read / Write
;;;   "gtk-icon-sizes"                    gchar*                : Read / Write
;;;   "gtk-icon-theme-name"               gchar*                : Read / Write
;;;   "gtk-im-module"                     gchar*                : Read / Write
;;;   "gtk-im-preedit-style"              GtkIMPreeditStyle     : Read / Write
;;;   "gtk-im-status-style"               GtkIMStatusStyle      : Read / Write
;;;   "gtk-key-theme-name"                gchar*                : Read / Write
;;;   "gtk-keynav-cursor-only"            gboolean              : Read / Write
;;;   "gtk-keynav-wrap-around"            gboolean              : Read / Write
;;;   "gtk-label-select-on-focus"         gboolean              : Read / Write
;;;   "gtk-menu-bar-accel"                gchar*                : Read / Write
;;;   "gtk-menu-bar-popup-delay"          gint                  : Read / Write
;;;   "gtk-menu-images"                   gboolean              : Read / Write
;;;   "gtk-menu-popdown-delay"            gint                  : Read / Write
;;;   "gtk-menu-popup-delay"              gint                  : Read / Write
;;;   "gtk-modules"                       gchar*                : Read / Write
;;;   "gtk-print-backends"                gchar*                : Read / Write
;;;   "gtk-print-preview-command"         gchar*                : Read / Write
;;;   "gtk-recent-files-limit"            gint                  : Read / Write
;;;   "gtk-recent-files-max-age"          gint                  : Read / Write
;;;   "gtk-scrolled-window-placement"     GtkCornerType         : Read / Write
;;;   "gtk-shell-shows-app-menu"          gboolean              : Read / Write
;;;   "gtk-shell-shows-menubar"           gboolean              : Read / Write
;;;   "gtk-show-input-method-menu"        gboolean              : Read / Write
;;;   "gtk-show-unicode-menu"             gboolean              : Read / Write
;;;   "gtk-sound-theme-name"              gchar*                : Read / Write
;;;   "gtk-split-cursor"                  gboolean              : Read / Write
;;;   "gtk-theme-name"                    gchar*                : Read / Write
;;;   "gtk-timeout-expand"                gint                  : Read / Write
;;;   "gtk-timeout-initial"               gint                  : Read / Write
;;;   "gtk-timeout-repeat"                gint                  : Read / Write
;;;   "gtk-toolbar-icon-size"             GtkIconSize           : Read / Write
;;;   "gtk-toolbar-style"                 GtkToolbarStyle       : Read / Write
;;;   "gtk-tooltip-browse-mode-timeout"   gint                  : Read / Write
;;;   "gtk-tooltip-browse-timeout"        gint                  : Read / Write
;;;   "gtk-tooltip-timeout"               gint                  : Read / Write
;;;   "gtk-touchscreen-mode"              gboolean              : Read / Write
;;;   "gtk-visible-focus"                 GtkPolicyType         : Read / Write
;;;   "gtk-xft-antialias"                 gint                  : Read / Write
;;;   "gtk-xft-dpi"                       gint                  : Read / Write
;;;   "gtk-xft-hinting"                   gint                  : Read / Write
;;;   "gtk-xft-hintstyle"                 gchar*                : Read / Write
;;;   "gtk-xft-rgba"                      gchar*                : Read / Write
;;;
;;; Description
;;;
;;; GtkSettings provide a mechanism to share global settings between
;;; applications.
;;;
;;; On the X window system, this sharing is realized by an XSettings manager
;;; that is usually part of the desktop environment, along with utilities that
;;; let the user change these settings. In the absence of an Xsettings manager,
;;; GTK+ reads default values for settings from settings.ini files in
;;; /etc/gtk-3.0 and $XDG_CONFIG_HOME/gtk-3.0. These files must be valid key
;;; files (see GKeyFile), and have a section called Settings. Themes can also
;;; provide default values for settings by installing a settings.ini file next
;;; to their gtk.css file.
;;;
;;; Applications can override system-wide settings with
;;; gtk_settings_set_string_property(), gtk_settings_set_long_property(), etc.
;;; This should be restricted to special cases though; GtkSettings are not meant
;;; as an application configuration facility. When doing so, you need to be
;;; aware that settings that are specific to individual widgets may not be
;;; available before the widget type has been realized at least once. The
;;; following example demonstrates a way to do this:
;;;
;;; gtk_init (&argc, &argv);
;;;
;;; /* make sure the type is realized */
;;; g_type_class_unref (g_type_class_ref (GTK_TYPE_IMAGE_MENU_ITEM));
;;;
;;; g_object_set (gtk_settings_get_default (), "gtk-menu-images", FALSE, NULL);
;;;
;;; There is one GtkSettings instance per screen. It can be obtained with
;;; gtk_settings_get_for_screen(), but in many cases, it is more convenient to
;;; use gtk_widget_get_settings(). gtk_settings_get_default() returns the
;;; GtkSettings instance for the default screen.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "color-hash" property
;;;
;;;   "color-hash"               GHashTable*           : Read
;;;
;;; Holds a hash table representation of the "gtk-color-scheme" setting, mapping
;;; color names to GdkColors.
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-alternative-button-order" property
;;;
;;;   "gtk-alternative-button-order" gboolean              : Read / Write
;;;
;;; Whether buttons in dialogs should use the alternative button order.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-alternative-sort-arrows" property
;;;
;;;   "gtk-alternative-sort-arrows" gboolean              : Read / Write
;;;
;;; Controls the direction of the sort indicators in sorted list and tree views.
;;; By default an arrow pointing down means the column is sorted in ascending
;;; order. When set to TRUE, this order will be inverted.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-application-prefer-dark-theme" property
;;;
;;;   "gtk-application-prefer-dark-theme" gboolean              : Read / Write
;;;
;;; Whether the application prefers to use a dark theme. If a GTK+ theme
;;; includes a dark variant, it will be used instead of the configured theme.
;;;
;;; Some applications benefit from minimizing the amount of light pollution that
;;; interferes with the content. Good candidates for dark themes are photo and
;;; video editors that make the actual content get all the attention and
;;; minimize the distraction of the chrome.
;;;
;;; Dark themes should not be used for documents, where large spaces are
;;; white/light and the dark chrome creates too much contrast (web browser, text
;;; editor...).
;;;
;;; Default value: FALSE
;;;
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-auto-mnemonics" property
;;;
;;;   "gtk-auto-mnemonics"       gboolean              : Read / Write
;;;
;;; Whether mnemonics should be automatically shown and hidden when the user
;;; presses the mnemonic activator.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-button-images" property
;;;
;;;   "gtk-button-images"        gboolean              : Read / Write
;;;
;;; Whether images should be shown on buttons.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-can-change-accels" property
;;;
;;;   "gtk-can-change-accels"    gboolean              : Read / Write
;;;
;;; Whether menu accelerators can be changed by pressing a key over the menu
;;; item.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-color-palette" property
;;;
;;;   "gtk-color-palette"        gchar*                : Read / Write
;;;
;;; Palette to use in the color selector.
;;;
;;; Default value: "black:white:gray50:red:purple:blue:light
;;; blue:green:yellow:orange:lavender:brown:goldenrod4:dodger blue:pink:light
;;; green:gray10:gray30:gray75:gray90"
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-color-scheme" property
;;;
;;;   "gtk-color-scheme"         gchar*                : Read / Write
;;;
;;; A palette of named colors for use in themes. The format of the string is
;;;
;;; name1: color1
;;; name2: color2
;;; ...
;;;
;;; Color names must be acceptable as identifiers in the gtkrc syntax, and color
;;; specifications must be in the format accepted by gdk_color_parse().
;;;
;;; Note that due to the way the color tables from different sources are merged,
;;; color specifications will be converted to hexadecimal form when getting this
;;; property.
;;;
;;; Starting with GTK+ 2.12, the entries can alternatively be separated by ';'
;;; instead of newlines:
;;;
;;; name1: color1; name2: color2; ...
;;;
;;; Default value: ""
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-cursor-blink" property
;;;
;;;   "gtk-cursor-blink"         gboolean              : Read / Write
;;;
;;; Whether the cursor should blink.
;;;
;;; Also see the "gtk-cursor-blink-timeout" setting, which allows more flexible
;;; control over cursor blinking.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-cursor-blink-time" property
;;;
;;;   "gtk-cursor-blink-time"    gint                  : Read / Write
;;;
;;; Length of the cursor blink cycle, in milliseconds.
;;;
;;; Allowed values: >= 100
;;;
;;; Default value: 1200
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-cursor-blink-timeout" property
;;;
;;;   "gtk-cursor-blink-timeout" gint                  : Read / Write
;;;
;;; Time after which the cursor stops blinking, in seconds. The timer is reset
;;; after each user interaction.
;;;
;;; Setting this to zero has the same effect as setting "gtk-cursor-blink" to
;;; FALSE.
;;;
;;; Allowed values: >= 1
;;;
;;; Default value: 2147483647
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-cursor-theme-name" property
;;;
;;;   "gtk-cursor-theme-name"    gchar*                : Read / Write
;;;
;;; Name of the cursor theme to use, or NULL to use the default theme.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-cursor-theme-size" property
;;;
;;;   "gtk-cursor-theme-size"    gint                  : Read / Write
;;;
;;; Size to use for cursors, or 0 to use the default size.
;;;
;;; Allowed values: [0,128]
;;;
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-dnd-drag-threshold" property
;;;
;;;   "gtk-dnd-drag-threshold"   gint                  : Read / Write
;;;
;;; Number of pixels the cursor can move before dragging.
;;;
;;; Allowed values: >= 1
;;;
;;; Default value: 8
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-double-click-distance" property
;;;
;;;   "gtk-double-click-distance" gint                  : Read / Write
;;;
;;; Maximum distance allowed between two clicks for them to be considered a
;;; double click (in pixels).
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 5
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-double-click-time" property
;;;
;;;   "gtk-double-click-time"    gint                  : Read / Write
;;;
;;; Maximum time allowed between two clicks for them to be considered a double
;;; click (in milliseconds).
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 250
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-enable-accels" property
;;;
;;;   "gtk-enable-accels"        gboolean              : Read / Write
;;;
;;; Whether menu items should have visible accelerators which can be activated.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-enable-animations" property
;;;
;;;   "gtk-enable-animations"    gboolean              : Read / Write
;;;
;;; Whether to enable toolkit-wide animations.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-enable-event-sounds" property
;;;
;;;   "gtk-enable-event-sounds"  gboolean              : Read / Write
;;;
;;; Whether to play any event sounds at all.
;;;
;;; See the Sound Theme spec for more information on event sounds and sound
;;; themes.
;;;
;;; GTK+ itself does not support event sounds, you have to use a loadable module
;;; like the one that comes with libcanberra.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-enable-input-feedback-sounds" property
;;;
;;;   "gtk-enable-input-feedback-sounds" gboolean              : Read / Write
;;;
;;; Whether to play event sounds as feedback to user input.
;;;
;;; See the Sound Theme spec for more information on event sounds and sound
;;; themes.
;;;
;;; GTK+ itself does not support event sounds, you have to use a loadable module
;;; like the one that comes with libcanberra.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-enable-mnemonics" property
;;;
;;;   "gtk-enable-mnemonics"     gboolean              : Read / Write
;;;
;;; Whether labels and menu items should have visible mnemonics which can be
;;; activated.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-enable-tooltips" property
;;;
;;;   "gtk-enable-tooltips"      gboolean              : Read / Write
;;;
;;; Whether tooltips should be shown on widgets.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-entry-password-hint-timeout" property
;;;
;;;   "gtk-entry-password-hint-timeout" guint                 : Read / Write
;;;
;;; How long to show the last input character in hidden entries. This value is
;;; in milliseconds. 0 disables showing the last char. 600 is a good value for
;;; enabling it.
;;;
;;; Default value: 0
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-entry-select-on-focus" property
;;;
;;;   "gtk-entry-select-on-focus" gboolean              : Read / Write
;;;
;;; Whether to select the contents of an entry when it is focused.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-error-bell" property
;;;
;;;   "gtk-error-bell"           gboolean              : Read / Write
;;;
;;; When TRUE, keyboard navigation and other input-related errors will cause a
;;; beep. Since the error bell is implemented using gdk_window_beep(), the
;;; windowing system may offer ways to configure the error bell in many ways,
;;; such as flashing the window or similar visual effects.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-fallback-icon-theme" property
;;;
;;;   "gtk-fallback-icon-theme"  gchar*                : Read / Write
;;;
;;; Name of a icon theme to fall back to.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-file-chooser-backend" property
;;;
;;;   "gtk-file-chooser-backend" gchar*                : Read / Write
;;;
;;; Name of the GtkFileChooser backend to use by default.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-font-name" property
;;;
;;;   "gtk-font-name"            gchar*                : Read / Write
;;;
;;; Name of default font to use.
;;;
;;; Default value: "Sans 10"
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-fontconfig-timestamp" property
;;;
;;;   "gtk-fontconfig-timestamp" guint                 : Read / Write
;;;
;;; Timestamp of current fontconfig configuration.
;;;
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-icon-sizes" property
;;;
;;;   "gtk-icon-sizes"           gchar*                : Read / Write
;;;
;;; A list of icon sizes. The list is separated by colons, and item has the
;;; form:
;;;
;;; size-name = width , height
;;;
;;; E.g. "gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48". GTK+ itself use the
;;; following named icon sizes: gtk-menu, gtk-button, gtk-small-toolbar,
;;; gtk-large-toolbar, gtk-dnd, gtk-dialog. Applications can register their own
;;; named icon sizes with gtk_icon_size_register().
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-icon-theme-name" property
;;;
;;;   "gtk-icon-theme-name"      gchar*                : Read / Write
;;;
;;; Name of icon theme to use.
;;;
;;; Default value: "hicolor"
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-im-module" property
;;;
;;;   "gtk-im-module"            gchar*                : Read / Write
;;;
;;; Which IM (input method) module should be used by default. This is the input
;;; method that will be used if the user has not explicitly chosen another input
;;; method from the IM context menu. This also can be a colon-separated list of
;;; input methods, which GTK+ will try in turn until it finds one available on
;;; the system.
;;;
;;; See GtkIMContext and see the "gtk-show-input-method-menu" property.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-im-preedit-style" property
;;;
;;;   "gtk-im-preedit-style"     GtkIMPreeditStyle     : Read / Write
;;;
;;; How to draw the input method preedit string.
;;;
;;; Default value: GTK_IM_PREEDIT_CALLBACK
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-im-status-style" property
;;;
;;;   "gtk-im-status-style"      GtkIMStatusStyle      : Read / Write
;;;
;;; How to draw the input method statusbar.
;;;
;;; Default value: GTK_IM_STATUS_CALLBACK
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-key-theme-name" property
;;;
;;;   "gtk-key-theme-name"       gchar*                : Read / Write
;;;
;;; Name of key theme to load.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-keynav-cursor-only" property
;;;
;;;   "gtk-keynav-cursor-only"   gboolean              : Read / Write
;;;
;;; When TRUE, keyboard navigation should be able to reach all widgets by using
;;; the cursor keys only. Tab, Shift etc. keys can't be expected to be present
;;; on the used input device.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-keynav-wrap-around" property
;;;
;;;   "gtk-keynav-wrap-around"   gboolean              : Read / Write
;;;
;;; When TRUE, some widgets will wrap around when doing keyboard navigation,
;;; such as menus, menubars and notebooks.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-label-select-on-focus" property
;;;
;;;   "gtk-label-select-on-focus" gboolean              : Read / Write
;;;
;;; Whether to select the contents of a selectable label when it is focused.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-menu-bar-accel" property
;;;
;;;   "gtk-menu-bar-accel"       gchar*                : Read / Write
;;;
;;; Keybinding to activate the menu bar.
;;;
;;; Default value: "F10"
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-menu-bar-popup-delay" property
;;;
;;;   "gtk-menu-bar-popup-delay" gint                  : Read / Write
;;;
;;; Delay before the submenus of a menu bar appear.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-menu-images" property
;;;
;;;   "gtk-menu-images"          gboolean              : Read / Write
;;;
;;; Whether images should be shown in menus.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-menu-popdown-delay" property
;;;
;;;   "gtk-menu-popdown-delay"   gint                  : Read / Write
;;;
;;; The time before hiding a submenu when the pointer is moving towards the
;;; submenu.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 1000
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-menu-popup-delay" property
;;;
;;;   "gtk-menu-popup-delay"     gint                  : Read / Write
;;;
;;; Minimum time the pointer must stay over a menu item before the submenu
;;; appear.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 225
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-modules" property
;;;
;;;   "gtk-modules"              gchar*                : Read / Write
;;;
;;; List of currently active GTK modules.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-print-backends" property
;;;
;;;   "gtk-print-backends"       gchar*                : Read / Write
;;;
;;; A comma-separated list of print backends to use in the print dialog.
;;; Available print backends depend on the GTK+ installation, and may include
;;; "file", "cups", "lpr" or "papi".
;;;
;;; Default value: "file,cups"
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-print-preview-command" property
;;;
;;;   "gtk-print-preview-command" gchar*                : Read / Write
;;;
;;; A command to run for displaying the print preview. The command should
;;; contain a f placeholder, which will get replaced by the path to the pdf
;;; file. The command may also contain a s placeholder, which will get replaced
;;; by the path to a file containing the print settings in the format produced
;;; by gtk_print_settings_to_file().
;;;
;;; The preview application is responsible for removing the pdf file and the
;;; print settings file when it is done.
;;;
;;; Default value: "evince --unlink-tempfile --preview --print-settings %s %f"
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-recent-files-limit" property
;;;
;;;   "gtk-recent-files-limit"   gint                  : Read / Write
;;;
;;; The number of recently used files that should be displayed by default by
;;; GtkRecentChooser implementations and by the GtkFileChooser. A value of -1
;;; means every recently used file stored.
;;;
;;; Allowed values: >= G_MAXULONG
;;;
;;; Default value: 50
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-recent-files-max-age" property
;;;
;;;   "gtk-recent-files-max-age" gint                  : Read / Write
;;;
;;; The maximum age, in days, of the items inside the recently used resources
;;; list. Items older than this setting will be excised from the list. If set to
;;; 0, the list will always be empty; if set to -1, no item will be removed.
;;;
;;; Allowed values: >= G_MAXULONG
;;;
;;; Default value: 30
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-scrolled-window-placement" property
;;;
;;;   "gtk-scrolled-window-placement" GtkCornerType         : Read / Write
;;;
;;; Where the contents of scrolled windows are located with respect to the
;;; scrollbars, if not overridden by the scrolled window's own placement.
;;;
;;; Default value: GTK_CORNER_TOP_LEFT
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-shell-shows-app-menu" property
;;;
;;;   "gtk-shell-shows-app-menu" gboolean              : Read / Write
;;;
;;; Set to TRUE if the desktop environment is displaying the app menu, FALSE if
;;; the app should display it itself.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-shell-shows-menubar" property
;;;
;;;   "gtk-shell-shows-menubar"  gboolean              : Read / Write
;;;
;;; Set to TRUE if the desktop environment is displaying the menubar, FALSE if
;;; the app should display it itself.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-show-input-method-menu" property
;;;
;;;   "gtk-show-input-method-menu" gboolean              : Read / Write
;;;
;;; Whether the context menus of entries and text views should offer to change
;;; the input method.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-show-unicode-menu" property
;;;
;;;   "gtk-show-unicode-menu"    gboolean              : Read / Write
;;;
;;; Whether the context menus of entries and text views should offer to insert
;;; control characters.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-sound-theme-name" property
;;;
;;;   "gtk-sound-theme-name"     gchar*                : Read / Write
;;;
;;; The XDG sound theme to use for event sounds.
;;;
;;; See the Sound Theme spec for more information on event sounds and sound
;;; themes.
;;;
;;; GTK+ itself does not support event sounds, you have to use a loadable module
;;; like the one that comes with libcanberra.
;;;
;;; Default value: "freedesktop"
;;;
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-split-cursor" property
;;;
;;;   "gtk-split-cursor"         gboolean              : Read / Write
;;;
;;; Whether two cursors should be displayed for mixed left-to-right and
;;; right-to-left text.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-theme-name" property
;;;
;;;   "gtk-theme-name"           gchar*                : Read / Write
;;;
;;; Name of theme to load.
;;;
;;; Default value: "Raleigh"
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-timeout-expand" property
;;;
;;;   "gtk-timeout-expand"       gint                  : Read / Write
;;;
;;; Expand value for timeouts, when a widget is expanding a new region.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 500
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-timeout-initial" property
;;;
;;;   "gtk-timeout-initial"      gint                  : Read / Write
;;;
;;; Starting value for timeouts, when button is pressed.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 200
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-timeout-repeat" property
;;;
;;;   "gtk-timeout-repeat"       gint                  : Read / Write
;;;
;;; Repeat value for timeouts, when button is pressed.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 20
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-toolbar-icon-size" property
;;;
;;;   "gtk-toolbar-icon-size"    GtkIconSize           : Read / Write
;;;
;;; The size of icons in default toolbars.
;;;
;;; Default value: GTK_ICON_SIZE_LARGE_TOOLBAR
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-toolbar-style" property
;;;
;;;   "gtk-toolbar-style"        GtkToolbarStyle       : Read / Write
;;;
;;; The size of icons in default toolbars.
;;;
;;; Default value: GTK_TOOLBAR_BOTH
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-tooltip-browse-mode-timeout" property
;;;
;;;   "gtk-tooltip-browse-mode-timeout" gint                  : Read / Write
;;;
;;; Amount of time, in milliseconds, after which the browse mode will be
;;; disabled.
;;;
;;; See "gtk-tooltip-browse-timeout" for more information about browse mode.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 500
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-tooltip-browse-timeout" property
;;;
;;;   "gtk-tooltip-browse-timeout" gint                  : Read / Write
;;;
;;; Controls the time after which tooltips will appear when browse mode is
;;; enabled, in milliseconds.
;;;
;;; Browse mode is enabled when the mouse pointer moves off an object where a
;;; tooltip was currently being displayed. If the mouse pointer hits another
;;; object before the browse mode timeout expires (see
;;; "gtk-tooltip-browse-mode-timeout"), it will take the amount of milliseconds
;;; specified by this setting to popup the tooltip for the new object.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 60
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-tooltip-timeout" property
;;;
;;;   "gtk-tooltip-timeout"      gint                  : Read / Write
;;;
;;; Time, in milliseconds, after which a tooltip could appear if the cursor is
;;; hovering on top of a widget.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 500
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-touchscreen-mode" property
;;;
;;;   "gtk-touchscreen-mode"     gboolean              : Read / Write
;;;
;;; Warning
;;;
;;; GtkSettings:gtk-touchscreen-mode is deprecated and should not be used in
;;; newly-written code. 3.4. Generally the behavior touchscreen input should be
;;; performed dynamically based on gdk_event_get_source_device().
;;;
;;; When TRUE, there are no motion notify events delivered on this screen, and
;;; widgets can't use the pointer hovering them for any essential functionality.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-visible-focus" property
;;;
;;;   "gtk-visible-focus"        GtkPolicyType         : Read / Write
;;;
;;; Whether 'focus rectangles' should be always visible, never visible, or
;;; hidden until the user starts to use the keyboard.
;;;
;;; Default value: GTK_POLICY_ALWAYS
;;;
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-xft-antialias" property
;;;
;;;   "gtk-xft-antialias"        gint                  : Read / Write
;;;
;;; Whether to antialias Xft fonts; 0=no, 1=yes, -1=default.
;;;
;;; Allowed values: [G_MAXULONG,1]
;;;
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-xft-dpi" property
;;;
;;;   "gtk-xft-dpi"              gint                  : Read / Write
;;;
;;; Resolution for Xft, in 1024 * dots/inch. -1 to use default value.
;;;
;;; Allowed values: [G_MAXULONG,1048576]
;;;
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-xft-hinting" property
;;;
;;;   "gtk-xft-hinting"          gint                  : Read / Write
;;;
;;; Whether to hint Xft fonts; 0=no, 1=yes, -1=default.
;;;
;;; Allowed values: [G_MAXULONG,1]
;;;
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-xft-hintstyle" property
;;;
;;;   "gtk-xft-hintstyle"        gchar*                : Read / Write
;;;
;;; What degree of hinting to use; hintnone, hintslight, hintmedium, or
;;; hintfull.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "gtk-xft-rgba" property
;;;
;;;   "gtk-xft-rgba"             gchar*                : Read / Write
;;;
;;; Type of subpixel antialiasing; none, rgb, bgr, vrgb, vbgr.
;;;
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSettings
;;;
;;; typedef struct _GtkSettings GtkSettings;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSettings" gtk-settings
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_settings_get_type")
  ((color-hash
    gtk-settings-color-hash
    "color-hash" "GHashTable" t nil)
   (gtk-alternative-button-order
    gtk-settings-gtk-alternative-button-order
    "gtk-alternative-button-order" "gboolean" t t)
   (gtk-alternative-sort-arrows
    gtk-settings-gtk-alternative-sort-arrows
    "gtk-alternative-sort-arrows" "gboolean" t t)
   (gtk-application-prefer-dark-theme
    gtk-settings-gtk-application-prefer-dark-theme
    "gtk-application-prefer-dark-theme" "gboolean" t t)
   (gtk-auto-mnemonics
    gtk-settings-gtk-auto-menemonics
    "gtk-auto-mnemonics" "gboolean" t t)
   (gtk-button-images
    gtk-settings-gtk-button-images
    "gtk-button-images" "gboolean" t t)
   (gtk-can-change-accels
    gtk-settings-gtk-can-change-accels
    "gtk-can-change-accels" "gboolean" t t)
   (gtk-color-palette
    gtk-settings-gtk-color-palette
    "gtk-color-palette" "gchararray" t t)
   (gtk-color-scheme
    gtk-settings-gtk-color-scheme
    "gtk-color-scheme" "gchararray" t t)
   (gtk-cursor-blink
    gtk-settings-gtk-cursor-blink
    "gtk-cursor-blink" "gboolean" t t)
   (gtk-cursor-blink-time
    gtk-settings-gtk-cursor-blink-time
    "gtk-cursor-blink-time" "gint" t t)
   (gtk-cursor-blink-timeout
    gtk-settings-gtk-cursor-blink-timeout
    "gtk-cursor-blink-timeout" "gint" t t)
   (gtk-cursor-theme-name
    gtk-settings-gtk-cursor-theme-name
    "gtk-cursor-theme-name" "gchararray" t t)
   (gtk-cursor-theme-size
    gtk-settings-gtk-cursor-theme-size
    "gtk-cursor-theme-size" "gint" t t)
   (gtk-dnd-drag-threshold
    gtk-settings-gtk-dnd-drag-threshold
    "gtk-dnd-drag-threshold" "gint" t t)
   (gtk-double-click-distance
    gtk-settings-gtk-double-click-distance
    "gtk-double-click-distance" "gint" t t)
   (gtk-double-click-time
    gtk-settings-gtk-double-click-time
    "gtk-double-click-time" "gint" t t)
   (gtk-enable-accels
    gtk-settings-gtk-enable-accels
    "gtk-enable-accels" "gboolean" t t)
   (gtk-enable-animations
    gtk-settings-gtk-enable-animations
    "gtk-enable-animations" "gboolean" t t)
   (gtk-enable-event-sounds
    gtk-settings-gtk-enable-event-sounds
    "gtk-enable-event-sounds" "gboolean" t t)
   (gtk-enable-input-feedback-sounds
    gtk-settings-gtk-enable-input-feedback-sounds
    "gtk-enable-input-feedback-sounds" "gboolean" t t)
   (gtk-enable-mnemonics
    gtk-settings-gtk-enable-mnemonics
    "gtk-enable-mnemonics" "gboolean" t t)
   (gtk-enable-tooltips
    gtk-settings-gtk-enable-tooltips
    "gtk-enable-tooltips" "gboolean" t t)
   (gtk-entry-password-hint-timeout
    gtk-settings-gtk-entry-password-hint-timeout
    "gtk-entry-password-hint-timeout" "guint" t t)
   (gtk-entry-select-on-focus
    gtk-settings-gtk-entry-select-on-focus
    "gtk-entry-select-on-focus" "gboolean" t t)
   (gtk-error-bell
    gtk-settings-gtk-error-bell
    "gtk-error-bell" "gboolean" t t)
   (gtk-fallback-icon-theme
    gtk-settings-gtk-fallback-icon-theme
    "gtk-fallback-icon-theme" "gchararray" t t)
   (gtk-file-chooser-backend
    gtk-settings-gtk-file-chooser-backend
    "gtk-file-chooser-backend" "gchararray" t t)
   (gtk-font-name
    gtk-settings-gtk-font-name
    "gtk-font-name" "gchararray" t t)
   (gtk-fontconfig-timestamp
    gtk-settings-gtk-fontconfig-timestamp
    "gtk-fontconfig-timestamp" "guint" t t)
   (gtk-icon-sizes
    gtk-settings-gtk-icon-sizes
    "gtk-icon-sizes" "gchararray" t t)
   (gtk-icon-theme-name
    gtk-settings-gtk-icon-theme-name
    "gtk-icon-theme-name" "gchararray" t t)
   (gtk-im-module
    gtk-settings-gtk-im-module "gtk-im-module"
    "gchararray" t t)
   (gtk-im-preedit-style
    gtk-settings-gtk-im-preedit-style
    "gtk-im-preedit-style" "GtkIMPreeditStyle" t t)
   (gtk-im-status-style
    gtk-settings-gtk-im-status-style
    "gtk-im-status-style" "GtkIMStatusStyle" t t)
   (gtk-key-theme-name
    gtk-settings-gtk-key-theme-name
    "gtk-key-theme-name" "gchararray" t t)
   (gtk-keynav-cursor-only
    gtk-settings-gtk-keynav-cursor-only
    "gtk-keynav-cursor-only" "gboolean" t t)
   (gtk-keynav-wrap-around
    gtk-settings-gtk-keynav-wrap-around
    "gtk-keynav-wrap-around" "gboolean" t t)
   (gtk-label-select-on-focus
    gtk-settings-gtk-label-select-on-focus
    "gtk-label-select-on-focus" "gboolean" t t)
   (gtk-menu-bar-accel
    gtk-settings-gtk-menu-bar-accel
    "gtk-menu-bar-accel" "gchararray" t t)
   (gtk-menu-bar-popup-delay
    gtk-settings-gtk-menu-bar-popup-delay
    "gtk-menu-bar-popup-delay" "gint" t t)
   (gtk-menu-images
    gtk-settings-gtk-menu-images
    "gtk-menu-images" "gboolean" t t)
   (gtk-menu-popdown-delay
    gtk-settings-gtk-menu-popdown-delay
    "gtk-menu-popdown-delay" "gint" t t)
   (gtk-menu-popup-delay
    gtk-settings-gtk-menu-popup-delay
    "gtk-menu-popup-delay" "gint" t t)
   (gtk-modules
    gtk-settings-gtk-modules
    "gtk-modules" "gchararray" t t)
   (gtk-print-backends
    gtk-settings-gtk-print-backends
    "gtk-print-backends" "gchararray" t t)
   (gtk-print-preview-command
    gtk-settings-gtk-print-preview-command
    "gtk-print-preview-command" "gchararray" t t)
   (gtk-recent-files-limit
    gtk-settings-gtk-recent-files-limit
    "gtk-recent-files-limit" "gint" t t)
   (gtk-recent-files-max-age
    gtk-settings-gtk-recent-files-max-age
    "gtk-recent-files-max-age" "gint" t t)
   (gtk-scrolled-window-placement
    gtk-settings-gtk-scrolled-window-placement
    "gtk-scrolled-window-placement" "GtkCornerType" t t)
   (gtk-shell-shows-app-menu
    gtk-settings-gtk-shell-shows-app-menu
    "gtk-shell-shows-app-menu" "gboolean" t t)
   (gtk-shell-shows-menubar
    gtk-settings-gtk-shell-shows-menubar
    "gtk-shell-shows-menubar" "gboolean" t t)
   (gtk-show-input-method-menu
    gtk-settings-gtk-show-input-method-menu
    "gtk-show-input-method-menu" "gboolean" t t)
   (gtk-show-unicode-menu
    gtk-settings-gtk-show-unicode-menu
    "gtk-show-unicode-menu" "gboolean" t t)
   (gtk-sound-theme-name
    gtk-settings-gtk-sound-theme-name
    "gtk-sound-theme-name" "gchararray" t t)
   (gtk-split-cursor
    gtk-settings-gtk-split-cursor
    "gtk-split-cursor" "gboolean" t t)
   (gtk-theme-name
    gtk-settings-gtk-theme-name
    "gtk-theme-name" "gchararray" t t)
   (gtk-timeout-expand
    gtk-settings-gtk-timeout-expand
    "gtk-timeout-expand" "gint" t t)
   (gtk-timeout-initial
    gtk-settings-gtk-timeout-initial
    "gtk-timeout-initial" "gint" t t)
   (gtk-timeout-repeat
    gtk-settings-gtk-timeout-repeat
    "gtk-timeout-repeat" "gint" t t)
   (gtk-toolbar-icon-size
    gtk-settings-gtk-toolbar-icon-size
    "gtk-toolbar-icon-size" "GtkIconSize" t t)
   (gtk-toolbar-style
    gtk-settings-gtk-toolbar-style
    "gtk-toolbar-style" "GtkToolbarStyle" t t)
   (gtk-tooltip-browse-mode-timeout
    gtk-settings-gtk-tooltip-browse-mode-timeout
    "gtk-tooltip-browse-mode-timeout" "gint" t t)
   (gtk-tooltip-browse-timeout
    gtk-settings-gtk-tooltip-browse-timeout
    "gtk-tooltip-browse-timeout" "gint" t t)
   (gtk-tooltip-timeout
    gtk-settings-gtk-tooltip-timeout
    "gtk-tooltip-timeout" "gint" t t)
   (gtk-touchscreen-mode
    gtk-settings-gtk-touchscreen-mode
    "gtk-touchscreen-mode" "gboolean" t t)
   (gtk-visible-focus
    gtk-settings-gtk-visible-focus
    "gtk-visible-focus" "GtkPolicyType" t t)
   (gtk-xft-antialias
    gtk-settings-gtk-xft-antialias
    "gtk-xft-antialias" "gint" t t)
   (gtk-xft-dpi
    gtk-settings-gtk-xft-dpi
    "gtk-xft-dpi" "gint" t t)
   (gtk-xft-hinting
    gtk-settings-gtk-xft-hinting
    "gtk-xft-hinting" "gint" t t)
   (gtk-xft-hintstyle
    gtk-settings-gtk-xft-hintstyle
    "gtk-xft-hintstyle" "gchararray" t t)
   (gtk-xft-rgba
    gtk-settings-gtk-xft-rgba
    "gtk-xft-rgba" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; struct GtkSettingsValue
;;;
;;; struct GtkSettingsValue {
;;;   /* origin should be something like "filename:linenumber" for rc files,
;;;    * or e.g. "XProperty" for other sources
;;;    */
;;;   gchar *origin;
;;;
;;;   /* valid types are LONG, DOUBLE and STRING corresponding to the token
;;;    * parsed, or a GSTRING holding an unparsed statement
;;;    */
;;;   GValue value;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_default ()
;;;
;;; GtkSettings * gtk_settings_get_default (void);
;;;
;;; Gets the GtkSettings object for the default GDK screen, creating it if
;;; necessary. See gtk_settings_get_for_screen().
;;;
;;; Returns :
;;;     a GtkSettings object. If there is no default screen, then returns NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_settings_get_default" gtk-settings-get-default)
    (g-object gtk-settings))

(export 'gtk-settings-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_for_screen ()
;;;
;;; GtkSettings * gtk_settings_get_for_screen (GdkScreen *screen);
;;;
;;; Gets the GtkSettings object for screen, creating it if necessary.
;;;
;;; screen :
;;;     a GdkScreen.
;;;
;;; Returns :
;;;     a GtkSettings object
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_settings_get_for_screen" gtk-settings-get-for-screen)
    (g-object gtk-settings)
  (screen (g-object gdk-screen)))

(export 'gtk-settings-get-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_install_property ()
;;;
;;; void gtk_settings_install_property (GParamSpec *pspec);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_install_property_parser ()
;;;
;;; void gtk_settings_install_property_parser (GParamSpec *pspec,
;;;                                            GtkRcPropertyParser parser);
;;;
;;; parser :
;;;     . [scope call]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_color ()
;;;
;;; gboolean gtk_rc_property_parse_color (const GParamSpec *pspec,
;;;                                       const GString *gstring,
;;;                                       GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser() or
;;; gtk_widget_class_install_style_property_parser() which parses a color given
;;; either by its name or in the form { red, green, blue } where red, green and
;;; blue are integers between 0 and 65535 or floating-point numbers between 0
;;; and 1.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold GdkColor values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GdkColor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_enum ()
;;;
;;; gboolean gtk_rc_property_parse_enum (const GParamSpec *pspec,
;;;                                      const GString *gstring,
;;;                                      GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser() or
;;; gtk_widget_class_install_style_property_parser() which parses a single
;;; enumeration value.
;;;
;;; The enumeration value can be specified by its name, its nickname or its
;;; numeric value. For consistency with flags parsing, the value may be
;;; surrounded by parentheses.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold enum values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GEnumValue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_flags ()
;;;
;;; gboolean gtk_rc_property_parse_flags (const GParamSpec *pspec,
;;;                                       const GString *gstring,
;;;                                       GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser() or
;;; gtk_widget_class_install_style_property_parser() which parses flags.
;;;
;;; Flags can be specified by their name, their nickname or numerically.
;;; Multiple flags can be specified in the form "( flag1 | flag2 | ... )".
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold flags values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting flags value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_requisition ()
;;;
;;; gboolean gtk_rc_property_parse_requisition (const GParamSpec *pspec,
;;;                                             const GString *gstring,
;;;                                             GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser() or
;;; gtk_widget_class_install_style_property_parser() which parses a requisition
;;; in the form "{ width, height }" for integers width and height.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold boxed values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GtkRequisition.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_border ()
;;;
;;; gboolean gtk_rc_property_parse_border (const GParamSpec *pspec,
;;;                                        const GString *gstring,
;;;                                        GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser() or
;;; gtk_widget_class_install_style_property_parser() which parses borders in the
;;; form "{ left, right, top, bottom }" for integers left, right, top and
;;; bottom.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold boxed values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GtkBorder.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_property_value ()
;;;
;;; void gtk_settings_set_property_value (GtkSettings *settings,
;;;                                       const gchar *name,
;;;                                       const GtkSettingsValue *svalue);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_string_property ()
;;;
;;; void gtk_settings_set_string_property (GtkSettings *settings,
;;;                                        const gchar *name,
;;;                                        const gchar *v_string,
;;;                                        const gchar *origin);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_long_property ()
;;;
;;; void gtk_settings_set_long_property (GtkSettings *settings,
;;;                                      const gchar *name,
;;;                                      glong v_long,
;;;                                      const gchar *origin);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_double_property ()
;;;
;;; void gtk_settings_set_double_property (GtkSettings *settings,
;;;                                        const gchar *name,
;;;                                        gdouble v_double,
;;;                                        const gchar *origin);
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.settings.lisp ------------------------------------------
