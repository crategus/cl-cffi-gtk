;;; ----------------------------------------------------------------------------
;;; gtk.settings.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     Sharing settings between applications
;;;
;;; Types and Values
;;;
;;;     GtkSettings
;;;     GtkSettingsValue
;;;     GtkIMPreeditStyle
;;;     GtkIMStatusStyle
;;;
;;; Functions
;;;
;;;     gtk_settings_get_default
;;;     gtk_settings_get_for_screen
;;;     gtk_settings_install_property                      * deprecated
;;;     gtk_settings_install_property_parser               * deprecated
;;;     gtk_rc_property_parse_color
;;;     gtk_rc_property_parse_enum
;;;     gtk_rc_property_parse_flags
;;;     gtk_rc_property_parse_requisition
;;;     gtk_rc_property_parse_border
;;;     gtk_settings_set_property_value                    * deprecated
;;;     gtk_settings_set_string_property                   * deprecated
;;;     gtk_settings_set_long_property                     * deprecated
;;;     gtk_settings_set_double_property                   * deprecated
;;;     gtk_settings_reset_property ()
;;;
;;; Properties
;;;
;;;        GHashTable*  color-hash                           Read
;;;          gboolean   gtk-alternative-button-order         Read / Write
;;;          gboolean   gtk-alternative-sort-arrows          Read / Write
;;;          gboolean   gtk-application-prefer-dark-theme    Read / Write
;;;          gboolean   gtk-auto-mnemonics                   Read / Write
;;;          gboolean   gtk-button-images                    Read / Write
;;;          gboolean   gtk-can-change-accels                Read / Write
;;;             gchar*  gtk-color-palette                    Read / Write
;;;             gchar*  gtk-color-scheme                     Read / Write
;;;          gboolean   gtk-cursor-blink                     Read / Write
;;;              gint   gtk-cursor-blink-time                Read / Write
;;;              gint   gtk-cursor-blink-timeout             Read / Write
;;;             gchar*  gtk-cursor-theme-name                Read / Write
;;;              gint   gtk-cursor-theme-size                Read / Write
;;;             gchar*  gtk-decoration-layout                Read / Write
;;;          gboolean   gtk-dialogs-use-header               Read / Write
;;;              gint   gtk-dnd-drag-threshold               Read / Write
;;;              gint   gtk-double-click-distance            Read / Write
;;;              gint   gtk-double-click-time                Read / Write
;;;          gboolean   gtk-enable-accels                    Read / Write
;;;          gboolean   gtk-enable-animations                Read / Write
;;;          gboolean   gtk-enable-event-sounds              Read / Write
;;;          gboolean   gtk-enable-input-feedback-sounds     Read / Write
;;;          gboolean   gtk-enable-mnemonics                 Read / Write
;;;          gboolean   gtk-enable-primary-paste             Read / Write
;;;          gboolean   gtk-enable-tooltips                  Read / Write
;;;             guint   gtk-entry-password-hint-timeout      Read / Write
;;;          gboolean   gtk-entry-select-on-focus            Read / Write
;;;          gboolean   gtk-error-bell                       Read / Write
;;;             gchar*  gtk-fallback-icon-theme              Read / Write
;;;             gchar*  gtk-file-chooser-backend             Read / Write
;;;             gchar*  gtk-font-name                        Read / Write
;;;             guint   gtk-fontconfig-timestamp             Read / Write
;;;             gchar*  gtk-icon-sizes                       Read / Write
;;;             gchar*  gtk-icon-theme-name                  Read / Write
;;;             gchar*  gtk-im-module                        Read / Write
;;; GtkIMPreeditStyle   gtk-im-preedit-style                 Read / Write
;;;  GtkIMStatusStyle   gtk-im-status-style                  Read / Write
;;;             gchar*  gtk-key-theme-name                   Read / Write
;;;          gboolean   gtk-keynav-cursor-only               Read / Write
;;;          gboolean   gtk-keynav-use-caret                 Read / Write
;;;          gboolean   gtk-keynav-wrap-around               Read / Write
;;;          gboolean   gtk-label-select-on-focus            Read / Write
;;;             guint   gtk-long-press-time                  Read / Write
;;;             gchar*  gtk-menu-bar-accel                   Read / Write
;;;              gint   gtk-menu-bar-popup-delay             Read / Write
;;;          gboolean   gtk-menu-images                      Read / Write
;;;              gint   gtk-menu-popdown-delay               Read / Write
;;;              gint   gtk-menu-popup-delay                 Read / Write
;;;             gchar*  gtk-modules                          Read / Write
;;;          gboolean   gtk-primary-button-warps-slider      Read / Write
;;;             gchar*  gtk-print-backends                   Read / Write
;;;             gchar*  gtk-print-preview-command            Read / Write
;;;          gboolean   gtk-recent-files-enabled             Read / Write
;;;              gint   gtk-recent-files-limit               Read / Write
;;;              gint   gtk-recent-files-max-age             Read / Write
;;;     GtkCornerType   gtk-scrolled-window-placement        Read / Write
;;;          gboolean   gtk-shell-shows-app-menu             Read / Write
;;;          gboolean   gtk-shell-shows-desktop              Read / Write
;;;          gboolean   gtk-shell-shows-menubar              Read / Write
;;;          gboolean   gtk-show-input-method-menu           Read / Write
;;;          gboolean   gtk-show-unicode-menu                Read / Write
;;;             gchar*  gtk-sound-theme-name                 Read / Write
;;;          gboolean   gtk-split-cursor                     Read / Write
;;;             gchar*  gtk-theme-name                       Read / Write
;;;              gint   gtk-timeout-expand                   Read / Write
;;;              gint   gtk-timeout-initial                  Read / Write
;;;              gint   gtk-timeout-repeat                   Read / Write
;;;             gchar*  gtk-titlebar-double-click            Read / Write
;;;             gchar*  gtk-titlebar-middle-click            Read / Write
;;;             gchar*  gtk-titlebar-right-click             Read / Write
;;;       GtkIconSize   gtk-toolbar-icon-size                Read / Write
;;;   GtkToolbarStyle   gtk-toolbar-style                    Read / Write
;;;              gint   gtk-tooltip-browse-mode-timeout      Read / Write
;;;              gint   gtk-tooltip-browse-timeout           Read / Write
;;;              gint   gtk-tooltip-timeout                  Read / Write
;;;          gboolean   gtk-touchscreen-mode                 Read / Write
;;;     GtkPolicyType   gtk-visible-focus                    Read / Write
;;;              gint   gtk-xft-antialias                    Read / Write
;;;              gint   gtk-xft-dpi                          Read / Write
;;;              gint   gtk-xft-hinting                      Read / Write
;;;             gchar*  gtk-xft-hintstyle                    Read / Write
;;;             gchar*  gtk-xft-rgba                         Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSettings
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSettings implements GtkStyleProvider and GtkStyleProviderPrivate.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSettings
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
   #+gtk-3-12
   (gtk-decoration-layout
    gtk-settings-gtk-decoration-layout
    "gtk-decoration-layout" "gchar" t t)
   #+gtk-3-12
   (gtk-dialogs-use-header
    gtk-settings-gtk-dialogs-use-header
    "gtk-dialogs-use-header" "gboolean" t t)
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
   (gtk-enable-primary-paste
    gtk-settings-gtk-enable-primary-paste
    "gtk-enable-primary-paste" "gboolean" t t)
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
   #+gtk-3-20
   (gtk-keynav-use-caret
    gtk-settings-gtk-keynav-use-caret
    "gtk-keynav-use-caret" "gboolean" t t)
   (gtk-keynav-wrap-around
    gtk-settings-gtk-keynav-wrap-around
    "gtk-keynav-wrap-around" "gboolean" t t)
   (gtk-label-select-on-focus
    gtk-settings-gtk-label-select-on-focus
    "gtk-label-select-on-focus" "gboolean" t t)
   #+gtk-3-14
   (gtk-long-press-time
    gtk-settings-gtk-long-press-time
    "gtk-long-press-time" "guint" t t)
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
   #+gtk-3-6
   (gtk-primary-button-warps-slider
    gtk-settings-gtk-primary-button-warps-slider
    "gtk-primary-button-warps-slider" "gboolean" t t)
   (gtk-print-backends
    gtk-settings-gtk-print-backends
    "gtk-print-backends" "gchararray" t t)
   (gtk-print-preview-command
    gtk-settings-gtk-print-preview-command
    "gtk-print-preview-command" "gchararray" t t)
   #+gtk-3-8
   (gtk-recent-files-enabled
    gtk-settings-gtk-recent-files-enabled
    "gtk-settings-gtk-recent-files-enabled" "gboolean" t t)
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
   #+gtk-3-10
   (gtk-shell-shows-desktop
    gtk-settings-gtk-shell-shows-desktop
    "gtk-shell-shows-desktop" "gboolean" t t)
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
   #+gtk-3-14
   (gtk-titlebar-double-click
    gtk-settings-gtk-titlebar-double-click
    "gtk-titlebar-double-click" "gchar" t t)
   #+gtk-3-14
   (gtk-titlebar-middle-click
    gtk-settings-gtk-titlebar-middle-click
    "gtk-titlebar-middle-click" "gchar" t t)
   #+gtk-3-14
   (gtk-titlebar-right-click
    gtk-settings-gtk-titlebar-right-click
    "gtk-titlebar-right-click" "gchar" t t)
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-settings 'type)
 "@version{2014-1-29}
  @begin{short}
    @sym{gtk-settings} provide a mechanism to share global settings between
    applications.
  @end{short}

  On the X window system, this sharing is realized by an XSettings manager
  that is usually part of the desktop environment, along with utilities that
  let the user change these settings. In the absence of an XSettings manager,
  GTK+ reads default values for settings from @code{settings.ini} files in
  @code{/etc/gtk-3.0} and @code{$XDG_CONFIG_HOME/gtk-3.0}. These files must
  be valid key files, see @type{g-key-file}, and have a section called
  \"Settings\". Themes can also provide default values for settings by
  installing a @code{settings.ini} file next to their @code{gtk.css} file.

  Applications can override system-wide settings with the accessor functions
  of the slots. This should be restricted to special cases though;
  @sym{gtk-settings} are not meant as an application configuration facility.
  When doing so, you need to be aware that settings that are specific to
  individual widgets may not be available before the widget type has been
  realized at least once. The following example demonstrates a way to do this:
  @begin{pre}
  ;; make sure the type is realized
  (g-type-class-unref (g-type-class-ref \"GtkMenuItem\"))
  (setf (gtk-settings-gtk-menu-images (gtk-settings-get-default)) t)
  @end{pre}
  There is one @sym{gtk-settings} instance per screen. It can be obtained with
  the @fun{gtk-settings-get-for-screen} function, but in many cases, it is more
  convenient to use the @fun{gtk-widget-get-settings} function. The
  @fun{gtk-settings-get-default} function returns the @sym{gtk-settings}
  instance for the default screen.
  @see-slot{gtk-settings-color-hash}
  @see-slot{gtk-settings-gtk-alternative-button-order}
  @see-slot{gtk-settings-gtk-alternative-sort-arrows}
  @see-slot{gtk-settings-gtk-application-prefer-dark-theme}
  @see-slot{gtk-settings-gtk-auto-mnemonics}
  @see-slot{gtk-settings-gtk-button-images}
  @see-slot{gtk-settings-gtk-can-change-accels}
  @see-slot{gtk-settings-gtk-color-palette}
  @see-slot{gtk-settings-gtk-color-scheme}
  @see-slot{gtk-settings-gtk-cursor-blink}
  @see-slot{gtk-settings-gtk-cursor-blink-time}
  @see-slot{gtk-settings-gtk-cursor-blink-timeout}
  @see-slot{gtk-settings-gtk-cursor-theme-name}
  @see-slot{gtk-settings-gtk-cursor-theme-size}
  @see-slot{gtk-settings-gtk-decoration-layout}
  @see-slot{gtk-settings-gtk-dialogs-use-header}
  @see-slot{gtk-settings-gtk-dnd-drag-threshold}
  @see-slot{gtk-settings-gtk-double-click-distance}
  @see-slot{gtk-settings-gtk-double-click-time}
  @see-slot{gtk-settings-gtk-enable-accels}
  @see-slot{gtk-settings-gtk-enable-animations}
  @see-slot{gtk-settings-gtk-enable-event-sounds}
  @see-slot{gtk-settings-gtk-enable-input-feedback-sounds}
  @see-slot{gtk-settings-gtk-enable-mnemonics}
  @see-slot{gtk-settings-gtk-enable-primary-paste}
  @see-slot{gtk-settings-gtk-enable-tooltips}
  @see-slot{gtk-settings-gtk-entry-password-hint-timeout}
  @see-slot{gtk-settings-gtk-entry-select-on-focus}
  @see-slot{gtk-settings-gtk-error-bell}
  @see-slot{gtk-settings-gtk-fallback-icon-theme}
  @see-slot{gtk-settings-gtk-file-chooser-backend}
  @see-slot{gtk-settings-gtk-font-name}
  @see-slot{gtk-settings-gtk-fontconfig-timestamp}
  @see-slot{gtk-settings-gtk-icon-sizes}
  @see-slot{gtk-settings-gtk-icon-theme-name}
  @see-slot{gtk-settings-gtk-im-module}
  @see-slot{gtk-settings-gtk-im-preedit-style}
  @see-slot{gtk-settings-gtk-im-status-style}
  @see-slot{gtk-settings-gtk-key-theme-name}
  @see-slot{gtk-settings-gtk-keynav-cursor-only}
  @see-slot{gtk-settings-gtk-keynav-use-caret}
  @see-slot{gtk-settings-gtk-keynav-wrap-around}
  @see-slot{gtk-settings-gtk-label-select-on-focus}
  @see-slot{gtk-settings-gtk-long-press-time}
  @see-slot{gtk-settings-gtk-menu-bar-accel}
  @see-slot{gtk-settings-gtk-menu-bar-popup-delay}
  @see-slot{gtk-settings-gtk-menu-images}
  @see-slot{gtk-settings-gtk-menu-popdown-delay}
  @see-slot{gtk-settings-gtk-menu-popup-delay}
  @see-slot{gtk-settings-gtk-modules}
  @see-slot{gtk-settings-gtk-primary-button-warps-slider}
  @see-slot{gtk-settings-gtk-print-backends}
  @see-slot{gtk-settings-gtk-print-preview-command}
  @see-slot{gtk-settings-gtk-recent-files-enabled}
  @see-slot{gtk-settings-gtk-recent-files-limit}
  @see-slot{gtk-settings-gtk-recent-files-max-age}
  @see-slot{gtk-settings-gtk-scrolled-window-placement}
  @see-slot{gtk-settings-gtk-shell-shows-app-menu}
  @see-slot{gtk-settings-gtk-shell-shows-desktop}
  @see-slot{gtk-settings-gtk-shell-shows-menubar}
  @see-slot{gtk-settings-gtk-show-input-method-menu}
  @see-slot{gtk-settings-gtk-show-unicode-menu}
  @see-slot{gtk-settings-gtk-sound-theme-name}
  @see-slot{gtk-settings-gtk-split-cursor}
  @see-slot{gtk-settings-gtk-theme-name}
  @see-slot{gtk-settings-gtk-timeout-expand}
  @see-slot{gtk-settings-gtk-timeout-initial}
  @see-slot{gtk-settings-gtk-timeout-repeat}
  @see-slot{gtk-settings-gtk-titlebar-double-click}
  @see-slot{gtk-settings-gtk-titlebar-middle-click}
  @see-slot{gtk-settings-gtk-titlebar-right-click}
  @see-slot{gtk-settings-gtk-toolbar-icon-size}
  @see-slot{gtk-settings-gtk-toolbar-style}
  @see-slot{gtk-settings-gtk-tooltip-browse-mode-timeout}
  @see-slot{gtk-settings-gtk-tooltip-browse-timeout}
  @see-slot{gtk-settings-gtk-tooltip-timeout}
  @see-slot{gtk-settings-gtk-touchscreen-mode}
  @see-slot{gtk-settings-gtk-visible-focus}
  @see-slot{gtk-settings-gtk-xft-antialias}
  @see-slot{gtk-settings-gtk-xft-dpi}
  @see-slot{gtk-settings-gtk-xft-hinting}
  @see-slot{gtk-settings-gtk-xft-hintstyle}
  @see-slot{gtk-settings-gtk-xft-rgba}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-settings-gtk-color-hash --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "color-hash" 'gtk-settings) 't)
 "The @code{color-hash} property of type @code{GHashTable} (Read) @br{}
  Holds a hash table representation of the @code{gtk-color-scheme} setting,
  mapping color names to @class{gdk-color} structures. @br{}
  @b{Warning:} @code{color-hash} has been deprecated since version 3.8 and
  should not be used in newly-written code. Will always return an empty hash
  table.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-color-hash atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-color-hash 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{color-hash} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-alternative-button-order ------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-alternative-button-order"
                                               'gtk-settings) 't)
 "The @code{gtk-alternative-button-order} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether buttons in dialogs should use the alternative button order. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-alternative-button-order
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-alternative-button-order 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-alternative-button-order} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-alternative-sort-arrows -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-alternative-sort-arrows"
                                               'gtk-settings) 't)
 "The @code{gtk-alternative-sort-arrows} property of type @code{:boolean}
  (Read / Write) @br{}
  Controls the direction of the sort indicators in sorted list and tree views.
  By default an arrow pointing down means the column is sorted in ascending
  order. When set to @em{true}, this order will be inverted. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-alternative-sort-arrows
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-alternative-sort-arrows 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-alternative-sort-arrows} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-application-prefer-dark-theme -------------------------

#+cl-cffi-gtk-documentation
(setf (documentation
        (atdoc:get-slot-from-name "gtk-application-prefer-dark-theme"
                                  'gtk-settings) 't)
 "The @code{gtk-application-prefer-dark-theme} property of type
  @code{:boolean} (Read / Write) @br{}
  Whether the application prefers to use a dark theme. If a GTK+ theme
  includes a dark variant, it will be used instead of the configured theme.
  Some applications benefit from minimizing the amount of light pollution that
  interferes with the content. Good candidates for dark themes are photo and
  video editors that make the actual content get all the attention and
  minimize the distraction of the chrome.
  Dark themes should not be used for documents, where large spaces are
  white/light and the dark chrome creates too much contrast (web browser, text
  editor ...). @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-application-prefer-dark-theme
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-application-prefer-dark-theme 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-application-prefer-dark-theme} slot
  of the @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-auto-mnemonics ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-auto-mnemonics"
                                               'gtk-settings) 't)
 "The @code{gtk-auto-mnemonics} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether mnemonics should be automatically shown and hidden when the user
  presses the mnemonic activator. @br{}
  @b{Warning:} @code{gtk-auto-mnemonics} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting is ignored.
  @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-auto-mnemonics atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-auto-mnemonics 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-auto-mnemonics} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-button-images -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-button-images"
                                               'gtk-settings) 't)
 "The @code{gtk-button-images} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether images should be shown on buttons. @br{}
  @b{Warning:} @code{gtk-button-images} has been deprecated since version 3.10
  and should not be used in newly-written code. This setting is deprecated.
  Application developers control whether a button should show an icon or not, on
  a per-button basis. If a @class{gtk-button} widget should show an icon, use
  the @slot[gtk-button]{always-show-image} property of @class{gtk-button}, and
  pack a @class{gtk-image} inside the @class{gtk-button}. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-button-images atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-button-images 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-button-images} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-can-change-accels -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-can-change-accels"
                                               'gtk-settings) 't)
 "The @code{gtk-can-change-accels} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether menu accelerators can be changed by pressing a key over the menu
  item. @br{}
  @b{Warning:} @code{gtk-can-change-accels} has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-can-change-accels atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-can-change-accels 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-can-change-accels} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-color-palette -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-color-palette"
                                               'gtk-settings) 't)
 "The @code{gtk-color-palette} property of type @code{:string}
  (Read / Write) @br{}
  Only used by the deprecated color selector widget. @br{}
  @b{Warning:} @code{gtk-color-palette} has been deprecated since version
  3.10 and should not be used in newly-written code. @br{}
  Default value: @br{}
    @code{\"black:
            white:
            gray50:
            red:
            purple:
            blue:
            light blue:
            green:
            yellow:
            orange:
            lavender:
            brown:
            goldenrod4:
            dodger blue:
            pink:
            light green:
            gray10:
            gray30:
            gray75:
            gray90\"}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-color-palette atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-color-palette 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-color-palette} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-color-scheme ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-color-scheme"
                                               'gtk-settings) 't)
 "The @code{gtk-color-scheme} property of type @code{:string}
  (Read / Write) @br{}
  A palette of named colors for use in themes. The format of the string is
  @begin{pre}
 name1: color1
 name2: color2
 ...
  @end{pre}
  Color names must be acceptable as identifiers in the gtkrc syntax, and color
  specifications must be in the format accepted by the function
  @fun{gdk-color-parse}. Note that due to the way the color tables from
  different sources are merged, color specifications will be converted to
  hexadecimal form when getting this property.
  Starting with GTK+ 2.12, the entries can alternatively be separated by ';'
  instead of newlines:
  @begin{pre}
 name1: color1; name2: color2; ...
  @end{pre}
  @b{Warning:}
  @code{gtk-color-scheme} has been deprecated since version 3.8 and should
  not be used in newly-written code. Color scheme support was dropped and is no
  longer supported. You can still set this property, but it will be ignored.
  @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-color-scheme atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-color-scheme 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-color-scheme} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-cursor-blink ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-cursor-blink"
                                               'gtk-settings) 't)
 "The @code{gtk-cursor-blink} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the cursor should blink.
  Also see the @code{gtk-cursor-blink-timeout} setting, which allows more
  flexible control over cursor blinking. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-cursor-blink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-cursor-blink 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-cursor-blink} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-cursor-blink-time -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-cursor-blink-time"
                                               'gtk-settings) 't)
 "The @code{gtk-cursor-blink-time} property of type @code{:int}
  (Read / Write) @br{}
  Length of the cursor blink cycle, in milliseconds. @br{}
  Allowed values: >= 100 @br{}
  Default value: 1200")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-cursor-blink-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-cursor-blink-time 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-cursor-blink-time} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-cursor-blink-timeout ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-cursor-blink-timeout"
                                               'gtk-settings) 't)
 "The @code{gtk-cursor-blink-timeout} property of type @code{:int}
  (Read / Write) @br{}
  Time after which the cursor stops blinking, in seconds. The timer is reset
  after each user interaction.
  Setting this to zero has the same effect as setting
  @code{gtk-cursor-blink} to @code{nil}. @br{}
  Allowed values: >= 1 @br{}
  Default value: 2147483647")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-cursor-blink-timeout
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-cursor-blink-timeout 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-cursor-blink-timeout} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-cursor-theme-name -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-cursor-theme-name"
                                               'gtk-settings) 't)
 "The @code{gtk-cursor-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  Name of the cursor theme to use, or @code{nil} to use the default theme. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-cursor-theme-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-cursor-theme-name 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-cursor-theme-name} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-cursor-theme-size -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-cursor-theme-size"
                                               'gtk-settings) 't)
 "The @code{gtk-cursor-theme-size} property of type @code{:int}
  (Read / Write) @br{}
  Size to use for cursors, or 0 to use the default size. @br{}
  Allowed values: [0,128] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-cursor-theme-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-cursor-theme-size 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-cursor-theme-size} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-decoration-layout -------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-decoration-layout"
                                               'gtk-settings) 't)
 "The @code{gtk-decoration-layout} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines which buttons should be put in the titlebar of
  client-side decorated windows, and whether they should be placed at the left
  of right.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are minimize, maximize, close, icon (the window icon)
  and menu (a menu button for the fallback app menu).

  For example, \"menu:minimize,maximize,close\" specifies a menu on the left,
  and minimize, maximize and close buttons on the right.

  Note that buttons will only be shown when they are meaningful. E.g. a menu
  button only appears when the desktop shell does not show the app menu, and a
  close button only appears on a window that can be closed.

  Also note that the setting can be overridden with the @code{decoration-layout}
  property.@br{}
  Default value: \"menu:minimize,maximize,close\" @br{}
  Since 3.12")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-decoration-layout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-decoration-layout 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-decoration-layout} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.12
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-dialogs-use-header ------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-dialogs-use-header"
                                               'gtk-settings) 't)
 "The @code{gtk-dialogs-use-header} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether builtin GTK+ dialogs such as the file chooser, the color chooser or
  the font chooser will use a header bar at the top to show action widgets, or
  an action area at the bottom.

  This setting does not affect custom dialogs using @class{gtk-dialog} directly,
  or message dialogs. @br{}
  Default value: @code{nil} @br{}
  Since 3.12")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-dialogs-use-header atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-dialogs-use-header 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-dialogs-use-header} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.12
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-dnd-drag-threshold ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-dnd-drag-threshold"
                                               'gtk-settings) 't)
 "The @code{gtk-dnd-drag-threshold} property of type @code{:int}
  (Read / Write) @br{}
  Number of pixels the cursor can move before dragging. @br{}
  Allowed values: >= 1 @br{}
  Default value: 8")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-dnd-drag-threshold atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-dnd-drag-threshold 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-dnd-drag-threshold} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-double-click-distance ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-double-click-distance"
                                               'gtk-settings) 't)
 "The @code{gtk-double-click-distance} property of type @code{:int}
  (Read / Write) @br{}
  Maximum distance in pixels allowed between two clicks for them to be
  considered a double click. @br{}
  Allowed values: >= 0 @br{}
  Default value: 5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-double-click-distance
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-double-click-distance 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-double-click-distance} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-double-click-time -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-double-click-time"
                                               'gtk-settings) 't)
 "The @code{gtk-double-click-time} property of type @code{:int}
  (Read / Write) @br{}
  Maximum time allowed between two clicks for them to be considered a double
  click (in milliseconds). @br{}
  Allowed values: >= 0 @br{}
  Default value: 250")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-double-click-time atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-double-click-time 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-double-click-time} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-accels -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-enable-accels"
                                               'gtk-settings) 't)
 "The @code{gtk-enable-accels} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether menu items should have visible accelerators which can be
  activated. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-accels atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-accels 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-accels} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-animations -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-enable-animations"
                                               'gtk-settings) 't)
 "The @code{gtk-enable-animations} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to enable toolkit-wide animations. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-animations atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-animations 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-animations} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-event-sounds -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-enable-event-sounds"
                                               'gtk-settings) 't)
 "The @code{gtk-enable-event-sounds} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to play any event sounds at all.
  See the Sound Theme spec for more information on event sounds and sound
  themes.
  GTK+ itself does not support event sounds, you have to use a loadable module
  like the one that comes with @code{libcanberra}. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-event-sounds
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-event-sounds 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-event-sounds} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-input-feedback-sounds --------------------------

#+cl-cffi-gtk-documentation
(setf (documentation
        (atdoc:get-slot-from-name "gtk-enable-input-feedback-sounds"
                                  'gtk-settings) 't)
 "The @code{gtk-enable-input-feedback-sounds} property of type
  @code{:boolean} (Read / Write) @br{}
  Whether to play event sounds as feedback to user input.
  See the Sound Theme spec for more information on event sounds and sound
  themes.
  GTK+ itself does not support event sounds, you have to use a loadable module
  like the one that comes with @code{libcanberra}. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-input-feedback-sounds
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-input-feedback-sounds 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-input-feedback-sounds} slot of
  the @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-mnemonics --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-enable-mnemonics"
                                               'gtk-settings) 't)
 "The @code{gtk-enable-mnemonics} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether labels and menu items should have visible mnemonics which can be
  activated. @br{}
  @b{Warning:} @code{gtk-enable-mnemonics} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting can still be
  used for application overrides, but will be ignored in the future. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-mnemonics atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-mnemonics 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-mnemonics} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-primary-paste ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-enable-primary-paste"
                                               'gtk-settings) t)
 "The @code{gtk-enable-primary-paste} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a middle click on a mouse should paste the @code{PRIMARY} clipboard
  content at the cursor location. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-primary-paste
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-primary-paste 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-primary-paste} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-enable-tooltips ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-enable-tooltips"
                                               'gtk-settings) 't)
 "The @code{gtk-enable-tooltips} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether tooltips should be shown on widgets. @br{}
  @b{Warning:} @code{gtk-enable-tooltips} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting is ignored.
  @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-enable-tooltips atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-enable-tooltips 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-enable-tooltips} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-entry-password-hint-timeout ---------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-entry-password-hint-timeout"
                                               'gtk-settings) 't)
 "The @code{gtk-entry-password-hint-timeout} property of type @code{:uint}
  (Read / Write) @br{}
  How long to show the last input character in hidden entries. This value is
  in milliseconds. The value 0 disables showing the last char. The value 600 is
  a good value for enabling it. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-entry-password-hint-timeout
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-entry-password-hint-timeout 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-entry-password-hint-timeout} slot of
  the @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-entry-select-on-focus ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-entry-select-on-focus"
                                               'gtk-settings) 't)
 "The @code{gtk-entry-select-on-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to select the contents of an entry when it is focused. @br{}
  Default value: @code{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-entry-select-on-focus
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-entry-select-on-focus 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-entry-select-on-focus} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-error-bell --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-error-bell"
                                               'gtk-settings) 't)
 "The @code{gtk-error-bell} property of type @code{:boolean}
  (Read / Write) @br{}
  When @em{true}, keyboard navigation and other input-related errors will cause
  a beep. Since the error bell is implemented using the function
  @fun{gdk-window-beep}, the windowing system may offer ways to configure the
  error bell in many ways, such as flashing the window or similar visual
  effects. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-error-bell atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-error-bell 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-error-bell} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-fallback-icon-theme -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-fallback-icon-theme"
                                               'gtk-settings) 't)
 "The @code{gtk-fallback-icon-theme} property of type @code{:string}
  (Read / Write) @br{}
  Name of a icon theme to fall back to. @br{}
  @b{Warning:} @code{gtk-fallback-icon-theme} has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-fallback-icon-theme
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-fallback-icon-theme 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-fallback-icon-theme} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-file-chooser-backend ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-file-chooser-backend"
                                               'gtk-settings) 't)
 "The @code{gtk-file-chooser-backend} property of type @code{:string}
  (Read / Write) @br{}
  Name of the @class{gtk-file-chooser} backend to use by default. @br{}
  @b{Warning:} @code{gtk-file-chooser-backend} has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @classd{gtk-file-chooser} uses GIO by default. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-file-chooser-backend
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-file-chooser-backend 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-file-chooser-backend} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-font-name ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-font-name"
                                               'gtk-settings) 't)
 "The @code{gtk-font-name} property of type @code{:string}
  (Read / Write) @br{}
  Name of default font to use. @br{}
  Default value: \"Sans 10\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-font-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-font-name 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-font-name} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-fontconfig-timestamp ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-fontconfig-timestamp"
                                               'gtk-settings) 't)
 "The @code{gtk-fontconfig-timestamp} property of type @code{:uint}
  (Read / Write) @br{}
  Timestamp of current fontconfig configuration. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-fontconfig-timestamp
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-fontconfig-timestamp 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-fontconfig-timestamp} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-icon-sizes --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-icon-sizes"
                                               'gtk-settings) 't)
 "The @code{gtk-icon-sizes} property of type @code{:string}
  (Read / Write) @br{}
  A list of icon sizes. The list is separated by colons, and item has the
  form:
  @begin{pre}
 size-name = width , height
  @end{pre}
  E. g. @code{\"gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48\"}. GTK+ itself
  use the following named icon sizes: gtk-menu, gtk-button, gtk-small-toolbar,
  gtk-large-toolbar, gtk-dnd, gtk-dialog. Applications can register their own
  named icon sizes with the @fun{gtk-icon-size-register} function. @br{}
  @b{Warning:} The @code{gtk-icon-sizes} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-icon-sizes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-icon-sizes 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-icon-sizes} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-icon-theme-name ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-icon-theme-name"
                                               'gtk-settings) 't)
 "The @code{gtk-icon-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  Name of icon theme to use. @br{}
  Default value: \"hicolor\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-icon-theme-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-icon-theme-name 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-icon-theme-name} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settigns-gtk-im-module ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-im-module"
                                               'gtk-settings) 't)
 "The @code{gtk-im-module} property of type @code{:string}
  (Read / Write) @br{}
  Which IM (input method) module should be used by default. This is the input
  method that will be used if the user has not explicitly chosen another input
  method from the IM context menu. This also can be a colon-separated list of
  input methods, which GTK+ will try in turn until it finds one available on
  the system. See the @class{gtk-im-context} class and the
  @code{gtk-show-input-method-menu} property. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-im-module atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-im-module 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-im-module} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-im-preedit-style --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-im-preedit-style"
                                               'gtk-settings) 't)
 "The @code{gtk-im-preedit-style} property of type
  @symbol{gtk-im-preedit-style} (Read / Write) @br{}
  How to draw the input method preedit string. @br{}
  @b{Warning:} The @code{gtk-im-preedit-style} property has been deprecated
  since version 3.10 and should not be used in newly-written code. This setting
  is ignored. @br{}
  Default value: @code{:callback}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-im-preedit-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-im-preedit-style 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-im-preedit-style} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-im-status-style ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-im-status-style"
                                               'gtk-settings) 't)
 "The @code{gtk-im-status-style} property of type
  @symbol{gtk-im-status-style} (Read / Write) @br{}
  How to draw the input method statusbar. @br{}
  @b{Warning:} @code{gtk-im-status-style} has been deprecated since version 3.10
  and should not be used in newly-written code. This setting is ignored. @br{}
  Default value: @code{:callback}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-im-status-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-im-status-style 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-im-status-style} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-key-theme-name ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-key-theme-name"
                                               'gtk-settings) 't)
 "The @code{gtk-key-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  Name of key theme to load. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-key-theme-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-key-theme-name 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-key-theme-name} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; gtk-settings-gtk-keynav-cursor-only ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-keynav-cursor-only"
                                               'gtk-settings) 't)
 "The @code{gtk-keynav-cursor-only} property of type @code{:boolean}
  (Read / Write) @br{}
  When @code{true}, keyboard navigation should be able to reach all widgets by
  using the cursor keys only. Tab, Shift etc. keys cannot be expected to be
  present on the used input device. @br{}
  @b{Warning:} The @code{gtk-keynav-cursor-only} property has been deprecated
  since version 3.10 and should not be used in newly-written code. Generally,
  the behavior for touchscreen input should be performed dynamically based on
  the @fun{gdk-event-get-source-device} function. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-keynav-cursor-only atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-keynav-cursor-only 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-keynav-cursor-only} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-keynav-use-caret --------------------------------------

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-keynav-use-caret"
                                               'gtk-settings) 't)
 "The @code{gtk-keynav-use-caret} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether GTK+ should make sure that text can be navigated with a caret, even
  if it is not editable. This is useful when using a screen reader. @br{}
  Default value: @code{nil} @br{}
  Since 3.20")

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-keynav-use-caret atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-keynav-use-caret 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-keynav-cursor-only} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.20
  @see-class{gtk-settings}")

;;;  --- gtk-settings-gtk-keynav-wrap-around -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-keynav-wrap-around"
                                               'gtk-settings) 't)
 "The @code{gtk-keynav-wrap-around} property of type @code{:boolean}
  (Read / Write) @br{}
  When @code{true}, some widgets will wrap around when doing keyboard
  navigation, such as menus, menubars and notebooks. @br{}
  @b{Warning:} @code{gtk-keynav-wrap-around} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting is ignored.
  @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-keynav-wrap-around atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-keynav-wrap-around 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-keynav-wrap-around} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-label-select-on-focus ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-label-select-on-focus"
                                               'gtk-settings) 't)
 "The @code{gtk-label-select-on-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to select the contents of a selectable label when it is focused. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-label-select-on-focus
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-label-select-on-focus 'function)
 "@version{2014-1-29}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-label-select-on-focus} slot of the
    @class{gtk-settings} class.
  @end{short}
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-long-press-time ---------------------------------------

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-long-press-time"
                                               'gtk-settings) 't)
 "The @code{gtk-long-press-time} property of type @code{:uint}
  (Read / Write) @br{}
  The time for a button or touch press to be considered a \"long press\". @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 50 @br{}
  Since 3.14")

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-long-press-time
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-long-press-time 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-long-press-time} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.14
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-menu-bar-accel ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-menu-bar-accel"
                                               'gtk-settings) 't)
 "The @code{gtk-menu-bar-accel} property of type @code{:string}
  (Read / Write) @br{}
  Keybinding to activate the menu bar. @br{}
  @b{Warning:} The @code{gtk-menu-bar-accel} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting can
  still be used for application overrides, but will be ignored in the future.
  @br{}
  Default value: \"F10\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-menu-bar-accel atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-menu-bar-accel 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-menu-bar-accel} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-menu-bar-popup-delay ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-menu-bar-popup-delay"
                                               'gtk-settings) 't)
 "The @code{gtk-menu-bar-popup-delay} property of type @code{:int}
  (Read / Write) @br{}
  Delay before the submenus of a menu bar appear. @br{}
  @b{Warning:} @code{gtk-menu-bar-popup-delay} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting is ignored.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-menu-bar-popup-delay
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-menu-bar-popup-delay 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-menu-bar-popup-delay} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-menu-images -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-menu-images"
                                               'gtk-settings) 't)
 "The @code{gtk-menu-images} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether images should be shown in menus. @br{}
  @b{Warning:} The @code{gtk-menu-images} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  deprecated. Application developers control whether or not a
  @class{gtk-menu-item} should have an icon or not, on a per widget basis.
  Either use a @class{gtk-menu-item} with a @class{gtk-box} containing a
  @class{gtk-image} and a @class{gtk-accel-label}, or describe your menus using
  a @class{g-menu} XML description. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-menu-images atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-menu-images 'function)
 "@version{2014-1-29}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-menu-images} slot of the
    @class{gtk-settings} class.
  @end{short}
  @see-class{gtk-settings}")

;;; gtk-settings-gtk-menu-popdown-delay ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-menu-popdown-delay"
                                               'gtk-settings) 't)
 "The @code{gtk-menu-popdown-delay} property of type @code{:int}
  (Read / Write) @br{}
  The time before hiding a submenu when the pointer is moving towards the
  submenu. @br{}
  @b{Warning:} @code{gtk-menu-popdown-delay} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting is ignored.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 1000")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-menu-popdown-delay atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-menu-popdown-delay 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-menu-popdown-delay} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-menu-popup-delay --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-menu-popup-delay"
                                               'gtk-settings) 't)
 "The @code{gtk-menu-popup-delay} property of type @code{:int}
  (Read / Write) @br{}
  Minimum time the pointer must stay over a menu item before the submenu
  appear. @br{}
  @b{Warning:} @code{gtk-menu-popup-delay} has been deprecated since version
  3.10 and should not be used in newly-written code. This setting is ignored.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 225")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-menu-popup-delay atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-menu-popup-delay 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-menu-popup-delay} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-modules -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-modules"
                                               'gtk-settings) 't)
 "The @code{gtk-modules} property of type @code{:string}
  (Read / Write) @br{}
  List of currently active GTK+ modules. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-modules atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-modules 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-modules} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-primary-button-warps-slider ---------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-primary-button-warps-slider"
                                               'gtk-settings) t)
 "The @code{gtk-primary-button-warps-slider} property of type
  @code{:boolean} (Read / Write) @br{}
  Whether a click in a @class{gtk-range} widget trough should scroll to the
  click position or scroll by a single page in the respective direction. @br{}
  Default value: @em{true} @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-primary-button-warps-slider
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-primary-button-warps-slider 'function)
 "@version{2014-1-29}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-primary-buton-warps-slider} slot of
    the @class{gtk-settings} class.
  @end{short}

  Since 3.6
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-print-backends ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-print-backends"
                                               'gtk-settings) 't)
 "The @code{gtk-print-backends} property of type @code{:string}
  (Read / Write) @br{}
  A comma-separated list of print backends to use in the print dialog.
  Available print backends depend on the GTK+ installation, and may include
  @code{\"file\"}, @code{\"cups\"}, @code{\"lpr\"} or @code{\"papi\"}. @br{}
  Default value: @code{\"file,cups\"}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-print-backends atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-print-backends 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-print-backends} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-print-preview-command ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-print-preview-command"
                                               'gtk-settings) 't)
 "The @code{gtk-print-preview-command} property of type @code{:string}
  (Read / Write) @br{}
  A command to run for displaying the print preview. The command should
  contain a @code{f} placeholder, which will get replaced by the path to the
  pdf file. The command may also contain a @code{s} placeholder, which will get
  replaced by the path to a file containing the print settings in the format
  produced by the @fun{gtk-print-settings-to-file} function.
  The preview application is responsible for removing the pdf file and the
  print settings file when it is done. @br{}
  Default value:
  @code{\"evince --unlink-tempfile --preview --print-settings %s %f\"}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-print-preview-command
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-print-preview-command 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-print-preview-command} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-recent-files-enabled ----------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-recent-files-enabled"
                                               'gtk-settings) 't)
 "The @code{gtk-recent-files-enabled} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether GTK+ should keep track of items inside the recently used resources
  list. If set to @code{nil}, the list will always be empty. @br{}
  Default value: @em{true} @br{}
  Since 3.8")

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-recent-files-enabled
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-recent-files-enabled 'function)
 "@version{2014-1-29}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-recent-files-enabled} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.8
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-recent-files-limit ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-recent-files-limit"
                                               'gtk-settings) 't)
 "The @code{gtk-recent-files-limit} property of type @code{:int}
  (Read / Write) @br{}
  The number of recently used files that should be displayed by default by
  @class{gtk-recent-chooser} implementations and by the
  @class{gtk-file-chooser} interface. A value of -1 means every recently used
  file stored. @br{}
  @b{Warning:} The @code{gtk-recent-files-limit} property has been deprecated
  since version 3.10 and should not be used in newly-written code. This setting
  is ignored. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: 50")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-recent-files-limit atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-recent-files-limit 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-recent-files-limit} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-recent-files-max-age ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-recent-files-max-age"
                                               'gtk-settings) 't)
 "The @code{gtk-recent-files-max-age} property of type @code{:int}
  (Read / Write) @br{}
  The maximum age, in days, of the items inside the recently used resources
  list. Items older than this setting will be excised from the list. If set to
  0, the list will always be empty; if set to -1, no item will be
  removed. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: 30")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-recent-files-max-age
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-recent-files-max-age 'function)
 "@version{2013-3-13}
  Accessor of the @slot[gtk-settings]{gtk-recent-files-max-page} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-scrolled-window-placement -----------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-scrolled-window-placement"
                                               'gtk-settings) 't)
 "The @code{gtk-scrolled-window-placement} property of type
  @symbol{gtk-corner-type} (Read / Write) @br{}
  Where the contents of scrolled windows are located with respect to the
  scrollbars, if not overridden by the scrolled window's own placement. @br{}
  @b{Warning:} The @code{gtk-scrolled-window-placement} property has been
  deprecated since version 3.10 and should not be used in newly-written code.
  This setting is ignored. @br{}
  Default value: @code{:top-left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-scrolled-window-placement
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-scrolled-window-placement 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-scrolled-window-placement} slot of
  the @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-shell-shows-app-menu ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-shell-shows-app-menu"
                                               'gtk-settings) 't)
 "The @code{gtk-shell-shows-app-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the app menu,
  @code{nil} if the app should display it itself. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-shell-shows-app-menu
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-shell-shows-app-menu 'function)
 "@version{2014-1-29}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-shell-shows-app-menu} slot of the
    @class{gtk-settings} class.
  @end{short}
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-shell-shows-desktop -----------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-shell-shows-desktop"
                                               'gtk-settings) 't)
 "The @code{gtk-shell-shows-desktop} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the desktop folder,
  @code{nil} if not. @br{}
  Default value: @em{true} @br{}
  Since 3.10")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-shell-shows-desktop
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-shell-shows-desktop 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-shell-shows-desktop} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.10
  @see-class{gtk-settings}")

;;; gtk-settings-gtk-shell-shows-menubar ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-shell-shows-menubar"
                                               'gtk-settings) 't)
 "The @code{gtk-shell-shows-menubar} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the menubar,
  @code{nil} if the app should display it itself. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-shell-shows-menubar
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-shell-shows-menubar 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-shell-shows-menubar} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-show-input-method-menu --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-show-input-method-menu"
                                               'gtk-settings) 't)
 "The @code{gtk-show-input-method-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the context menus of entries and text views should offer to change
  the input method. @br{}
  @b{Warning:} The @code{gtk-show-input-method-menu} property has been
  deprecated since version 3.10 and should not be used in newly-written code.
  This setting is ignored. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-show-input-method-menu
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-show-input-method-menu 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-show-input-method-menu} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-show-unicode-menu -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-show-unicode-menu"
                                               'gtk-settings) 't)
 "The @code{gtk-show-unicode-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the context menus of entries and text views should offer to insert
  control characters. @br{}
  @b{Warning:} The @code{gtk-show-unicode-menu} property has been deprecated
  since version 3.10 and should not be used in newly-written code. This setting
  is ignored. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-show-unicode-menu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-show-unicode-menu 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-show-unicode-menu} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-sound-theme-name --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-sound-theme-name"
                                               'gtk-settings) 't)
 "The @code{gtk-sound-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  The XDG sound theme to use for event sounds.
  See the Sound Theme spec for more information on event sounds and sound
  themes.
  GTK+ itself does not support event sounds, you have to use a loadable module
  like the one that comes with libcanberra. @br{}
  Default value: @code{\"freedesktop\"}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-sound-theme-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-sound-theme-name 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-sound-theme-name} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-split-cursor ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-split-cursor"
                                               'gtk-settings) 't)
 "The @code{gtk-split-cursor} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether two cursors should be displayed for mixed left-to-right and
  right-to-left text. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-split-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-split-cursor 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-split-cursor} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-theme-name --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-theme-name"
                                               'gtk-settings) 't)
 "The @code{gtk-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  Name of theme to load. @br{}
  Default value: @code{\"Raleigh\"}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-theme-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-theme-name 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-theme-name} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-timeout-expand ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-timeout-expand"
                                               'gtk-settings) 't)
 "The @code{gtk-timeout-expand} property of type @code{:int}
  (Read / Write) @br{}
  Expand value for timeouts, when a widget is expanding a new region. @br{}
  @b{Warning:} @code{gtk-timeout-expand} has been deprecated since version 3.10
  and should not be used in newly-written code. This setting is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 500")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-timeout-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-timeout-expand 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-timeout-expand} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; gtk-settings-gtk-timeout-initial -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-timeout-initial"
                                               'gtk-settings) 't)
 "The @code{gtk-timeout-initial} property of type @code{:int}
  (Read / Write) @br{}
  Starting value for timeouts, when button is pressed. @br{}
  @b{Warning:} The @code{gtk-timeout-initial} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 200")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-timeout-initial atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-timeout-initial 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-timeout-initial} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-timeout-repeat ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-timeout-repeat"
                                               'gtk-settings) 't)
 "The @code{gtk-timeout-repeat} property of type @code{:int}
  (Read / Write) @br{}
  Repeat value for timeouts, when button is pressed. @br{}
  @b{Warning:} The @code{gtk-timeout-repeat} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 20")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-timeout-repeat atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-timeout-repeat 'function)
 "@version{2014-1-29}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-timeout-repeat} slot of the
    @class{gtk-settings} class.
  @end{short}
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-titlebar-double-click ---------------------------------

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-titlebar-double-click"
                                               'gtk-settings) 't)
 "The @code{gtk-titlebar-double-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a double-click occure on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"toggle-maximize\" @br{}
  Since 3.14")

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-titlebar-double-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-titlebar-double-click 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-titlebar-double-click} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.14
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-titlebar-middle-click ---------------------------------

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-titlebar-middle-click"
                                               'gtk-settings) 't)
 "The @code{gtk-titlebar-middle-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a middle-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"none\" @br{}
  Since 3.14")

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-titlebar-midle-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-titlebar-midle-click 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-titlebar-midle-click} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.14
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-titlebar-right-click ----------------------------------

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "gtk-titlebar-right-click"
                                               'gtk-settings) 't)
 "The @code{gtk-titlebar-right-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a right-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"menu\" @br{}
  Since 3.14")

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-settings-gtk-titlebar-right-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-titlebar-right-click 'function)
 "@version{2019-4-19}
  @begin{short}
    Accessor of the @slot[gtk-settings]{gtk-titlebar-right-click} slot of the
    @class{gtk-settings} class.
  @end{short}

  Since 3.14
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-toolbar-icon-size -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-toolbar-icon-size"
                                               'gtk-settings) 't)
 "The @code{gtk-toolbar-icon-size} property of type @symbol{gtk-icon-size}
  (Read / Write) @br{}
  The size of icons in default toolbars. @br{}
  @b{Warning:} The @code{gtk-toolbar-icon-size} property has been deprecated
  since version 3.10 and should not be used in newly-written code. This setting
  is ignored. @br{}
  Default value: @code{:large-toolbar}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-toolbar-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-toolbar-icon-size 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-toolbar-icon-size} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-toolbar-style -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-toolbar-style"
                                               'gtk-settings) 't)
 "The @code{gtk-toolbar-style} property of type @symbol{gtk-toolbar-style}
  (Read / Write) @br{}
  The size of icons in default toolbars. @br{}
  @b{Warning:} The @code{gtk-toolbar-style} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Default value: @code{:both}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-toolbar-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-toolbar-style 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-toolbar-style} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-tooltip-browse-mode-timeout ---------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-tooltip-browse-mode-timeout"
                                               'gtk-settings) 't)
 "The @code{gtk-tooltip-browse-mode-timeout} property of type @code{:int}
  (Read / Write) @br{}
  Amount of time, in milliseconds, after which the browse mode will be
  disabled.
  See the @slot[gtk-settings]{gtk-tooltip-browse-timeout} setting for more
  information about browse mode. @br{}
  @b{Warning:} The @code{gtk-tooltip-browse-mode-timeout} property has been
  deprecated since version 3.10 and should not be used in newly-written code.
  This setting is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 500")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-tooltip-browse-mode-timeout
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-tooltip-browse-mode-timeout 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-tooltip-browse-mode-timeout} slot
  of the @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-tooltip-browse-timeout --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-tooltip-browse-timeout"
                                               'gtk-settings) 't)
 "The @code{gtk-tooltip-browse-timeout} property of type @code{:int}
  (Read / Write) @br{}
  Controls the time after which tooltips will appear when browse mode is
  enabled, in milliseconds. Browse mode is enabled when the mouse pointer moves
  off an object where a tooltip was currently being displayed. If the mouse
  pointer hits another object before the browse mode timeout expires, see the
  @slot[gtk-settings]{gtk-tooltip-browse-mode-timeout} setting, it will take the
  amount of milliseconds specified by this setting to popup the tooltip for the
  new object. @br{}
  @b{Warning:} The @code{gtk-tooltip-browse-timeout} property has been
  deprecated since version 3.10 and should not be used in newly-written code.
  This setting is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 60")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-tooltip-browse-timeout
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-tooltip-browse-timeout 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-tooltip-browse-timeout} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-tooltip-timeout ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-tooltip-timeout"
                                               'gtk-settings) 't)
 "The @code{gtk-tooltip-timeout} property of type @code{:int}
  (Read / Write) @br{}
  Time, in milliseconds, after which a tooltip could appear if the cursor is
  hovering on top of a widget. @br{}
  @b{Warning:} The @code{gtk-tooltip-timeout} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 500")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-tooltip-timeout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-tooltip-timeout 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-tooltip-timeout} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-touchscreen-mode --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-touchscreen-mode"
                                               'gtk-settings) 't)
 "The @code{gtk-touchscreen-mode} property of type @code{:boolean}
  (Read / Write) @br{}
  When @em{true}, there are no motion notify events delivered on this screen,
  and widgets cannot use the pointer hovering them for any essential
  functionality. @br{}
  @b{Warning:} The @code{gtk-touchscreen-mode} property is deprecated since
  version 3.4 and should not be used in newly-written code. Generally the
  behavior touchscreen input should be performed dynamically based on the
  @fun{gdk-event-get-source-device} function. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-touchscreen-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-touchscreen-mode 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-touchscreen-mode} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-visible-focus -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-visible-focus"
                                               'gtk-settings) 't)
 "The @code{gtk-visible-focus} property of type @symbol{gtk-policy-type}
  (Read / Write) @br{}
  Whether focus rectangles should be always visible, never visible, or
  hidden until the user starts to use the keyboard. @br{}
  @b{Warning:} The @code{gtk-visible-focus} property has been deprecated since
  version 3.10 and should not be used in newly-written code. This setting is
  ignored. @br{}
  Default value: @code{:always}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-visible-focus atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-visible-focus 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-visible-focus} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-xft-antialias -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-xft-antialias"
                                               'gtk-settings) 't)
 "The @code{gtk-xft-antialias} property of type @code{:int}
  (Read / Write) @br{}
  Whether to antialias Xft fonts: 0 = no, 1 = yes, -1 = default. @br{}
  Allowed values: [@code{G_MAXULONG},1] @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-xft-antialias atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-xft-antialias 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-xft-antialias} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-xft-dpi -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-xft-dpi" 'gtk-settings) 't)
 "The @code{gtk-xft-dpi} property of type @code{:int} (Read / Write) @br{}
  Resolution for Xft, in 1024 * dots/inch. -1 to use default value. @br{}
  Allowed values: [@code{G_MAXULONG},1048576] @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-xft-dpi atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-xft-dpi 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-xft-dpi} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-xft-hinting -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-xft-hinting"
                                               'gtk-settings) 't)
 "The @code{gtk-xft-hinting} property of type @code{:int}
  (Read / Write) @br{}
  Whether to hint Xft fonts: 0 = no, 1 = yes, -1 = default. @br{}
  Allowed values: [@code{G_MAXULONG},1] @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-xft-hinting atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-xft-hinting 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-xft-hinting} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-xft-hintstyle -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-xft-hintstyle"
                                               'gtk-settings) 't)
 "The @code{gtk-xft-hintstyle} property of type @code{:string}
  (Read / Write) @br{}
  What degree of hinting to use: @code{hintnone}, @code{hintslight},
  @code{hintmedium}, or @code{hintfull}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-xft-hintstyle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-xft-hintstyle 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-xft-hintstyle} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

;;; --- gtk-settings-gtk-xft-rgba ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gtk-xft-rgba" 'gtk-settings) 't)
 "The @code{gtk-xft-rgba} property of type @code{:string}
  (Read / Write) @br{}
  Type of subpixel antialiasing:
  @code{none}, @code{rgb}, @code{bgr}, @code{vrgb}, @code{vbgr}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-settings-gtk-xft-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-settings-gtk-xft-rgba 'function)
 "@version{2014-1-29}
  Accessor of the @slot[gtk-settings]{gtk-xft-rgba} slot of the
  @class{gtk-settings} class.
  @see-class{gtk-settings}")

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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_settings_get_default" gtk-settings-get-default)
    (g-object gtk-settings)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-29}
  @begin{return}
    A @class{gtk-settings} object. If there is no default screen, then returns
    @code{nil}.
  @end{return}
  Gets the @class{gtk-settings} object for the default GDK screen, creating it
  if necessary. See the @fun{gtk-settings-get-for-screen} function.
  @see-class{gtk-settings}
  @see-function{gtk-settings-get-for-screen}")

(export 'gtk-settings-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_settings_get_for_screen" gtk-settings-get-for-screen)
    (g-object gtk-settings)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-29}
  @argument[screen]{a @class{gdk-screen} object}
  @return{A @class{gtk-settings} object.}
  @begin{short}
    Gets the @class{gtk-settings} object for @arg{screen}, creating it if
    necessary.
  @end{short}
  @see-class{gtk-settings}
  @see-class{gdk-screen}"
  (screen (g-object gdk-screen)))

(export 'gtk-settings-get-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_install_property ()
;;;
;;; void gtk_settings_install_property (GParamSpec *pspec);
;;;
;;; Warning
;;;
;;; gtk_settings_install_property has been deprecated since version 3.16 and
;;; should not be used in newly-written code.
;;;
;;; This function is not useful outside GTK+.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_install_property_parser ()
;;;
;;; void gtk_settings_install_property_parser (GParamSpec *pspec,
;;;                                            GtkRcPropertyParser parser)
;;;
;;; Warning
;;;
;;; gtk_settings_install_property_parser has been deprecated since version 3.16
;;; and should not be used in newly-written code.
;;;
;;; This function is not useful outside GTK+.
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
;;;
;;; Warning
;;;
;;; gtk_settings_set_property_value has been deprecated since version 3.16 and
;;; should not be used in newly-written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_string_property ()
;;;
;;; void gtk_settings_set_string_property (GtkSettings *settings,
;;;                                        const gchar *name,
;;;                                        const gchar *v_string,
;;;                                        const gchar *origin);
;;;
;;; Warning
;;;
;;; gtk_settings_set_string_property has been deprecated since version 3.16 and
;;; should not be used in newly-written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_long_property ()
;;;
;;; void gtk_settings_set_long_property (GtkSettings *settings,
;;;                                      const gchar *name,
;;;                                      glong v_long,
;;;                                      const gchar *origin);
;;;
;;; Warning
;;;
;;; gtk_settings_set_long_property has been deprecated since version 3.16 and
;;; should not be used in newly-written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_double_property ()
;;;
;;; void gtk_settings_set_double_property (GtkSettings *settings,
;;;                                        const gchar *name,
;;;                                        gdouble v_double,
;;;                                        const gchar *origin);
;;;
;;; Warning
;;;
;;; gtk_settings_set_double_property has been deprecated since version 3.16 and
;;; should not be used in newly-written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_reset_property ()
;;;
;;; void
;;; gtk_settings_reset_property (GtkSettings *settings, const gchar *name);
;;;
;;; Undoes the effect of calling g_object_set() to install an
;;; application-specific value for a setting. After this call, the setting will
;;; again follow the session-wide value for this setting.
;;;
;;; settings :
;;;     a GtkSettings object
;;;
;;; name :
;;;     the name of the setting to reset
;;;
;;; Since: 3.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.settings.lisp ------------------------------------------
