;;; ----------------------------------------------------------------------------
;;; gtk.package.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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

(in-package :cl-user)

(defvar *cl-cffi-gtk-build-time* (multiple-value-list (get-decoded-time)))
(defvar *cl-cffi-gtk-version* "1.0.0")

(export '*cl-cffi-gtk-build-time*)
(export '*cl-cffi-gtk-version*)

(defpackage :gtk
  (:use :cl :cl-user :cffi
   :gobject :gdk :gdk-pixbuf :glib :gio :pango :cairo :iter :bordeaux-threads)
  (:export #:cl-cffi-gtk-build-info))

(in-package :gtk)

#|
;;; Loading of the GTK+ library is moved to gdk.package.lisp,
;;; because we need the version info from GTK+ when compiling GDK.
(glib::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gtk
      ((:and :unix (:not :darwin))
       (:or "libgtk-3.so.0" "libgtk-3.so"))
      (:darwin (:or "libgtk-3.0.dylib"
                    "libgtk-3.dylib"
                    "libgtk-x11-3.0.0.dylib"
                    "libgtk-x11-3.0.dylib"))
      (:windows (:or "libgtk-3-0.dll" "libgtk-win32-2.0-0.dll"))
      (t "libgtk-3-0")))
  (use-foreign-library gtk))
|#

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :gtk) t)
 "This is the API documentation of a Lisp binding to GTK+.
  GTK+ is a library for creating graphical user interfaces. It works on many
  UNIX-like platforms, Windows, and OS X. GTK+ is released under the GNU Library
  General Public License (GNU LGPL), which allows for flexible licensing of
  client applications. GTK+ has a C-based object-oriented architecture that
  allows for maximum flexibility. Bindings for many other languages have been
  written, including C++, Objective-C, Guile/Scheme, Perl, Python, TOM, Ada95,
  Free Pascal, and Eiffel.
  @begin[Application support]{section}
    @begin[GtkApplication]{subsection}
      Application class

      @about-symbol{gtk-application-inhibit-flags}
      @about-class{gtk-application}

      @about-generic{gtk-application-active-window}
      @about-generic{gtk-application-app-menu}
      @about-generic{gtk-application-menubar}
      @about-generic{gtk-application-register-session}
      @about-generic{gtk-application-screensaver-active}

      @about-function{gtk-application-new}
      @about-function{gtk-application-add-window}
      @about-function{gtk-application-remove-window}
      @about-function{gtk-application-get-windows}
      @about-function{gtk-application-get-window-by-id}
      @about-function{gtk-application-inhibit}
      @about-function{gtk-application-uninhibit}
      @about-function{gtk-application-is-inhibited}
      @about-function{gtk-application-prefers-app-menu}
      @about-function{gtk-application-get-menu-by-id}
      @about-function{gtk-application-add-accelerator}
      @about-function{gtk-application-remove-accelerator}
      @about-function{gtk-application-list-action-descriptions}
      @about-function{gtk-application-get-accels-for-action}
      @about-function{gtk-application-set-accels-for-action}
      @about-function{gtk-application-get-actions-for-accel}
    @end{subsection}
    @begin[GtkApplicationWindow]{subsection}
      @class{gtk-window} subclass with @class{gtk-application} support.

      @about-class{gtk-application-window}

      @about-generic{gtk-application-window-show-menubar}

      @about-function{gtk-application-window-new}
      @about-function{gtk-application-window-get-id}
      @about-function{gtk-application-window-set-help-overlay}
      @about-function{gtk-application-window-get-help-overlay}
    @end{subsection}
    @begin[GtkActionable]{subsection}
      An interface for widgets that can be associated with actions.

      @about-class{gtk-actionable}

      @about-generic{gtk-actionable-action-name}
      @about-generic{gtk-actionable-action-target}

      @about-function{gtk-actionable-get-action-target-value}
      @about-function{gtk-actionable-set-action-target-value}
      @about-function{gtk-actionable-set-action-target}
      @about-function{gtk-actionable-set-detailed-action-name}
    @end{subsection}
  @end{section}
  @begin[Interface builder]{section}
    @begin[GtkBuilder]{subsection}
      Build an interface from an XML UI definition.

      @about-symbol{gtk-builder-error}
      @about-class{gtk-builder}

      @about-generic{gtk-builder-translation-domain}

      @about-function{gtk-builder-new}
      @about-function{gtk-builder-new-from-file}
      @about-function{gtk-builder-new-from-resource}
      @about-function{gtk-builder-new-from-string}
      @about-function{gtk-builder-add-callback-symbol}
      @about-function{gtk-builder-add-callback-symbols}
      @about-function{gtk-builder-lookup-callback-symbol}
      @about-function{gtk-builder-add-from-file}
      @about-function{gtk-builder-add-from-resource}
      @about-function{gtk-builder-add-from-string}
      @about-function{gtk-builder-add-objects-from-file}
      @about-function{gtk-builder-add-objects-from-string}
      @about-function{gtk-builder-add-objects-from-resource}
      @about-function{gtk-builder-extend-with-template}
      @about-function{gtk-builder-get-object}
      @about-function{gtk-builder-get-objects}
      @about-function{gtk-builder-expose-object}
      @about-function{gtk-builder-connect-signals}
      @about-function{gtk-builder-connect-signals-full}
      @about-function{gtk-builder-set-application}
      @about-function{gtk-builder-get-application}
      @about-function{gtk-builder-get-type-from-name}
      @about-function{gtk-builder-value-from-string}
      @about-function{gtk-builder-value-from-string-type}
    @end{subsection}
    @begin[GtkBuildable]{subsection}
      Interface for objects that can be built by @class{gtk-builder}.

      @about-class{gtk-buildable}

      @about-function{gtk-buildable-set-name}
      @about-function{gtk-buildable-get-name}
      @about-function{gtk-buildable-add-child}
      @about-function{gtk-buildable-set-buildable-property}
      @about-function{gtk-buildable-construct-child}
      @about-function{gtk-buildable-custom-tag-start}
      @about-function{gtk-buildable-custom-tag-end}
      @about-function{gtk-buildable-custom-finished}
      @about-function{gtk-buildable-parser-finished}
      @about-function{gtk-buildable-get-internal-child}
    @end{subsection}
  @end{section}
  @begin[Windows]{section}
    @begin[GtkWindow]{subsection}
      Toplevel which can contain other widgets.

      @about-class{gtk-window}

      @about-generic{gtk-window-accept-focus}
      @about-generic{gtk-window-application}
      @about-generic{gtk-window-attached-to}
      @about-generic{gtk-window-decorated}
      @about-generic{gtk-window-default-height}
      @about-generic{gtk-window-default-width}
      @about-generic{gtk-window-deletable}
      @about-generic{gtk-window-destroy-with-parent}
      @about-generic{gtk-window-focus-on-map}
      @about-generic{gtk-window-focus-visible}
      @about-generic{gtk-window-gravity}
      @about-generic{gtk-window-has-resize-grip}
      @about-generic{gtk-window-has-toplevel-focus}
      @about-generic{gtk-window-hide-titlebar-when-maximized}
      @about-generic{gtk-window-icon}
      @about-generic{gtk-window-icon-name}
      @about-generic{gtk-window-is-active}
      @about-generic{gtk-window-is-maximized}
      @about-generic{gtk-window-mnemonics-visible}
      @about-generic{gtk-window-modal}
      @about-generic{gtk-window-resizable}
      @about-generic{gtk-window-resize-grip-visible}
      @about-generic{gtk-window-role}
      @about-generic{gtk-window-screen}
      @about-generic{gtk-window-skip-pager-hint}
      @about-generic{gtk-window-skip-taskbar-hint}
      @about-generic{gtk-window-startup-id}
      @about-generic{gtk-window-title}
      @about-generic{gtk-window-transient-for}
      @about-generic{gtk-window-type}
      @about-generic{gtk-window-type-hint}
      @about-generic{gtk-window-urgency-hint}
      @about-generic{gtk-window-window-position}

      @about-function{gtk-window-new}
      @about-function{gtk-window-set-wmclass}
      @about-function{gtk-window-add-accel-group}
      @about-function{gtk-window-remove-accel-group}
      @about-function{gtk-window-activate-focus}
      @about-function{gtk-window-activate-default}
      @about-function{gtk-window-set-default-geometry}
      @about-function{gtk-window-set-geometry-hints}
      @about-function{gtk-window-set-position}
      @about-function{gtk-window-is-active}
      @about-function{gtk-window-has-toplevel-focus}
      @about-function{gtk-window-list-toplevels}
      @about-function{gtk-window-add-mnemonic}
      @about-function{gtk-window-remove-mnemonic}
      @about-function{gtk-window-mnemonic-activate}
      @about-function{gtk-window-activate-key}
      @about-function{gtk-window-propagate-key-event}
      @about-function{gtk-window-get-focus}
      @about-function{gtk-window-set-focus}
      @about-function{gtk-window-get-default-widget}
      @about-function{gtk-window-set-default}
      @about-function{gtk-window-present}
      @about-function{gtk-window-present-with-time}
      @about-function{gtk-window-close}
      @about-function{gtk-window-iconify}
      @about-function{gtk-window-deiconify}
      @about-function{gtk-window-stick}
      @about-function{gtk-window-unstick}
      @about-function{gtk-window-maximize}
      @about-function{gtk-window-unmaximize}
      @about-function{gtk-window-fullscreen}
      @about-function{gtk-window-fullscreen-on-monitor}
      @about-function{gtk-window-unfullscreen}
      @about-function{gtk-window-set-keep-above}
      @about-function{gtk-window-set-keep-below}
      @about-function{gtk-window-begin-resize-drag}
      @about-function{gtk-window-begin-move-drag}
      @about-function{gtk-window-set-mnemonic-modifier}
      @about-function{gtk-window-get-default-icon-list}
      @about-function{gtk-window-get-default-icon-name}
      @about-generic{gtk-window-default-size}
      @about-function{gtk-window-get-icon-list}
      @about-function{gtk-window-get-mnemonic-modifier}
      @about-function{gtk-window-get-position}
      @about-function{gtk-window-get-size}
      @about-function{gtk-window-get-group}
      @about-function{gtk-window-has-group}
      @about-function{gtk-window-get-window-type}
      @about-function{gtk-window-move}
      @about-function{gtk-window-parse-geometry}
      @about-function{gtk-window-reshow-with-initial-size}
      @about-function{gtk-window-resize}
      @about-function{gtk-window-resize-to-geometry}
      @about-function{gtk-window-set-default-icon-list}
      @about-function{gtk-window-set-default-icon}
      @about-function{gtk-window-set-default-icon-from-file}
      @about-function{gtk-window-set-default-icon-name}
      @about-function{gtk-window-set-icon-list}
      @about-function{gtk-window-set-icon-from-file}
      @about-function{gtk-window-set-auto-startup-notification}
      @about-function{gtk-window-get-opacity}
      @about-function{gtk-window-set-opacity}
      @about-function{gtk-window-resize-grip-is-visible}
      @about-function{gtk-window-get-resize-grip-area}
      @about-function{gtk-window-set-has-user-ref-count}
      @about-function{gtk-window-set-titlebar}
      @about-function{gtk-window-get-titlebar}
      @about-function{gtk-window-set-interactive-debugging}
    @end{subsection}
    @begin[GtkDialog]{subsection}
      Create popup windows.

      @about-symbol{gtk-dialog-flags}
      @about-symbol{gtk-response-type}

      @about-class{gtk-dialog}

      @about-generic{gtk-dialog-use-header-bar}

      @about-function{gtk-dialog-new}
      @about-function{gtk-dialog-new-with-buttons}
      @about-function{gtk-dialog-run}
      @about-function{gtk-dialog-response}
      @about-function{gtk-dialog-add-button}
      @about-function{gtk-dialog-add-buttons}
      @about-function{gtk-dialog-add-action-widget}
      @about-function{gtk-dialog-set-default-response}
      @about-function{gtk-dialog-set-response-sensitive}
      @about-function{gtk-dialog-get-response-for-widget}
      @about-function{gtk-dialog-get-widget-for-response}
      @about-function{gtk-dialog-get-action-area}
      @about-function{gtk-dialog-get-content-area}
      @about-function{gtk-dialog-get-header-bar}
      @about-function{gtk-alternative-dialog-button-order}
      @about-function{gtk-dialog-set-alternative-button-order}
      @about-function{gtk-dialog-set-alternative-button-order-from-array}
    @end{subsection}
    @begin[GtkMessageDialog]{subsection}
      A convenient message window.

      @about-symbol{gtk-message-type}
      @about-symbol{gtk-buttons-type}

      @about-class{gtk-message-dialog}

      @about-generic{gtk-message-dialog-buttons}
      @about-generic{gtk-message-dialog-image}
      @about-generic{gtk-message-dialog-message-area}
      @about-generic{gtk-message-dialog-message-type}
      @about-generic{gtk-message-dialog-secondary-text}
      @about-generic{gtk-message-dialog-secondary-use-markup}
      @about-generic{gtk-message-dialog-text}
      @about-generic{gtk-message-dialog-use-markup}

      @about-function{gtk-message-dialog-new}
      @about-function{gtk-message-dialog-new-with-markup}
      @about-function{gtk-message-dialog-set-markup}
      @about-function{gtk-message-dialog-format-secondary-text}
      @about-function{gtk-message-dialog-format-secondary-markup}
      @about-function{gtk-message-dialog-get-message-area}
    @end{subsection}
    @begin[GtkAboutDialog]{subsection}
      Display information about an application.

      @about-symbol{gtk-license}
      @about-class{gtk-about-dialog}

      @about-generic{gtk-about-dialog-artists}
      @about-generic{gtk-about-dialog-authors}
      @about-generic{gtk-about-dialog-comments}
      @about-generic{gtk-about-dialog-copyright}
      @about-generic{gtk-about-dialog-documenters}
      @about-generic{gtk-about-dialog-license}
      @about-generic{gtk-about-dialog-license-type}
      @about-generic{gtk-about-dialog-logo}
      @about-generic{gtk-about-dialog-logo-icon-name}
      @about-generic{gtk-about-dialog-program-name}
      @about-generic{gtk-about-dialog-translator-credits}
      @about-generic{gtk-about-dialog-version}
      @about-generic{gtk-about-dialog-website}
      @about-generic{gtk-about-dialog-website-label}
      @about-generic{gtk-about-dialog-wrap-license}

      @about-function{gtk-about-dialog-new}
      @about-function{gtk-about-dialog-add-credit-section}
      @about-function{gtk-show-about-dialog}
    @end{subsection}
    @begin[GtkAssistant]{subsection}
      A widget used to guide users through multi-step operations.

      @about-symbol{gtk-assistant-page-type}
      @about-class{gtk-assistant}

      @about-function{gtk-assistant-child-complete}
      @about-function{gtk-assistant-child-has-padding}
      @about-function{gtk-assistant-child-header-image}
      @about-function{gtk-assistant-child-page-type}
      @about-function{gtk-assistant-child-sidebar-image}
      @about-function{gtk-assistant-child-title}

      @about-function{gtk-assistant-new}
      @about-function{gtk-assistant-get-current-page}
      @about-function{gtk-assistant-set-current-page}
      @about-function{gtk-assistant-get-n-pages}
      @about-function{gtk-assistant-get-nth-page}
      @about-function{gtk-assistant-prepend-page}
      @about-function{gtk-assistant-append-page}
      @about-function{gtk-assistant-insert-page}
      @about-function{gtk-assistant-remove-page}
      @about-function{gtk-assistant-set-forward-page-func}
      @about-function{gtk-assistant-set-page-type}
      @about-function{gtk-assistant-get-page-type}
      @about-function{gtk-assistant-set-page-title}
      @about-function{gtk-assistant-get-page-title}
      @about-function{gtk-assistant-set-page-header-image}
      @about-function{gtk-assistant-get-page-header-image}
      @about-function{gtk-assistant-set-page-side-image}
      @about-function{gtk-assistant-get-page-side-image}
      @about-function{gtk-assistant-set-page-complete}
      @about-function{gtk-assistant-get-page-complete}
      @about-function{gtk-assistant-add-action-widget}
      @about-function{gtk-assistant-remove-action-widget}
      @about-function{gtk-assistant-update-buttons-state}
      @about-function{gtk-assistant-commit}
      @about-function{gtk-assistant-next-page}
      @about-function{gtk-assistant-previous-page}
    @end{subsection}
    @begin[GtkInvisible]{subsection}
      A widget which is not displayed.

      @about-class{gtk-invisible}

      @about-generic{gtk-invisible-screen}

      @about-function{gtk-invisible-new}
      @about-function{gtk-invisible-new-for-screen}
    @end{subsection}
    @begin[GtkOffscreenWindow]{subsection}
      A toplevel widget to manage offscreen rendering of child widgets.

      @about-class{gtk-offscreen-window}

      @about-function{gtk-offscreen-window-new}
      @about-function{gtk-offscreen-window-get-surface}
      @about-function{gtk-offscreen-window-get-pixbuf}
    @end{subsection}
    @begin[GtkWindowGroup]{subsection}
      Limit the effect of grabs.

      @about-class{gtk-window-group}

      @about-function{gtk-window-group-new}
      @about-function{gtk-window-group-add-window}
      @about-function{gtk-window-group-remove-window}
      @about-function{gtk-window-group-list-windows}
      @about-function{gtk-window-group-get-current-grab}
      @about-function{gtk-window-group-get-current-device-grab}
    @end{subsection}
  @end{section}
  @begin[Layout Containers]{section}
    @begin[GtkBox]{subsection}
      A container box.

      @about-class{gtk-box}

      @about-generic{gtk-box-baseline-position}
      @about-generic{gtk-box-homogeneous}
      @about-generic{gtk-box-spacing}

      @about-function{gtk-box-child-expand}
      @about-function{gtk-box-child-fill}
      @about-function{gtk-box-child-pack-type}
      @about-function{gtk-box-child-padding}
      @about-function{gtk-box-child-position}

      @about-function{gtk-box-new}
      @about-function{gtk-box-pack-start}
      @about-function{gtk-box-pack-end}
      @about-function{gtk-box-reorder-child}
      @about-function{gtk-box-query-child-packing}
      @about-function{gtk-box-set-child-packing}
      @about-function{gtk-box-get-center-widget}
      @about-function{gtk-box-set-center-widget}
    @end{subsection}
    @begin[GtkGrid]{subsection}
      Pack widgets in a rows and columns.

      @about-class{gtk-grid}

      @about-generic{gtk-grid-baseline-row}
      @about-generic{gtk-grid-column-homogeneous}
      @about-generic{gtk-grid-column-spacing}
      @about-generic{gtk-grid-row-homogeneous}
      @about-generic{gtk-grid-row-spacing}

      @about-function{gtk-grid-child-height}
      @about-function{gtk-grid-child-left-attach}
      @about-function{gtk-grid-child-top-attach}
      @about-function{gtk-grid-child-width}

      @about-function{gtk-grid-new}
      @about-function{gtk-grid-attach}
      @about-function{gtk-grid-attach-next-to}
      @about-function{gtk-grid-get-child-at}
      @about-function{gtk-grid-insert-row}
      @about-function{gtk-grid-insert-column}
      @about-function{gtk-grid-remove-row}
      @about-function{gtk-grid-remove-column}
      @about-function{gtk-grid-insert-next-to}
      @about-function{gtk-grid-get-row-baseline-position}
      @about-function{gtk-grid-set-row-baseline-position}
    @end{subsection}
    @begin[GtkRevealer]{subsection}
      Hide and show with animation.

      @about-symbol{gtk-revealer-transition-type}

      @about-class{gtk-revealer}

      @about-generic{gtk-revealer-child-revealed}
      @about-generic{gtk-revealer-reveal-child}
      @about-generic{gtk-revealer-transition-duration}
      @about-generic{gtk-revealer-transition-type}

      @about-function{gtk-revealer-new}
    @end{subsection}
    @begin[GtkListBox]{subsection}
      A list container.

      @about-class{gtk-list-box}

      @about-generic{gtk-list-box-activate-on-single-click}
      @about-generic{gtk-list-box-selection-mode}
      @about-generic{gtk-list-box-activatable}
      @about-generic{gtk-list-box-selectable}

      @about-function{gtk-list-box-new}
      @about-function{gtk-list-box-prepend}
      @about-function{gtk-list-box-insert}
      @about-function{gtk-list-box-select-row}
      @about-function{gtk-list-box-unselect-row}
      @about-function{gtk-list-box-select-all}
      @about-function{gtk-list-box-unselect-all}
      @about-function{gtk-list-box-get-selected-row}
      @about-function{gtk-list-box-selected-foreach}
      @about-function{gtk-list-box-get-selected-rows}
      @about-function{gtk-list-box-get-adjustment}
      @about-function{gtk-list-box-set-adjustment}
      @about-function{gtk-list-box-set-placeholder}
      @about-function{gtk-list-box-get-row-at-index}
      @about-function{gtk-list-box-get-row-at-y}
      @about-function{gtk-list-box-invalidate-filter}
      @about-function{gtk-list-box-invalidate-headers}
      @about-function{gtk-list-box-invalidate-sort}
      @about-function{gtk-list-box-set-filter-func}
      @about-function{gtk-list-box-set-header-func}
      @about-function{gtk-list-box-set-sort-func}
      @about-function{gtk-list-box-drag-highlight-row}
      @about-function{gtk-list-box-drag-unhighlight-row}
      @about-function{gtk-list-box-bind-model}

      @about-class{gtk-list-box-row}

      @about-function{gtk-list-box-row-new}
      @about-function{gtk-list-box-row-changed}
      @about-function{gtk-list-box-row-is-selected}
      @about-function{gtk-list-box-row-get-header}
      @about-function{gtk-list-box-row-set-header}
      @about-function{gtk-list-box-row-get-index}
    @end{subsection}
    @begin[GtkFlowBox]{subsection}
      A container that allows reflowing its children.

      @about-class{gtk-flow-box}

      @about-generic{gtk-flow-box-activate-on-single-click}
      @about-generic{gtk-flow-box-column-spacing}
      @about-generic{gtk-flow-box-homogeneous}
      @about-generic{gtk-flow-box-max-children-per-line}
      @about-generic{gtk-flow-box-min-children-per-line}
      @about-generic{gtk-flow-box-row-spacing}
      @about-generic{gtk-flow-box-selection-mode}

      @about-function{gtk-flow-box-new}
      @about-function{gtk-flow-box-insert}
      @about-function{gtk-flow-box-get-child-at-index}
      @about-function{gtk-flow-box-get-child-at-pos}
      @about-function{gtk-flow-box-set-hadjustment}
      @about-function{gtk-flow-box-set-vadjustment}
      @about-function{gtk-flow-box-selected-foreach}
      @about-function{gtk-flow-box-get-selected-children}
      @about-function{gtk-flow-box-select-child}
      @about-function{gtk-flow-box-unselect-child}
      @about-function{gtk-flow-box-select-all}
      @about-function{gtk-flow-box-unselect-all}
      @about-function{gtk-flow-box-set-filter-func}
      @about-function{gtk-flow-box-invalidate-filter}
      @about-function{gtk-flow-box-set-sort-func}
      @about-function{gtk-flow-box-invalidate-sort}
      @about-function{gtk-flow-box-bind-model}

      @about-class{gtk-flow-box-child}

      @about-function{gtk-flow-box-child-new}
      @about-function{gtk-flow-box-child-get-index}
      @about-function{gtk-flow-box-child-is-selected}
      @about-function{gtk-flow-box-child-changed}
    @end{subsection}
    @begin[GtkStack]{subsection}
      A stacking container.

      @about-symbol{gtk-stack-transition-type}

      @about-class{gtk-stack}

      @about-generic{gtk-stack-visible-child}
      @about-generic{gtk-stack-visible-child-name}
      @about-generic{gtk-stack-homogeneous}
      @about-generic{gtk-stack-hhomogeneous}
      @about-generic{gtk-stack-vhomogeneous}
      @about-generic{gtk-stack-transition-duration}
      @about-generic{gtk-stack-transition-type}
      @about-generic{gtk-stack-transition-running}
      @about-generic{gtk-stack-interpolate-size}

      @about-function{gtk-stack-child-icon-name}
      @about-function{gtk-stack-child-name}
      @about-function{gtk-stack-child-needs-attention}
      @about-function{gtk-stack-child-position}
      @about-function{gtk-stack-child-title}

      @about-function{gtk-stack-new}
      @about-function{gtk-stack-add-named}
      @about-function{gtk-stack-add-titled}
      @about-function{gtk-stack-get-child-by-name}
      @about-function{gtk-stack-set-visible-child-full}
    @end{subsection}
    @begin[GtkStackSwitcher]{subsection}
      A controller for GtkStack.

      @about-class{gtk-stack-switcher}

      @about-generic{gtk-stack-switcher-stack}
      @about-generic{gtk-stack-switcher-icon-size}

      @about-function{gtk-stack-switcher-new}
    @end{subsection}
    @begin[GtkStackSidebar]{subsection}
      An automatic sidebar widget

      @about-class{gtk-stack-sidebar}

      @about-generic{gtk-stack-sidebar-stack}

      @about-function{gtk-stack-sidebar-new}
    @end{subsection}
    @begin[GtkActionBar]{subsection}
      A full width bar for presenting contextual actions

      @about-class{gtk-action-bar}

      @about-function{gtk-action-bar-child-pack-type}
      @about-function{gtk-action-bar-child-position}

      @about-function{gtk-action-bar-new}
      @about-function{gtk-action-bar-pack-start}
      @about-function{gtk-action-bar-pack-end}
      @about-function{gtk-action-bar-get-center-widget}
      @about-function{gtk-action-bar-set-center-widget}
    @end{subsection}
    @begin[GtkHeaderBar]{subsection}
      A box with a centered child.

      @about-class{gtk-header-bar}

      @about-generic{gtk-header-bar-custom-title}
      @about-generic{gtk-header-bar-decoration-layout}
      @about-generic{gtk-header-bar-decoration-layout-set}
      @about-generic{gtk-header-bar-has-subtitle}
      @about-generic{gtk-header-bar-show-close-button}
      @about-generic{gtk-header-bar-spacing}
      @about-generic{gtk-header-bar-subtitle}
      @about-generic{gtk-header-bar-title}

      @about-function{gtk-header-bar-child-pack-type}
      @about-function{gtk-header-bar-child-position}

      @about-function{gtk-header-bar-new}
      @about-function{gtk-header-bar-pack-start}
      @about-function{gtk-header-bar-pack-end}
    @end{subsection}
    @begin[GtkOverlay]{subsection}
      A container which overlays widgets on top of each other.

      @about-class{gtk-overlay}

      @about-function{gtk-overlay-child-index}
      @about-function{gtk-overlay-child-pass-through}

      @about-function{gtk-overlay-new}
      @about-function{gtk-overlay-add-overlay}
      @about-function{gtk-overlay-reorder-overlay}
      @about-function{gtk-overlay-get-overlay-pass-through}
      @about-function{gtk-overlay-set-overlay-pass-through}
    @end{subsection}
    @begin[GtkButtonBox]{subsection}
      A container for arranging buttons

      @about-symbol{gtk-button-box-style}

      @about-class{gtk-button-box}

      @about-generic{gtk-button-box-layout-style}

      @about-function{gtk-button-box-child-non-homogeneous}
      @about-function{gtk-button-box-child-secondary}

      @about-function{gtk-button-box-new}
      @about-function{gtk-button-box-get-layout}
      @about-function{gtk-button-box-get-child-secondary}
      @about-function{gtk-button-box-get-child-non-homogeneous}
      @about-function{gtk-button-box-set-layout}
      @about-function{gtk-button-box-set-child-secondary}
      @about-function{gtk-button-box-set-child-non-homogeneous}
    @end{subsection}
    @begin[GtkPaned]{subsection}
      A widget with two adjustable panes.

      @about-class{gtk-paned}

      @about-generic{gtk-paned-max-position}
      @about-generic{gtk-paned-min-position}
      @about-generic{gtk-paned-position}
      @about-generic{gtk-paned-position-set}
      @about-generic{gtk-paned-wide-handle}

      @about-function{gtk-paned-child-resize}
      @about-function{gtk-paned-child-shrink}

      @about-function{gtk-paned-new}
      @about-function{gtk-paned-add1}
      @about-function{gtk-paned-add2}
      @about-function{gtk-paned-pack1}
      @about-function{gtk-paned-pack2}
      @about-function{gtk-paned-get-child1}
      @about-function{gtk-paned-get-child2}
      @about-function{gtk-paned-get-handle-window}
    @end{subsection}
    @begin[GtkLayout]{subsection}
      Infinite scrollable area containing child widgets and/or custom drawing.

      @about-class{gtk-layout}

      @about-generic{gtk-layout-height}
      @about-generic{gtk-layout-width}

      @about-function{gtk-layout-child-x}
      @about-function{gtk-layout-child-y}

      @about-function{gtk-layout-new}
      @about-function{gtk-layout-put}
      @about-function{gtk-layout-move}
      @about-function{gtk-layout-set-size}
      @about-function{gtk-layout-get-size}
      @about-function{gtk-layout-get-hadjustment}
      @about-function{gtk-layout-get-vadjustment}
      @about-function{gtk-layout-set-hadjustment}
      @about-function{gtk-layout-set-vadjustment}
      @about-function{gtk-layout-get-bin-window}
    @end{subsection}
    @begin[GtkNotebook]{subsection}
      A tabbed notebook container.

      @about-class{gtk-notebook}

      @about-generic{gtk-notebook-enable-popup}
      @about-generic{gtk-notebook-group-name}
      @about-generic{gtk-notebook-page}
      @about-generic{gtk-notebook-scrollable}
      @about-generic{gtk-notebook-show-border}
      @about-generic{gtk-notebook-show-tabs}
      @about-generic{gtk-notebook-tab-pos}

      @about-function{gtk-notebook-child-detachable}
      @about-function{gtk-notebook-child-menu-label}
      @about-function{gtk-notebook-child-position}
      @about-function{gtk-notebook-child-reorderable}
      @about-function{gtk-notebook-child-tab-expand}
      @about-function{gtk-notebook-child-tab-fill}
      @about-function{gtk-notebook-child-tab-label}

      @about-function{gtk-notebook-new}
      @about-function{gtk-notebook-append-page}
      @about-function{gtk-notebook-append-page-menu}
      @about-function{gtk-notebook-prepend-page}
      @about-function{gtk-notebook-prepend-page-menu}
      @about-function{gtk-notebook-insert-page}
      @about-function{gtk-notebook-insert-page-menu}
      @about-function{gtk-notebook-remove-page}
      @about-function{gtk-notebook-detach-tab}
      @about-function{gtk-notebook-page-num}
      @about-function{gtk-notebook-next-page}
      @about-function{gtk-notebook-prev-page}
      @about-function{gtk-notebook-reorder-child}
      @about-function{gtk-notebook-set-tab-pos}
      @about-function{gtk-notebook-set-show-tabs}
      @about-function{gtk-notebook-set-show-border}
      @about-function{gtk-notebook-set-scrollable}
      @about-function{gtk-notebook-popup-enable}
      @about-function{gtk-notebook-popup-disable}
      @about-function{gtk-notebook-get-current-page}
      @about-function{gtk-notebook-get-menu-label}
      @about-function{gtk-notebook-get-nth-page}
      @about-function{gtk-notebook-get-n-pages}
      @about-function{gtk-notebook-get-tab-label}
      @about-function{gtk-notebook-set-menu-label}
      @about-function{gtk-notebook-set-menu-label-text}
      @about-function{gtk-notebook-set-tab-label}
      @about-function{gtk-notebook-set-tab-label-text}
      @about-function{gtk-notebook-set-tab-reorderable}
      @about-function{gtk-notebook-set-tab-detachable}
      @about-function{gtk-notebook-get-menu-label-text}
      @about-function{gtk-notebook-get-scrollable}
      @about-function{gtk-notebook-get-show-border}
      @about-function{gtk-notebook-get-show-tabs}
      @about-function{gtk-notebook-get-tab-label-text}
      @about-function{gtk-notebook-get-tab-pos}
      @about-function{gtk-notebook-get-tab-reorderable}
      @about-function{gtk-notebook-get-tab-detachable}
      @about-function{gtk-notebook-get-tab-hborder}
      @about-function{gtk-notebook-get-tab-vborder}
      @about-function{gtk-notebook-set-current-page}
      @about-function{gtk-notebook-set-group-name}
      @about-function{gtk-notebook-get-group-name}
      @about-function{gtk-notebook-set-action-widget}
      @about-function{gtk-notebook-get-action-widget}
    @end{subsection}
    @begin[GtkExpander]{subsection}
      A container which can hide its child.

      @about-class{gtk-expander}

      @about-generic{gtk-expander-expanded}
      @about-generic{gtk-expander-label}
      @about-generic{gtk-expander-label-fill}
      @about-generic{gtk-expander-label-widget}
      @about-generic{gtk-expander-resize-toplevel}
      @about-generic{gtk-expander-spacing}
      @about-generic{gtk-expander-use-markup}
      @about-generic{gtk-expander-use-underline}

      @about-function{gtk-expander-new}
      @about-function{gtk-expander-new-with-mnemonic}
    @end{subsection}
    @begin[GtkOrientable]{subsection}
      An interface for flippable widgets.

      @about-class{gtk-orientable}

      @about-generic{gtk-orientable-orientation}
    @end{subsection}
    @begin[GtkAspectFrame]{subsection}
      A frame that constrains its child to a particular aspect ratio.

      @about-class{gtk-aspect-frame}

      @about-generic{gtk-aspect-frame-obey-child}
      @about-generic{gtk-aspect-frame-ratio}
      @about-generic{gtk-aspect-frame-xalign}
      @about-generic{gtk-aspect-frame-yalign}

      @about-function{gtk-aspect-frame-new}
      @about-function{gtk-aspect-frame-set}
    @end{subsection}
    @begin[GtkFixed]{subsection}
      A container which allows you to position widgets at fixed coordinates.

      @about-class{gtk-fixed}

      @about-function{gtk-fixed-child-x}
      @about-function{gtk-fixed-child-y}

      @about-function{gtk-fixed-new}
      @about-function{gtk-fixed-put}
      @about-function{gtk-fixed-move}
    @end{subsection}
  @end{section}
  @begin[Display Widgets]{section}
    @begin[GtkLabel]{subsection}
      A widget that displays a small to medium amount of text.

      @about-class{gtk-label}

      @about-generic{gtk-label-angle}
      @about-generic{gtk-label-attributes}
      @about-generic{gtk-label-cursor-position}
      @about-generic{gtk-label-ellipsize}
      @about-generic{gtk-label-justify}
      @about-generic{gtk-label-label}
      @about-generic{gtk-label-lines}
      @about-generic{gtk-label-max-width-chars}
      @about-generic{gtk-label-mnemonic-keyval}
      @about-generic{gtk-label-mnemonic-widget}
      @about-generic{gtk-label-pattern}
      @about-generic{gtk-label-selectable}
      @about-generic{gtk-label-selection-bound}
      @about-generic{gtk-label-single-line-mode}
      @about-generic{gtk-label-track-visited-links}
      @about-generic{gtk-label-use-markup}
      @about-generic{gtk-label-use-underline}
      @about-generic{gtk-label-width-chars}
      @about-generic{gtk-label-wrap}
      @about-generic{gtk-label-wrap-mode}
      @about-generic{gtk-label-xalign}
      @about-generic{gtk-label-yalign}

      @about-function{gtk-label-new}
      @about-function{gtk-label-set-text}
      @about-function{gtk-label-set-markup}
      @about-function{gtk-label-set-markup-with-mnemonic}
      @about-function{gtk-label-set-line-wrap}
      @about-function{gtk-label-set-line-wrap-mode}
      @about-function{gtk-label-get-layout-offsets}
      @about-function{gtk-label-get-text}
      @about-function{gtk-label-new-with-mnemonic}
      @about-function{gtk-label-select-region}
      @about-function{gtk-label-set-text-with-mnemonic}
      @about-function{gtk-label-get-layout}
      @about-function{gtk-label-get-line-wrap}
      @about-function{gtk-label-get-line-wrap-mode}
      @about-function{gtk-label-get-selection-bounds}
      @about-function{gtk-label-get-current-uri}
    @end{subsection}
    @begin[GtkImage]{subsection}
      A widget displaying an image.

      @about-symbol{gtk-image-type}

      @about-class{gtk-image}

      @about-generic{gtk-image-file}
      @about-generic{gtk-image-gicon}
      @about-generic{gtk-image-icon-name}
      @about-generic{gtk-image-icon-set}
      @about-generic{gtk-image-icon-size}
      @about-generic{gtk-image-pixbuf}
      @about-generic{gtk-image-pixbuf-animation}
      @about-generic{gtk-image-pixel-size}
      @about-generic{gtk-image-resource}
      @about-generic{gtk-image-stock}
      @about-generic{gtk-image-storage-type}
      @about-generic{gtk-image-surface}
      @about-generic{gtk-image-use-fallback}

      @about-function{gtk-image-get-icon-set}
      @about-function{gtk-image-get-stock}
      @about-function{gtk-image-get-animation}
      @about-function{gtk-image-get-icon-name}
      @about-function{gtk-image-get-gicon}
      @about-function{gtk-image-new-from-file}
      @about-function{gtk-image-new-from-icon-set}
      @about-function{gtk-image-new-from-pixbuf}
      @about-function{gtk-image-new-from-stock}
      @about-function{gtk-image-new-from-animation}
      @about-function{gtk-image-new-from-icon-name}
      @about-function{gtk-image-new-from-gicon}
      @about-function{gtk-image-new-from-resource}
      @about-function{gtk-image-new-from-surface}
      @about-function{gtk-image-set-from-file}
      @about-function{gtk-image-set-from-icon-set}
      @about-function{gtk-image-set-from-pixbuf}
      @about-function{gtk-image-set-from-stock}
      @about-function{gtk-image-set-from-animation}
      @about-function{gtk-image-set-from-icon-name}
      @about-function{gtk-image-set-from-gicon}
      @about-function{gtk-image-set-from-resource}
      @about-function{gtk-image-set-from-surface}
      @about-function{gtk-image-clear}
      @about-function{gtk-image-new}
    @end{subsection}
    @begin[GtkSpinner]{subsection}
      Show a spinner animation.

      @about-class{gtk-spinner}

      @about-function{gtk-spinner-new}
      @about-function{gtk-spinner-start}
      @about-function{gtk-spinner-stop}
    @end{subsection}
    @begin[GtkInfoBar]{subsection}
      Report important messages to the user.

      @about-class{gtk-info-bar}

      @about-generic{gtk-info-bar-message-type}
      @about-generic{gtk-info-bar-revealed}
      @about-generic{gtk-info-bar-show-close-button}

      @about-function{gtk-info-bar-new}
      @about-function{gtk-info-bar-new-with-buttons}
      @about-function{gtk-info-bar-add-action-widget}
      @about-function{gtk-info-bar-add-button}
      @about-function{gtk-info-bar-add-buttons}
      @about-function{gtk-info-bar-set-response-sensitive}
      @about-function{gtk-info-bar-set-default-response}
      @about-function{gtk-info-bar-response}
      @about-function{gtk-info-bar-get-action-area}
      @about-function{gtk-info-bar-get-content-area}
    @end{subsection}
    @begin[GtkProgressBar]{subsection}
      A widget which indicates progress visually.

      @about-class{gtk-progress-bar}

      @about-generic{gtk-progress-bar-fraction}
      @about-generic{gtk-progress-bar-inverted}
      @about-generic{gtk-progress-bar-show-text}
      @about-generic{gtk-progress-bar-text}
      @about-generic{gtk-progress-bar-ellipsize}
      @about-generic{gtk-progress-bar-pulse-step}

      @about-function{gtk-progress-bar-new}
      @about-function{gtk-progress-bar-pulse}
    @end{subsection}
    @begin[GtkLevelBar]{subsection}
      A bar that can be used as a level indicator.

      @about-symbol{gtk-level-bar-mode}
      @about-symbol{GTK_LEVEL_BAR_OFFSET_LOW}
      @about-symbol{GTK_LEVEL_BAR_OFFSET_HIGH}
      @about-symbol{GTK_LEVEL_BAR_OFFSET_FULL}

      @about-class{gtk-level-bar}

      @about-generic{gtk-level-bar-inverted}
      @about-generic{gtk-level-bar-max-value}
      @about-generic{gtk-level-bar-min-value}
      @about-generic{gtk-level-bar-mode}
      @about-generic{gtk-level-bar-value}

      @about-function{gtk-level-bar-new}
      @about-function{gtk-level-bar-new-for-interval}
      @about-function{gtk-level-bar-add-offset-value}
      @about-function{gtk-level-bar-remove-offset-value}
      @about-function{gtk-level-bar-get-offset-value}
    @end{subsection}
    @begin[GtkStatusbar]{subsection}
      Report messages of minor importance to the user.

      @about-class{gtk-statusbar}

      @about-function{gtk-statusbar-new}
      @about-function{gtk-statusbar-get-context-id}
      @about-function{gtk-statusbar-push}
      @about-function{gtk-statusbar-pop}
      @about-function{gtk-statusbar-remove}
      @about-function{gtk-statusbar-remove-all}
      @about-function{gtk-statusbar-get-message-area}
    @end{subsection}
    @begin[GtkAccelLabel]{subsection}
      A label which displays an accelerator key on the right of the text.

      @about-class{gtk-accel-label}

      @about-generic{gtk-accel-label-accel-closure}
      @about-generic{gtk-accel-label-accel-widget}

      @about-function{gtk-accel-label-new}
      @about-function{gtk-accel-label-get-accel-width}
      @about-function{gtk-accel-label-set-accel}
      @about-function{gtk-accel-label-get-accel}
      @about-function{gtk-accel-label-refetch}
    @end{subsection}
  @end{section}
  @begin[Buttons and Toggles]{section}
    @begin[GtkButton]{subsection}
      A widget that emits a signal when clicked on.

      @about-class{gtk-button}

      @about-generic{gtk-button-always-show-image}
      @about-generic{gtk-button-image}
      @about-generic{gtk-button-image-position}
      @about-generic{gtk-button-label}
      @about-generic{gtk-button-relief}
      @about-generic{gtk-button-use-stock}
      @about-generic{gtk-button-use-underline}
      @about-generic{gtk-button-xalign}
      @about-generic{gtk-button-yalign}

      @about-function{gtk-button-new}
      @about-function{gtk-button-new-with-label}
      @about-function{gtk-button-new-with-mnemonic}
      @about-function{gtk-button-new-from-icon-name}
      @about-function{gtk-button-new-from-stock}
      @about-function{gtk-button-pressed}
      @about-function{gtk-button-released}
      @about-function{gtk-button-clicked}
      @about-function{gtk-button-enter}
      @about-function{gtk-button-leave}
      @about-function{gtk-button-set-focus-on-click}
      @about-function{gtk-button-get-focus-on-click}
      @about-function{gtk-button-set-alignment}
      @about-function{gtk-button-get-alignment}
      @about-function{gtk-button-get-event-window}
    @end{subsection}
    @begin[GtkCheckButton]{subsection}
      Create widgets with a discrete toggle button.

      @about-class{gtk-check-button}

      @about-function{gtk-check-button-new}
      @about-function{gtk-check-button-new-with-label}
      @about-function{gtk-check-button-new-with-mnemonic}
    @end{subsection}
    @begin[GtkRadioButton]{subsection}
      A choice from multiple check buttons.

      @about-class{gtk-radio-button}

      @about-generic{gtk-radio-button-group}

      @about-function{gtk-radio-button-new}
      @about-function{gtk-radio-button-new-from-widget}
      @about-function{gtk-radio-button-new-with-label}
      @about-function{gtk-radio-button-new-with-label-from-widget}
      @about-function{gtk-radio-button-new-with-mnemonic}
      @about-function{gtk-radio-button-new-with-mnemonic-from-widget}
      @about-function{gtk-radio-button-set-group}
      @about-function{gtk-radio-button-get-group}
      @about-function{gtk-radio-button-join-group}
    @end{subsection}
    @begin[GtkToggleButton]{subsection}
      Create buttons which retain their state.

      @about-class{gtk-toggle-button}

      @about-generic{gtk-toggle-button-active}
      @about-generic{gtk-toggle-button-draw-indicator}
      @about-generic{gtk-toggle-button-inconsistent}

      @about-function{gtk-toggle-button-new}
      @about-function{gtk-toggle-button-new-with-label}
      @about-function{gtk-toggle-button-new-with-mnemonic}
      @about-function{gtk-toggle-button-set-mode}
      @about-function{gtk-toggle-button-get-mode}
      @about-function{gtk-toggle-button-toggled}
    @end{subsection}
    @begin[GtkLinkButton]{subsection}
      Create buttons bound to a URL.

      @about-class{gtk-link-button}

      @about-generic{gtk-link-button-uri}
      @about-generic{gtk-link-button-visited}

      @about-function{gtk-link-button-new}
      @about-function{gtk-link-button-new-with-label}
    @end{subsection}
    @begin[GtkMenuButton]{subsection}
      A widget that shows a popup when clicked on.

      @about-symbol{gtk-arrow-type}

      @about-class{gtk-menu-button}

      @about-generic{gtk-menu-button-align-widget}
      @about-generic{gtk-menu-button-direction}
      @about-generic{gtk-menu-button-menu-model}
      @about-generic{gtk-menu-button-popover}
      @about-generic{gtk-menu-button-popup}
      @about-generic{gtk-menu-button-use-popover}

      @about-function{gtk-menu-button-new}
    @end{subsection}
    @begin[GtkSwitch]{subsection}
      A \"light switch\" style toggle.

      @about-class{gtk-switch}

      @about-generic{gtk-switch-active}
      @about-generic{gtk-switch-state}

      @about-function{gtk-switch-new}
    @end{subsection}
    @begin[GtkScaleButton]{subsection}
      A button which pops up a scale.

      @about-class{gtk-scale-button}

      @about-generic{gtk-scale-button-adjustment}
      @about-generic{gtk-scale-button-icons}
      @about-generic{gtk-scale-button-size}
      @about-generic{gtk-scale-button-value}

      @about-function{gtk-scale-button-new}
      @about-function{gtk-scale-button-get-popup}
      @about-function{gtk-scale-button-get-plus-button}
      @about-function{gtk-scale-button-get-minus-button}
    @end{subsection}
    @begin[GtkVolumeButton]{subsection}use-symbolic
      A button which pops up a volume control.

      @about-class{gtk-volume-button}

      @about-generic{gtk-volume-use-symbolic}

      @about-function{gtk-volume-button-new}
    @end{subsection}
    @begin[GtkLockButton]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkModelButton]{subsection}
      A button that uses a GAction as model.

      @about-symbol{gtk-button-role}

      @about-class{gtk-model-button}

      @about-generic{gtk-model-button-active}
      @about-generic{gtk-model-button-centered}
      @about-generic{gtk-model-button-icon}
      @about-generic{gtk-model-button-iconic}
      @about-generic{gtk-model-button-inverted}
      @about-generic{gtk-model-button-menu-name}
      @about-generic{gtk-model-button-role}
      @about-generic{gtk-model-button-text}
      @about-generic{gtk-model-button-use-markup}

      @about-function{gtk-model-button-new}
    @end{subsection}
  @end{section}
  @begin[Numeric and Text Data Entry]{section}
    @begin[GtkEntry]{subsection}
      A single line text entry field.

      @about-symbol{gtk-entry-icon-position}
      @about-symbol{gtk-input-purpose}
      @about-symbol{gtk-input-hints}
      @about-class{gtk-entry}
      @about-generic{gtk-entry-activates-default}
      @about-generic{gtk-entry-attributes}
      @about-generic{gtk-entry-buffer}
      @about-generic{gtk-entry-caps-lock-warning}
      @about-generic{gtk-entry-completion}
      @about-generic{gtk-entry-cursor-position}
      @about-generic{gtk-entry-editable}
      @about-generic{gtk-entry-enable-emoji-completion}
      @about-generic{gtk-entry-has-frame}
      @about-generic{gtk-entry-im-module}
      @about-generic{gtk-entry-inner-border}
      @about-generic{gtk-entry-input-hints}
      @about-generic{gtk-entry-input-purpose}
      @about-generic{gtk-entry-invisible-char}
      @about-generic{gtk-entry-invisible-char-set}
      @about-generic{gtk-entry-max-length}
      @about-generic{gtk-entry-max-width-chars}
      @about-generic{gtk-entry-overwrite-mode}
      @about-generic{gtk-entry-placeholder-text}
      @about-generic{gtk-entry-populate-all}
      @about-generic{gtk-entry-primary-icon-activatable}
      @about-generic{gtk-entry-primary-icon-gicon}
      @about-generic{gtk-entry-primary-icon-name}
      @about-generic{gtk-entry-primary-icon-pixbuf}
      @about-generic{gtk-entry-primary-icon-sensitive}
      @about-generic{gtk-entry-primary-icon-stock}
      @about-generic{gtk-entry-primary-icon-storage-type}
      @about-generic{gtk-entry-primary-icon-tooltip-markup}
      @about-generic{gtk-entry-primary-icon-tooltip-text}
      @about-generic{gtk-entry-progress-fraction}
      @about-generic{gtk-entry-progress-pulse-step}
      @about-generic{gtk-entry-scroll-offset}
      @about-generic{gtk-entry-secondary-icon-activatable}
      @about-generic{gtk-entry-secondary-icon-gicon}
      @about-generic{gtk-entry-secondary-icon-name}
      @about-generic{gtk-entry-secondary-icon-pixbuf}
      @about-generic{gtk-entry-secondary-icon-sensitive}
      @about-generic{gtk-entry-secondary-icon-stock}
      @about-generic{gtk-entry-secondary-icon-storage-type}
      @about-generic{gtk-entry-secondary-icon-tooltip-markup}
      @about-generic{gtk-entry-secondary-icon-tooltip-text}
      @about-generic{gtk-entry-selection-bound}
      @about-generic{gtk-entry-shadow-type}
      @about-generic{gtk-entry-show-emoji-icon}
      @about-generic{gtk-entry-tabs}
      @about-generic{gtk-entry-text}
      @about-generic{gtk-entry-text-length}
      @about-generic{gtk-entry-truncate-multiline}
      @about-generic{gtk-entry-visibility}
      @about-generic{gtk-entry-width-chars}
      @about-generic{gtk-entry-xalign}
      @about-function{gtk-entry-new}
      @about-function{gtk-entry-new-with-buffer}
      @about-function{gtk-entry-get-text-area}
      @about-function{gtk-entry-unset-invisible-char}
      @about-function{gtk-entry-set-alignment}
      @about-function{gtk-entry-get-alignment}
      @about-function{gtk-entry-get-layout}
      @about-function{gtk-entry-get-layout-offsets}
      @about-function{gtk-entry-layout-index-to-text-index}
      @about-function{gtk-entry-text-index-to-layout-index}
      @about-function{gtk-entry-set-cursor-hadjustment}
      @about-function{gtk-entry-get-cursor-hadjustment}
      @about-function{gtk-entry-progress-pulse}
      @about-function{gtk-entry-im-context-filter-keypress}
      @about-function{gtk-entry-reset-im-context}
      @about-function{gtk-entry-set-icon-from-pixbuf}
      @about-function{gtk-entry-set-icon-from-stock}
      @about-function{gtk-entry-set-icon-from-icon-name}
      @about-function{gtk-entry-set-icon-from-gicon}
      @about-function{gtk-entry-get-icon-storage-type}
      @about-function{gtk-entry-get-icon-pixbuf}
      @about-function{gtk-entry-get-icon-stock}
      @about-function{gtk-entry-get-icon-name}
      @about-function{gtk-entry-get-icon-gicon}
      @about-function{gtk-entry-set-icon-activatable}
      @about-function{gtk-entry-get-icon-activatable}
      @about-function{gtk-entry-set-icon-sensitive}
      @about-function{gtk-entry-get-icon-sensitive}
      @about-function{gtk-entry-get-icon-at-pos}
      @about-function{gtk-entry-set-icon-tooltip-text}
      @about-function{gtk-entry-get-icon-tooltip-text}
      @about-function{gtk-entry-set-icon-tooltip-markup}
      @about-function{gtk-entry-get-icon-tooltip-markup}
      @about-function{gtk-entry-set-icon-drag-source}
      @about-function{gtk-entry-get-current-icon-drag-source}
      @about-function{gtk-entry-get-icon-area}
      @about-function{gtk-entry-grab-focus-without-selecting}
    @end{subsection}
    @begin[GtkEntryBuffer]{subsection}
      Text buffer for @class{gtk-entry}.

      @about-class{gtk-entry-buffer}

      @about-generic{gtk-entry-buffer-length}
      @about-generic{gtk-entry-buffer-max-length}
      @about-generic{gtk-entry-buffer-text}

      @about-function{gtk-entry-buffer-new}
      @about-function{gtk-entry-buffer-get-bytes}
      @about-function{gtk-entry-buffer-insert-text}
      @about-function{gtk-entry-buffer-delete-text}
      @about-function{gtk-entry-buffer-emit-deleted-text}
      @about-function{gtk-entry-buffer-emit-inserted-text}
    @end{subsection}
    @begin[GtkEntryCompletion]{subsection}
      Completion functionality for GtkEntry.

      @about-class{gtk-entry-completion}

      @about-generic{gtk-entry-completion-cell-area}
      @about-generic{gtk-entry-completion-inline-completion}
      @about-generic{gtk-entry-completion-inline-selection}
      @about-generic{gtk-entry-completion-minimum-key-length}
      @about-generic{gtk-entry-completion-model}
      @about-generic{gtk-entry-completion-popup-completion}
      @about-generic{gtk-entry-completion-popup-set-width}
      @about-generic{gtk-entry-completion-popup-single-match}
      @about-generic{gtk-entry-completion-text-column}

      @about-function{gtk-entry-completion-new}
      @about-function{gtk-entry-completion-new-with-area}
      @about-function{gtk-entry-completion-get-entry}
      @about-function{gtk-entry-completion-set-match-func}
      @about-function{gtk-entry-completion-compute-prefix}
      @about-function{gtk-entry-completion-complete}
      @about-function{gtk-entry-completion-get-completion-prefix}
      @about-function{gtk-entry-completion-insert-prefix}
      @about-function{gtk-entry-completion-insert-action-text}
      @about-function{gtk-entry-completion-insert-action-markup}
      @about-function{gtk-entry-completion-delete-action}
    @end{subsection}
    @begin[GtkScale]{subsection}
      A slider widget for selecting a value from a range.

      @about-class{gtk-scale}

      @about-generic{gtk-scale-digits}
      @about-generic{gtk-scale-draw-value}
      @about-generic{gtk-scale-has-origin}
      @about-generic{gtk-scale-value-pos}

      @about-function{gtk-scale-new}
      @about-function{gtk-scale-new-with-range}
      @about-function{gtk-scale-get-layout}
      @about-function{gtk-scale-get-layout-offsets}
      @about-function{gtk-scale-add-mark}
      @about-function{gtk-scale-clear-marks}
    @end{subsection}
    @begin[GtkSpinButton]{subsection}
      Retrieve an integer or floating-point number from the user.

      @about-symbol{gtk-spin-button-update-policy}
      @about-symbol{gtk-spin-type}

      @about-class{gtk-spin-button}

      @about-generic{gtk-spin-button-adjustment}
      @about-generic{gtk-spin-button-climb-rate}
      @about-generic{gtk-spin-button-digits}
      @about-generic{gtk-spin-button-numeric}
      @about-generic{gtk-spin-button-snap-to-ticks}
      @about-generic{gtk-spin-button-update-policy}
      @about-generic{gtk-spin-button-value}
      @about-generic{gtk-spin-button-wrap}

      @about-function{gtk-spin-button-configure}
      @about-function{gtk-spin-button-new}
      @about-function{gtk-spin-button-new-with-range}
      @about-function{gtk-spin-button-set-increments}
      @about-function{gtk-spin-button-set-range}
      @about-function{gtk-spin-button-get-value-as-int}
      @about-function{gtk-spin-button-spin}
      @about-function{gtk-spin-button-update}
      @about-function{gtk-spin-button-get-increments}
      @about-function{gtk-spin-button-get-range}
    @end{subsection}
    @begin[GtkSearchEntry]{subsection}
      An entry which shows a search icon.

      @about-class{gtk-search-entry}

      @about-function{gtk-search-entry-new}
      @about-function{gtk-search-entry-handle-event}
    @end{subsection}
    @begin[GtkSearchBar]{subsection}
      A toolbar to integrate a search entry with.

      @about-class{gtk-search-bar}

      @about-generic{gtk-search-bar-search-mode-enabled}
      @about-generic{gtk-search-bar-show-close-button}

      @about-function{gtk-search-bar-new}
      @about-function{gtk-search-bar-connect-entry}
      @about-function{gtk-search-bar-get-search-mode}
      @about-function{gtk-search-bar-set-search-mode}
      @about-function{gtk-search-bar-handle-event}
    @end{subsection}
    @begin[GtkEditable]{subsection}
      Interface for text-editing widgets.

      @about-class{gtk-editable}

      @about-function{gtk-editable-select-region}
      @about-function{gtk-editable-get-selection-bounds}
      @about-function{gtk-editable-insert-text}
      @about-function{gtk-editable-delete-text}
      @about-function{gtk-editable-get-chars}
      @about-function{gtk-editable-cut-clipboard}
      @about-function{gtk-editable-copy-clipboard}
      @about-function{gtk-editable-paste-clipboard}
      @about-function{gtk-editable-delete-selection}
      @about-function{gtk-editable-set-position}
      @about-function{gtk-editable-get-position}
      @about-function{gtk-editable-set-editable}
      @about-function{gtk-editable-get-editable}
    @end{subsection}
  @end{section}
  @begin[Multiline Text Editor]{section}
    Overview of @class{gtk-text-buffer}, @class{gtk-text-view}, and friends.

    @subheading{Conceptual Overview}
      GTK+ has an extremely powerful framework for multiline text editing. The
      primary objects involved in the process are @class{gtk-text-buffer}, which
      represents the text being edited, and @class{gtk-text-view}, a widget
      which can display a @class{gtk-text-buffer}. Each buffer can be displayed
      by any number of views.

      One of the important things to remember about text in GTK+ is that it is
      in the UTF-8 encoding. This means that one character can be encoded as
      multiple bytes. Character counts are usually referred to as offsets, while
      byte counts are called indexes. If you confuse these two, things will work
      fine with ASCII, but as soon as your buffer contains multibyte characters,
      bad things will happen.

      Text in a buffer can be marked with tags. A tag is an attribute that can
      be applied to some range of text. For example, a tag might be called
      \"bold\" and make the text inside the tag bold. However, the tag concept
      is more general than that; tags do not have to affect appearance. They can
      instead affect the behavior of mouse and key presses, \"lock\" a range of
      text so the user cannot edit it, or countless other things. A tag is
      represented by a @class{gtk-text-tag} object. One @class{gtk-text-tag} can
      be applied to any number of text ranges in any number of buffers.

      Each tag is stored in a @class{gtk-text-tag-table}. A tag table defines a
      set of tags that can be used together. Each buffer has one tag table
      associated with it; only tags from that tag table can be used with the
      buffer. A single tag table can be shared between multiple buffers,
      however.

      Tags can have names, which is convenient sometimes (for example, you can
      name your tag that makes things bold \"bold\"), but they can also be
      anonymous (which is convenient if you are creating tags on-the-fly).

      Most text manipulation is accomplished with iterators, represented by a
      @class{gtk-text-iter}. An iterator represents a position between two
      characters in the text buffer. @class{gtk-text-iter} is a structure
      designed to be allocated on the stack; it is guaranteed to be copiable
      by value and never contain any heap-allocated data. Iterators are not
      valid indefinitely; whenever the buffer is modified in a way that affects
      the number of characters in the buffer, all outstanding iterators become
      invalid. (Note that deleting 5 characters and then reinserting 5 still
      invalidates iterators, though you end up with the same number of
      characters you pass through a state with a different number).

      Because of this, iterators cannot be used to preserve positions across
      buffer modifications. To preserve a position, the @class{gtk-text-mark}
      object is ideal. You can think of a mark as an invisible cursor or
      insertion point; it floats in the buffer, saving a position. If the text
      surrounding the mark is deleted, the mark remains in the position the text
      once occupied; if text is inserted at the mark, the mark ends up either to
      the left or to the right of the new text, depending on its gravity. The
      standard text cursor in left-to-right languages is a mark with right
      gravity, because it stays to the right of inserted text.

      Like tags, marks can be either named or anonymous. There are two marks
      built-in to @class{gtk-text-buffer}; these are named \"insert\" and
      \"selection_bound\" and refer to the insertion point and the boundary of
      the selection which is not the insertion point, respectively. If no text
      is selected, these two marks will be in the same position. You can
      manipulate what is selected and where the cursor appears by moving these
      marks around. [2]

      Text buffers always contain at least one line, but may be empty (that is,
      buffers can contain zero characters). The last line in the text buffer
      never ends in a line separator (such as newline); the other lines in the
      buffer always end in a line separator. Line separators count as characters
      when computing character counts and character offsets. Note that some
      Unicode line separators are represented with multiple bytes in UTF-8, and
      the two-character sequence \"\r\n\" is also considered a line separator.

    @subheading{Simple Example}
      The simplest usage of @class{gtk-text-view} might look like this:
      @begin{pre}
GtkWidget *view;
GtkTextBuffer *buffer;

view = gtk_text_view_new ();

buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

gtk_text_buffer_set_text (buffer, \"Hello, this is some text\", -1);

/* Now you might put the view in a container and display it on the
 * screen; when the user edits the text, signals on the buffer
 * will be emitted, such as \"changed\", \"insert_text\", and so on.
 */
      @end{pre}
      In many cases it is also convenient to first create the buffer with
      @fun{gtk-text-buffer-new}, then create a widget for that buffer with
      @fun{gtk-text-view-new-with-buffer}. Or you can change the buffer the
      widget displays after the widget is created with
      @fun{gtk-text-view-buffer}.

    @subheading{Example of Changing Text Attributes}
      There are two ways to affect text attributes in @class{gtk-text-view}. You
      can change the default attributes for a given @class{gtk-text-view}, and
      you can apply tags that change the attributes for a region of text. For
      text features that come from the theme - such as font and foreground color
      - use standard @class{gtk-widget} functions such as the function
      @fun{gtk-widget-override-font}. For other attributes there are dedicated
      methods on @class{gtk-text-view} such as the function
      @fun{gtk-text-view-set-tabs}.
      @begin{pre}
GtkWidget *view;
GtkTextBuffer *buffer;
GtkTextIter start, end;
PangoFontDescription *font_desc;
GdkRGBA rgba;
GtkTextTag *tag;

view = gtk_text_view_new ();

buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

gtk_text_buffer_set_text (buffer, \"Hello, this is some text\", -1);

/* Change default font throughout the widget */
font_desc = pango_font_description_from_string (\"Serif 15\");
gtk_widget_modify_font (view, font_desc);
pango_font_description_free (font_desc);

/* Change default color throughout the widget */
gdk_rgba_parse (\"green\", &rgba);
gtk_widget_override_color (view, GTK_STATE_FLAG_NORMAL, &rgba);

/* Change left margin throughout the widget */
gtk_text_view_set_left_margin (GTK_TEXT_VIEW (view), 30);

/* Use a tag to change the color for just one part of the widget */
tag = gtk_text_buffer_create_tag (buffer, \"blue_foreground\",
                        \"foreground\", \"blue\", NULL);
gtk_text_buffer_get_iter_at_offset (buffer, &start, 7);
gtk_text_buffer_get_iter_at_offset (buffer, &end, 12);
gtk_text_buffer_apply_tag (buffer, tag, &start, &end);
      @end{pre}
      The gtk-demo application that comes with GTK+ contains more example code
      for @class{gtk-text-view}.
    @begin[GtkTextIter]{subsection}
      Text buffer iterator.

      @about-class{gtk-text-iter}

      @about-function{gtk-text-iter-get-buffer}
      @about-function{gtk-text-iter-copy}
      @about-function{gtk-text-iter-assign}
      @about-function{gtk-text-iter-free}
      @about-function{gtk-text-iter-get-offset}
      @about-function{gtk-text-iter-get-line}
      @about-function{gtk-text-iter-get-line-offset}
      @about-function{gtk-text-iter-get-line-index}
      @about-function{gtk-text-iter-get-visible-line-index}
      @about-function{gtk-text-iter-get-visible-line-offset}
      @about-function{gtk-text-iter-get-char}
      @about-function{gtk-text-iter-get-slice}
      @about-function{gtk-text-iter-get-text}
      @about-function{gtk-text-iter-get-visible-slice}
      @about-function{gtk-text-iter-get-visible-text}
      @about-function{gtk-text-iter-get-pixbuf}
      @about-function{gtk-text-iter-get-marks}
      @about-function{gtk-text-iter-get-toggled-tags}
      @about-function{gtk-text-iter-get-child-anchor}
      @about-function{gtk-text-iter-begins-tag}
      @about-function{gtk-text-iter-ends-tag}
      @about-function{gtk-text-iter-toggles-tag}
      @about-function{gtk-text-iter-has-tag}
      @about-function{gtk-text-iter-get-tags}
      @about-function{gtk-text-iter-editable}
      @about-function{gtk-text-iter-can-insert}
      @about-function{gtk-text-iter-starts-word}
      @about-function{gtk-text-iter-ends-word}
      @about-function{gtk-text-iter-inside-word}
      @about-function{gtk-text-iter-starts-line}
      @about-function{gtk-text-iter-ends-line}
      @about-function{gtk-text-iter-starts-sentence}
      @about-function{gtk-text-iter-ends-sentence}
      @about-function{gtk-text-iter-inside-sentence}
      @about-function{gtk-text-iter-is-cursor-position}
      @about-function{gtk-text-iter-get-chars-in-line}
      @about-function{gtk-text-iter-get-bytes-in-line}
      @about-function{gtk-text-iter-get-attributes}
      @about-function{gtk-text-iter-get-language}
      @about-function{gtk-text-iter-is-end}
      @about-function{gtk-text-iter-is-start}
      @about-function{gtk-text-iter-forward-char}
      @about-function{gtk-text-iter-move}
      @about-function{gtk-text-iter-backward-char}
      @about-function{gtk-text-iter-forward-chars}
      @about-function{gtk-text-iter-backward-chars}
      @about-function{gtk-text-iter-forward-line}
      @about-function{gtk-text-iter-backward-line}
      @about-function{gtk-text-iter-forward-lines}
      @about-function{gtk-text-iter-backward-lines}
      @about-function{gtk-text-iter-forward-word-ends}
      @about-function{gtk-text-iter-backward-word-starts}
      @about-function{gtk-text-iter-forward-word-end}
      @about-function{gtk-text-iter-backward-word-start}
      @about-function{gtk-text-iter-forward-cursor-position}
      @about-function{gtk-text-iter-backward-cursor-position}
      @about-function{gtk-text-iter-forward-cursor-positions}
      @about-function{gtk-text-iter-backward-cursor-positions}
      @about-function{gtk-text-iter-backward-sentence-start}
      @about-function{gtk-text-iter-backward-sentence-starts}
      @about-function{gtk-text-iter-forward-sentence-end}
      @about-function{gtk-text-iter-forward-sentence-ends}
      @about-function{gtk-text-iter-forward-visible-word-ends}
      @about-function{gtk-text-iter-backward-visible-word-starts}
      @about-function{gtk-text-iter-forward-visible-word-end}
      @about-function{gtk-text-iter-backward-visible-word-start}
      @about-function{gtk-text-iter-forward-visible-cursor-position}
      @about-function{gtk-text-iter-backward-visible-cursor-position}
      @about-function{gtk-text-iter-forward-visible-cursor-positions}
      @about-function{gtk-text-iter-backward-visible-cursor-positions}
      @about-function{gtk-text-iter-forward-visible-line}
      @about-function{gtk-text-iter-backward-visible-line}
      @about-function{gtk-text-iter-forward-visible-lines}
      @about-function{gtk-text-iter-backward-visible-lines}
      @about-function{gtk-text-iter-set-offset}
      @about-function{gtk-text-iter-set-line}
      @about-function{gtk-text-iter-set-line-offset}
      @about-function{gtk-text-iter-set-line-index}
      @about-function{gtk-text-iter-set-visible-line-index}
      @about-function{gtk-text-iter-set-visible-line-offset}
      @about-function{gtk-text-iter-forward-to-end}
      @about-function{gtk-text-iter-forward-to-line-end}
      @about-function{gtk-text-iter-forward-to-tag-toggle}
      @about-function{gtk-text-iter-backward-to-tag-toggle}
      @about-function{gtk-text-iter-find-char}
      @about-function{gtk-text-iter-forward-find-char}
      @about-function{gtk-text-iter-backward-find-char}

      @about-symbol{gtk-text-search-flags}

      @about-function{gtk-text-iter-search}
      @about-function{gtk-text-iter-forward-search}
      @about-function{gtk-text-iter-backward-search}
      @about-function{gtk-text-iter-equal}
      @about-function{gtk-text-iter-compare}
      @about-function{gtk-text-iter-in-range}
      @about-function{gtk-text-iter-order}
    @end{subsection}
    @begin[GtkTextMark]{subsection}
      A position in the buffer preserved across buffer modifications.

      @about-class{gtk-text-mark}

      @about-generic{gtk-text-mark-left-gravity}
      @about-generic{gtk-text-mark-name}

      @about-function{gtk-text-mark-new}
      @about-function{gtk-text-mark-set-visible}
      @about-function{gtk-text-mark-get-visible}
      @about-function{gtk-text-mark-get-deleted}
      @about-function{gtk-text-mark-get-buffer}
    @end{subsection}
    @begin[GtkTextBuffer]{subsection}
      Stores attributed text for display in a @class{gtk-text-view} widget.

      @about-class{gtk-text-buffer}

      @about-generic{gtk-text-buffer-copy-target-list}
      @about-generic{gtk-text-buffer-cursor-position}
      @about-generic{gtk-text-buffer-has-selection}
      @about-generic{gtk-text-buffer-paste-target-list}
      @about-generic{gtk-text-buffer-tag-table}
      @about-generic{gtk-text-buffer-text}

      @about-function{gtk-text-buffer-new}
      @about-function{gtk-text-buffer-get-line-count}
      @about-function{gtk-text-buffer-get-char-count}
      @about-function{gtk-text-buffer-insert}
      @about-function{gtk-text-buffer-insert-at-cursor}
      @about-function{gtk-text-buffer-insert-interactive}
      @about-function{gtk-text-buffer-insert-interactive-at-cursor}
      @about-function{gtk-text-buffer-insert-range}
      @about-function{gtk-text-buffer-insert-range-interactive}
      @about-function{gtk-text-buffer-insert-with-tags}
      @about-function{gtk-text-buffer-insert-with-tags-by-name}
      @about-function{gtk-text-buffer-insert-markup}
      @about-function{gtk-text-buffer-delete}
      @about-function{gtk-text-buffer-delete-interactive}
      @about-function{gtk-text-buffer-backspace}
      @about-function{gtk-text-buffer-get-text}
      @about-function{gtk-text-buffer-get-slice}
      @about-function{gtk-text-buffer-insert-pixbuf}
      @about-function{gtk-text-buffer-insert-child-anchor}
      @about-function{gtk-text-buffer-create-child-anchor}
      @about-function{gtk-text-buffer-create-mark}
      @about-function{gtk-text-buffer-move-mark}
      @about-function{gtk-text-buffer-move-mark-by-name}
      @about-function{gtk-text-buffer-add-mark}
      @about-function{gtk-text-buffer-delete-mark}
      @about-function{gtk-text-buffer-delete-mark-by-name}
      @about-function{gtk-text-buffer-get-mark}
      @about-function{gtk-text-buffer-get-insert}
      @about-function{gtk-text-buffer-get-selection-bound}
      @about-function{gtk-text-buffer-get-has-selection}
      @about-function{gtk-text-buffer-place-cursor}
      @about-function{gtk-text-buffer-select-range}
      @about-function{gtk-text-buffer-apply-tag}
      @about-function{gtk-text-buffer-remove-tag}
      @about-function{gtk-text-buffer-apply-tag-by-name}
      @about-function{gtk-text-buffer-remove-tag-by-name}
      @about-function{gtk-text-buffer-remove-all-tags}
      @about-function{gtk-text-buffer-create-tag}
      @about-function{gtk-text-buffer-get-iter-at-line-offset}
      @about-function{gtk-text-buffer-get-iter-at-offset}
      @about-function{gtk-text-buffer-get-iter-at-line}
      @about-function{gtk-text-buffer-get-iter-at-line-index}
      @about-function{gtk-text-buffer-get-iter-at-mark}
      @about-function{gtk-text-buffer-get-iter-at-child-anchor}
      @about-function{gtk-text-buffer-get-start-iter}
      @about-function{gtk-text-buffer-get-end-iter}
      @about-function{gtk-text-buffer-get-bounds}
      @about-function{gtk-text-buffer-get-modified}
      @about-function{gtk-text-buffer-set-modified}
      @about-function{gtk-text-buffer-delete-selection}
      @about-function{gtk-text-buffer-paste-clipboard}
      @about-function{gtk-text-buffer-copy-clipboard}
      @about-function{gtk-text-buffer-cut-clipboard}
      @about-function{gtk-text-buffer-get-selection-bounds}
      @about-function{gtk-text-buffer-begin-user-action}
      @about-function{gtk-text-buffer-end-user-action}
      @about-function{gtk-text-buffer-add-selection-clipboard}
      @about-function{gtk-text-buffer-remove-selection-clipboard}

      @about-symbol{gtk-text-buffer-target-info}

      @about-function{gtk-text-buffer-deserialize}
      @about-function{gtk-text-buffer-deserialize-get-can-create-tags}
      @about-function{gtk-text-buffer-deserialize-set-can-create-tags}
      @about-function{gtk-text-buffer-get-copy-target-list}
      @about-function{gtk-text-buffer-get-deserialize-formats}
      @about-function{gtk-text-buffer-get-paste-target-list}
      @about-function{gtk-text-buffer-get-serialize-formats}
      @about-function{gtk-text-buffer-register-deserialize-format}
      @about-function{gtk-text-buffer-register-deserialize-tagset}
      @about-function{gtk-text-buffer-register-serialize-format}
      @about-function{gtk-text-buffer-register-serialize-tagset}
      @about-function{gtk-text-buffer-serialize}
      @about-function{gtk-text-buffer-unregister-deserialize-format}
      @about-function{gtk-text-buffer-unregister-serialize-format}
    @end{subsection}
    @begin[GtkTextTag]{subsection}
      A tag that can be applied to text in a @class{gtk-text-buffer}.

      @about-symbol{gtk-wrap-mode}
      @about-symbol{gtk-text-appearance}

      @about-class{gtk-text-tag}

      @about-generic{gtk-text-tag-accumulative-margin}
      @about-generic{gtk-text-tag-background}
      @about-generic{gtk-text-tag-background-full-height}
      @about-generic{gtk-text-tag-background-full-height-set}
      @about-generic{gtk-text-tag-background-gdk}
      @about-generic{gtk-text-tag-background-rgba}
      @about-generic{gtk-text-tag-background-set}
      @about-generic{gtk-text-tag-direction}
      @about-generic{gtk-text-tag-editable}
      @about-generic{gtk-text-tag-editable-set}
      @about-generic{gtk-text-tag-fallback}
      @about-generic{gtk-text-tag-fallback-set}
      @about-generic{gtk-text-tag-family}
      @about-generic{gtk-text-tag-family-set}
      @about-generic{gtk-text-tag-font}
      @about-generic{gtk-text-tag-font-desc}
      @about-generic{gtk-text-tag-font-features}
      @about-generic{gtk-text-tag-font-features-set}
      @about-generic{gtk-text-tag-foreground}
      @about-generic{gtk-text-tag-foreground-gdk}
      @about-generic{gtk-text-tag-foreground-rgba}
      @about-generic{gtk-text-tag-foreground-set}
      @about-generic{gtk-text-tag-indent}
      @about-generic{gtk-text-tag-indent-set}
      @about-generic{gtk-text-tag-invisible}
      @about-generic{gtk-text-tag-invisible-set}
      @about-generic{gtk-text-tag-justification}
      @about-generic{gtk-text-tag-justification-set}
      @about-generic{gtk-text-tag-language}
      @about-generic{gtk-text-tag-language-set}
      @about-generic{gtk-text-tag-left-margin}
      @about-generic{gtk-text-tag-left-margin-set}
      @about-generic{gtk-text-tag-letter-spacing}
      @about-generic{gtk-text-tag-letter-spacing-set}
      @about-generic{gtk-text-tag-name}
      @about-generic{gtk-text-tag-paragraph-background}
      @about-generic{gtk-text-tag-paragraph-background-gdk}
      @about-generic{gtk-text-tag-paragraph-background-rgba}
      @about-generic{gtk-text-tag-paragraph-background-set}
      @about-generic{gtk-text-tag-pixels-above-lines}
      @about-generic{gtk-text-tag-pixels-above-lines-set}
      @about-generic{gtk-text-tag-pixels-below-lines}
      @about-generic{gtk-text-tag-pixels-below-lines-set}
      @about-generic{gtk-text-tag-pixels-inside-wrap}
      @about-generic{gtk-text-tag-pixels-inside-wrap-set}
      @about-generic{gtk-text-tag-right-margin}
      @about-generic{gtk-text-tag-right-margin-set}
      @about-generic{gtk-text-tag-rise}
      @about-generic{gtk-text-tag-rise-set}
      @about-generic{gtk-text-tag-scale}
      @about-generic{gtk-text-tag-scale-set}
      @about-generic{gtk-text-tag-size}
      @about-generic{gtk-text-tag-size-points}
      @about-generic{gtk-text-tag-size-set}
      @about-generic{gtk-text-tag-stretch}
      @about-generic{gtk-text-tag-stretch-set}
      @about-generic{gtk-text-tag-strikethrough}
      @about-generic{gtk-text-tag-strikethrough-rgba}
      @about-generic{gtk-text-tag-strikethrough-rgba-set}
      @about-generic{gtk-text-tag-strikethrough-set}
      @about-generic{gtk-text-tag-style}
      @about-generic{gtk-text-tag-style-set}
      @about-generic{gtk-text-tag-tabs}
      @about-generic{gtk-text-tag-tabs-set}
      @about-generic{gtk-text-tag-underline}
      @about-generic{gtk-text-tag-underline-rgba}
      @about-generic{gtk-text-tag-underline-rgba-set}
      @about-generic{gtk-text-tag-underline-set}
      @about-generic{gtk-text-tag-variant}
      @about-generic{gtk-text-tag-variant-set}
      @about-generic{gtk-text-tag-weight}
      @about-generic{gtk-text-tag-weight-set}
      @about-generic{gtk-text-tag-wrap-mode}
      @about-generic{gtk-text-tag-wrap-mode-set}

      @about-function{gtk-text-tag-new}
      @about-function{gtk-text-tag-get-priority}
      @about-function{gtk-text-tag-set-priority}
      @about-function{gtk-text-tag-event}

      @about-symbol{gtk-text-attributes}

      @about-function{gtk-text-attributes-new}
      @about-function{gtk-text-attributes-copy}
      @about-function{gtk-text-attributes-copy-values}
      @about-function{gtk-text-attributes-unref}
      @about-function{gtk-text-attributes-ref}
    @end{subsection}
    @begin[GtkTextTagTable]{subsection}
      Collection of tags that can be used together.

      @about-class{gtk-text-tag-table}

      @about-function{gtk-text-tag-table-new}
      @about-function{gtk-text-tag-table-add}
      @about-function{gtk-text-tag-table-remove}
      @about-function{gtk-text-tag-table-lookup}
      @about-function{gtk-text-tag-table-foreach}
      @about-function{gtk-text-tag-table-get-size}
    @end{subsection}
    @begin[GtkTextView]{subsection}
      Widget that displays a @class{gtk-text-buffer} object.

      @about-symbol{gtk-text-window-type}
      @about-symbol{gtk-text-view-layer}
      @about-symbol{gtk-text-window-type}
      @about-symbol{gtk-text-extend-selection}

      @about-class{gtk-text-view}

      @about-generic{gtk-text-view-accepts-tab}
      @about-generic{gtk-text-view-bottom-margin}
      @about-generic{gtk-text-view-buffer}
      @about-generic{gtk-text-view-cursor-visible}
      @about-generic{gtk-text-view-editable}
      @about-generic{gtk-text-view-im-module}
      @about-generic{gtk-text-view-indent}
      @about-generic{gtk-text-view-input-hints}
      @about-generic{gtk-text-view-input-purpose}
      @about-generic{gtk-text-view-justification}
      @about-generic{gtk-text-view-left-margin}
      @about-generic{gtk-text-view-monospace}
      @about-generic{gtk-text-view-overwrite}
      @about-generic{gtk-text-view-pixels-above-lines}
      @about-generic{gtk-text-view-pixels-below-lines}
      @about-generic{gtk-text-view-pixels-inside-wrap}
      @about-generic{gtk-text-view-populate-all}
      @about-generic{gtk-text-view-right-margin}
      @about-generic{gtk-text-view-tabs}
      @about-generic{gtk-text-view-top-margin}
      @about-generic{gtk-text-view-wrap-mode}

      @about-function{gtk-text-view-new}
      @about-function{gtk-text-view-new-with-buffer}
      @about-function{gtk-text-view-set-buffer}
      @about-function{gtk-text-view-get-buffer}
      @about-function{gtk-text-view-get-hadjustment}
      @about-function{gtk-text-view-get-vadjustment}
      @about-function{gtk-text-view-scroll-to-mark}
      @about-function{gtk-text-view-scroll-to-iter}
      @about-function{gtk-text-view-scroll-mark-onscreen}
      @about-function{gtk-text-view-move-mark-onscreen}
      @about-function{gtk-text-view-place-cursor-onscreen}
      @about-function{gtk-text-view-get-visible-rect}
      @about-function{gtk-text-view-get-iter-location}
      @about-function{gtk-text-view-get-cursor-locations}
      @about-function{gtk-text-view-get-line-at-y}
      @about-function{gtk-text-view-get-line-yrange}
      @about-function{gtk-text-view-get-iter-at-location}
      @about-function{gtk-text-view-get-iter-at-position}
      @about-function{gtk-text-view-buffer-to-window-coords}
      @about-function{gtk-text-view-window-to-buffer-coords}
      @about-function{gtk-text-view-get-window}
      @about-function{gtk-text-view-get-window-type}
      @about-function{gtk-text-view-set-border-window-size}
      @about-function{gtk-text-view-get-border-window-size}
      @about-function{gtk-text-view-forward-display-line}
      @about-function{gtk-text-view-backward-display-line}
      @about-function{gtk-text-view-forward-display-line-end}
      @about-function{gtk-text-view-backward-display-line-start}
      @about-function{gtk-text-view-starts-display-line}
      @about-function{gtk-text-view-move-visually}
      @about-function{gtk-text-view-add-child-at-anchor}

      @about-class{gtk-text-child-anchor}

      @about-function{gtk-text-child-anchor-new}
      @about-function{gtk-text-child-anchor-get-widgets}
      @about-function{gtk-text-child-anchor-get-deleted}

      @about-function{gtk-text-view-add-child-in-window}
      @about-function{gtk-text-view-move-child}
      @about-function{gtk-text-view-set-wrap-mode}
      @about-function{gtk-text-view-get-wrap-mode}
      @about-function{gtk-text-view-set-editable}
      @about-function{gtk-text-view-get-editable}
      @about-function{gtk-text-view-set-cursor-visible}
      @about-function{gtk-text-view-get-cursor-visible}
      @about-function{gtk-text-view-set-overwrite}
      @about-function{gtk-text-view-get-overwrite}
      @about-function{gtk-text-view-set-pixels-above-lines}
      @about-function{gtk-text-view-get-pixels-above-lines}
      @about-function{gtk-text-view-set-pixels-below-lines}
      @about-function{gtk-text-view-get-pixels-below-lines}
      @about-function{gtk-text-view-set-pixels-inside-wrap}
      @about-function{gtk-text-view-get-pixels-inside-wrap}
      @about-function{gtk-text-view-set-justification}
      @about-function{gtk-text-view-get-justification}
      @about-function{gtk-text-view-set-left-margin}
      @about-function{gtk-text-view-get-left-margin}
      @about-function{gtk-text-view-set-right-margin}
      @about-function{gtk-text-view-get-right-margin}
      @about-function{gtk-text-view-set-indent}
      @about-function{gtk-text-view-get-indent}
      @about-function{gtk-text-view-set-tabs}
      @about-function{gtk-text-view-get-tabs}
      @about-function{gtk-text-view-set-accepts-tab}
      @about-function{gtk-text-view-get-accepts-tab}
      @about-function{gtk-text-view-get-default-attributes}
      @about-function{gtk-text-view-im-context-filter-keypress}
      @about-function{gtk-text-view-reset-im-context}
      @about-function{gtk-text-view-set-input-purpose}
      @about-function{gtk-text-view-get-input-purpose}
      @about-function{gtk-text-view-set-input-hints}
      @about-function{gtk-text-view-get-input-hints}
      @about-function{GTK-TEXT-VIEW-PRIORITY_VALIDATE}
    @end{subsection}
  @end{section}
  @begin[Tree, List and Icon Grid Widgets]{section}
    Overview of @class{gtk-tree-model}, @class{gtk-tree-view}, and friends.

    @subheading{Overview}
      To create a tree or list in GTK+, use the @class{gtk-tree-model} interface
      in conjunction with the @class{gtk-tree-view} widget. This widget is
      designed around a Model/View/Controller design and consists of four major
      parts:
      @begin{itemize}
        @item{The tree view widget (@class{gtk-tree-view}).}
        @item{The view column (@class{gtk-tree-view-column}).}
        @item{The cell renderers (@class{gtk-cell-renderer} etc.).}
        @item{The model interface (@class{gtk-tree-model}).}
      @end{itemize}
      The View is composed of the first three objects, while the last is the
      Model. One of the prime benefits of the MVC design is that multiple views
      can be created of a single model. For example, a model mapping the file
      system could be created for a file manager. Many views could be created to
      display various parts of the file system, but only one copy need be kept
      in memory.

      The purpose of the cell renderers is to provide extensibility to the
      widget and to allow multiple ways of rendering the same type of data. For
      example, consider how to render a boolean variable. Should it render it
      as a string of \"True\" or \"False\", \"On\" or \"Off\", or should it be
      rendered as a checkbox?

    @subheading{Creating a model}
      GTK+ provides two simple models that can be used: the
      @class{gtk-list-store} and the @class{gtk-tree-store}.
      @class{gtk-list-store} is used to model list widgets, while the
      @class{gtk-tree-store} models trees. It is possible to develop a new type
      of model, but the existing models should be satisfactory for all but the
      most specialized of situations. Creating the model is quite simple:
      @begin{pre}
GtkListStore *store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_BOOLEAN);
      @end{pre}
      This creates a list store with two columns: a string column and a boolean
      column. Typically the 2 is never passed directly like that; usually an
      enum is created wherein the different columns are enumerated, followed by
      a token that represents the total number of columns. The next example will
      illustrate this, only using a tree store instead of a list store. Creating
      a tree store operates almost exactly the same.
      @begin{pre}
enum
{
   TITLE_COLUMN,
   AUTHOR_COLUMN,
   CHECKED_COLUMN,
   N_COLUMNS
@};

GtkTreeStore *store = gtk_tree_store_new (N_COLUMNS,       /* Total number of columns */
                                          G_TYPE_STRING,   /* Book title              */
                                          G_TYPE_STRING,   /* Author                  */
                                          G_TYPE_BOOLEAN); /* Is checked out?         */
      @end{pre}
      Adding data to the model is done using @fun{gtk-tree-store-set} or
      @fun{gtk-list-store-set}, depending upon which sort of model was created.
      To do this, a @class{gtk-tree-iter} must be acquired. The iterator points
      to the location where data will be added.

      Once an iterator has been acquired, @fun{gtk-tree-store-set} is used to
      apply data to the part of the model that the iterator points to. Consider
      the following example:
      @begin{pre}
GtkTreeIter   iter;

gtk_tree_store_append (store, &iter, NULL);  /* Acquire an iterator */

gtk_tree_store_set (store, &iter,
                    TITLE_COLUMN, \"The Principle of Reason\",
                    AUTHOR_COLUMN, \"Martin Heidegger\",
                    CHECKED_COLUMN, FALSE,
                    -1);
      @end{pre}
      Notice that the last argument is -1. This is always done because this is
      a variable-argument function and it needs to know when to stop processing
      arguments. It can be used to set the data in any or all columns in a given
      row.

      The third argument to @fun{gtk-tree-store-append} is the parent iterator.
      It is used to add a row to a @class{gtk-tree-store} as a child of an
      existing row. This means that the new row will only be visible when its
      parent is visible and in its expanded state. Consider the following
      example:
      @begin{pre}
GtkTreeIter iter1;  /* Parent iter */
GtkTreeIter iter2;  /* Child iter  */

gtk_tree_store_append (store, &iter1, NULL);  /* Acquire a top-level iterator */
gtk_tree_store_set (store, &iter1,
                    TITLE_COLUMN, \"The Art of Computer Programming\",
                    AUTHOR_COLUMN, \"Donald E. Knuth\",
                    CHECKED_COLUMN, FALSE,
                    -1);

gtk_tree_store_append (store, &iter2, &iter1);  /* Acquire a child iterator */
gtk_tree_store_set (store, &iter2,
                    TITLE_COLUMN, \"Volume 1: Fundamental Algorithms\",
                    -1);

gtk_tree_store_append (store, &iter2, &iter1);
gtk_tree_store_set (store, &iter2,
                    TITLE_COLUMN, \"Volume 2: Seminumerical Algorithms\",
                    -1);

gtk_tree_store_append (store, &iter2, &iter1);
gtk_tree_store_set (store, &iter2,
                    TITLE_COLUMN, \"Volume 3: Sorting and Searching\",
                    -1);
      @end{pre}

    @subheading{Creating the view component}
      While there are several different models to choose from, there is only one
      view widget to deal with. It works with either the list or the tree store.
      Setting up a @class{gtk-tree-view} is not a difficult matter. It needs a
      @class{gtk-tree-model} to know where to retrieve its data from.
      @begin{pre}
GtkWidget *tree;

tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));

Columns and cell renderers
      @end{pre}
      Once the @class{gtk-tree-view} widget has a model, it will need to know
      how to display the model. It does this with columns and cell renderers.

      Cell renderers are used to draw the data in the tree model in a way. There
      are a number of cell renderers that come with GTK+ 2.x, including the
      @class{gtk-cell-renderer-text}, @class{gtk-cell-renderer-pixbuf} and the
      @class{gtk-cell-renderer-toggle}. It is relatively easy to write a custom
      renderer.

      A @class{gtk-tree-view-column} is the object that @class{gtk-tree-view}
      uses to organize the vertical columns in the tree view. It needs to know
      the name of the column to label for the user, what type of cell renderer
      to use, and which piece of data to retrieve from the model for a given
      row.
      @begin{pre}
GtkCellRenderer *renderer;
GtkTreeViewColumn *column;

renderer = gtk_cell_renderer_text_new ();
column = gtk_tree_view_column_new_with_attributes (\"Author\",
                                                   renderer,
                                                   \"text\", AUTHOR_COLUMN,
                                                   NULL);
gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);
      @end{pre}
      At this point, all the steps in creating a displayable tree have been
      covered. The model is created, data is stored in it, a tree view is
      created and columns are added to it.

    @subheading{Selection handling}
      Most applications will need to not only deal with displaying data, but
      also receiving input events from users. To do this, simply get a reference
      to a selection object and connect to the \"changed\" signal.
      @begin{pre}
/* Prototype for selection handler callback */
static void tree_selection_changed_cb (GtkTreeSelection *selection, gpointer data);

/* Setup the selection handler */
GtkTreeSelection *select;

select = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree));
gtk_tree_selection_set_mode (select, GTK_SELECTION_SINGLE);
g_signal_connect (G_OBJECT (select), \"changed\",
                  G_CALLBACK (tree_selection_changed_cb),
                  NULL);
      @end{pre}
      Then to retrieve data for the row selected:
      @begin{pre}
static void
tree_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
        GtkTreeIter iter;
        GtkTreeModel *model;
        gchar *author;

        if (gtk_tree_selection_get_selected (selection, &model, &iter))
        {
                gtk_tree_model_get (model, &iter, AUTHOR_COLUMN, &author, -1);

                g_print (\"You selected a book by %s\n\", author);

                g_free (author);
        @}
@}
      @end{pre}

    @subheading{Simple Example}
      Here is a simple example of using a @class{gtk-tree-view} widget in
      context of the other widgets. It simply creates a simple model and view,
      and puts them together. Note that the model is never populated with data
      - that is left as an exercise for the reader. More information can be
      found on this in the GtkTreeModel section.
      @begin{pre}
enum
{
   TITLE_COLUMN,
   AUTHOR_COLUMN,
   CHECKED_COLUMN,
   N_COLUMNS
@};

void
setup_tree (void)
{
   GtkTreeStore *store;
   GtkWidget *tree;
   GtkTreeViewColumn *column;
   GtkCellRenderer *renderer;

   /* Create a model.  We are using the store model for now, though we
    * could use any other GtkTreeModel */
   store = gtk_tree_store_new (N_COLUMNS,
                               G_TYPE_STRING,
                               G_TYPE_STRING,
                               G_TYPE_BOOLEAN);

   /* custom function to fill the model with data */
   populate_tree_model (store);

   /* Create a view */
   tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));

   /* The view now holds a reference.  We can get rid of our own
    * reference */
   g_object_unref (G_OBJECT (store));

   /* Create a cell render and arbitrarily make it red for demonstration
    * purposes */
   renderer = gtk_cell_renderer_text_new ();
   g_object_set (G_OBJECT (renderer),
                 \"foreground\", \"red\",
                 NULL);

   /* Create a column, associating the \"text\" attribute of the
    * cell_renderer to the first column of the model */
   column = gtk_tree_view_column_new_with_attributes (\"Author\", renderer,
                                                      \"text\", AUTHOR_COLUMN,
                                                      NULL);

   /* Add the column to the view. */
   gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

   /* Second column.. title of the book. */
   renderer = gtk_cell_renderer_text_new ();
   column = gtk_tree_view_column_new_with_attributes (\"Title\",
                                                      renderer,
                                                      \"text\", TITLE_COLUMN,
                                                      NULL);
   gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

   /* Last column.. whether a book is checked out. */
   renderer = gtk_cell_renderer_toggle_new ();
   column = gtk_tree_view_column_new_with_attributes (\"Checked out\",
                                                      renderer,
                                                      \"active\", CHECKED_COLUMN,
                                                      NULL);
   gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

   /* Now we can manipulate the view just like any other GTK+ widget */
   ...
@}
      @end{pre}
    @begin[GtkTreeModel]{subsection}
      The tree interface used by @class{gtk-tree-view}.

      @about-class{gtk-tree-model}
      @about-struct{gtk-tree-iter}
      @about-function{gtk-tree-iter-stamp}
      @about-function{gtk-tree-iter-user-data}
      @about-class{gtk-tree-path}
      @about-class{gtk-tree-row-reference}
      @about-symbol{gtk-tree-model-iface}
      @about-symbol{gtk-tree-model-flags}
      @about-function{gtk-tree-path-new}
      @about-function{gtk-tree-path-new-from-string}
      @about-function{gtk-tree-path-new-from-indices}
      @about-function{gtk-tree-path-to-string}
      @about-function{gtk-tree-path-new-first}
      @about-function{gtk-tree-path-append-index}
      @about-function{gtk-tree-path-prepend-index}
      @about-function{gtk-tree-path-get-depth}
      @about-function{gtk-tree-path-get-indices}
      @about-function{gtk-tree-path-get-indices-with-depth}
      @about-function{gtk-tree-path-free}
      @about-function{gtk-tree-path-copy}
      @about-function{gtk-tree-path-compare}
      @about-function{gtk-tree-path-next}
      @about-function{gtk-tree-path-prev}
      @about-function{gtk-tree-path-up}
      @about-function{gtk-tree-path-down}
      @about-function{gtk-tree-path-is-ancestor}
      @about-function{gtk-tree-path-is-descendant}
      @about-function{gtk-tree-row-reference-new}
      @about-function{gtk-tree-row-reference-new-proxy}
      @about-function{gtk-tree-row-reference-get-model}
      @about-function{gtk-tree-row-reference-get-path}
      @about-function{gtk-tree-row-reference-valid}
      @about-function{gtk-tree-row-reference-free}
      @about-function{gtk-tree-row-reference-copy}
      @about-function{gtk-tree-row-reference-inserted}
      @about-function{gtk-tree-row-reference-deleted}
      @about-function{gtk-tree-row-reference-reordered}
      @about-function{gtk-tree-iter-copy}
      @about-function{gtk-tree-iter-free}
      @about-function{gtk-tree-model-get-flags}
      @about-function{gtk-tree-model-get-n-columns}
      @about-function{gtk-tree-model-get-column-type}
      @about-function{gtk-tree-model-get-iter}
      @about-function{gtk-tree-model-get-iter-from-string}
      @about-function{gtk-tree-model-get-iter-first}
      @about-function{gtk-tree-model-get-path}
      @about-function{gtk-tree-model-get-value}
      @about-function{gtk-tree-model-iter-next}
      @about-function{gtk-tree-model-iter-previous}
      @about-function{gtk-tree-model-iter-children}
      @about-function{gtk-tree-model-iter-has-child}
      @about-function{gtk-tree-model-iter-n-children}
      @about-function{gtk-tree-model-iter-nth-child}
      @about-function{gtk-tree-model-iter-parent}
      @about-function{gtk-tree-model-get-string-from-iter}
      @about-function{gtk-tree-model-ref-node}
      @about-function{gtk-tree-model-unref-node}
      @about-function{gtk-tree-model-get}
      @about-function{gtk-tree-model-get-valist}
      @about-function{gtk-tree-model-foreach}
      @about-function{gtk-tree-model-row-changed}
      @about-function{gtk-tree-model-row-inserted}
      @about-function{gtk-tree-model-row-has-child-toggled}
      @about-function{gtk-tree-model-row-deleted}
      @about-function{gtk-tree-model-rows-reordered}
    @end{subsection}
    @begin[GtkTreeSelection]{subsection}
      The selection object for @class{gtk-tree-view}.

      @about-class{gtk-tree-selection}

      @about-generic{gtk-tree-selection-mode}

      @about-function{gtk-tree-selection-set-select-function}
      @about-function{gtk-tree-selection-get-select-function}
      @about-function{gtk-tree-selection-get-user-data}
      @about-function{gtk-tree-selection-get-tree-view}
      @about-function{gtk-tree-selection-get-selected}
      @about-function{gtk-tree-selection-selected-foreach}
      @about-function{gtk-tree-selection-get-selected-rows}
      @about-function{gtk-tree-selection-count-selected-rows}
      @about-function{gtk-tree-selection-select-path}
      @about-function{gtk-tree-selection-unselect-path}
      @about-function{gtk-tree-selection-path-is-selected}
      @about-function{gtk-tree-selection-select-iter}
      @about-function{gtk-tree-selection-unselect-iter}
      @about-function{gtk-tree-selection-iter-is-selected}
      @about-function{gtk-tree-selection-select-all}
      @about-function{gtk-tree-selection-unselect-all}
      @about-function{gtk-tree-selection-select-range}
      @about-function{gtk-tree-selection-unselect-range}
    @end{subsection}
    @begin[GtkTreeViewColumn]{subsection}
      A visible column in a @class{gtk-tree-view} widget.

      @about-symbol{gtk-tree-view-column-sizing}

      @about-class{gtk-tree-view-column}

      @about-generic{gtk-tree-view-column-alignment}
      @about-generic{gtk-tree-view-column-cell-area}
      @about-generic{gtk-tree-view-column-clickable}
      @about-generic{gtk-tree-view-column-expand}
      @about-generic{gtk-tree-view-column-fixed-width}
      @about-generic{gtk-tree-view-column-max-width}
      @about-generic{gtk-tree-view-column-min-width}
      @about-generic{gtk-tree-view-column-reorderable}
      @about-generic{gtk-tree-view-column-resizable}
      @about-generic{gtk-tree-view-column-sizing}
      @about-generic{gtk-tree-view-column-sort-column-id}
      @about-generic{gtk-tree-view-column-sort-indicator}
      @about-generic{gtk-tree-view-column-sort-order}
      @about-generic{gtk-tree-view-column-spacing}
      @about-generic{gtk-tree-view-column-title}
      @about-generic{gtk-tree-view-column-visible}
      @about-generic{gtk-tree-view-column-widget}
      @about-generic{gtk-tree-view-column-width}
      @about-generic{gtk-tree-view-column-x-offset}

      @about-function{gtk-tree-view-column-new}
      @about-function{gtk-tree-view-column-new-with-area}
      @about-function{gtk-tree-view-column-new-with-attributes}
      @about-function{gtk-tree-view-column-pack-start}
      @about-function{gtk-tree-view-column-pack-end}
      @about-function{gtk-tree-view-column-clear}
      @about-function{gtk-tree-view-column-add-attribute}
      @about-function{gtk-tree-view-column-set-attributes}
      @about-function{gtk-tree-view-column-set-cell-data-func}
      @about-function{gtk-tree-view-column-clear-attributes}
      @about-function{gtk-tree-view-column-clicked}
      @about-function{gtk-tree-view-column-get-button}
      @about-function{gtk-tree-view-column-cell-set-cell-data}
      @about-function{gtk-tree-view-column-cell-get-size}
      @about-function{gtk-tree-view-column-cell-get-position}
      @about-function{gtk-tree-view-column-cell-is-visible}
      @about-function{gtk-tree-view-column-focus-cell}
      @about-function{gtk-tree-view-column-queue-resize}
      @about-function{gtk-tree-view-column-get-tree-view}
    @end{subsection}
    @begin[GtkTreeView]{subsection}
      A widget for displaying both trees and lists.

      @about-symbol{gtk-tree-view-drop-position}
      @about-symbol{gtk-tree-view-private}

      @about-class{gtk-tree-view}

      @about-generic{gtk-tree-view-activate-on-single-click}
      @about-generic{gtk-tree-view-enable-grid-lines}
      @about-generic{gtk-tree-view-enable-search}
      @about-generic{gtk-tree-view-enable-tree-lines}
      @about-generic{gtk-tree-view-expander-column}
      @about-generic{gtk-tree-view-fixed-height-mode}
      @about-generic{gtk-tree-view-headers-clickable}
      @about-generic{gtk-tree-view-headers-visible}
      @about-generic{gtk-tree-view-hover-expand}
      @about-generic{gtk-tree-view-hover-selection}
      @about-generic{gtk-tree-view-level-indentation}
      @about-generic{gtk-tree-view-model}
      @about-generic{gtk-tree-view-reorderable}
      @about-generic{gtk-tree-view-rubber-banding}
      @about-generic{gtk-tree-view-rules-hint}
      @about-generic{gtk-tree-view-search-column}
      @about-generic{gtk-tree-view-show-expanders}
      @about-generic{gtk-tree-view-tooltip-column}

      @about-function{gtk-tree-view-new}
      @about-function{gtk-tree-view-new-with-model}
      @about-function{gtk-tree-view-get-selection}
      @about-function{gtk-tree-view-get-hadjustment}
      @about-function{gtk-tree-view-set-hadjustment}
      @about-function{gtk-tree-view-get-vadjustment}
      @about-function{gtk-tree-view-set-vadjustment}
      @about-function{gtk-tree-view-columns-autosize}
      @about-function{gtk-tree-view-append-column}
      @about-function{gtk-tree-view-remove-column}
      @about-function{gtk-tree-view-insert-column}
      @about-function{gtk-tree-view-insert-column-with-attributes}
      @about-function{gtk-tree-view-insert-column-with-data-func}
      @about-function{gtk-tree-view-get-n-columns}
      @about-function{gtk-tree-view-get-column}
      @about-function{gtk-tree-view-get-columns}
      @about-function{gtk-tree-view-move-column-after}
      @about-function{gtk-tree-view-set-column-drag-function}
      @about-function{gtk-tree-view-scroll-to-point}
      @about-function{gtk-tree-view-scroll-to-cell}
      @about-function{gtk-tree-view-set-cursor}
      @about-function{gtk-tree-view-set-cursor-on-cell}
      @about-function{gtk-tree-view-get-cursor}
      @about-function{gtk-tree-view-row-activated}
      @about-function{gtk-tree-view-expand-all}
      @about-function{gtk-tree-view-collapse-all}
      @about-function{gtk-tree-view-expand-to-path}
      @about-function{gtk-tree-view-expand-row}
      @about-function{gtk-tree-view-collapse-row}
      @about-function{gtk-tree-view-map-expanded-rows}
      @about-function{gtk-tree-view-row-expanded}
      @about-function{gtk-tree-view-get-path-at-pos}
      @about-function{gtk-tree-view-is-blank-at-pos}
      @about-function{gtk-tree-view-get-cell-area}
      @about-function{gtk-tree-view-get-background-area}
      @about-function{gtk-tree-view-get-visible-rect}
      @about-function{gtk-tree-view-get-visible-range}
      @about-function{gtk-tree-view-get-bin-window}
      @about-function{gtk-tree-view-convert-bin-window-to-tree-coords}
      @about-function{gtk-tree-view-convert-bin-window-to-widget-coords}
      @about-function{gtk-tree-view-convert-tree-to-bin-window-coords}
      @about-function{gtk-tree-view-convert-tree-to-widget-coords}
      @about-function{gtk-tree-view-convert-widget-to-bin-window-coords}
      @about-function{gtk-tree-view-convert-widget-to-tree-coords}
      @about-function{gtk-tree-view-enable-model-drag-dest}
      @about-function{gtk-tree-view-enable-model-drag-source}
      @about-function{gtk-tree-view-unset-rows-drag-source}
      @about-function{gtk-tree-view-unset-rows-drag-dest}
      @about-function{gtk-tree-view-set-drag-dest-row}
      @about-function{gtk-tree-view-get-drag-dest-row}
      @about-function{gtk-tree-view-get-dest-row-at-pos}
      @about-function{gtk-tree-view-create-row-drag-icon}
      @about-function{gtk-tree-view-get-search-equal-func}
      @about-function{gtk-tree-view-set-search-equal-func}
      @about-function{gtk-tree-view-get-search-entry}
      @about-function{gtk-tree-view-set-search-entry}
      @about-function{gtk-tree-view-get-search-position-func}
      @about-function{gtk-tree-view-set-search-position-func}
      @about-function{gtk-tree-view-set-destroy-count-func}
      @about-function{gtk-tree-view-get-row-separator-func}
      @about-function{gtk-tree-view-set-row-separator-func}
      @about-function{gtk-tree-view-is-rubber-banding-active}

      @about-symbol{gtk-tree-view-grid-lines}

      @about-function{gtk-tree-view-get-grid-lines}
      @about-function{gtk-tree-view-set-grid-lines}
      @about-function{gtk-tree-view-set-tooltip-row}
      @about-function{gtk-tree-view-set-tooltip-cell}
      @about-function{gtk-tree-view-get-tooltip-context}
    @end{subsection}
    @begin[GtkTreeView drag and drop]{subsection}
      Interfaces for drag-and-drop support in @class{gtk-tree-view}.

      @about-class{gtk-tree-drag-source}
      @about-class{gtk-tree-drag-source-iface}

      @about-function{gtk-tree-drag-source-drag-data-delete}
      @about-function{gtk-tree-drag-source-drag-data-get}
      @about-function{gtk-tree-drag-source-drag-row-draggable}

      @about-class{gtk-tree-drag-dest}
      @about-class{gtk-tree-drag-dest-iface}

      @about-function{gtk-tree-drag-dest-drag-data-received}
      @about-function{gtk-tree-drag-dest-row-drop-possible}
      @about-function{gtk-tree-set-row-drag-data}
      @about-function{gtk-tree-get-row-drag-data}
    @end{subsection}
    @begin[GtkCellView]{subsection}
      A widget displaying a single row of a @class{gtk-tree-model}.

      @about-class{gtk-cell-view}

      @about-generic{gtk-cell-view-background}
      @about-generic{gtk-cell-view-background-gdk}
      @about-generic{gtk-cell-view-background-rgba}
      @about-generic{gtk-cell-view-background-set}
      @about-generic{gtk-cell-view-cell-area}
      @about-generic{gtk-cell-view-cell-area-context}
      @about-generic{gtk-cell-view-draw-sensitive}
      @about-generic{gtk-cell-view-fit-model}
      @about-generic{gtk-cell-view-model}

      @about-function{gtk-cell-view-new}
      @about-function{gtk-cell-view-new-with-context}
      @about-function{gtk-cell-view-new-with-text}
      @about-function{gtk-cell-view-new-with-markup}
      @about-function{gtk-cell-view-new-with-pixbuf}
      @about-function{gtk-cell-view-set-displayed-row}
      @about-function{gtk-cell-view-get-displayed-row}
      @about-function{gtk-cell-view-get-size-of-row}
      @about-function{gtk-cell-view-set-background-color}
      @about-function{gtk-cell-view-set-background-rgba}
    @end{subsection}
    @begin[GtkIconView]{subsection}
      A widget which displays a list of icons in a grid.

      @about-class{gtk-icon-view}

      @about-generic{gtk-icon-view-activate-on-single-click}
      @about-generic{gtk-icon-view-cell-area}
      @about-generic{gtk-icon-view-column-spacing}
      @about-generic{gtk-icon-view-columns}
      @about-generic{gtk-icon-view-item-orientation}
      @about-generic{gtk-icon-view-item-padding}
      @about-generic{gtk-icon-view-item-width}
      @about-generic{gtk-icon-view-margin}
      @about-generic{gtk-icon-view-markup-column}
      @about-generic{gtk-icon-view-model}
      @about-generic{gtk-icon-view-pixbuf-column}
      @about-generic{gtk-icon-view-reorderable}
      @about-generic{gtk-icon-view-row-spacing}
      @about-generic{gtk-icon-view-selection-mode}
      @about-generic{gtk-icon-view-spacing}
      @about-generic{gtk-icon-view-text-column}
      @about-generic{gtk-icon-view-tooltip-column}

      @about-function{gtk-icon-view-new}
      @about-function{gtk-icon-view-new-with-area}
      @about-function{gtk-icon-view-new-with-model}
      @about-function{gtk-icon-view-get-path-at-pos}
      @about-function{gtk-icon-view-get-item-at-pos}
      @about-function{gtk-icon-view-convert-widget-to-bin-window-coords}
      @about-function{gtk-icon-view-set-cursor}
      @about-function{gtk-icon-view-get-cursor}
      @about-function{gtk-icon-view-selected-foreach}
      @about-function{gtk-icon-view-get-cell-rect}
      @about-function{gtk-icon-view-select-path}
      @about-function{gtk-icon-view-unselect-path}
      @about-function{gtk-icon-view-path-is-selected}
      @about-function{gtk-icon-view-get-selected-items}
      @about-function{gtk-icon-view-select-all}
      @about-function{gtk-icon-view-unselect-all}
      @about-function{gtk-icon-view-item-activated}
      @about-function{gtk-icon-view-scroll-to-path}
      @about-function{gtk-icon-view-get-visible-range}
      @about-function{gtk-icon-view-set-tooltip-item}
      @about-function{gtk-icon-view-set-tooltip-cell}
      @about-function{gtk-icon-view-get-tooltip-context}
      @about-function{gtk-icon-view-get-item-row}
      @about-function{gtk-icon-view-get-item-column}

      @about-symbol{gtk-icon-view-drop-position}

      @about-function{gtk-icon-view-enable-model-drag-source}
      @about-function{gtk-icon-view-enable-model-drag-dest}
      @about-function{gtk-icon-view-unset-model-drag-source}
      @about-function{gtk-icon-view-unset-model-drag-dest}
      @about-function{gtk-icon-view-set-drag-dest-item}
      @about-function{gtk-icon-view-get-drag-dest-item}
      @about-function{gtk-icon-view-get-dest-item-at-pos}
      @about-function{gtk-icon-view-create-drag-icon}
    @end{subsection}
    @begin[GtkTreeSortable]{subsection}
      The interface for sortable models used by @class{gtk-tree-view}.

      @about-class{gtk-tree-sortable}
      @about-class{gtk-tree-sortable-iface}

      @about-function{gtk-tree-sortable-sort-column-changed}
      @about-function{gtk-tree-sortable-get-sort-column-id}
      @about-function{gtk-tree-sortable-set-sort-column-id}
      @about-function{gtk-tree-sortable-set-sort-func}
      @about-function{gtk-tree-sortable-set-default-sort-func}
      @about-function{gtk-tree-sortable-has-default-sort-func}
    @end{subsection}
    @begin[GtkTreeModelSort]{subsection}
      A @class{gtk-tree-model} which makes an underlying tree model sortable.

      @about-class{gtk-tree-model-sort}

      @about-generic{gtk-tree-model-sort-model}

      @about-function{gtk-tree-model-sort-new-with-model}
      @about-function{gtk-tree-model-sort-convert-child-path-to-path}
      @about-function{gtk-tree-model-sort-convert-child-iter-to-iter}
      @about-function{gtk-tree-model-sort-convert-path-to-child-path}
      @about-function{gtk-tree-model-sort-convert-iter-to-child-iter}
      @about-function{gtk-tree-model-sort-reset-default-sort-func}
      @about-function{gtk-tree-model-sort-clear-cache}
      @about-function{gtk-tree-model-sort-iter-is-valid}
    @end{subsection}
    @begin[GtkTreeModelFilter]{subsection}
      A @class{gtk-tree-model} which hides parts of an underlying tree model.

      @about-class{gtk-tree-model-filter}

      @about-generic{gtk-tree-model-filter-child-model}
      @about-generic{gtk-tree-model-filter-virtual-root}

      @about-function{gtk-tree-model-filter-new}
      @about-function{gtk-tree-model-filter-set-visible-func}
      @about-function{gtk-tree-model-filter-set-modify-func}
      @about-function{gtk-tree-model-filter-set-visible-column}
      @about-function{gtk-tree-model-filter-get-model}
      @about-function{gtk-tree-model-filter-convert-child-iter-to-iter}
      @about-function{gtk-tree-model-filter-convert-iter-to-child-iter}
      @about-function{gtk-tree-model-filter-convert-child-path-to-path}
      @about-function{gtk-tree-model-filter-convert-path-to-child-path}
      @about-function{gtk-tree-model-filter-refilter}
      @about-function{gtk-tree-model-filter-clear-cache}
    @end{subsection}
    @begin[GtkCellLayout]{subsection}
      An interface for packing cells.

      @about-class{gtk-cell-layout}
      @about-class{gtk-cell-layout-iface}

      @about-function{gtk-cell-layout-pack-start}
      @about-function{gtk-cell-layout-pack-end}
      @about-function{gtk-cell-layout-get-area}
      @about-function{gtk-cell-layout-get-cells}
      @about-function{gtk-cell-layout-reorder}
      @about-function{gtk-cell-layout-clear}
      @about-function{gtk-cell-layout-set-attributes}
      @about-function{gtk-cell-layout-add-attribute}
      @about-function{gtk-cell-layout-set-cell-data-func}
      @about-function{gtk-cell-layout-clear-attributes}
    @end{subsection}
    @begin[GtkCellArea]{subsection}
      An abstract class for laying out @class{gtk-cell-renderer}'s.

      @about-class{gtk-cell-area}

      @about-generic{gtk-cell-area-edit-widget}
      @about-generic{gtk-cell-area-edited-cell}
      @about-generic{gtk-cell-area-focus-cell}

      @about-function{gtk-cell-area-box-child-align}
      @about-function{gtk-cell-area-box-child-expand}
      @about-function{gtk-cell-area-box-child-fixed-size}
      @about-function{gtk-cell-area-box-child-pack-type}

      @about-class{gtk-cell-area-class}
      @about-symbol{GTK-CELL-AREA-WARN-INVALID-CELL-PROPERTY-ID}
      @about-function{gtk-cell-area-add}
      @about-function{gtk-cell-area-remove}
      @about-function{gtk-cell-area-has-renderer}
      @about-function{gtk-cell-area-foreach}
      @about-function{gtk-cell-area-foreach-alloc}
      @about-function{gtk-cell-area-event}
      @about-function{gtk-cell-area-render}
      @about-function{gtk-cell-area-get-cell-allocation}
      @about-function{gtk-cell-area-get-cell-at-position}
      @about-function{gtk-cell-area-create-context}
      @about-function{gtk-cell-area-copy-context}
      @about-function{gtk-cell-area-get-request-mode}
      @about-function{gtk-cell-area-get-preferred-width}
      @about-function{gtk-cell-area-get-preferred-height-for-width}
      @about-function{gtk-cell-area-get-preferred-height}
      @about-function{gtk-cell-area-get-preferred-width-for-height}
      @about-function{gtk-cell-area-get-current-path-string}
      @about-function{gtk-cell-area-apply-attributes}
      @about-function{gtk-cell-area-attribute-connect}
      @about-function{gtk-cell-area-attribute-disconnect}
      @about-function{gtk-cell-area-class-install-cell-property}
      @about-function{gtk-cell-area-class-find-cell-property}
      @about-function{gtk-cell-area-class-list-cell-properties}
      @about-function{gtk-cell-area-add-with-properties}
      @about-function{gtk-cell-area-cell-set}
      @about-function{gtk-cell-area-cell-get}
      @about-function{gtk-cell-area-cell-set-valist}
      @about-function{gtk-cell-area-cell-get-valist}
      @about-function{gtk-cell-area-cell-set-property}
      @about-function{gtk-cell-area-cell-get-property}
      @about-function{gtk-cell-area-is-activatable}
      @about-function{gtk-cell-area-activate}
      @about-function{gtk-cell-area-focus}
      @about-function{gtk-cell-area-set-focus-cell}
      @about-function{gtk-cell-area-get-focus-cell}
      @about-function{gtk-cell-area-add-focus-sibling}
      @about-function{gtk-cell-area-remove-focus-sibling}
      @about-function{gtk-cell-area-is-focus-sibling}
      @about-function{gtk-cell-area-get-focus-siblings}
      @about-function{gtk-cell-area-get-focus-from-sibling}
      @about-function{gtk-cell-area-get-edited-cell}
      @about-function{gtk-cell-area-get-edit-widget}
      @about-function{gtk-cell-area-activate-cell}
      @about-function{gtk-cell-area-stop-editing}
      @about-function{gtk-cell-area-inner-cell-area}
      @about-function{gtk-cell-area-request-renderer}
    @end{subsection}
    @begin[GtkCellAreaBox]{subsection}
      A cell area that renders @class{gtk-cell-renderer}s into a row or a
      column.

      @about-class{gtk-cell-area-box}

      @about-generic{gtk-cell-area-box-spacing}

      @about-function{gtk-cell-area-box-new}
      @about-function{gtk-cell-area-box-pack-start}
      @about-function{gtk-cell-area-box-pack-end}
      @about-function{gtk-cell-area-box-get-spacing}
      @about-function{gtk-cell-area-box-set-spacing}
    @end{subsection}
    @begin[GtkcellAreaContext]{subsection}
      Stores geometrical information for a series of rows in a
      @class{gtk-cell-area}.

      @about-class{gtk-cell-area-context}

      @about-generic{gtk-cell-area-context-area}
      @about-generic{gtk-cell-area-context-minimum-height}
      @about-generic{gtk-cell-area-context-minimum-width}
      @about-generic{gtk-cell-area-context-natural-height}
      @about-generic{gtk-cell-area-context-natural-width}

      @about-function{gtk-cell-area-context-get-area}
      @about-function{gtk-cell-area-context-allocate}
      @about-function{gtk-cell-area-context-reset}
      @about-function{gtk-cell-area-context-get-preferred-width}
      @about-function{gtk-cell-area-context-get-preferred-height}
      @about-function{gtk-cell-area-context-get-preferred-height-for-width}
      @about-function{gtk-cell-area-context-get-preferred-width-for-height}
      @about-function{gtk-cell-area-context-get-allocation}
      @about-function{gtk-cell-area-context-push-preferred-width}
      @about-function{gtk-cell-area-context-push-preferred-height}
    @end{subsection}
    @begin[GtkCellRenderer]{subsection}
      An object for rendering a single cell.

      @about-symbol{gtk-cell-renderer-state}
      @about-symbol{gtk-cell-renderer-mode}
      @about-class{gtk-cell-renderer}

      @about-generic{gtk-cell-renderer-cell-background}
      @about-generic{gtk-cell-renderer-cell-background-gdk}
      @about-generic{gtk-cell-renderer-cell-background-rgba}
      @about-generic{gtk-cell-renderer-cell-background-set}
      @about-generic{gtk-cell-renderer-editing}
      @about-generic{gtk-cell-renderer-height}
      @about-generic{gtk-cell-renderer-is-expanded}
      @about-generic{gtk-cell-renderer-is-expander}
      @about-generic{gtk-cell-renderer-mode}
      @about-generic{gtk-cell-renderer-sensitive}
      @about-generic{gtk-cell-renderer-visible}
      @about-generic{gtk-cell-renderer-width}
      @about-generic{gtk-cell-renderer-xalign}
      @about-generic{gtk-cell-renderer-xpad}
      @about-generic{gtk-cell-renderer-yalign}
      @about-generic{gtk-cell-renderer-ypad}

      @about-function{gtk-cell-renderer-get-aligned-area}
      @about-function{gtk-cell-renderer-get-size}
      @about-function{gtk-cell-renderer-render}
      @about-function{gtk-cell-renderer-activate}
      @about-function{gtk-cell-renderer-start-editing}
      @about-function{gtk-cell-renderer-stop-editing}
      @about-function{gtk-cell-renderer-get-fixed-size}
      @about-function{gtk-cell-renderer-set-fixed-size}
      @about-function{gtk-cell-renderer-get-alignment}
      @about-function{gtk-cell-renderer-set-alignment}
      @about-function{gtk-cell-renderer-get-padding}
      @about-function{gtk-cell-renderer-set-padding}
      @about-function{gtk-cell-renderer-get-state}
      @about-function{gtk-cell-renderer-is-activatable}
      @about-function{gtk-cell-renderer-get-preferred-height}
      @about-function{gtk-cell-renderer-get-preferred-height-for-width}
      @about-function{gtk-cell-renderer-get-preferred-size}
      @about-function{gtk-cell-renderer-get-preferred-width}
      @about-function{gtk-cell-renderer-get-preferred-width-for-height}
      @about-function{gtk-cell-renderer-get-request-mode}
    @end{subsection}
    @begin[GtkCellEditable]{subsection}
      Interface for widgets which can are used for editing cells.

      @about-class{gtk-cell-editable}

      @about-generic{gtk-cell-editable-editing-canceled}

      @about-function{gtk-cell-editable-start-editing}
      @about-function{gtk-cell-editable-editing-done}
      @about-function{gtk-cell-editable-remove-widget}
    @end{subsection}
    @begin[GtkCellRendererAccel]{subsection}
      Renders a keyboard accelerator in a cell.

      @about-symbol{gtk-cell-renderer-accel-mode}

      @about-class{gtk-cell-renderer-accel}

      @about-generic{gtk-cell-renderer-accel-accel-key}
      @about-generic{gtk-cell-renderer-accel-accel-mode}
      @about-generic{gtk-cell-renderer-accel-accel-mods}
      @about-generic{gtk-cell-renderer-accel-keycode}

      @about-function{gtk-cell-renderer-accel-new}
    @end{subsection}
    @begin[GtkCellRendererCombo]{subsection}
      Renders a combobox in a cell.

      @about-class{gtk-cell-renderer-combo}

      @about-generic{gtk-cell-renderer-combo-has-entry}
      @about-generic{gtk-cell-renderer-combo-model}
      @about-generic{gtk-cell-renderer-combo-text-column}

      @about-function{gtk-cell-renderer-combo-new}
    @end{subsection}
    @begin[GtkCellRendererPixbuf]{subsection}
      Renders a pixbuf in a cell.

      @about-class{gtk-cell-renderer-pixbuf}

      @about-generic{gtk-cell-renderer-pixbuf-follow-state}
      @about-generic{gtk-cell-renderer-pixbuf-gicon}
      @about-generic{gtk-cell-renderer-pixbuf-icon-name}
      @about-generic{gtk-cell-renderer-pixbuf-pixbuf}
      @about-generic{gtk-cell-renderer-pixbuf-pixbuf-expander-closed}
      @about-generic{gtk-cell-renderer-pixbuf-pixbuf-expander-open}
      @about-generic{gtk-cell-renderer-pixbuf-stock-detail}
      @about-generic{gtk-cell-renderer-pixbuf-stock-id}
      @about-generic{gtk-cell-renderer-pixbuf-stock-size}
      @about-generic{gtk-cell-renderer-pixbuf-surface}

      @about-function{gtk-cell-renderer-pixbuf-new}
    @end{subsection}
    @begin[GtkCellRendererProgress]{subsection}
      Renders numbers as progress bars.

      @about-class{gtk-cell-renderer-progress}

      @about-generic{gtk-cell-renderer-progress-inverted}
      @about-generic{gtk-cell-renderer-progress-pulse}
      @about-generic{gtk-cell-renderer-progress-text}
      @about-generic{gtk-cell-renderer-progress-text-xalign}
      @about-generic{gtk-cell-renderer-progress-text-yalign}
      @about-generic{gtk-cell-renderer-progress-value}

      @about-function{gtk-cell-renderer-progress-new}
    @end{subsection}
    @begin[GtkCellRendererSpin]{subsection}
      Renders a spin button in a cell.

      @about-class{gtk-cell-renderer-spin}

      @about-generic{gtk-cell-renderer-spin-adjustment}
      @about-generic{gtk-cell-renderer-spin-climb-rate}
      @about-generic{gtk-cell-renderer-spin-digits}

      @about-function{gtk-cell-renderer-spin-new}
    @end{subsection}
    @begin[GtkCellRendererText]{subsection}
      Renders text in a cell.

      @about-class{gtk-cell-renderer-text}

      @about-generic{gtk-cell-renderer-text-align-set}
      @about-generic{gtk-cell-renderer-text-alignment}
      @about-generic{gtk-cell-renderer-text-attributes}
      @about-generic{gtk-cell-renderer-text-background}
      @about-generic{gtk-cell-renderer-text-background-gdk}
      @about-generic{gtk-cell-renderer-text-background-rgba}
      @about-generic{gtk-cell-renderer-text-background-set}
      @about-generic{gtk-cell-renderer-text-editable}
      @about-generic{gtk-cell-renderer-text-editable-set}
      @about-generic{gtk-cell-renderer-text-ellipsize}
      @about-generic{gtk-cell-renderer-text-ellipsize-set}
      @about-generic{gtk-cell-renderer-text-family}
      @about-generic{gtk-cell-renderer-text-family-set}
      @about-generic{gtk-cell-renderer-text-font}
      @about-generic{gtk-cell-renderer-text-font-desc}
      @about-generic{gtk-cell-renderer-text-foreground}
      @about-generic{gtk-cell-renderer-text-foreground-gdk}
      @about-generic{gtk-cell-renderer-text-foreground-rgba}
      @about-generic{gtk-cell-renderer-text-foreground-set}
      @about-generic{gtk-cell-renderer-text-language}
      @about-generic{gtk-cell-renderer-text-language-set}
      @about-generic{gtk-cell-renderer-text-markup}
      @about-generic{gtk-cell-renderer-text-max-width-chars}
      @about-generic{gtk-cell-renderer-text-placeholder-text}
      @about-generic{gtk-cell-renderer-text-rise}
      @about-generic{gtk-cell-renderer-text-rise-set}
      @about-generic{gtk-cell-renderer-text-scale}
      @about-generic{gtk-cell-renderer-text-scale-set}
      @about-generic{gtk-cell-renderer-text-single-paragraph-mode}
      @about-generic{gtk-cell-renderer-text-size}
      @about-generic{gtk-cell-renderer-text-size-points}
      @about-generic{gtk-cell-renderer-text-size-set}
      @about-generic{gtk-cell-renderer-text-stretch}
      @about-generic{gtk-cell-renderer-text-stretch-set}
      @about-generic{gtk-cell-renderer-text-strikethrough}
      @about-generic{gtk-cell-renderer-text-strikethrough-set}
      @about-generic{gtk-cell-renderer-text-style}
      @about-generic{gtk-cell-renderer-text-style-set}
      @about-generic{gtk-cell-renderer-text-text}
      @about-generic{gtk-cell-renderer-text-underline}
      @about-generic{gtk-cell-renderer-text-underline-set}
      @about-generic{gtk-cell-renderer-text-variant}
      @about-generic{gtk-cell-renderer-text-variant-set}
      @about-generic{gtk-cell-renderer-text-weight}
      @about-generic{gtk-cell-renderer-text-weight-set}
      @about-generic{gtk-cell-renderer-text-width-chars}
      @about-generic{gtk-cell-renderer-text-wrap-mode}
      @about-generic{gtk-cell-renderer-text-wrap-width}

      @about-function{gtk-cell-renderer-text-new}
      @about-function{gtk-cell-renderer-text-set-fixed-height-from-font}
    @end{subsection}
    @begin[GtkCellRendererToggle]{subsection}
      Renders a toggle button in a cell.

      @about-class{gtk-cell-renderer-toggle}

      @about-generic{gtk-cell-renderer-toggle-activatable}
      @about-generic{gtk-cell-renderer-toggle-active}
      @about-generic{gtk-cell-renderer-toggle-inconsistent}
      @about-generic{gtk-cell-renderer-toggle-indicator-size}
      @about-generic{gtk-cell-renderer-toggle-radio}

      @about-function{gtk-cell-renderer-toggle-new}
    @end{subsection}
    @begin[GtkCellRendererSpinner]{subsection}
      Renders a spinning animation in a cell.

      @about-class{gtk-cell-renderer-spinner}

      @about-generic{gtk-cell-renderer-spinner-active}
      @about-generic{gtk-cell-renderer-spinner-pulse}
      @about-generic{gtk-cell-renderer-spinner-size}

      @about-function{gtk-cell-renderer-spinner-new}
    @end{subsection}
    @begin[GtkListStore]{subsection}
      A list-like data structure that can be used with the
      @class{gtk-tree-view}.

      @about-class{gtk-list-store}

      @about-function{gtk-list-store-new}
      @about-function{gtk-list-store-newv}
      @about-function{gtk-list-store-set-column-types}
      @about-function{gtk-list-store-set}
      @about-function{gtk-list-store-set-valist}
      @about-function{gtk-list-store-set-value}
      @about-function{gtk-list-store-set-valuesv}
      @about-function{gtk-list-store-remove}
      @about-function{gtk-list-store-insert}
      @about-function{gtk-list-store-insert-before}
      @about-function{gtk-list-store-insert-after}
      @about-function{gtk-list-store-insert-with-values}
      @about-function{gtk-list-store-insert-with-valuesv}
      @about-function{gtk-list-store-prepend}
      @about-function{gtk-list-store-append}
      @about-function{gtk-list-store-clear}
      @about-function{gtk-list-store-iter-is-valid}
      @about-function{gtk-list-store-reorder}
      @about-function{gtk-list-store-swap}
      @about-function{gtk-list-store-move-before}
      @about-function{gtk-list-store-move-after}
    @end{subsection}
    @begin[GtkTreeStore]{subsection}
      A tree-like data structure that can be used with the
      @class{gtk-tree-view}.

      @about-class{gtk-tree-store}

      @about-function{gtk-tree-store-new}
      @about-function{gtk-tree-store-newv}
      @about-function{gtk-tree-store-set-column-types}
      @about-function{gtk-tree-store-set-value}
      @about-function{gtk-tree-store-set}
      @about-function{gtk-tree-store-set-valist}
      @about-function{gtk-tree-store-set-valuesv}
      @about-function{gtk-tree-store-remove}
      @about-function{gtk-tree-store-insert}
      @about-function{gtk-tree-store-insert-before}
      @about-function{gtk-tree-store-insert-after}
      @about-function{gtk-tree-store-insert-with-values}
      @about-function{gtk-tree-store-insert-with-valuesv}
      @about-function{gtk-tree-store-prepend}
      @about-function{gtk-tree-store-append}
      @about-function{gtk-tree-store-is-ancestor}
      @about-function{gtk-tree-store-iter-depth}
      @about-function{gtk-tree-store-clear}
      @about-function{gtk-tree-store-iter-is-valid}
      @about-function{gtk-tree-store-reorder}
      @about-function{gtk-tree-store-swap}
      @about-function{gtk-tree-store-move-before}
      @about-function{gtk-tree-store-move-after}
    @end{subsection}
  @end{section}
  @begin[Menus, Combo Box, Toolbar]{section}
    @begin[GtkComboBox]{subsection}
      A widget used to choose from a list of items.

      @about-class{gtk-combo-box}

      @about-generic{gtk-combo-box-active}
      @about-generic{gtk-combo-box-active-id}
      @about-generic{gtk-combo-box-add-tearoffs}
      @about-generic{gtk-combo-box-button-sensitivity}
      @about-generic{gtk-combo-box-cell-area}
      @about-generic{gtk-combo-box-column-span-column}
      @about-generic{gtk-combo-box-entry-text-column}
      @about-generic{gtk-combo-box-focus-on-click}
      @about-generic{gtk-combo-box-has-entry}
      @about-generic{gtk-combo-box-has-frame}
      @about-generic{gtk-combo-box-id-column}
      @about-generic{gtk-combo-box-model}
      @about-generic{gtk-combo-box-popup-fixed-width}
      @about-generic{gtk-combo-box-popup-shown}
      @about-generic{gtk-combo-box-row-span-column}
      @about-generic{gtk-combo-box-tearoff-title}
      @about-generic{gtk-combo-box-wrap-width}

      @about-function{gtk-combo-box-new}
      @about-function{gtk-combo-box-new-with-entry}
      @about-function{gtk-combo-box-new-with-model}
      @about-function{gtk-combo-box-new-with-model-and-entry}
      @about-function{gtk-combo-box-new-with-area}
      @about-function{gtk-combo-box-new-with-area-and-entry}
      @about-function{gtk-combo-box-get-wrap-width}
      @about-function{gtk-combo-box-set-wrap-width}
      @about-function{gtk-combo-box-get-row-span-column}
      @about-function{gtk-combo-box-set-row-span-column}
      @about-function{gtk-combo-box-get-column-span-column}
      @about-function{gtk-combo-box-set-column-span-column}
      @about-function{gtk-combo-box-get-active-iter}
      @about-function{gtk-combo-box-set-active-iter}
      @about-function{gtk-combo-box-get-id-column}
      @about-function{gtk-combo-box-set-id-column}
      @about-function{gtk-combo-box-get-active-id}
      @about-function{gtk-combo-box-set-active-id}
      @about-function{gtk-combo-box-get-model}
      @about-function{gtk-combo-box-set-model}
      @about-function{gtk-combo-box-popup-for-device}
      @about-function{gtk-combo-box-popup}
      @about-function{gtk-combo-box-popdown}
      @about-function{gtk-combo-box-get-popup-accessible}
      @about-function{gtk-combo-box-get-row-separator-func}
      @about-function{gtk-combo-box-set-row-separator-func}
      @about-function{gtk-combo-box-set-add-tearoffs}
      @about-function{gtk-combo-box-get-add-tearoffs}
      @about-function{gtk-combo-box-set-title}
      @about-function{gtk-combo-box-get-title}
      @about-function{gtk-combo-box-set-focus-on-click}
      @about-function{gtk-combo-box-get-focus-on-click}
      @about-function{gtk-combo-box-set-button-sensitivity}
      @about-function{gtk-combo-box-get-button-sensitivity}
      @about-function{gtk-combo-box-get-has-entry}
      @about-function{gtk-combo-box-set-entry-text-column}
      @about-function{gtk-combo-box-get-entry-text-column}
      @about-function{gtk-combo-box-set-popup-fixed-width}
      @about-function{gtk-combo-box-get-popup-fixed-width}
    @end{subsection}
    @begin[GtkComboBoxText]{subsection}
      A simple, text-only combo box.

      @about-class{gtk-combo-box-text}
      @about-function{gtk-combo-box-text-new}
      @about-function{gtk-combo-box-text-new-with-entry}
      @about-function{gtk-combo-box-text-append}
      @about-function{gtk-combo-box-text-prepend}
      @about-function{gtk-combo-box-text-insert}
      @about-function{gtk-combo-box-text-append-text}
      @about-function{gtk-combo-box-text-prepend-text}
      @about-function{gtk-combo-box-text-insert-text}
      @about-function{gtk-combo-box-text-remove}
      @about-function{gtk-combo-box-text-remove-all}
      @about-function{gtk-combo-box-text-get-active-text}
    @end{subsection}
    @begin[GtkMenu]{subsection}
      A menu widget

      @about-symbol{gtk-arrow-placement}
      @about-class{gtk-menu}

      @about-generic{gtk-combo-box-accel-group}
      @about-generic{gtk-combo-box-accel-path}
      @about-generic{gtk-combo-box-active}
      @about-generic{gtk-combo-box-anchor-hints}
      @about-generic{gtk-combo-box-attach-widget}
      @about-generic{gtk-combo-box-menu-type-hint}
      @about-generic{gtk-combo-box-monitor}
      @about-generic{gtk-combo-box-rect-anchor-dx}
      @about-generic{gtk-combo-box-rect-anchor-dy}
      @about-generic{gtk-combo-box-reserve-toggle-size}
      @about-generic{gtk-combo-box-tearoff-state}
      @about-generic{gtk-combo-box-tearoff-title}

      @about-function{gtk-menu-child-left-attach}
      @about-function{gtk-menu-child-right-attach}
      @about-function{gtk-menu-child-top-attach}
      @about-function{gtk-menu-child-bottom-attach}

      @about-function{gtk-menu-new}
      @about-function{gtk-menu-new-from-model}
      @about-function{gtk-menu-set-screen}
      @about-function{gtk-menu-reorder-child}
      @about-function{gtk-menu-attach}
      @about-function{gtk-menu-popup-for-device}
      @about-function{gtk-menu-popup}
      @about-function{gtk-menu-set-acrcel-group}
      @about-function{gtk-menu-get-accel-group}
      @about-function{gtk-menu-set-accel-path}
      @about-function{gtk-menu-get-accel-path}
      @about-function{gtk-menu-set-title}
      @about-function{gtk-menu-get-title}
      @about-function{gtk-menu-set-monitor}
      @about-function{gtk-menu-get-monitor}
      @about-function{gtk-menu-get-tearoff-state}
      @about-function{gtk-menu-set-reserve-toggle-size}
      @about-function{gtk-menu-get-reserve-toggle-size}
      @about-function{gtk-menu-popdown}
      @about-function{gtk-menu-reposition}
      @about-function{gtk-menu-get-active}
      @about-function{gtk-menu-set-active}
      @about-function{gtk-menu-set-tearoff-state}
      @about-function{gtk-menu-attach-to-widget}
      @about-function{gtk-menu-detach}
      @about-function{gtk-menu-get-attach-widget}
      @about-function{gtk-menu-get-for-attach-widget}
    @end{subsection}
    @begin[GtkMenuBar]{subsection}
      A subclass of @class{gtk-menu-shell} which holds @class{gtk-menu-item}
      widgets.

      @about-class{gtk-menu-bar}

      @about-generic{gtk-menu-bar-child-pack-direction}
      @about-generic{gtk-menu-bar-pack-direction}

      @about-function{gtk-menu-bar-new}
      @about-function{gtk-menu-bar-new-from-model}
      @about-symbol{gtk-pack-direction}
      @about-function{gtk-menu-bar-set-pack-direction}
      @about-function{gtk-menu-bar-get-pack-direction}
      @about-function{gtk-menu-bar-set-child-pack-direction}
      @about-function{gtk-menu-bar-get-child-pack-direction}
    @end{subsection}
    @begin[GtkMenuItem]{subsection}
      The widget used for item in menus.

      @about-class{gtk-menu-item}

      @about-generic{gtk-menu-item-accel-path}
      @about-generic{gtk-menu-item-label}
      @about-generic{gtk-menu-item-right-justified}
      @about-generic{gtk-menu-item-submenu}
      @about-generic{gtk-menu-item-use-underline}

      @about-function{gtk-menu-item-new}
      @about-function{gtk-menu-item-new-with-label}
      @about-function{gtk-menu-item-new-with-mnemonic}
      @about-function{gtk-menu-item-set-right-justified}
      @about-function{gtk-menu-item-get-right-justified}
      @about-function{gtk-menu-item-get-use-underline}
      @about-function{gtk-menu-item-set-use-underline}
      @about-function{gtk-menu-item-set-accel-path}
      @about-function{gtk-menu-item-get-accel-path}
      @about-function{gtk-menu-item-select}
      @about-function{gtk-menu-item-deselect}
      @about-function{gtk-menu-item-activate}
      @about-function{gtk-menu-item-toggle-size-request}
      @about-function{gtk-menu-item-toggle-size-allocate}
      @about-function{gtk-menu-item-get-reserve-indicator}
      @about-function{gtk-menu-item-set-reserve-indicator}
    @end{subsection}
    @begin[GtkRadioMenuItem]{subsection}
      A choice from multiple check menu items

      @about-class{gtk-radio-menu-item}

      @about-generic{gtk-radio-menu-item-group}

      @about-function{gtk-radio-menu-item-new}
      @about-function{gtk-radio-menu-item-new-with-label}
      @about-function{gtk-radio-menu-item-new-with-mnemonic}
      @about-function{gtk-radio-menu-item-new-from-widget}
      @about-function{gtk-radio-menu-item-new-with-label-from-widget}
      @about-function{gtk-radio-menu-item-new-with-mnemonic-from-widget}
      @about-function{gtk-radio-menu-item-set-group}
      @about-function{gtk-radio-menu-item-get-group}
    @end{subsection}
    @begin[GtkCheckMenuItem]{subsection}
      A menu item with a check box.

      @about-class{gtk-check-menu-item}

      @about-generic{gtk-check-menu-item-active}
      @about-generic{gtk-check-menu-item-draw-as-radio}
      @about-generic{gtk-check-menu-item-inconsistent}

      @about-function{gtk-check-menu-item-new}
      @about-function{gtk-check-menu-item-new-with-label}
      @about-function{gtk-check-menu-item-new-with-mnemonic}
      @about-function{gtk-check-menu-item-get-active}
      @about-function{gtk-check-menu-item-set-active}
      @about-function{gtk-check-menu-item-toggled}
      @about-function{gtk-check-menu-item-get-inconsistent}
      @about-function{gtk-check-menu-item-set-inconsistent}
      @about-function{gtk-check-menu-item-set-draw-as-radio}
      @about-function{gtk-check-menu-item-get-draw-as-radio}
    @end{subsection}
    @begin[GtkSeparatorMenuItem]{subsection}
      A separator used in menus.

      @about-class{gtk-separator-menu-item}
      @about-function{gtk-separator-menu-item-new}
    @end{subsection}
    @begin[GtkToolShell]{subsection}
      Interface for containers containing GtkToolItem widgets.

      @about-class{gtk-tool-shell}
      @about-class{gtk-tool-shell-iface}
      @about-function{gtk-tool-shell-get-ellipsize-mode}
      @about-function{gtk-tool-shell-get-icon-size}
      @about-function{gtk-tool-shell-get-orientation}
      @about-function{gtk-tool-shell-get-relief-style}
      @about-function{gtk-tool-shell-get-style}
      @about-function{gtk-tool-shell-get-text-alignment}
      @about-function{gtk-tool-shell-get-text-orientation}
      @about-function{gtk-tool-shell-rebuild-menu}
      @about-function{gtk-tool-shell-get-text-size-group}
    @end{subsection}
    @begin[GtkToolbar]{subsection}
      Create bars of buttons and other widgets.

      @about-symbol{gtk-toolbar-space-style}
      @about-class{gtk-toolbar}

      @about-generic{gtk-toolbar-icon-size}
      @about-generic{gtk-toolbar-icon-size-set}
      @about-generic{gtk-toolbar-show-arrow}
      @about-generic{gtk-toolbar-toolbar-style}

      @about-function{gtk-toolbar-child-expand}
      @about-function{gtk-toolbar-child-homogeneous}

      @about-function{gtk-toolbar-new}
      @about-function{gtk-toolbar-insert}
      @about-function{gtk-toolbar-get-item-index}
      @about-function{gtk-toolbar-get-n-items}
      @about-function{gtk-toolbar-get-nth-item}
      @about-function{gtk-toolbar-get-drop-index}
      @about-function{gtk-toolbar-set-drop-highlight-item}
      @about-function{gtk-toolbar-set-show-arrow}
      @about-function{gtk-toolbar-unset-icon-size}
      @about-function{gtk-toolbar-get-show-arrow}
      @about-function{gtk-toolbar-get-style}
      @about-function{gtk-toolbar-get-icon-size}
      @about-function{gtk-toolbar-get-relief-style}
      @about-function{gtk-toolbar-set-style}
      @about-function{gtk-toolbar-set-icon-size}
      @about-function{gtk-toolbar-unset-style}
    @end{subsection}
    @begin[GtkToolItem]{subsection}
      The base class of widgets that can be added to @class{gtk-tool-shell}.

      @about-class{gtk-tool-item}

      @about-generic{gtk-tool-item-is-important}
      @about-generic{gtk-tool-item-visible-horizontal}
      @about-generic{gtk-tool-item-visible-vertical}

      @about-function{gtk-tool-item-new}
      @about-function{gtk-tool-item-set-homogeneous}
      @about-function{gtk-tool-item-get-homogeneous}
      @about-function{gtk-tool-item-set-expand}
      @about-function{gtk-tool-item-get-expand}
      @about-function{gtk-tool-item-set-tooltip-text}
      @about-function{gtk-tool-item-set-tooltip-markup}
      @about-function{gtk-tool-item-set-use-drag-window}
      @about-function{gtk-tool-item-get-use-drag-window}
      @about-function{gtk-tool-item-set-visible-horizontal}
      @about-function{gtk-tool-item-get-visible-horizontal}
      @about-function{gtk-tool-item-set-visible-vertical}
      @about-function{gtk-tool-item-get-visible-vertical}
      @about-function{gtk-tool-item-set-is-important}
      @about-function{gtk-tool-item-get-is-important}
      @about-function{gtk-tool-item-get-ellipsize-mode}
      @about-function{gtk-tool-item-get-icon-size}
      @about-function{gtk-tool-item-get-orientation}
      @about-function{gtk-tool-item-get-toolbar-style}
      @about-function{gtk-tool-item-get-relief-style}
      @about-function{gtk-tool-item-get-text-alignment}
      @about-function{gtk-tool-item-get-text-orientation}
      @about-function{gtk-tool-item-retrieve-proxy-menu-item}
      @about-function{gtk-tool-item-get-proxy-menu-item}
      @about-function{gtk-tool-item-set-proxy-menu-item}
      @about-function{gtk-tool-item-rebuild-menu}
      @about-function{gtk-tool-item-toolbar-reconfigured}
      @about-function{gtk-tool-item-get-text-size-group}
    @end{subsection}
    @begin[GtkToolPalette]{subsection}
      A tool palette with categories.

      @about-class{gtk-tool-palette}

      @about-generic{gtk-tool-palette-icon-size}
      @about-generic{gtk-tool-palette-icon-size-set}
      @about-generic{gtk-tool-palette-toolbar-style}

      @about-function{gtk-tool-palette-child-exclusive}
      @about-function{gtk-tool-palette-child-expand}

      @about-function{gtk-tool-palette-new}
      @about-function{gtk-tool-palette-get-exclusive}
      @about-function{gtk-tool-palette-set-exclusive}
      @about-function{gtk-tool-palette-get-expand}
      @about-function{gtk-tool-palette-set-expand}
      @about-function{gtk-tool-palette-get-group-position}
      @about-function{gtk-tool-palette-set-group-position}
      @about-function{gtk-tool-palette-unset-icon-size}
      @about-function{gtk-tool-palette-get-style}
      @about-function{gtk-tool-palette-set-style}
      @about-function{gtk-tool-palette-unset-style}
      @about-function{gtk-tool-palette-add-drag-dest}
      @about-function{gtk-tool-palette-get-drag-item}
      @about-function{gtk-tool-palette-get-drag-target-group}
      @about-function{gtk-tool-palette-get-drag-target-item}
      @about-function{gtk-tool-palette-get-drop-group}
      @about-function{gtk-tool-palette-get-drop-item}
      @about-symbol{gtk-tool-palette-drag-targets}
      @about-function{gtk-tool-palette-set-drag-source}
      @about-function{gtk-tool-palette-get-hadjustment}
      @about-function{gtk-tool-palette-get-vadjustment}
    @end{subsection}
    @begin[GtkToolItemGroup]{subsection}
      A sub container used in a tool palette.

      @about-class{gtk-tool-item-group}

      @about-generic{gtk-tool-item-group-collapsed}
      @about-generic{gtk-tool-item-group-ellipsize}
      @about-generic{gtk-tool-item-group-header-relief}
      @about-generic{gtk-tool-item-group-label}
      @about-generic{gtk-tool-item-group-label-widget}

      @about-function{gtk-tool-item-group-child-expand}
      @about-function{gtk-tool-item-group-child-fill}
      @about-function{gtk-tool-item-group-child-homogeneous}
      @about-function{gtk-tool-item-group-child-new-row}
      @about-function{gtk-tool-item-group-child-position}

      @about-function{gtk-tool-item-group-get-collapsed}
      @about-function{gtk-tool-item-group-get-drop-item}
      @about-function{gtk-tool-item-group-get-ellipsize}
      @about-function{gtk-tool-item-group-get-item-position}
      @about-function{gtk-tool-item-group-get-n-items}
      @about-function{gtk-tool-item-group-get-label}
      @about-function{gtk-tool-item-group-get-label-widget}
      @about-function{gtk-tool-item-group-get-nth-item}
      @about-function{gtk-tool-item-group-get-header-relief}
      @about-function{gtk-tool-item-group-insert}
      @about-function{gtk-tool-item-group-new}
      @about-function{gtk-tool-item-group-set-collapsed}
      @about-function{gtk-tool-item-group-set-ellipsize}
      @about-function{gtk-tool-item-group-set-item-position}
      @about-function{gtk-tool-item-group-set-label}
      @about-function{gtk-tool-item-group-set-label-widget}
      @about-function{gtk-tool-item-group-set-header-relief}
    @end{subsection}
    @begin[GtkSeparatorToolItem]{subsection}
      A toolbar item that separates groups of other toolbar items.

      @about-class{gtk-separator-tool-item}

      @about-generic{gtk-separator-tool-item-draw}

      @about-function{gtk-separator-tool-item-new}
    @end{subsection}
    @begin[GtkToolButton]{subsection}
      A @class{gtk-tool-item} subclass that displays buttons.

      @about-class{gtk-tool-button}

      @about-generic{gtk-tool-button-icon-name}
      @about-generic{gtk-tool-button-icon-widget}
      @about-generic{gtk-tool-button-label}
      @about-generic{gtk-tool-button-label-widget}
      @about-generic{gtk-tool-button-stock-id}
      @about-generic{gtk-tool-button-use-underline}

      @about-function{gtk-tool-button-new}
      @about-function{gtk-tool-button-new-from-stock}
      @about-function{gtk-tool-button-set-label}
      @about-function{gtk-tool-button-get-label}
      @about-function{gtk-tool-button-set-use-underline}
      @about-function{gtk-tool-button-get-use-underline}
      @about-function{gtk-tool-button-set-stock-id}
      @about-function{gtk-tool-button-get-stock-id}
      @about-function{gtk-tool-button-set-icon-name}
      @about-function{gtk-tool-button-get-icon-name}
      @about-function{gtk-tool-button-set-icon-widget}
      @about-function{gtk-tool-button-get-icon-widget}
      @about-function{gtk-tool-button-set-label-widget}
      @about-function{gtk-tool-button-get-label-widget}
    @end{subsection}
    @begin[GtkMenuToolButton]{subsection}
      A @class{gtk-tool-item} containing a button with an additional dropdown
      menu.

      @about-class{gtk-menu-tool-button}
      @about-generic{gtk-menu-tool-button-menu}
      @about-function{gtk-menu-tool-button-new}
      @about-function{gtk-menu-tool-button-new-from-stock}
      @about-function{gtk-menu-tool-button-set-arrow-tooltip-text}
      @about-function{gtk-menu-tool-button-set-arrow-tooltip-markup}
    @end{subsection}
    @begin[GtkToogleToolButton]{subsection}
      A @class{gtk-tool-item} containing a toggle button.

      @about-class{gtk-toggle-tool-button}

      @about-generic{gtk-toggle-tool-button-active}

      @about-function{gtk-toggle-tool-button-new}
      @about-function{gtk-toggle-tool-button-new-from-stock}
    @end{subsection}
    @begin[GtkRadioToolButton]{subsection}
      A toolbar item that contains a radio button.

      @about-class{gtk-radio-tool-button}

      @about-generic{gtk-radio-tool-button-group}

      @about-function{gtk-radio-tool-button-new}
      @about-function{gtk-radio-tool-button-new-from-stock}
      @about-function{gtk-radio-tool-button-new-from-widget}
      @about-function{gtk-radio-tool-button-new-with-stock-from-widget}
      @about-function{gtk-radio-tool-button-get-group}
      @about-function{gtk-radio-tool-button-set-group}
    @end{subsection}
    @begin[GtkPopover]{subsection}

      @about-symbol{gtk-popover-constraint}

      @about-class{gtk-popover}

      @about-generic{gtk-popover-constrain-to}
      @about-generic{gtk-popover-modal}
      @about-generic{gtk-popover-pointing-to}
      @about-generic{gtk-popover-position}
      @about-generic{gtk-popover-relative-to}
      @about-generic{gtk-popover-transitions-enabled}

      @about-function{gtk-popover-new}
      @about-function{gtk-popover-new-from-model}
      @about-function{gtk-popover-bind-model}
      @about-function{gtk-popover-popup}
      @about-function{gtk-popover-popdown}
      @about-function{gtk-popover-set-default-widget}
      @about-function{gtk-popover-get-default-widget}
    @end{subsection}
    @begin[GtkPopoverMenu]{subsection}

      @about-class{gtk-popover-menu}
      @about-generic{gtk-popover-visible-submenu}
      @about-function{gtk-popover-menu-child-position}
      @about-function{gtk-popover-menu-child-submenu}
      @about-function{gtk-popover-menu-new}
      @about-function{gtk-popover-menu-open-submenu}
    @end{subsection}
  @end{section}
  @begin[Selectors (Color, File and Font)]{section}
    @begin[GtkColorChooser]{subsection}
      Interface implemented by widgets for choosing colors.

      @about-class{gtk-color-chooser}

      @about-generic{gtk-color-chooser-rgba}
      @about-generic{gtk-color-chooser-use-alpha}

      @about-function{gtk-color-chooser-add-palette}
    @end{subsection}
    @begin[GtkColorButton]{subsection}
      A button to launch a color selection dialog.

      @about-class{gtk-color-button}

      @about-generic{gtk-color-button-alpha}
      @about-generic{gtk-color-button-color}
      @about-generic{gtk-color-button-rgba}
      @about-generic{gtk-color-button-show-editor}
      @about-generic{gtk-color-button-title}
      @about-generic{gtk-color-button-use-alpha}

      @about-function{gtk-color-button-new}
      @about-function{gtk-color-button-new-with-color}
      @about-function{gtk-color-button-new-with-rgba}
    @end{subsection}
    @begin[GtkColorChooserWidget]{subsection}
      A widget for choosing colors.

      @about-class{gtk-color-chooser-widget}

      @about-generic{gtk-color-chooser-widget-show-editor}

      @about-function{gtk-color-chooser-widget-new}
    @end{subsection}
    @begin[GtkColorChooserDialog]{subsection}
      A dialog for choosing colors.

      @about-class{gtk-color-chooser-dialog}

      @about-generic{gtk-color-chooser-dialog-show-editor}

      @about-function{gtk-color-chooser-dialog-new}
    @end{subsection}
    @begin[GtkFileChooser]{subsection}
      File chooser interface used by @class{gtk-file-chooser-widget} and
      @class{gtk-file-chooser-dialog}.

      @about-symbol{gtk-file-chooser-action}
      @about-symbol{gtk-file-chooser-confirmation}
      @about-symbol{GTK_FILE_CHOOSER_ERROR}
      @about-symbol{gtk-file-chooser-error}

      @about-class{gtk-file-chooser}

      @about-generic{gtk-file-chooser-action}
      @about-generic{gtk-file-chooser-create-folders}
      @about-generic{gtk-file-chooser-do-overwrite-confirmation}
      @about-generic{gtk-file-chooser-extra-widget}
      @about-generic{gtk-file-chooser-filter}
      @about-generic{gtk-file-chooser-local-only}
      @about-generic{gtk-file-chooser-preview-widget}
      @about-generic{gtk-file-chooser-preview-widget-active}
      @about-generic{gtk-file-chooser-select-multiple}
      @about-generic{gtk-file-chooser-show-hidden}
      @about-generic{gtk-file-chooser-use-preview-label}

      @about-function{gtk-file-chooser-set-action}
      @about-function{gtk-file-chooser-get-action}
      @about-function{gtk-file-chooser-set-local-only}
      @about-function{gtk-file-chooser-get-local-only}
      @about-function{gtk-file-chooser-set-select-multiple}
      @about-function{gtk-file-chooser-get-select-multiple}
      @about-function{gtk-file-chooser-set-show-hidden}
      @about-function{gtk-file-chooser-get-show-hidden}
      @about-function{gtk-file-chooser-set-create-folders}
      @about-function{gtk-file-chooser-get-create-folders}
      @about-function{gtk-file-chooser-set-current-name}
      @about-function{gtk-file-chooser-get-filename}
      @about-function{gtk-file-chooser-set-filename}
      @about-function{gtk-file-chooser-select-filename}
      @about-function{gtk-file-chooser-unselect-filename}
      @about-function{gtk-file-chooser-select-all}
      @about-function{gtk-file-chooser-unselect-all}
      @about-function{gtk-file-chooser-get-filenames}
      @about-function{gtk-file-chooser-set-current-folder}
      @about-function{gtk-file-chooser-get-current-folder}
      @about-function{gtk-file-chooser-get-uri}
      @about-function{gtk-file-chooser-set-uri}
      @about-function{gtk-file-chooser-select-uri}
      @about-function{gtk-file-chooser-unselect-uri}
      @about-function{gtk-file-chooser-get-uris}
      @about-function{gtk-file-chooser-set-current-folder-uri}
      @about-function{gtk-file-chooser-get-current-folder-uri}
      @about-function{gtk-file-chooser-set-preview-widget}
      @about-function{gtk-file-chooser-get-preview-widget}
      @about-function{gtk-file-chooser-set-preview-widget-active}
      @about-function{gtk-file-chooser-get-preview-widget-active}
      @about-function{gtk-file-chooser-set-use-preview-label}
      @about-function{gtk-file-chooser-get-use-preview-label}
      @about-function{gtk-file-chooser-get-preview-filename}
      @about-function{gtk-file-chooser-get-preview-uri}
      @about-function{gtk-file-chooser-set-extra-widget}
      @about-function{gtk-file-chooser-get-extra-widget}
      @about-function{gtk-file-chooser-add-filter}
      @about-function{gtk-file-chooser-remove-filter}
      @about-function{gtk-file-chooser-list-filters}
      @about-function{gtk-file-chooser-set-filter}
      @about-function{gtk-file-chooser-get-filter}
      @about-function{gtk-file-chooser-add-shortcut-folder}
      @about-function{gtk-file-chooser-remove-shortcut-folder}
      @about-function{gtk-file-chooser-list-shortcut-folders}
      @about-function{gtk-file-chooser-add-shortcut-folder-uri}
      @about-function{gtk-file-chooser-remove-shortcut-folder-uri}
      @about-function{gtk-file-chooser-list-shortcut-folder-uris}
      @about-function{gtk-file-chooser-get-current-folder-file}
      @about-function{gtk-file-chooser-get-file}
      @about-function{gtk-file-chooser-get-files}
      @about-function{gtk-file-chooser-get-preview-file}
      @about-function{gtk-file-chooser-select-file}
      @about-function{gtk-file-chooser-set-current-folder-file}
      @about-function{gtk-file-chooser-set-file}
      @about-function{gtk-file-chooser-unselect-file}
    @end{subsection}
    @begin[GtkFileChooserButton]{subsection}
      A button to launch a file selection dialog.

      @about-class{gtk-file-chooser-button}

      @about-generic{gtk-file-chooser-button-dialog}
      @about-generic{gtk-file-chooser-button-focus-on-click}
      @about-generic{gtk-file-chooser-button-title}
      @about-generic{gtk-file-chooser-button-width-chars}

      @about-function{gtk-file-chooser-button-child-expand}
      @about-function{gtk-file-chooser-button-child-fill}
      @about-function{gtk-file-chooser-button-child-padding}
      @about-function{gtk-file-chooser-button-child-pack-type}
      @about-function{gtk-file-chooser-button-child-position}

      @about-function{gtk-file-chooser-button-new}
      @about-function{gtk-file-chooser-button-new-with-dialog}
      @about-function{gtk-file-chooser-button-get-title}
      @about-function{gtk-file-chooser-button-set-title}
      @about-function{gtk-file-chooser-button-get-width-chars}
      @about-function{gtk-file-chooser-button-set-width-chars}
      @about-function{gtk-file-chooser-button-get-focus-on-click}
      @about-function{gtk-file-chooser-button-set-focus-on-click}
    @end{subsection}
    @begin[GtkFileChooserNative]{subsection}
      A native file chooser dialog, suitable for “File/Open” or “File/Save”
      commands.

      @about-function{gtk-file-chooser-native-new}
      @about-function{gtk-file-chooser-native-get-accept-label}
      @about-function{gtk-file-chooser-native-set-accept-label}
      @about-function{gtk-file-chooser-native-get-cancel-label}
      @about-function{gtk-file-chooser-native-set-cancel-label}
    @end{subsection}
    @begin[GtkFileChooserDialog]{subsection}
      A file chooser dialog, suitable for \"File/Open\" or \"File/Save\"
      commands.

      @about-class{gtk-file-chooser-dialog}
      @about-function{gtk-file-chooser-dialog-new}
    @end{subsection}
    @begin[GtkFileChooserWidget]{subsection}
      File chooser widget that can be embedded in other widgets.

      @about-class{gtk-file-chooser-widget}

      @about-generic{gtk-file-chooser-widget-search-mode}
      @about-generic{gtk-file-chooser-widget-subtitle}

      @about-function{gtk-file-chooser-widget-child-expand}
      @about-function{gtk-file-chooser-widget-child-fill}
      @about-function{gtk-file-chooser-widget-child-padding}
      @about-function{gtk-file-chooser-widget-child-pack-type}
      @about-function{gtk-file-chooser-widget-child-position}

      @about-function{gtk-file-chooser-widget-new}
    @end{subsection}
    @begin[GtkFileFilter]{subsection}
      A filter for selecting a file subset.

      @about-class{gtk-file-filter}
      @about-symbol{gtk-file-filter-flags}
      @about-symbol{gtk-file-filter-info}

      @about-function{gtk-file-filter-new}
      @about-function{gtk-file-filter-set-name}
      @about-function{gtk-file-filter-get-name}
      @about-function{gtk-file-filter-add-mime-type}
      @about-function{gtk-file-filter-add-pattern}
      @about-function{gtk-file-filter-add-pixbuf-formats}
      @about-function{gtk-file-filter-add-custom}
      @about-function{gtk-file-filter-get-needed}
      @about-function{gtk-file-filter-filter}
      @about-function{gtk-file-filter-new-from-gvariant}
      @about-function{gtk-file-filter-to-gvariant}
    @end{subsection}
    @begin[GtkFontChooser]{subsection}
      Interface implemented by widgets displaying fonts.

      @about-class{gtk-font-chooser}

      @about-generic{gtk-font-chooser-font}
      @about-generic{gtk-font-chooser-font-desc}
      @about-generic{gtk-font-chooser-font-features}
      @about-generic{gtk-font-chooser-language}
      @about-generic{gtk-font-chooser-level}
      @about-generic{gtk-font-chooser-preview-text}
      @about-generic{gtk-font-chooser-show-preview-entry}

      @about-function{gtk-font-chooser-get-font-family}
      @about-function{gtk-font-chooser-get-font-face}
      @about-function{gtk-font-chooser-get-font-size}
      @about-function{gtk-font-chooser-get-preview-text}
      @about-function{gtk-font-chooser-set-preview-text}
      @about-function{gtk-font-chooser-get-show-preview-entry}
      @about-function{gtk-font-chooser-set-show-preview-entry}
      @about-function{gtk-font-chooser-set-filter-func}
    @end{subsection}
    @begin[GtkFontButton]{subsection}
      A button to launch a font chooser dialog.

      @about-class{gtk-font-button}

      @about-generic{gtk-font-button-font-name}
      @about-generic{gtk-font-button-show-size}
      @about-generic{gtk-font-button-show-style}
      @about-generic{gtk-font-button-title}
      @about-generic{gtk-font-button-use-font}
      @about-generic{gtk-font-button-use-size}

      @about-function{gtk-font-button-new}
      @about-function{gtk-font-button-new-with-font}
    @end{subsection}
    @begin[GtkFontChooserWidget]{subsection}
      A widget for selecting fonts.

      @about-class{gtk-font-chooser-widget}
      @about-function{gtk-font-chooser-widget-new}
    @end{subsection}
    @begin[GtkFontChooserDialog]{subsection}
      A dialog for selecting fonts.

      @about-class{gtk-font-chooser-dialog}
      @about-function{gtk-font-chooser-dialog-new}
    @end{subsection}

    @begin[GtkPlacesSidebar]{subsection}
      Sidebar that displays frequently-used places in the file system.

      @about-symbol{gtk-places-open-flags}

      @about-class{gtk-places-sidebar}

      @about-generic{gtk-places-sidebar-local-only}
      @about-generic{gtk-places-sidebar-location}
      @about-generic{gtk-places-sidebar-open-flags}
      @about-generic{gtk-places-sidebar-populate-all}
      @about-generic{gtk-places-sidebar-show-connect-to-server}
      @about-generic{gtk-places-sidebar-show-desktop}
      @about-generic{gtk-places-sidebar-show-enter-location}
      @about-generic{gtk-places-sidebar-show-other-locations}
      @about-generic{gtk-places-sidebar-show-recent}
      @about-generic{gtk-places-sidebar-show-starred-location}
      @about-generic{gtk-places-sidebar-show-trash}

      @about-function{gtk-places-sidebar-new}
      @about-function{gtk-places-sidebar-add-shortcut}
      @about-function{gtk-places-sidebar-remove-shortcut}
      @about-function{gtk-places-sidebar-list-shortcuts}
      @about-function{gtk-places-sidebar-get-nth-bookmark}
      @about-function{gtk-places-sidebar-set-drop-targets-visible}
    @end{subsection}
  @end{section}
  @begin[Ornaments]{section}
    @begin[GtkFrame]{subsection}
      A bin with a decorative frame and optional label.

      @about-class{gtk-frame}

      @about-generic{gtk-frame-label}
      @about-generic{gtk-frame-label-widget}
      @about-generic{gtk-frame-label-xalign}
      @about-generic{gtk-frame-label-yalign}
      @about-generic{gtk-frame-shadow-type}

      @about-function{gtk-frame-new}
      @about-function{gtk-frame-set-label-align}
      @about-function{gtk-frame-get-label-align}
    @end{subsection}
    @begin[GtkSeparator]{subsection}
      A separator widget.

      @about-class{gtk-separator}

      @about-function{gtk-separator-new}
    @end{subsection}
  @end{section}
  @begin[Scrolling]{section}
    @begin[GtkScrollbar]{subsection}
      A Scrollbar.

      @about-class{gtk-scrollbar}

      @about-function{gtk-scrollbar-new}
    @end{subsection}
    @begin[GtkScrolledWindow]{subsection}
      Adds scrollbars to its child widget.

      @about-symbol{gtk-policy-type}
      @about-symbol{gtk-corner-type}

      @about-class{gtk-scrolled-window}

      @about-generic{gtk-scrolled-window-hadjustment}
      @about-generic{gtk-scrolled-window-hscrollbar-policy}
      @about-generic{gtk-scrolled-window-kinetic-scrolling}
      @about-generic{gtk-scrolled-window-max-content-height}
      @about-generic{gtk-scrolled-window-max-content-width}
      @about-generic{gtk-scrolled-window-min-content-height}
      @about-generic{gtk-scrolled-window-min-content-width}
      @about-generic{gtk-scrolled-window-overlay-scrolling}
      @about-generic{gtk-scrolled-window-propagate-natural-height}
      @about-generic{gtk-scrolled-window-propagate-natural-width}
      @about-generic{gtk-scrolled-window-shadow-type}
      @about-generic{gtk-scrolled-window-vadjustment}
      @about-generic{gtk-scrolled-window-vscrollbar-policy}
      @about-generic{gtk-scrolled-window-window-placement}
      @about-generic{gtk-scrolled-window-window-placement-set}

      @about-function{gtk-scrolled-window-new}
      @about-function{gtk-scrolled-window-get-hscrollbar}
      @about-function{gtk-scrolled-window-get-vscrollbar}
      @about-function{gtk-scrolled-window-get-policy}
      @about-function{gtk-scrolled-window-set-policy}
      @about-function{gtk-scrolled-window-add-with-viewport}
      @about-function{gtk-scrolled-window-get-placement}
      @about-function{gtk-scrolled-window-set-placement}
      @about-function{gtk-scrolled-window-unset-placement}
      @about-function{gtk-scrolled-window-get-capture-button-press}
      @about-function{gtk-scrolled-window-set-capture-button-press}
    @end{subsection}
    @begin[GtkScrollable]{subsection}
      An interface for scrollable widgets.

      @about-symbol{gtk-scrollable-policy}
      @about-class{gtk-scrollable}

      @about-generic{gtk-scrollable-hadjustment}
      @about-generic{gtk-scrollable-vadjustment}
      @about-generic{gtk-scrollable-hscroll-policy}
      @about-generic{gtk-scrollable-vscroll-policy}

      @about-function{gtk-scrollable-get-border}
    @end{subsection}
  @end{section}
  @begin[Printing]{section}
    @begin[GtkPrintOperation]{subsection}
      High-level Printing API.

      @about-symbol{gtk-print-status}
      @about-symbol{gtk-print-operation-action}
      @about-symbol{gtk-print-operation-result}
      @about-symbol{gtk-print-error}
      @about-symbol{GTK_PRINT_ERROR}

      @about-class{gtk-print-operation}

      @about-generic{gtk-print-operation-allow-async}
      @about-generic{gtk-print-operation-current-page}
      @about-generic{gtk-print-operation-custom-tab-label}
      @about-generic{gtk-print-operation-default-page-setup}
      @about-generic{gtk-print-operation-embed-page-setup}
      @about-generic{gtk-print-operation-export-filename}
      @about-generic{gtk-print-operation-has-selection}
      @about-generic{gtk-print-operation-job-name}
      @about-generic{gtk-print-operation-n-pages}
      @about-generic{gtk-print-operation-n-pages-to-print}
      @about-generic{gtk-print-operation-print-settings}
      @about-generic{gtk-print-operation-show-progress}
      @about-generic{gtk-print-operation-status}
      @about-generic{gtk-print-operation-status-string}
      @about-generic{gtk-print-operation-support-selection}
      @about-generic{gtk-print-operation-track-print-status}
      @about-generic{gtk-print-operation-unit}
      @about-generic{gtk-print-operation-use-full-page}

      @about-function{gtk-print-operation-new}
      @about-function{gtk-print-operation-get-error}
      @about-function{gtk-print-operation-run}
      @about-function{gtk-print-operation-cancel}
      @about-function{gtk-print-operation-draw-page-finish}
      @about-function{gtk-print-operation-set-defer-drawing}
      @about-function{gtk-print-operation-is-finished}
      @about-function{gtk-print-run-page-setup-dialog}
      @about-function{gtk-print-run-page-setup-dialog-async}

      @about-class{gtk-print-operation-preview}

      @about-function{gtk-print-operation-preview-end-preview}
      @about-function{gtk-print-operation-preview-is-selected}
      @about-function{gtk-print-operation-preview-render-page}
    @end{subsection}
    @begin[GtkPrintContext]{subsection}
      Encapsulates context for drawing pages.

      @about-class{gtk-print-context}

      @about-function{gtk-print-context-get-cairo-context}
      @about-function{gtk-print-context-set-cairo-context}
      @about-function{gtk-print-context-get-page-setup}
      @about-function{gtk-print-context-get-width}
      @about-function{gtk-print-context-get-height}
      @about-function{gtk-print-context-get-dpi-x}
      @about-function{gtk-print-context-get-dpi-y}
      @about-function{gtk-print-context-get-pango-fontmap}
      @about-function{gtk-print-context-create-pango-context}
      @about-function{gtk-print-context-create-pango-layout}
      @about-function{gtk-print-context-get-hard-margins}
    @end{subsection}
    @begin[GtkPrintSettings]{subsection}
      Stores print settings.

      @about-class{gtk-print-settings}

      @about-function{gtk-print-settings-new}
      @about-function{gtk-print-settings-copy}
      @about-function{gtk-print-settings-has-key}
      @about-function{gtk-print-settings-get}
      @about-function{gtk-print-settings-set}
      @about-function{gtk-print-settings-unset}
      @about-function{gtk-print-settings-foreach}
      @about-function{gtk-print-settings-get-bool}
      @about-function{gtk-print-settings-set-bool}
      @about-function{gtk-print-settings-get-double}
      @about-function{gtk-print-settings-get-double-with-default}
      @about-function{gtk-print-settings-set-double}
      @about-function{gtk-print-settings-get-length}
      @about-function{gtk-print-settings-set-length}
      @about-function{gtk-print-settings-get-int}
      @about-function{gtk-print-settings-get-int-with-default}
      @about-function{gtk-print-settings-set-int}

      @about-function{GTK_PRINT_SETTINGS_PRINTER}
      @about-function{gtk-print-settings-get-printer}
      @about-function{gtk-print-settings-set-printer}

      @about-symbol{gtk-page-orientation}
      @about-function{GTK_PRINT_SETTINGS_ORIENTATION}
      @about-function{gtk-print-settings-get-orientation}
      @about-function{gtk-print-settings-set-orientation}
      @about-function{GTK_PRINT_SETTINGS_PAPER_FORMAT}
      @about-function{gtk-print-settings-get-paper-size}
      @about-function{gtk-print-settings-set-paper-size}
      @about-function{GTK_PRINT_SETTINGS_PAPER_WIDTH}
      @about-function{gtk-print-settings-get-paper-width}
      @about-function{gtk-print-settings-set-paper-width}
      @about-function{GTK_PRINT_SETTINGS_PAPER_HEIGHT}
      @about-function{gtk-print-settings-get-paper-height}
      @about-function{gtk-print-settings-set-paper-height}
      @about-function{GTK_PRINT_SETTINGS_USE_COLOR}
      @about-function{gtk-print-settings-get-use-color}
      @about-function{gtk-print-settings-set-use-color}
      @about-function{GTK_PRINT_SETTINGS_COLLATE}
      @about-function{gtk-print-settings-get-collate}
      @about-function{gtk-print-settings-set-collate}
      @about-function{GTK_PRINT_SETTINGS_REVERSE}
      @about-function{gtk-print-settings-get-reverse}
      @about-function{gtk-print-settings-set-reverse}

      @about-symbol{gtk-print-duplex}
      @about-function{GTK_PRINT_SETTINGS_DUPLEX}
      @about-function{gtk-print-settings-get-duplex}
      @about-function{gtk-print-settings-set-duplex}

      @about-symbol{gtk-print-quality}
      @about-function{GTK_PRINT_SETTINGS_QUALITY}
      @about-function{gtk-print-settings-get-quality}
      @about-function{gtk-print-settings-set-quality}
      @about-function{GTK_PRINT_SETTINGS_N_COPIES}
      @about-function{gtk-print-settings-get-n-copies}
      @about-function{gtk-print-settings-set-n-copies}
      @about-function{GTK_PRINT_SETTINGS_NUMBER_UP}
      @about-function{gtk-print-settings-get-number-up}
      @about-function{gtk-print-settings-set-number-up}

      @about-symbol{gtk-number-up-layout}
      @about-function{GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT}
      @about-function{gtk-print-settings-get-number-up-layout}
      @about-function{gtk-print-settings-set-number-up-layout}
      @about-function{GTK_PRINT_SETTINGS_RESOLUTION}
      @about-function{gtk-print-settings-get-resolution}
      @about-function{gtk-print-settings-set-resolution}
      @about-function{gtk-print-settings-set-resolution-xy}
      @about-function{GTK_PRINT_SETTINGS_RESOLUTION_X}
      @about-function{gtk-print-settings-get-resolution-x}
      @about-function{GTK_PRINT_SETTINGS_RESOLUTION_Y}
      @about-function{gtk-print-settings-get-resolution-y}
      @about-function{GTK_PRINT_SETTINGS_PRINTER_LPI}
      @about-function{gtk-print-settings-get-printer-lpi}
      @about-function{gtk-print-settings-set-printer-lpi}
      @about-function{GTK_PRINT_SETTINGS_SCALE}
      @about-function{gtk-print-settings-get-scale}
      @about-function{gtk-print-settings-set-scale}

      @about-symbol{gtk-print-pages}
      @about-function{GTK_PRINT_SETTINGS_PRINT_PAGES}
      @about-function{gtk-print-settings-get-print-pages}
      @about-function{gtk-print-settings-set-print-pages}

      @about-symbol{gtk-page-range}
      @about-function{GTK_PRINT_SETTINGS_PAGE_RANGES}
      @about-function{gtk-print-settings-get-page-ranges}
      @about-function{gtk-print-settings-set-page-ranges}

      @about-symbol{gtk-page-set}
      @about-function{GTK_PRINT_SETTINGS_PAGE_SET}
      @about-function{gtk-print-settings-get-page-set}
      @about-function{gtk-print-settings-set-page-set}
      @about-function{GTK_PRINT_SETTINGS_DEFAULT_SOURCE}
      @about-function{gtk-print-settings-get-default-source}
      @about-function{gtk-print-settings-set-default-source}
      @about-function{GTK_PRINT_SETTINGS_MEDIA_TYPE}
      @about-function{gtk-print-settings-get-media-type}
      @about-function{gtk-print-settings-set-media-type}
      @about-function{GTK_PRINT_SETTINGS_DITHER}
      @about-function{gtk-print-settings-get-dither}
      @about-function{gtk-print-settings-set-dither}
      @about-function{GTK_PRINT_SETTINGS_FINISHINGS}
      @about-function{gtk-print-settings-get-finishings}
      @about-function{gtk-print-settings-set-finishings}
      @about-function{GTK_PRINT_SETTINGS_OUTPUT_BIN}
      @about-function{gtk-print-settings-get-output-bin}
      @about-function{gtk-print-settings-set-output-bin}
      @about-function{GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT}
      @about-function{GTK_PRINT_SETTINGS_OUTPUT_URI}
      @about-function{GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA}
      @about-function{GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION}

      @about-function{gtk-print-settings-new-from-file}
      @about-function{gtk-print-settings-new-from-key-file}
      @about-function{gtk-print-settings-new-from-gvariant}
      @about-function{gtk-print-settings-load-file}
      @about-function{gtk-print-settings-load-key-file}
      @about-function{gtk-print-settings-to-file}
      @about-function{gtk-print-settings-to-key-file}
      @about-function{gtk-print-settings-to-gvariant}
    @end{subsection}
    @begin[GtkPageSetup]{subsection}
      Stores page setup information.

      @about-class{gtk-page-setup}
      @about-function{gtk-page-setup-new}
      @about-function{gtk-page-setup-copy}
      @about-function{gtk-page-setup-get-orientation}
      @about-function{gtk-page-setup-set-orientation}
      @about-function{gtk-page-setup-get-paper-size}
      @about-function{gtk-page-setup-set-paper-size}
      @about-function{gtk-page-setup-get-top-margin}
      @about-function{gtk-page-setup-set-top-margin}
      @about-function{gtk-page-setup-get-bottom-margin}
      @about-function{gtk-page-setup-set-bottom-margin}
      @about-function{gtk-page-setup-get-left-margin}
      @about-function{gtk-page-setup-set-left-margin}
      @about-function{gtk-page-setup-get-right-margin}
      @about-function{gtk-page-setup-set-right-margin}
      @about-function{gtk-page-setup-set-paper-size-and-default-margins}
      @about-function{gtk-page-setup-get-paper-width}
      @about-function{gtk-page-setup-get-paper-height}
      @about-function{gtk-page-setup-get-page-width}
      @about-function{gtk-page-setup-get-page-height}
      @about-function{gtk-page-setup-new-from-file}
      @about-function{gtk-page-setup-new-from-key-file}
      @about-function{gtk-page-setup-load-file}
      @about-function{gtk-page-setup-load-key-file}
      @about-function{gtk-page-setup-to-file}
      @about-function{gtk-page-setup-to-key-file}
    @end{subsection}
    @begin[GtkPaperSize]{subsection}
      Support for named paper sizes.

      @about-class{gtk-paper-size}
      @about-symbol{gtk-unit}
      @about-symbol{GTK-PAPER-NAME-A3}
      @about-symbol{GTK-PAPER-NAME-A4}
      @about-symbol{GTK-PAPER-NAME-A5}
      @about-symbol{GTK-PAPER-NAME-B5}
      @about-symbol{GTK-PAPER-NAME-LETTER}
      @about-symbol{GTK-PAPER-NAME-EXECUTIVE}
      @about-symbol{GTK-PAPER-NAME-LEGAL}
      @about-function{gtk-paper-size-new}
      @about-function{gtk-paper-size-new-from-ppd}
      @about-function{gtk-paper-size-new-custom}
      @about-function{gtk-paper-size-copy}
      @about-function{gtk-paper-size-free}
      @about-function{gtk-paper-size-is-equal}
      @about-function{gtk-paper-size-get-paper-sizes}
      @about-function{gtk-paper-size-get-name}
      @about-function{gtk-paper-size-get-display-name}
      @about-function{gtk-paper-size-get-ppd-name}
      @about-function{gtk-paper-size-get-width}
      @about-function{gtk-paper-size-get-height}
      @about-function{gtk-paper-size-is-custom}
      @about-function{gtk-paper-size-set-size}
      @about-function{gtk-paper-size-get-default-top-margin}
      @about-function{gtk-paper-size-get-default-bottom-margin}
      @about-function{gtk-paper-size-get-default-left-margin}
      @about-function{gtk-paper-size-get-default-right-margin}
      @about-function{gtk-paper-size-get-default}
      @about-function{gtk-paper-size-new-from-key-file}
      @about-function{gtk-paper-size-to-key-file}
    @end{subsection}
    @begin[GtkPrinter]{subsection}
      Represents a printer.

      @about-class{gtk-print-backend}

      @about-class{gtk-printer}

      @about-generic{gtk-printer-accepting-jobs}
      @about-generic{gtk-printer-accepts-pdf}
      @about-generic{gtk-printer-accepts-ps}
      @about-generic{gtk-printer-backend}
      @about-generic{gtk-printer-icon-name}
      @about-generic{gtk-printer-is-virtual}
      @about-generic{gtk-printer-job-count}
      @about-generic{gtk-printer-location}
      @about-generic{gtk-printer-name}
      @about-generic{gtk-printer-paused}
      @about-generic{gtk-printer-state-message}

      @about-function{gtk-printer-new}
      @about-function{gtk-printer-get-backend}
      @about-function{gtk-printer-get-name}
      @about-function{gtk-printer-get-state-message}
      @about-function{gtk-printer-get-description}
      @about-function{gtk-printer-get-location}
      @about-function{gtk-printer-get-icon-name}
      @about-function{gtk-printer-get-job-count}
      @about-function{gtk-printer-is-active}
      @about-function{gtk-printer-is-paused}
      @about-function{gtk-printer-is-accepting-jobs}
      @about-function{gtk-printer-is-default}
      @about-function{gtk-printer-list-papers}
      @about-function{gtk-printer-compare}
      @about-function{gtk-printer-has-details}
      @about-function{gtk-printer-request-details}
      @about-function{gtk-printer-get-capabilities}
      @about-function{gtk-printer-get-default-page-size}
      @about-function{gtk-printer-get-hard-margins}
      @about-function{gtk-enumerate-printers}
    @end{subsection}
    @begin[GtkPrintJob]{subsection}
      Represents a print job.

      @about-class{gtk-print-job}

      @about-generic{gtk-print-job-page-setup}
      @about-generic{gtk-print-job-printer}
      @about-generic{gtk-print-job-settings}
      @about-generic{gtk-print-job-title}
      @about-generic{gtk-print-job-track-print-status}

      @about-function{gtk-print-job-new}
      @about-function{gtk-print-job-get-settings}
      @about-function{gtk-print-job-get-printer}
      @about-function{gtk-print-job-get-title}
      @about-function{gtk-print-job-get-status}
      @about-function{gtk-print-job-set-source-file}
      @about-function{gtk-print-job-get-surface}
      @about-function{gtk-print-job-send}
      @about-function{gtk-print-job-set-track-print-status}
      @about-function{gtk-print-job-get-track-print-status}
      @about-function{gtk-print-job-get-pages}
      @about-function{gtk-print-job-set-pages}
      @about-function{gtk-print-job-get-page-ranges}
      @about-function{gtk-print-job-set-page-ranges}
      @about-function{gtk-print-job-get-page-set}
      @about-function{gtk-print-job-set-page-set}
      @about-function{gtk-print-job-get-num-copies}
      @about-function{gtk-print-job-set-num-copies}
      @about-function{gtk-print-job-get-scale}
      @about-function{gtk-print-job-set-scale}
      @about-function{gtk-print-job-get-n-up}
      @about-function{gtk-print-job-set-n-up}
      @about-function{gtk-print-job-get-n-up-layout}
      @about-function{gtk-print-job-set-n-up-layout}
      @about-function{gtk-print-job-get-rotate}
      @about-function{gtk-print-job-set-rotate}
      @about-function{gtk-print-job-get-collate}
      @about-function{gtk-print-job-set-collate}
      @about-function{gtk-print-job-get-reverse}
      @about-function{gtk-print-job-set-reverse}
    @end{subsection}
    @begin[GtkPrintUnixDialog]{subsection}
      A print dialog.

      @about-class{gtk-print-unix-dialog}

      @about-generic{gtk-print-unix-dialog-current-page}
      @about-generic{gtk-print-unix-dialog-embed-page-setup}
      @about-generic{gtk-print-unix-dialog-has-selection}
      @about-generic{gtk-print-unix-dialog-manual-capabilities}
      @about-generic{gtk-print-unix-dialog-page-setup}
      @about-generic{gtk-print-unix-dialog-print-settings}
      @about-generic{gtk-print-unix-dialog-selected-printer}
      @about-generic{gtk-print-unix-dialog-support-selection}

      @about-function{gtk-print-unix-dialog-new}
      @about-function{gtk-print-unix-dialog-set-page-setup}
      @about-function{gtk-print-unix-dialog-get-page-setup}
      @about-function{gtk-print-unix-dialog-set-current-page}
      @about-function{gtk-print-unix-dialog-get-current-page}
      @about-function{gtk-print-unix-dialog-set-settings}
      @about-function{gtk-print-unix-dialog-get-settings}
      @about-function{gtk-print-unix-dialog-get-selected-printer}
      @about-function{gtk-print-unix-dialog-add-custom-tab}
      @about-function{gtk-print-unix-dialog-set-support-selection}
      @about-function{gtk-print-unix-dialog-get-support-selection}
      @about-function{gtk-print-unix-dialog-set-has-selection}
      @about-function{gtk-print-unix-dialog-get-has-selection}
      @about-function{gtk-print-unix-dialog-set-embed-page-setup}
      @about-function{gtk-print-unix-dialog-get-embed-page-setup}
      @about-function{gtk-print-unix-dialog-get-page-setup-set}
      @about-symbol{gtk-print-capabilities}
      @about-function{gtk-print-unix-dialog-set-manual-capabilities}
      @about-function{gtk-print-unix-dialog-get-manual-capabilities}
    @end{subsection}
    @begin[GtkPageSetupUnixDialog]{subsection}
      A page setup dialog.

      @about-class{gtk-page-setup-unix-dialog}

      @about-function{gtk-page-setup-unix-dialog-new}
      @about-function{gtk-page-setup-unix-dialog-set-page-setup}
      @about-function{gtk-page-setup-unix-dialog-get-page-setup}
      @about-function{gtk-page-setup-unix-dialog-set-print-settings}
      @about-function{gtk-page-setup-unix-dialog-get-print-settings}
    @end{subsection}
  @end{section}
  @begin[Shortcuts Overview]{section}
    @begin[GtkShortcutsWindow]{subsection}
      Toplevel which shows help for shortcuts

      @about-class{gtk-shortcuts-window}

      @about-generic{gtk-shortcuts-window-section-name}
      @about-generic{gtk-shortcuts-window-view-name}
    @end{subsection}
    @begin[GtkShortcutsSection]{subsection}
      Represents an application mode in a GtkShortcutsWindow

      @about-class{gtk-shortcuts-section}

      @about-generic{gtk-shortcuts-section-max-height}
      @about-generic{gtk-shortcuts-section-section-name}
      @about-generic{gtk-shortcuts-section-title}
      @about-generic{gtk-shortcuts-section-view-name}
    @end{subsection}
    @begin[GtkShortcutsGroup]{subsection}
      Represents a group of shortcuts in a GtkShortcutsWindow

      @about-class{gtk-shortcuts-group}

      @about-generic{gtk-shortcuts-group-accel-size-group}
      @about-generic{gtk-shortcuts-group-height}
      @about-generic{gtk-shortcuts-group-title}
      @about-generic{gtk-shortcuts-group-title-size-group}
      @about-generic{gtk-shortcuts-group-view}
    @end{subsection}
    @begin[GtkShortcutsShortcut]{subsection}
      Represents a keyboard shortcut in a GtkShortcutsWindow

      @about-symbol{gtk-shortcut-type}
      @about-class{gtk-shortcuts-shortcut}

      @about-generic{gtk-shortcuts-shortcut-accel-size-group}
      @about-generic{gtk-shortcuts-shortcut-accelerator}
      @about-generic{gtk-shortcuts-shortcut-action-name}
      @about-generic{gtk-shortcuts-shortcut-direction}
      @about-generic{gtk-shortcuts-shortcut-icon}
      @about-generic{gtk-shortcuts-shortcut-icon-set}
      @about-generic{gtk-shortcuts-shortcut-shortcut-type}
      @about-generic{gtk-shortcuts-shortcut-subtitle}
      @about-generic{gtk-shortcuts-shortcut-subtitle-set}
      @about-generic{gtk-shortcuts-shortcut-title}
      @about-generic{gtk-shortcuts-shortcut-title-size-group}
    @end{subsection}
  @end{section}
  @begin[Miscellaneous]{section}
    @begin[GtkAdjustment]{subsection}
      A representation of an adjustable bounded value.

      @about-class{gtk-adjustment}

      @about-generic{gtk-adjustment-lower}
      @about-generic{gtk-adjustment-page-increment}
      @about-generic{gtk-adjustment-page-size}
      @about-generic{gtk-adjustment-step-increment}
      @about-generic{gtk-adjustment-upper}
      @about-generic{gtk-adjustment-value}

      @about-function{gtk-adjustment-new}
      @about-function{gtk-adjustment-get-value}
      @about-function{gtk-adjustment-set-value}
      @about-function{gtk-adjustment-clamp-page}
      @about-function{gtk-adjustment-changed}
      @about-function{gtk-adjustment-value-changed}
      @about-function{gtk-adjustment-configure}
      @about-function{gtk-adjustment-get-lower}
      @about-function{gtk-adjustment-get-page-increment}
      @about-function{gtk-adjustment-get-page-size}
      @about-function{gtk-adjustment-get-step-increment}
      @about-function{gtk-adjustment-get-minimum-increment}
      @about-function{gtk-adjustment-get-upper}
      @about-function{gtk-adjustment-set-lower}
      @about-function{gtk-adjustment-set-page-increment}
      @about-function{gtk-adjustment-set-page-size}
      @about-function{gtk-adjustment-set-step-increment}
      @about-function{gtk-adjustment-set-upper}
    @end{subsection}
    @begin[GtkCalendar]{subsection}
      Displays a calendar and allows the user to select a date.

      @about-symbol{gtk-calendar-display-options}

      @about-class{gtk-calendar}

      @about-generic{gtk-calendar-day}
      @about-generic{gtk-calendar-detail-height-rows}
      @about-generic{gtk-calendar-detail-width-chars}
      @about-generic{gtk-calendar-month}
      @about-generic{gtk-calendar-no-month-change}
      @about-generic{gtk-calendar-show-day-names}
      @about-generic{gtk-calendar-show-details}
      @about-generic{gtk-calendar-show-heading}
      @about-generic{gtk-calendar-show-week-numbers}
      @about-generic{gtk-calendar-year}

      @about-function{gtk-calendar-new}
      @about-function{gtk-calendar-select-month}
      @about-function{gtk-calendar-select-day}
      @about-function{gtk-calendar-mark-day}
      @about-function{gtk-calendar-unmark-day}
      @about-function{gtk-calendar-get-day-is-marked}
      @about-function{gtk-calendar-clear-marks}
      @about-function{gtk-calendar-get-display-options}
      @about-function{gtk-calendar-set-display-options}
      @about-function{gtk-calendar-get-date}
      @about-function{gtk-calendar-set-detail-func}
    @end{subsection}
    @begin[GtkDrawingArea]{subsection}
      A widget for custom user interface elements.

      @about-class{gtk-drawing-area}
      @about-function{gtk-drawing-area-new}
    @end{subsection}
    @begin[GtkEventBox]{subsection}
      A widget used to catch events for widgets which do not have their own
      window.

      @about-class{gtk-event-box}

      @about-generic{gtk-event-box-above-child}
      @about-generic{gtk-event-box-visible-window}

      @about-function{gtk-event-box-new}
    @end{subsection}
    @begin[GtkIMContexSimple]{subsection}
      An input method context supporting table-based input methods.

      @about-class{gtk-im-context-simple}
      @about-function{gtk-im-context-simple-new}
      @about-function{gtk-im-context-simple-add-table}
    @end{subsection}
    @begin[GtkIMMulticontex]{subsection}
      An input method context supporting multiple, loadable input methods.

      @about-class{gtk-im-multicontext}
      @about-function{gtk-im-multicontext-new}
      @about-function{gtk-im-multicontext-append-menuitems}
      @about-function{gtk-im-multicontext-get-context-id}
      @about-function{gtk-im-multicontext-set-context-id}
    @end{subsection}
    @begin[GtkSizeGroup]{subsection}
      Grouping widgets so they request the same size.

      @about-symbol{gtk-size-group-mode}

      @about-class{gtk-size-group}

      @about-generic{gtk-size-group-ignore-hidden}
      @about-generic{gtk-size-group-mode}

      @about-function{gtk-size-group-new}
      @about-function{gtk-size-group-add-widget}
      @about-function{gtk-size-group-remove-widget}
      @about-function{gtk-size-group-get-widgets}
    @end{subsection}
    @begin[GtkTooltip]{subsection}
      Add tips to your widgets.

      @about-class{gtk-tooltip}
      @about-function{gtk-tooltip-set-markup}
      @about-function{gtk-tooltip-set-text}
      @about-function{gtk-tooltip-set-icon}
      @about-function{gtk-tooltip-set-icon-from-stock}
      @about-function{gtk-tooltip-set-icon-from-icon-name}
      @about-function{gtk-tooltip-set-icon-from-gicon}
      @about-function{gtk-tooltip-set-custom}
      @about-function{gtk-tooltip-trigger-tooltip-query}
      @about-function{gtk-tooltip-set-tip-area}
    @end{subsection}
    @begin[GtkViewport]{subsection}
      An adapter which makes widgets scrollable.

      @about-class{gtk-viewport}

      @about-generic{gtk-viewport-shadow-type}

      @about-function{gtk-viewport-new}
      @about-function{gtk-viewport-get-hadjustment}
      @about-function{gtk-viewport-get-vadjustment}
      @about-function{gtk-viewport-set-hadjustment}
      @about-function{gtk-viewport-set-vadjustment}
      @about-function{gtk-viewport-get-bin-window}
      @about-function{gtk-viewport-get-view-window}
    @end{subsection}
    @begin[GtkAccessible]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Abstract Base Classes]{section}
    @begin[GtkWidget]{subsection}
      Base class for all widgets.

      @about-class{gtk-widget}

      @about-generic{gtk-widget-app-paintable}
      @about-generic{gtk-widget-can-default}
      @about-generic{gtk-widget-can-focus}
      @about-generic{gtk-widget-composite-child}
      @about-generic{gtk-widget-double-buffered}
      @about-generic{gtk-widget-events}
      @about-generic{gtk-widget-expand}
      @about-generic{gtk-widget-halign}
      @about-generic{gtk-widget-has-default}
      @about-generic{gtk-widget-has-focus}
      @about-generic{gtk-widget-has-tooltip}
      @about-generic{gtk-widget-height-request}
      @about-generic{gtk-widget-hexpand}
      @about-generic{gtk-widget-hexpand-set}
      @about-generic{gtk-widget-is-focus}
      @about-generic{gtk-widget-margin}
      @about-generic{gtk-widget-margin-bottom}
      @about-generic{gtk-widget-margin-end}
      @about-generic{gtk-widget-margin-left}
      @about-generic{gtk-widget-margin-right}
      @about-generic{gtk-widget-margin-start}
      @about-generic{gtk-widget-margin-top}
      @about-generic{gtk-widget-name}
      @about-generic{gtk-widget-no-show-all}
      @about-generic{gtk-widget-opacity}
      @about-generic{gtk-widget-parent}
      @about-generic{gtk-widget-receives-default}
      @about-generic{gtk-widget-scale-factor}
      @about-generic{gtk-widget-sensitive}
      @about-generic{gtk-widget-style}
      @about-generic{gtk-widget-tooltip-markup}
      @about-generic{gtk-widget-tooltip-text}
      @about-generic{gtk-widget-valign}
      @about-generic{gtk-widget-vexpand}
      @about-generic{gtk-widget-vexpand-set}
      @about-generic{gtk-widget-visible}
      @about-generic{gtk-widget-width-request}
      @about-generic{gtk-widget-window}

      @about-struct{gtk-requisition}
      @about-function{make-gtk-requisition}
      @about-function{copy-gtk-requisition}
      @about-function{gtk-requisition-width}
      @about-function{gtk-requisition-height}
      @about-symbol{gtk-align}
      @about-symbol{gtk-widget-aux-info}
      @about-symbol{gtk-widget-help-type}
      @about-symbol{gtk-text-direction}
      @about-function{gtk-widget-new}
      @about-function{gtk-widget-destroy}
      @about-function{gtk-widget-in-destruction}
      @about-function{gtk-widget-destroyed}
      @about-function{gtk-widget-unparent}
      @about-function{gtk-widget-show}
      @about-function{gtk-widget-show-now}
      @about-function{gtk-widget-hide}
      @about-function{gtk-widget-show-all}
      @about-function{gtk-widget-map}
      @about-function{gtk-widget-unmap}
      @about-function{gtk-widget-realize}
      @about-function{gtk-widget-unrealize}
      @about-function{gtk-widget-draw}
      @about-function{gtk-widget-queue-draw}
      @about-function{gtk-widget-queue-resize}
      @about-function{gtk-widget-queue-resize-no-redraw}
      @about-function{gtk-widget-get-frame-clock}
      @about-function{gtk-widget-add-tick-callback}
      @about-function{gtk-widget-remove-tick-callback}
      @about-generic{gtk-widget-size-request}
      @about-function{gtk-widget-get-child-requisition}
      @about-function{gtk-widget-size-allocate}
      @about-function{gtk-widget-add-accelerator}
      @about-function{gtk-widget-remove-accelerator}
      @about-function{gtk-widget-set-accel-path}
      @about-function{gtk-widget-list-accel-closures}
      @about-function{gtk-widget-can-activate-accel}
      @about-function{gtk-widget-event}
      @about-function{gtk-widget-activate}
      @about-function{gtk-widget-reparent}
      @about-function{gtk-widget-intersect}
      @about-function{gtk-widget-is-focus}
      @about-function{gtk-widget-grab-focus}
      @about-function{gtk-widget-grab-default}
      @about-function{gtk-widget-set-state}
      @about-function{gtk-widget-set-parent-window}
      @about-function{gtk-widget-get-parent-window}
      @about-function{gtk-widget-add-events}
      @about-function{gtk-widget-set-device-events}
      @about-function{gtk-widget-get-device-events}
      @about-function{gtk-widget-add-device-events}
      @about-function{gtk-widget-set-device-enabled}
      @about-function{gtk-widget-get-device-enabled}
      @about-function{gtk-widget-get-toplevel}
      @about-function{gtk-widget-get-ancestor}
      @about-function{gtk-widget-get-visual}
      @about-function{gtk-widget-set-visual}
      @about-function{gtk-widget-get-pointer}
      @about-function{gtk-widget-is-ancestor}
      @about-function{gtk-widget-translate-coordinates}
      @about-function{gtk-widget-hide-on-delete}
      @about-function{gtk-widget-set-style}
      @about-function{gtk-widget-ensure-style}
      @about-function{gtk-widget-get-style}
      @about-function{gtk-widget-reset-rc-styles}
      @about-function{gtk-widget-get-default-style}
      @about-function{gtk-widget-set-direction}
      @about-function{gtk-widget-get-direction}
      @about-function{gtk-widget-set-default-direction}
      @about-function{gtk-widget-get-default-direction}
      @about-function{gtk-widget-shape-combine-region}
      @about-function{gtk-widget-input-shape-combine-region}
      @about-function{gtk-widget-path}
      @about-function{gtk-widget-class-path}
      @about-function{gtk-widget-get-composite-name}
      @about-function{gtk-widget-override-background-color}
      @about-function{gtk-widget-override-color}
      @about-function{gtk-widget-override-font}
      @about-function{gtk-widget-override-symbolic-color}
      @about-function{gtk-widget-override-cursor}
      @about-function{gtk-widget-modify-style}
      @about-function{gtk-widget-get-modifier-style}
      @about-function{gtk-widget-modify-fg}
      @about-function{gtk-widget-modify-bg}
      @about-function{gtk-widget-modify-text}
      @about-function{gtk-widget-modify-base}
      @about-function{gtk-widget-modify-font}
      @about-function{gtk-widget-modify-cursor}
      @about-function{gtk-widget-create-pango-context}
      @about-function{gtk-widget-get-pango-context}
      @about-function{gtk-widget-create-pango-layout}
      @about-function{gtk-widget-render-icon}
      @about-function{gtk-widget-render-icon-pixbuf}
      @about-function{gtk-widget-pop-composite-child}
      @about-function{gtk-widget-push-composite-child}
      @about-function{gtk-widget-queue-draw-area}
      @about-function{gtk-widget-queue-draw-region}
      @about-function{gtk-widget-set-app-paintable}
      @about-function{gtk-widget-set-double-buffered}
      @about-function{gtk-widget-set-redraw-on-allocate}
      @about-function{gtk-widget-set-composite-name}
      @about-function{gtk-widget-mnemonic-activate}
      @about-function{gtk-widget-class-install-style-property}
      @about-function{gtk-widget-class-install-style-property-parser}
      @about-function{gtk-widget-class-find-style-property}
      @about-function{gtk-widget-class-list-style-properties}
      @about-function{gtk-widget-region-intersect}
      @about-function{gtk-widget-send-expose}
      @about-function{gtk-widget-send-focus-change}
      @about-function{gtk-widget-style-get}
      @about-function{gtk-widget-style-get-property}
      @about-function{gtk-widget-style-get-valist}
      @about-function{gtk-widget-style-attach}
      @about-function{gtk-widget-class-set-accessible-type}
      @about-function{gtk-widget-class-set-accessible-role}
      @about-function{gtk-widget-get-accessible}
      @about-function{gtk-widget-child-focus}
      @about-function{gtk-widget-child-notify}
      @about-function{gtk-widget-freeze-child-notify}
      @about-function{gtk-widget-get-child-visible}
      @about-function{gtk-widget-get-settings}
      @about-function{gtk-widget-get-clipboard}
      @about-function{gtk-widget-get-display}
      @about-function{gtk-widget-get-root-window}
      @about-function{gtk-widget-get-screen}
      @about-function{gtk-widget-has-screen}
      @about-function{gtk-widget-set-child-visible}
      @about-function{gtk-widget-thaw-child-notify}
      @about-function{gtk-widget-list-mnemonic-labels}
      @about-function{gtk-widget-add-mnemonic-label}
      @about-function{gtk-widget-remove-mnemonic-label}
      @about-function{gtk-widget-is-composited}
      @about-function{gtk-widget-error-bell}
      @about-function{gtk-widget-keynav-failed}
      @about-function{gtk-widget-get-tooltip-window}
      @about-function{gtk-widget-set-tooltip-window}
      @about-function{gtk-widget-trigger-tooltip-query}
      @about-function{gtk-widget-get-window}
      @about-function{gtk-widget-register-window}
      @about-function{gtk-widget-unregister-window}
      @about-function{gtk-cairo-should-draw-window}
      @about-function{gtk-cairo-transform-to-window}
      @about-function{gtk-widget-get-allocated-width}
      @about-function{gtk-widget-get-allocated-height}
      @about-function{gtk-widget-get-allocation}
      @about-function{gtk-widget-set-allocation}
      @about-function{gtk-widget-get-app-paintable}
      @about-function{gtk-widget-get-can-focus}
      @about-function{gtk-widget-set-can-focus}
      @about-function{gtk-widget-get-double-buffered}
      @about-function{gtk-widget-get-has-window}
      @about-function{gtk-widget-set-has-window}
      @about-function{gtk-widget-is-sensitive}
      @about-function{gtk-widget-get-state}
      @about-function{gtk-widget-is-visible}
      @about-function{gtk-widget-set-visible}
      @about-function{gtk-widget-set-state-flags}
      @about-function{gtk-widget-unset-state-flags}
      @about-function{gtk-widget-get-state-flags}
      @about-function{gtk-widget-has-default}
      @about-function{gtk-widget-has-focus}
      @about-function{gtk-widget-has-visible-focus}
      @about-function{gtk-widget-has-grab}
      @about-function{gtk-widget-has-rc-style}
      @about-function{gtk-widget-is-drawable}
      @about-function{gtk-widget-is-toplevel}
      @about-function{gtk-widget-set-support-multidevice}
      @about-function{gtk-widget-get-support-multidevice}
      @about-function{gtk-widget-set-realized}
      @about-function{gtk-widget-get-realized}
      @about-function{gtk-widget-set-mapped}
      @about-function{gtk-widget-get-mapped}
      @about-function{gtk-widget-get-requisition}
      @about-function{gtk-widget-device-is-shadowed}
      @about-function{gtk-widget-get-modifier-mask}
      @about-function{gtk-widget-insert-action-group}
      @about-function{gtk-widget-get-path}
      @about-function{gtk-widget-get-style-context}
      @about-function{gtk-widget-reset-style}
      @about-function{gtk-requisition-new}
      @about-function{gtk-requisition-copy}
      @about-function{gtk-requisition-free}
      @about-symbol{gtk-size-request-mode}
      @about-symbol{gtk-requested-size}
      @about-function{copy-gtk-requested-size}
      @about-function{make-gtk-requested-size}
      @about-function{gtk-requested-size-data}
      @about-function{gtk-requested-size-minimum-size}
      @about-function{gtk-requested-size-natural-size}
      @about-function{gtk-widget-get-preferred-height}
      @about-function{gtk-widget-get-preferred-width}
      @about-function{gtk-widget-get-preferred-height-for-width}
      @about-function{gtk-widget-get-preferred-width-for-height}
      @about-function{gtk-widget-get-request-mode}
      @about-function{gtk-widget-get-preferred-size}
      @about-function{gtk-distribute-natural-allocation}
      @about-function{gtk-widget-get-valign}
      @about-function{gtk-widget-set-valign}
      @about-function{gtk-widget-get-vexpand}
      @about-function{gtk-widget-set-vexpand}
      @about-function{gtk-widget-get-vexpand-set}
      @about-function{gtk-widget-set-vexpand-set}
      @about-function{gtk-widget-queue-compute-expand}
      @about-function{gtk-widget-compute-expand}
    @end{subsection}
    @begin[GtkContainer]{subsection}
      Base class for widgets which contain other widgets.

      @about-class{gtk-container}

      @about-generic{gtk-container-border-width}
      @about-generic{gtk-container-child}
      @about-generic{gtk-container-resize-mode}

      @about-function{gtk-container-add}
      @about-function{gtk-container-remove}
      @about-function{gtk-container-add-with-properties}
      @about-function{gtk-container-get-resize-mode}
      @about-function{gtk-container-set-resize-mode}
      @about-function{gtk-container-check-resize}
      @about-function{gtk-container-foreach}
      @about-function{gtk-container-get-children}
      @about-function{gtk-container-get-path-for-child}
      @about-function{gtk-container-set-reallocate-redraws}
      @about-function{gtk-container-get-focus-child}
      @about-function{gtk-container-set-focus-child}
      @about-function{gtk-container-get-focus-vadjustment}
      @about-function{gtk-container-set-focus-vadjustment}
      @about-function{gtk-container-get-focus-hadjustment}
      @about-function{gtk-container-set-focus-hadjustment}
      @about-function{gtk-container-resize-children}
      @about-function{gtk-container-child-type}
      @about-function{gtk-container-child-get}
      @about-function{gtk-container-child-set}
      @about-function{gtk-container-child-get-property}
      @about-function{gtk-container-child-set-property}
      @about-function{gtk-container-child-get-valist}
      @about-function{gtk-container-child-set-valist}
      @about-function{gtk-container-child-notify}
      @about-function{gtk-container-forall}
      @about-function{gtk-container-propagate-draw}
      @about-function{gtk-container-get-focus-chain}
      @about-function{gtk-container-set-focus-chain}
      @about-function{gtk-container-unset-focus-chain}
      @about-function{gtk-container-class-find-child-property}
      @about-function{gtk-container-class-install-child-property}
      @about-function{gtk-container-class-list-child-properties}
      @about-function{gtk-container-class-handle-border-width}
    @end{subsection}
    @begin[GtkBin]{subsection}
      A container with just one child.

      @about-class{gtk-bin}
      @about-function{gtk-bin-get-child}
    @end{subsection}
    @begin[GtkMenuShell]{subsection}
      A base class for menu objects.

      @about-class{gtk-menu-shell}

      @about-generic{gtk-menu-shell-take-focus}

      @about-function{gtk-menu-shell-append}
      @about-function{gtk-menu-shell-prepend}
      @about-function{gtk-menu-shell-insert}
      @about-function{gtk-menu-shell-deactivate}
      @about-function{gtk-menu-shell-select-item}
      @about-function{gtk-menu-shell-select-first}
      @about-function{gtk-menu-shell-deselect}
      @about-function{gtk-menu-shell-activate-item}
      @about-function{gtk-menu-shell-cancel}
      @about-function{gtk-menu-shell-get-selected-item}
      @about-function{gtk-menu-shell-get-parent-shell}
      @about-function{gtk-menu-shell-bind-model}
      @about-symbol{gtk-menu-direction-type}
    @end{subsection}
    @begin[GtkRange]{subsection}
      Base class for widgets which visualize an adjustment

      @about-class{gtk-range}

      @about-generic{gtk-range-adjustment}
      @about-generic{gtk-range-fill-level}
      @about-generic{gtk-range-inverted}
      @about-generic{gtk-range-lower-stepper-sensitivity}
      @about-generic{gtk-range-restrict-to-fill-level}
      @about-generic{gtk-range-round-digits}
      @about-generic{gtk-range-show-fill-level}
      @about-generic{gtk-range-upper-stepper-sensitivity}

      @about-function{gtk-range-get-fill-level}
      @about-function{gtk-range-get-restrict-to-fill-level}
      @about-function{gtk-range-get-show-fill-level}
      @about-function{gtk-range-set-fill-level}
      @about-function{gtk-range-set-restrict-to-fill-level}
      @about-function{gtk-range-set-show-fill-level}
      @about-function{gtk-range-get-inverted}
      @about-function{gtk-range-set-inverted}
      @about-function{gtk-range-get-value}
      @about-function{gtk-range-set-value}
      @about-function{gtk-range-set-increments}
      @about-function{gtk-range-set-range}
      @about-function{gtk-range-get-round-digits}
      @about-function{gtk-range-set-round-digits}
      @about-symbol{gtk-sensitivity-type}
      @about-function{gtk-range-set-lower-stepper-sensitivity}
      @about-function{gtk-range-get-lower-stepper-sensitivity}
      @about-function{gtk-range-set-upper-stepper-sensitivity}
      @about-function{gtk-range-get-upper-stepper-sensitivity}
      @about-function{gtk-range-get-flippable}
      @about-function{gtk-range-set-flippable}
      @about-function{gtk-range-get-min-slider-size}
      @about-function{gtk-range-get-range-rect}
      @about-function{gtk-range-get-slider-range}
      @about-function{gtk-range-get-slider-size-fixed}
      @about-function{gtk-range-set-min-slider-size}
      @about-function{gtk-range-set-slider-size-fixed}
    @end{subsection}
    @begin[GtkIMContext]{subsection}
      Base class for input method contexts.

      @about-class{gtk-im-context}

      @about-generic{gtk-im-context-input-hints}
      @about-generic{gtk-im-context-input-purpose}

      @about-symbol{gtk-im-contextInfo}

      @about-function{gtk-im-context-set-client-window}
      @about-function{gtk-im-context-get-preedit-string}
      @about-function{gtk-im-context-filter-keypress}
      @about-function{gtk-im-context-focus-in}
      @about-function{gtk-im-context-focus-out}
      @about-function{gtk-im-context-reset}
      @about-function{gtk-im-context-set-cursor-location}
      @about-function{gtk-im-context-set-use-preedit}
      @about-function{gtk-im-context-set-surrounding}
      @about-function{gtk-im-context-get-surrounding}
      @about-function{gtk-im-context-delete-surrounding}
    @end{subsection}
  @end{section}
  @begin[Cross-process Embedding]{section}
    @begin[GtkPlug]{subsection}
      Toplevel for embedding into other processes.

      @about-class{gtk-plug}

      @about-generic{gtk-plug-embedded}
      @about-generic{gtk-plug-socket-window}

      @about-function{gtk-plug-construct}
      @about-function{gtk-plug-construct-for-display}
      @about-function{gtk-plug-new}
      @about-function{gtk-plug-new-for-display}
      @about-function{gtk-plug-get-id}
    @end{subsection}
    @begin[GtkSocket]{subsection}
      Container for widgets from other processes.

      @about-class{gtk-socket}

      @about-function{gtk-socket-new}
      @about-function{gtk-socket-add-id}
      @about-function{gtk-socket-get-id}
      @about-function{gtk-socket-get-plug-window}
    @end{subsection}
  @end{section}
  @begin[Recently Used Documents]{section}
    @begin[GtkRecentManager]{subsection}
      Managing recently used files.

      @about-class{gtk-recent-info}
      @about-class{gtk-recent-data}
      @about-symbol{gtk-recent-manager-error}

      @about-class{gtk-recent-manager}

      @about-generic{gtk-recent-manager-filename}
      @about-generic{gtk-recent-manager-size}

      @about-function{gtk-recent-manager-new}
      @about-function{gtk-recent-manager-get-default}
      @about-function{gtk-recent-manager-add-item}
      @about-function{gtk-recent-manager-add-full}
      @about-function{gtk-recent-manager-remove-item}
      @about-function{gtk-recent-manager-lookup-item}
      @about-function{gtk-recent-manager-has-item}
      @about-function{gtk-recent-manager-move-item}
      @about-function{gtk-recent-manager-get-items}
      @about-function{gtk-recent-manager-purge-items}

      @about-function{gtk-recent-info-ref}
      @about-function{gtk-recent-info-unref}
      @about-function{gtk-recent-info-get-uri}
      @about-function{gtk-recent-info-get-display-name}
      @about-function{gtk-recent-info-get-description}
      @about-function{gtk-recent-info-get-mime-type}
      @about-function{gtk-recent-info-get-added}
      @about-function{gtk-recent-info-get-modified}
      @about-function{gtk-recent-info-get-visited}
      @about-function{gtk-recent-info-get-private-hint}
      @about-function{gtk-recent-info-get-application-info}
      @about-function{gtk-recent-info-get-applications}
      @about-function{gtk-recent-info-last-application}
      @about-function{gtk-recent-info-has-application}
      @about-function{gtk-recent-info-create-app-info}
      @about-function{gtk-recent-info-get-groups}
      @about-function{gtk-recent-info-has-group}
      @about-function{gtk-recent-info-get-icon}
      @about-function{gtk-recent-info-get-gicon}
      @about-function{gtk-recent-info-get-short-name}
      @about-function{gtk-recent-info-get-uri-display}
      @about-function{gtk-recent-info-get-age}
      @about-function{gtk-recent-info-is-local}
      @about-function{gtk-recent-info-exists}
      @about-function{gtk-recent-info-match}
    @end{subsection}
    @begin[GtkRecentChooser]{subsection}
      Interface implemented by widgets displaying recently used files.

      @about-symbol{gtk-recent-sort-type}
      @about-symbol{GTK_RECENT_CHOOSER_ERROR}
      @about-symbol{gtk-recent-chooser-error}

      @about-class{gtk-recent-chooser}

      @about-generic{gtk-recent-chooser-filter}
      @about-generic{gtk-recent-chooser-limit}
      @about-generic{gtk-recent-chooser-local-only}
      @about-generic{gtk-recent-chooser-recent-manager}
      @about-generic{gtk-recent-chooser-select-multiple}
      @about-generic{gtk-recent-chooser-show-icons}
      @about-generic{gtk-recent-chooser-show-not-found}
      @about-generic{gtk-recent-chooser-show-private}
      @about-generic{gtk-recent-chooser-show-tips}
      @about-generic{gtk-recent-chooser-sort-type}

      @about-function{gtk-recent-chooser-set-show-private}
      @about-function{gtk-recent-chooser-get-show-private}
      @about-function{gtk-recent-chooser-set-show-not-found}
      @about-function{gtk-recent-chooser-get-show-not-found}
      @about-function{gtk-recent-chooser-set-show-icons}
      @about-function{gtk-recent-chooser-get-show-icons}
      @about-function{gtk-recent-chooser-set-select-multiple}
      @about-function{gtk-recent-chooser-get-select-multiple}
      @about-function{gtk-recent-chooser-set-local-only}
      @about-function{gtk-recent-chooser-get-local-only}
      @about-function{gtk-recent-chooser-set-limit}
      @about-function{gtk-recent-chooser-get-limit}
      @about-function{gtk-recent-chooser-set-show-tips}
      @about-function{gtk-recent-chooser-get-show-tips}

      @about-function{gtk-recent-chooser-set-sort-type}
      @about-function{gtk-recent-chooser-get-sort-type}
      @about-function{gtk-recent-chooser-set-sort-func}
      @about-function{gtk-recent-chooser-set-current-uri}
      @about-function{gtk-recent-chooser-get-current-uri}
      @about-function{gtk-recent-chooser-get-current-item}
      @about-function{gtk-recent-chooser-select-uri}
      @about-function{gtk-recent-chooser-unselect-uri}
      @about-function{gtk-recent-chooser-select-all}
      @about-function{gtk-recent-chooser-unselect-all}
      @about-function{gtk-recent-chooser-get-items}
      @about-function{gtk-recent-chooser-get-uris}
      @about-function{gtk-recent-chooser-add-filter}
      @about-function{gtk-recent-chooser-remove-filter}
      @about-function{gtk-recent-chooser-list-filters}
      @about-function{gtk-recent-chooser-set-filter}
      @about-function{gtk-recent-chooser-get-filter}
    @end{subsection}
    @begin[GtkRecentChooserDialog]{subsection}
      Displays recently used files in a dialog.

      @about-class{gtk-recent-chooser-dialog}

      @about-function{gtk-recent-chooser-dialog-new}
      @about-function{gtk-recent-chooser-dialog-new-for-manager}
    @end{subsection}
    @begin[GtkRecentChooserMenu]{subsection}
      Displays recently used files in a menu.

      @about-class{gtk-recent-chooser-menu}

      @about-function{gtk-recent-chooser-menu-new}
      @about-function{gtk-recent-chooser-menu-new-for-manager}
      @about-function{gtk-recent-chooser-menu-get-show-numbers}
      @about-function{gtk-recent-chooser-menu-set-show-numbers}
    @end{subsection}
    @begin[GtkRecentChooserWidget]{subsection}
      Displays recently used files.

      @about-class{gtk-recent-chooser-widget}

      @about-function{gtk-recent-chooser-widget-new}
      @about-function{gtk-recent-chooser-widget-new-for-manager}
    @end{subsection}
    @begin[GtkRecentFilter]{subsection}
      A filter for selecting a subset of recently used files.

      @about-symbol{gtk-recent-filter-info}
      @about-symbol{gtk-recent-filter-flags}
      @about-class{gtk-recent-filter}

      @about-function{gtk-recent-filter-new}
      @about-function{gtk-recent-filter-get-name}
      @about-function{gtk-recent-filter-set-name}
      @about-function{gtk-recent-filter-add-mime-type}
      @about-function{gtk-recent-filter-add-pattern}
      @about-function{gtk-recent-filter-add-pixbuf-formats}
      @about-function{gtk-recent-filter-add-application}
      @about-function{gtk-recent-filter-add-group}
      @about-function{gtk-recent-filter-add-age}
      @about-function{gtk-recent-filter-add-custom}
      @about-function{gtk-recent-filter-get-needed}
      @about-function{gtk-recent-filter-filter}
    @end{subsection}
  @end{section}
  @begin[Choosing from installed applications]{section}
    @begin[GtkAppChooser]{subsection}
      Interface implemented by widgets for choosing an application.

      @about-class{gtk-app-chooser}

      @about-generic{gtk-app-chooser-content-type}

      @about-function{gtk-app-chooser-get-app-info}
      @about-function{gtk-app-chooser-get-content-type}
      @about-function{gtk-app-chooser-refresh}
    @end{subsection}
    @begin[GtkAppChooserButton]{subsection}
      A button to launch an application chooser dialog.

      @about-class{gtk-app-chooser-button}

      @about-generic{gtk-app-chooser-button-heading}
      @about-generic{gtk-app-chooser-button-show-default-item}
      @about-generic{gtk-app-chooser-button-show-dialog-item}

      @about-function{gtk-app-chooser-button-new}
      @about-function{gtk-app-chooser-button-append-custom-item}
      @about-function{gtk-app-chooser-button-append-separator}
      @about-function{gtk-app-chooser-button-set-active-custom-item}
      @about-function{gtk-app-chooser-button-get-show-default-item}
      @about-function{gtk-app-chooser-button-set-show-default-item}
      @about-function{gtk-app-chooser-button-get-show-dialog-item}
      @about-function{gtk-app-chooser-button-set-show-dialog-item}
      @about-function{gtk-app-chooser-button-get-heading}
      @about-function{gtk-app-chooser-button-set-heading}
    @end{subsection}
    @begin[GtkAppChooserDialog]{subsection}
      An application chooser dialog.

      @about-class{gtk-app-chooser-dialog}

      @about-generic{gtk-app-chooser-dialog-gfile}
      @about-generic{gtk-app-chooser-dialog-heading}

      @about-function{gtk-app-chooser-dialog-new}
      @about-function{gtk-app-chooser-dialog-new-for-content-type}
      @about-function{gtk-app-chooser-dialog-get-widget}
      @about-function{gtk-app-chooser-dialog-set-heading}
      @about-function{gtk-app-chooser-dialog-get-heading}
    @end{subsection}
    @begin[GtkAppChooserWidget]{subsection}
      Application chooser widget that can be embedded in other widgets.

      @about-class{gtk-app-chooser-widget}

      @about-generic{gtk-app-chooser-widget-default-text}
      @about-generic{gtk-app-chooser-widget-show-all}
      @about-generic{gtk-app-chooser-widget-show-default}
      @about-generic{gtk-app-chooser-widget-show-fallback}
      @about-generic{gtk-app-chooser-widget-show-other}
      @about-generic{gtk-app-chooser-widget-show-recommended}

      @about-function{gtk-app-chooser-widget-new}
      @about-function{gtk-app-chooser-widget-set-show-default}
      @about-function{gtk-app-chooser-widget-get-show-default}
      @about-function{gtk-app-chooser-widget-set-show-recommended}
      @about-function{gtk-app-chooser-widget-get-show-recommended}
      @about-function{gtk-app-chooser-widget-set-show-fallback}
      @about-function{gtk-app-chooser-widget-get-show-fallback}
      @about-function{gtk-app-chooser-widget-set-show-other}
      @about-function{gtk-app-chooser-widget-get-show-other}
      @about-function{gtk-app-chooser-widget-set-show-all}
      @about-function{gtk-app-chooser-widget-get-show-all}
      @about-function{gtk-app-chooser-widget-set-default-text}
      @about-function{gtk-app-chooser-widget-get-default-text}
    @end{subsection}
  @end{section}
  @begin[Gestures and event handling]{section}
    @begin[GtkEventController]{subsection}
      Self-contained handler of series of events

      @about-symbol{gtk-propagation-phase}
      @about-class{gtk-event-controller}

      @about-generic{gtk-event-controller-propagation-phase}
      @about-generic{gtk-event-controller-widget}

      @about-function{gtk-event-controller-handle-event}
      @about-function{gtk-event-controller-reset}
    @end{subsection}
    @begin[GtkEventControllerKey]{subsection}
      Event controller for key events

      @about-class{gtk-event-controller-key}
      @about-function{gtk-event-controller-key-new}
    @end{subsection}
    @begin[GtkEventControllerScroll]{subsection}
      Event controller for scroll events

      @about-symbol{gtk-event-controller-scroll-flags}
      @about-class{gtk-event-controller-scroll}

      @about-generic{gtk-event-controller-scroll-flags}

      @about-function{gtk-event-controller-scroll-new}
    @end{subsection}

    @begin[GtkEventControllerMotion]{subsection}
      Event controller for motion events

      @about-class{gtk-event-controller-motion}

      @about-function{gtk-event-controller-motion-new}
    @end{subsection}
    @begin[GtkGesture]{subsection}
      Base class for gestures

      @about-symbol{gtk-event-sequence-state}
      @about-class{gtk-gesture}
      @about-generic{gtk-gesture-n-points}
      @about-generic{gtk-gesture-window}
      @about-function{gtk-gesture-get-device}
      @about-function{gtk-gesture-is-active}
      @about-function{gtk-gesture-is-recognized}
      @about-function{gtk-gesture-get-sequence-state}
      @about-function{gtk-gesture-set-sequence-state}
      @about-function{gtk-gesture-set-state}
      @about-function{gtk-gesture-get-sequences}
      @about-function{gtk-gesture-handles-sequence}
      @about-function{gtk-gesture-get-last-updated-sequence}
      @about-function{gtk-gesture-get-last-event}
      @about-function{gtk-gesture-get-point}
      @about-function{gtk-gesture-get-bounding-box}
      @about-function{gtk-gesture-get-bounding-box-center}
      @about-function{gtk-gesture-group}
      @about-function{gtk-gesture-ungroup}
      @about-function{gtk-gesture-get-group}
      @about-function{gtk-gesture-is-grouped-with}
    @end{subsection}
    @begin[GtkGestureSingle]{subsection}
      Base class for mouse/single-touch gestures.

      @about-class{gtk-gesture-single}
      @about-generic{gtk-gesture-single-button}
      @about-generic{gtk-gesture-single-exclusive}
      @about-generic{gtk-gesture-single-touch-only}
      @about-function{gtk-gesture-single-get-current-button}
      @about-function{gtk-gesture-single-get-current-sequence}
    @end{subsection}

    @begin[GtkGestureDrag]{subsection}
      Drag gesture

      @about-class{gtk-gesture-drag}

      @about-function{gtk-gesture-drag-new}
      @about-function{gtk-gesture-drag-get-start-point}
      @about-function{gtk-gesture-drag-get-offset}
    @end{subsection}

    @begin[GtkGestureLongPress]{subsection}
      \"Press and Hold\" gesture

      @about-class{gtk-gesture-long-press}

      @about-generic{gtk-gesture-long-press-delay-factor}

      @about-function{gtk-gesture-long-press-new}
    @end{subsection}

    @begin[GtkGestureMultiPress]{subsection}
      Multipress gesture.

      @about-class{gtk-gesture-multi-press}

      @about-function{gtk-gesture-multi-press-new}
      @about-function{gtk-gesture-multi-press-set-area}
      @about-function{gtk-gesture-multi-press-get-area}
    @end{subsection}

    @begin[GtkGesturePan]{subsection}
      Pan gesture

      @about-symbol{gtk-pan-direction}
      @about-class{gtk-gesture-pan}

      @about-generic{gtk-gesture-pan-orientation}

      @about-function{gtk-gesture-pan-new}
    @end{subsection}

    @begin[GtkGestureSwipe]{subsection}
      Swipe gesture

      @about-class{gtk-gesture-swipe}

      @about-function{gtk-gesture-swipe-new}
      @about-function{gtk-gesture-swipe-get-velocity}
    @end{subsection}
    @begin[GtkGestureRotate]{subsection}
      Rotate gesture.

      @about-class{gtk-gesture-rotate}
      @about-function{gtk-gesture-rotate-new}
      @about-function{gtk-gesture-rotate-get-angle-delta}
    @end{subsection}
    @begin[GtkGestureZoom]{subsection}
      Zoom gesture.

      @about-class{gtk-gesture-zoom}

      @about-function{gtk-gesture-zoom-new}
      @about-function{gtk-gesture-zoom-get-scale-delta}
    @end{subsection}

    @begin[GtkGestureStylus]{subsection}
      Gesture for stylus input

      @about-class{gtk-gesture-stylus}

      @about-function{gtk-gesture-stylus-new}
      @about-function{gtk-gesture-stylus-get-axis}
      @about-function{gtk-gesture-stylus-get-axes}
      @about-function{gtk-gesture-stylus-get-device-tool}
    @end{subsection}

    @begin[GtkPadController]{subsection}
      Controller for drawing tablet pads

      @about-symbol{gtk-pad-action-type}
      @about-symbol{gtk-pad-action-entry}
      @about-class{gtk-pad-controller}

      @about-generic{gtk-pad-controller-action-group}
      @about-generic{gtk-pad-controller-pad}

      @about-function{gtk-pad-controller-new}
      @about-function{gtk-pad-controller-set-action-entries}
      @about-function{gtk-pad-controller-set-action}
    @end{subsection}
  @end{section}
  @begin[GTK+ Core Reference]{section}
    @begin[Main loop and Events]{subsection}
      Library initialization, main event loop, and events.

      Before using GTK+, it needs to be initialized; initialization connects to
      the window system display, and parses some standard command line
      arguments. In the C library the macro @code{gtk_init()} initializes GTK+.
      In the Lisp binding to GTK+, GTK+ is initialized, when loading the
      @code{cl-cffi-gtk} library. Therefore, no functions are exported, which
      initialize GTK+.

      Like all GUI toolkits, GTK+ uses an event-driven programming model. When
      the user is doing nothing, GTK+ sits in the main loop and waits for input.
      If the user performs some action - say, a mouse click - then the main loop
      \"wakes up\" and delivers an event to GTK+. GTK+ forwards the event to one
      or more widgets.

      In the C library the main loop is executed with the function
      @code{gtk_main()}. In the Lisp binding this function is implemented as
      the function @fun{gtk-main}, but in general it is not used. The function
      @code{gtk_main()} is replaced with the macro @fun{within-main-loop}, which
      does all necessary work to run the main loop. See the example for a
      typical main function in the Lisp binding.

      When widgets receive an event, they frequently emit one or more signals.
      Signals notify your program that \"something interesting happened\" by
      invoking functions you have connected to the signal with the function
      @fun{g-signal-connect}. Functions connected to a signal are often termed
      callbacks.

      When your callbacks are invoked, you would typically take some action -
      for example, when an Open button is clicked you might display a
      @class{gtk-file-chooser-dialog} window. After a callback finishes, GTK+
      will return to the main loop and await more user input.

      @b{Example:} Typical main function in Lisp for a GTK+ application.
      @begin{pre}
(defun main ()
  (within-main-loop
    (let (;; Create the main window.
          (window (gtk-window-new :toplevel)))

      ;; Set up the GUI elements
      ...

      ;; Show the application window.
      (gtk-widget-show-all window))))
      @end{pre}

      @about-function{gtk-disable-setlocale}
      @about-function{gtk-get-default-language}
      @about-function{gtk-parse-args}
      @about-function{gtk-init}
      @about-function{gtk-init-check}
      @about-function{gtk-init-with-args}
      @about-function{gtk-get-option-group}
      @about-function{gtk-events-pending}
      @about-function{gtk-main}
      @about-function{gtk-main-level}
      @about-function{gtk-main-quit}
      @about-function{gtk-main-iteration}
      @about-function{gtk-main-iteration-do}
      @about-function{gtk-main-do-event}
      @about-function{gtk-true}
      @about-function{gtk-false}
      @about-function{gtk-grab-add}
      @about-function{gtk-grab-get-current}
      @about-function{gtk-grab-remove}
      @about-function{gtk-device-grab-add}
      @about-function{gtk-device-grab-remove}
      @about-function{gtk-priority-resize}
      @about-function{gtk-key-snooper-install}
      @about-function{gtk-key-snooper-remove}
      @about-function{gtk-get-current-event}
      @about-function{gtk-get-current-event-time}
      @about-function{gtk-get-current-event-state}
      @about-function{gtk-get-current-event-device}
      @about-function{gtk-get-event-widget}
      @about-function{gtk-propagate-event}
    @end{subsection}
    @begin[Version Information]{subsection}
      GTK+ provides version information, primarily useful in configure checks
      for builds that have a configure script. Applications will not typically
      use the features described here.

      @about-function{gtk-get-major-version}
      @about-function{gtk-get-minor-version}
      @about-function{gtk-get-micro-version}
      @about-function{gtk-get-binary-age}
      @about-function{gtk-get-interface-age}
      @about-function{gtk-check-version}
      @about-function{cl-cffi-gtk-build-info}
      @about-variable{+gtk-minor-version+}
      @about-variable{+gtk-micro-version+}
      @about-variable{+gtk-binary-age+}
      @about-variable{+gtk-interface-age+}
    @end{subsection}
    @begin[Accelerator Groups]{subsection}
      Groups of global keyboard accelerators for an entire @class{gtk-window}
      widget.

      @about-symbol{gtk-accel-flags}
      @about-class{gtk-accel-group}
      @about-function{gtk-accel-group-new}
      @about-function{gtk-accel-group-connect}
      @about-function{gtk-accel-group-connect-by-path}
      @about-function{gtk-accel-group-disconnect}
      @about-function{gtk-accel-group-disconnect-key}
      @about-function{gtk-accel-group-activate}
      @about-function{gtk-accel-group-lock}
      @about-function{gtk-accel-group-unlock}
      @about-function{gtk-accel-group-from-accel-closure}
      @about-function{gtk-accel-groups-activate}
      @about-function{gtk-accel-groups-from-object}
      @about-function{gtk-accel-group-find}
      @about-symbol{gtk-accel-key}
      @about-function{gtk-accelerator-valid}
      @about-function{gtk-accelerator-parse}
      @about-function{gtk-accelerator-name}
      @about-function{gtk-accelerator-get-label}
      @about-function{gtk-accelerator-parse-with-keycode}
      @about-function{gtk-accelerator-name-with-keycode}
      @about-function{gtk-accelerator-get-label-with-keycode}
      @about-function{gtk-accelerator-set-default-mod-mask}
      @about-function{gtk-accelerator-get-default-mod-mask}
    @end{subsection}
    @begin[Accelerator Maps]{subsection}
      Loadable keyboard accelerator specifications.

      @about-class{gtk-accel-map}
      @about-function{gtk-accel-map-add-entry}
      @about-function{gtk-accel-map-lookup-entry}
      @about-function{gtk-accel-map-change-entry}
      @about-function{gtk-accel-map-load}
      @about-function{gtk-accel-map-save}
      @about-function{gtk-accel-map-foreach}
      @about-function{gtk-accel-map-load-fd}
      @about-function{gtk-accel-map-save-fd}
      @about-function{gtk-accel-map-load-scanner}
      @about-function{gtk-accel-map-add-filter}
      @about-function{gtk-accel-map-foreach-unfiltered}
      @about-function{gtk-accel-map-get}
      @about-function{gtk-accel-map-lock-path}
      @about-function{gtk-accel-map-unlock-path}
    @end{subsection}
    @begin[GtkClipboard]{subsection}
      Storing data on clipboards.

      @about-class{gtk-clipboard}
      @about-function{gtk-clipboard-get}
      @about-function{gtk-clipboard-get-for-display}
      @about-function{gtk-clipboard-get-display}
      @about-function{gtk-clipboard-set-with-data}
      @about-function{gtk-clipboard-set-with-owner}
      @about-function{gtk-clipboard-get-owner}
      @about-function{gtk-clipboard-clear}
      @about-function{gtk-clipboard-set-text}
      @about-function{gtk-clipboard-set-image}
      @about-function{gtk-clipboard-request-contents}
      @about-function{gtk-clipboard-request-text}
      @about-function{gtk-clipboard-request-image}
      @about-function{gtk-clipboard-request-targets}
      @about-function{gtk-clipboard-request-rich-text}
      @about-function{gtk-clipboard-request-uris}
      @about-function{gtk-clipboard-wait-for-contents}
      @about-function{gtk-clipboard-wait-for-text}
      @about-function{gtk-clipboard-wait-for-image}
      @about-function{gtk-clipboard-wait-for-rich-text}
      @about-function{gtk-clipboard-wait-for-uris}
      @about-function{gtk-clipboard-wait-is-text-available}
      @about-function{gtk-clipboard-wait-is-image-available}
      @about-function{gtk-clipboard-wait-is-rich-text-available}
      @about-function{gtk-clipboard-wait-is-uris-available}
      @about-function{gtk-clipboard-wait-for-targets}
      @about-function{gtk-clipboard-wait-is-target-available}
      @about-function{gtk-clipboard-set-can-store}
      @about-function{gtk-clipboard-store}
    @end{subsection}
    @begin[Drag and drop handling]{subsection}
      GTK+ has a rich set of functions for doing inter-process communication via
      the drag-and-drop metaphor. GTK+ can do drag-and-drop (DND) via multiple
      protocols. The currently supported protocols are the Xdnd and Motif
      protocols.

      As well as the functions listed here, applications may need to use some
      facilities provided for Selections. Also, the Drag and Drop API makes use
      of signals in the @class{gtk-widget} class.

      @about-symbol{gtk-dest-defaults}
      @about-symbol{gtk-target-flags}
      @about-function{gtk-drag-dest-set}
      @about-function{gtk-drag-dest-set-proxy}
      @about-function{gtk-drag-dest-unset}
      @about-function{gtk-drag-dest-find-target}
      @about-function{gtk-drag-dest-get-target-list}
      @about-function{gtk-drag-dest-set-target-list}
      @about-function{gtk-drag-dest-add-text-targets}
      @about-function{gtk-drag-dest-add-image-targets}
      @about-function{gtk-drag-dest-add-uri-targets}
      @about-function{gtk-drag-dest-set-track-motion}
      @about-function{gtk-drag-dest-get-track-motion}
      @about-function{gtk-drag-finish}
      @about-function{gtk-drag-get-data}
      @about-function{gtk-drag-get-source-widget}
      @about-function{gtk-drag-highlight}
      @about-function{gtk-drag-unhighlight}
      @about-function{gtk-drag-begin}
      @about-function{gtk-drag-set-icon-widget}
      @about-function{gtk-drag-set-icon-pixbuf}
      @about-function{gtk-drag-set-icon-stock}
      @about-function{gtk-drag-set-icon-surface}
      @about-function{gtk-drag-set-icon-name}
      @about-function{gtk-drag-set-icon-gicon}
      @about-function{gtk-drag-set-icon-default}
      @about-function{gtk-drag-check-threshold}
      @about-function{gtk-drag-source-set}
      @about-function{gtk-drag-source-set-icon-pixbuf}
      @about-function{gtk-drag-source-set-icon-stock}
      @about-function{gtk-drag-source-set-icon-name}
      @about-function{gtk-drag-source-set-icon-gicon}
      @about-function{gtk-drag-source-unset}
      @about-function{gtk-drag-source-set-target-list}
      @about-function{gtk-drag-source-get-target-list}
      @about-function{gtk-drag-source-add-text-targets}
      @about-function{gtk-drag-source-add-image-targets}
      @about-function{gtk-drag-source-add-uri-targets}
    @end{subsection}
    @begin[GtkSettings]{subsection}
      Sharing settings between applications.

      @about-symbol{GtkSettingsValue}

      @about-class{gtk-settings}

      @about-generic{gtk-settings-color-hash}
      @about-generic{gtk-settings-gtk-alternative-button-order}
      @about-generic{gtk-settings-gtk-alternative-sort-arrows}
      @about-generic{gtk-settings-gtk-application-prefer-dark-theme}
      @about-generic{gtk-settings-gtk-auto-mnemonics}
      @about-generic{gtk-settings-gtk-button-images}
      @about-generic{gtk-settings-gtk-can-change-accels}
      @about-generic{gtk-settings-gtk-color-palette}
      @about-generic{gtk-settings-gtk-color-scheme}
      @about-generic{gtk-settings-gtk-cursor-blink}
      @about-generic{gtk-settings-gtk-cursor-blink-time}
      @about-generic{gtk-settings-gtk-cursor-blink-timeout}
      @about-generic{gtk-settings-gtk-cursor-theme-name}
      @about-generic{gtk-settings-gtk-cursor-theme-size}
      @about-generic{gtk-settings-gtk-decoration-layout}
      @about-generic{gtk-settings-gtk-dialogs-use-header}
      @about-generic{gtk-settings-gtk-dnd-drag-threshold}
      @about-generic{gtk-settings-gtk-double-click-distance}
      @about-generic{gtk-settings-gtk-double-click-time}
      @about-generic{gtk-settings-gtk-enable-accels}
      @about-generic{gtk-settings-gtk-enable-animations}
      @about-generic{gtk-settings-gtk-enable-event-sounds}
      @about-generic{gtk-settings-gtk-enable-input-feedback-sounds}
      @about-generic{gtk-settings-gtk-enable-mnemonics}
      @about-generic{gtk-settings-gtk-enable-primary-paste}
      @about-generic{gtk-settings-gtk-enable-tooltips}
      @about-generic{gtk-settings-gtk-entry-password-hint-timeout}
      @about-generic{gtk-settings-gtk-entry-select-on-focus}
      @about-generic{gtk-settings-gtk-error-bell}
      @about-generic{gtk-settings-gtk-fallback-icon-theme}
      @about-generic{gtk-settings-gtk-file-chooser-backend}
      @about-generic{gtk-settings-gtk-font-name}
      @about-generic{gtk-settings-gtk-fontconfig-timestamp}
      @about-generic{gtk-settings-gtk-icon-sizes}
      @about-generic{gtk-settings-gtk-icon-theme-name}
      @about-generic{gtk-settings-gtk-im-module}
      @about-generic{gtk-settings-gtk-im-preedit-style}
      @about-generic{gtk-settings-gtk-im-status-style}
      @about-generic{gtk-settings-gtk-key-theme-name}
      @about-generic{gtk-settings-gtk-keynav-cursor-only}
      @about-generic{gtk-settings-gtk-keynav-use-caret}
      @about-generic{gtk-settings-gtk-keynav-wrap-around}
      @about-generic{gtk-settings-gtk-label-select-on-focus}
      @about-generic{gtk-settings-gtk-long-press-time}
      @about-generic{gtk-settings-gtk-menu-bar-accel}
      @about-generic{gtk-settings-gtk-menu-bar-popup-delay}
      @about-generic{gtk-settings-gtk-menu-images}
      @about-generic{gtk-settings-gtk-menu-popdown-delay}
      @about-generic{gtk-settings-gtk-menu-popup-delay}
      @about-generic{gtk-settings-gtk-modules}
      @about-generic{gtk-settings-gtk-primary-button-warps-slider}
      @about-generic{gtk-settings-gtk-print-backends}
      @about-generic{gtk-settings-gtk-print-preview-command}
      @about-generic{gtk-settings-gtk-recent-files-enabled}
      @about-generic{gtk-settings-gtk-recent-files-limit}
      @about-generic{gtk-settings-gtk-recent-files-max-age}
      @about-generic{gtk-settings-gtk-scrolled-window-placement}
      @about-generic{gtk-settings-gtk-shell-shows-app-menu}
      @about-generic{gtk-settings-gtk-shell-shows-desktop}
      @about-generic{gtk-settings-gtk-shell-shows-menubar}
      @about-generic{gtk-settings-gtk-show-input-method-menu}
      @about-generic{gtk-settings-gtk-show-unicode-menu}
      @about-generic{gtk-settings-gtk-sound-theme-name}
      @about-generic{gtk-settings-gtk-split-cursor}
      @about-generic{gtk-settings-gtk-theme-name}
      @about-generic{gtk-settings-gtk-timeout-expand}
      @about-generic{gtk-settings-gtk-timeout-initial}
      @about-generic{gtk-settings-gtk-timeout-repeat}
      @about-generic{gtk-settings-gtk-titlebar-double-click}
      @about-generic{gtk-settings-gtk-titlebar-middle-click}
      @about-generic{gtk-settings-gtk-titlebar-right-click}
      @about-generic{gtk-settings-gtk-toolbar-icon-size}
      @about-generic{gtk-settings-gtk-toolbar-style}
      @about-generic{gtk-settings-gtk-tooltip-browse-mode-timeout}
      @about-generic{gtk-settings-gtk-tooltip-browse-timeout}
      @about-generic{gtk-settings-gtk-tooltip-timeout}
      @about-generic{gtk-settings-gtk-touchscreen-mode}
      @about-generic{gtk-settings-gtk-visible-focus}
      @about-generic{gtk-settings-gtk-xft-antialias}
      @about-generic{gtk-settings-gtk-xft-dpi}
      @about-generic{gtk-settings-gtk-xft-hinting}
      @about-generic{gtk-settings-gtk-xft-hintstyle}
      @about-generic{gtk-settings-gtk-xft-rgba}

      @about-function{gtk-settings-get-default}
      @about-function{gtk-settings-get-for-screen}
      @about-function{gtk-settings-install-property}
      @about-function{gtk-settings-install-property-parser}
      @about-function{gtk-rc-property-parse-color}
      @about-function{gtk-rc-property-parse-enum}
      @about-function{gtk-rc-property-parse-flags}
      @about-function{gtk-rc-property-parse-requisition}
      @about-function{gtk-rc-property-parse-border}
      @about-function{gtk-settings-set-property-value}
      @about-function{gtk-settings-set-string-property}
      @about-function{gtk-settings-set-long-property}
      @about-function{gtk-settings-set-double-property}
    @end{subsection}
    @begin[Bindings]{subsection}
      not implemented
    @end{subsection}
    @begin[Standard Enumerations]{subsection}
      Public enumerated types used throughout GTK+.

      @about-symbol{gtk-accel-flags}
      @about-symbol{gtk-arrow-placement}
      @about-symbol{gtk-arrow-type}
      @about-symbol{gtk-attach-options}
      @about-symbol{gtk-baseline-position}
      @about-symbol{gtk-corner-type}
      @about-symbol{gtk-delete-type}
      @about-symbol{gtk-direction-type}
      @about-symbol{gtk-expander-style}
      @about-symbol{gtk-im-preedit-style}
      @about-symbol{gtk-im-status-style}
      @about-symbol{gtk-justification}
      @about-symbol{gtk-movement-step}
      @about-symbol{gtk-orientation}
      @about-symbol{gtk-pack-type}
      @about-symbol{gtk-path-priority-type}
      @about-symbol{gtk-path-type}
      @about-symbol{gtk-policy-type}
      @about-symbol{gtk-position-type}
      @about-symbol{gtk-relief-style}
      @about-symbol{gtk-resize-mode}
      @about-symbol{gtk-scroll-step}
      @about-symbol{gtk-scroll-type}
      @about-symbol{gtk-selection-mode}
      @about-symbol{gtk-shadow-type}
      @about-symbol{gtk-state-type}
      @about-symbol{gtk-state-flags}
      @about-symbol{gtk-toolbar-style}
      @about-symbol{gtk-window-position}
      @about-symbol{gtk-window-type}
      @about-symbol{gtk-sort-type}
      @about-symbol{gtk-drag-result}
      @about-symbol{gtk-junction-sides}
      @about-symbol{gtk-border-style}
      @about-symbol{gtk-region-flags}
    @end{subsection}
    @begin[Selections]{subsection}
      The selection mechanism provides the basis for different types of
      communication between processes. In particular, drag and drop and
      @class{gtk-clipboard} work via selections. You will very seldom or never
      need to use most of the functions in this section directly;
      @class{gtk-clipboard} provides a nicer interface to the same
      functionality.

      Some of the datatypes defined this section are used in the
      @class{gtk-clipboard} and drag-and-drop API's as well. The
      @class{gtk-target-entry} structure and @class{gtk-target-list}
      objects represent lists of data types that are supported when sending or
      receiving data. The @class{gtk-selection-data} object is used to store a
      chunk of data along with the data type and other associated information.

      @about-struct{gtk-selection-data}
      @about-function{make-gtk-selection-data}
      @about-function{copy-gtk-selection-data}
      @about-function{gtk-selection-data-selection}
      @about-function{gtk-selection-data-target}
      @about-function{make-gtk-target-entry}
      @about-function{copy-gtk-target-entry}
      @about-function{gtk-target-entry-target}
      @about-function{gtk-target-entry-flags}
      @about-function{gtk-target-entry-info}
      @about-function{gtk-selection-data-type}
      @about-function{gtk-selection-data-format}
      @about-function{gtk-selection-data-data}
      @about-function{gtk-selection-data-length}
      @about-function{gtk-selection-data-display}
      @about-struct{gtk-target-entry}
      @about-class{gtk-target-list}
      @about-function{gtk-target-entry-new}
      @about-function{gtk-target-entry-copy}
      @about-function{gtk-target-entry-free}
      @about-function{gtk-target-list-new}
      @about-function{gtk-target-list-ref}
      @about-function{gtk-target-list-unref}
      @about-function{gtk-target-list-add}
      @about-function{gtk-target-list-add-table}
      @about-function{gtk-target-list-add-text-targets}
      @about-function{gtk-target-list-add-image-targets}
      @about-function{gtk-target-list-add-uri-targets}
      @about-function{gtk-target-list-add-rich-text-targets}
      @about-function{gtk-target-list-remove}
      @about-function{gtk-target-list-find}
      @about-function{gtk-target-table-free}
      @about-function{gtk-target-table-new-from-list}
      @about-function{gtk-selection-owner-set}
      @about-function{gtk-selection-owner-set-for-display}
      @about-function{gtk-selection-add-target}
      @about-function{gtk-selection-add-targets}
      @about-function{gtk-selection-clear-targets}
      @about-function{gtk-selection-convert}
      @about-function{gtk-selection-data-set}
      @about-function{gtk-selection-data-set-text}
      @about-function{gtk-selection-data-get-text}
      @about-function{gtk-selection-data-set-pixbuf}
      @about-function{gtk-selection-data-get-pixbuf}
      @about-function{gtk-selection-data-set-uris}
      @about-function{gtk-selection-data-get-uris}
      @about-function{gtk-selection-data-get-targets}
      @about-function{gtk-selection-data-targets-include-image}
      @about-function{gtk-selection-data-targets-include-text}
      @about-function{gtk-selection-data-targets-include-uri}
      @about-function{gtk-selection-data-targets-include-rich-text}
      @about-function{gtk-selection-data-get-selection}
      @about-function{gtk-selection-data-get-data}
      @about-function{gtk-selection-data-get-length}
      @about-function{gtk-selection-data-get-data-with-length}
      @about-function{gtk-selection-data-get-data-type}
      @about-function{gtk-selection-data-get-display}
      @about-function{gtk-selection-data-get-format}
      @about-function{gtk-selection-data-get-target}
      @about-function{gtk-targets-include-image}
      @about-function{gtk-targets-include-text}
      @about-function{gtk-targets-include-uri}
      @about-function{gtk-targets-include-rich-text}
      @about-function{gtk-selection-remove-all}
      @about-function{gtk-selection-data-copy}
      @about-function{gtk-selection-data-free}
    @end{subsection}
    @begin[Filesystem utilities]{subsection}
      Functions for working with GIO

      @about-class{gtk-mount-operation}
      @about-function{gtk-mount-operation-new}
      @about-function{gtk-mount-operation-is-showing}
      @about-function{gtk-mount-operation-set-parent}
      @about-function{gtk-mount-operation-get-parent}
      @about-function{gtk-mount-operation-set-screen}
      @about-function{gtk-mount-operation-get-screen}
      @about-function{gtk-show-uri}
    @end{subsection}
  @end{section}
  @begin[Theming in GTK+]{section}
    @begin[GtkStyleContext]{subsection}
      Rendering UI elements

      @about-symbol{gtk-style-context-print-flags}

      @about-class{gtk-style-context}

      @about-generic{gtk-style-context-direction}
      @about-generic{gtk-style-context-paint-clock}
      @about-generic{gtk-style-context-parent}
      @about-generic{gtk-style-context-screen}

      @about-function{gtk-style-context-new}
      @about-function{gtk-style-context-add-provider}
      @about-function{gtk-style-context-add-provider-for-screen}
      @about-function{gtk-style-context-get}
      @about-function{gtk-style-context-get-junction-sides}
      @about-function{gtk-style-context-get-path}
      @about-function{gtk-style-context-get-property}
      @about-function{gtk-style-context-get-frame-clock}
      @about-function{gtk-style-context-get-state}
      @about-function{gtk-style-context-get-style}
      @about-function{gtk-style-context-get-style-property}
      @about-function{gtk-style-context-get-style-valist}
      @about-function{gtk-style-context-get-valist}
      @about-function{gtk-style-context-get-section}
      @about-function{gtk-style-context-get-color}
      @about-function{gtk-style-context-get-background-color}
      @about-function{gtk-style-context-get-border-color}
      @about-function{gtk-style-context-get-border}
      @about-function{gtk-style-context-get-padding}
      @about-function{gtk-style-context-get-margin}
      @about-function{gtk-style-context-get-font}
      @about-function{gtk-style-context-invalidate}
      @about-function{gtk-style-context-state-is-running}
      @about-function{gtk-style-context-lookup-color}
      @about-function{gtk-style-context-lookup-icon-set}
      @about-function{gtk-style-context-notify-state-change}
      @about-function{gtk-style-context-pop-animatable-region}
      @about-function{gtk-style-context-push-animatable-region}
      @about-function{gtk-style-context-cancel-animations}
      @about-function{gtk-style-context-scroll-animations}
      @about-function{gtk-style-context-remove-provider}
      @about-function{gtk-style-context-remove-provider-for-screen}
      @about-function{gtk-style-context-reset-widgets}
      @about-function{gtk-style-context-set-background}
      @about-function{gtk-style-context-restore}
      @about-function{gtk-style-context-save}
      @about-function{gtk-style-context-set-junction-sides}
      @about-function{gtk-style-context-set-path}
      @about-function{gtk-style-context-add-class}
      @about-function{gtk-style-context-remove-class}
      @about-function{gtk-style-context-has-class}
      @about-function{gtk-style-context-list-classes}
      @about-function{gtk-style-context-add-region}
      @about-function{gtk-style-context-remove-region}
      @about-function{gtk-style-context-has-region}
      @about-function{gtk-style-context-list-regions}
      @about-function{gtk-style-context-set-frame-clock}
      @about-function{gtk-style-context-set-state}
      @about-function{gtk-style-context-set-scale}
      @about-function{gtk-style-context-get-scale}
      @about-function{gtk-style-context-to-string}

      @about-struct{gtk-border}
      @about-function{make-gtk-border}
      @about-function{copy-gtk-border}
      @about-function{gtk-border-left}
      @about-function{gtk-border-right}
      @about-function{gtk-border-top}
      @about-function{gtk-border-bottom}
      @about-function{gtk-border-new}
      @about-function{gtk-border-copy}
      @about-function{gtk-border-free}

      @about-function{gtk-render-arrow}
      @about-function{gtk-render-background}
      @about-function{gtk-render-background-get-clip}
      @about-function{gtk-render-check}
      @about-function{gtk-render-expander}
      @about-function{gtk-render-extension}
      @about-function{gtk-render-focus}
      @about-function{gtk-render-frame}
      @about-function{gtk-render-frame-gap}
      @about-function{gtk-render-handle}
      @about-function{gtk-render-layout}
      @about-function{gtk-render-line}
      @about-function{gtk-render-option}
      @about-function{gtk-render-slider}
      @about-function{gtk-render-activity}
      @about-function{gtk-render-icon-pixbuf}
      @about-function{gtk-render-icon-surface}
      @about-function{gtk-render-icon}
      @about-function{gtk-render-insertion-cursor}
    @end{subsection}
    @begin[GtkCssProvider]{subsection}
      CSS-like styling for widgets.

      @about-class{gtk-css-provider}
      @about-function{gtk-css-provider-get-default}
      @about-function{gtk-css-provider-get-named}
      @about-function{gtk-css-provider-load-from-data}
      @about-function{gtk-css-provider-load-from-file}
      @about-function{gtk-css-provider-load-from-path}
      @about-function{gtk-css-provider-new}
      @about-function{gtk-css-provider-to-string}
      @about-symbol{GTK-CSS-PROVIDER-ERROR}
      @about-symbol{GtkCssProviderError}
      @about-symbol{GtkCssSection}
      @about-symbol{GtkCssSectionType}
      @about-function{gtk-css-section-get-end-line}
      @about-function{gtk-css-section-get-end-position}
      @about-function{gtk-css-section-get-file}
      @about-function{gtk-css-section-get-parent}
      @about-function{gtk-css-section-get-section-type}
      @about-function{gtk-css-section-get-start-line}
      @about-function{gtk-css-section-get-start-position}
      @about-function{gtk-css-section-ref}
      @about-function{gtk-css-section-unref}
    @end{subsection}
    @begin[GtkStyleProvider]{subsection}
      Interface to provide style information to @class{gtk-style-context}.

      @about-variable{+gtk-style-provider-priority-fallback+}
      @about-variable{+gtk-style-provider-priority-theme+}
      @about-variable{+gtk-style-provider-priority-settings+}
      @about-variable{+gtk-style-provider-priority-application+}
      @about-variable{+gtk-style-provider-priority-user+}
      @about-class{gtk-style-provider}
      @about-function{gtk-style-provider-get-icon-factory}
      @about-function{gtk-style-provider-get-style}
      @about-function{gtk-style-provider-get-style-property}
    @end{subsection}
    @begin[GtkStyleProperties]{subsection}
      Store for style property information

      @about-class{gtk-style-properties}
      @about-function{gtk-style-properties-clear}
      @about-function{gtk-style-properties-get}
      @about-function{gtk-style-properties-get-property}
      @about-function{gtk-style-properties-get-valist}
      @about-function{gtk-style-properties-lookup-color}
      @about-function{gtk-style-properties-lookup-property}
      @about-function{gtk-style-properties-map-color}
      @about-function{gtk-style-properties-merge}
      @about-function{gtk-style-properties-new}
      @about-function{gtk-style-properties-register-property}
      @about-function{gtk-style-properties-set}
      @about-function{gtk-style-properties-set-property}
      @about-function{gtk-style-properties-set-valist}
      @about-function{gtk-style-properties-unset-property}
    @end{subsection}
    @begin[GtkWidgetPath]{subsection}
      Widget path abstraction.

      @about-class{gtk-widget-path}
      @about-function{gtk-widget-path-append-type}
      @about-function{gtk-widget-path-append-with-siblings}
      @about-function{gtk-widget-path-append-for-widget}
      @about-function{gtk-widget-path-copy}
      @about-function{gtk-widget-path-ref}
      @about-function{gtk-widget-path-unref}
      @about-function{gtk-widget-path-free}
      @about-function{gtk-widget-path-get-object-type}
      @about-function{gtk-widget-path-has-parent}
      @about-function{gtk-widget-path-is-type}
      @about-function{gtk-widget-path-iter-add-class}
      @about-function{gtk-widget-path-iter-add-region}
      @about-function{gtk-widget-path-iter-clear-classes}
      @about-function{gtk-widget-path-iter-clear-regions}
      @about-function{gtk-widget-path-iter-get-name}
      @about-function{gtk-widget-path-iter-get-object-type}
      @about-function{gtk-widget-path-iter-get-siblings}
      @about-function{gtk-widget-path-iter-get-sibling-index}
      @about-function{gtk-widget-path-iter-has-class}
      @about-function{gtk-widget-path-iter-has-name}
      @about-function{gtk-widget-path-iter-has-qclass}
      @about-function{gtk-widget-path-iter-has-qname}
      @about-function{gtk-widget-path-iter-has-qregion}
      @about-function{gtk-widget-path-iter-has-region}
      @about-function{gtk-widget-path-iter-list-classes}
      @about-function{gtk-widget-path-iter-list-regions}
      @about-function{gtk-widget-path-iter-remove-class}
      @about-function{gtk-widget-path-iter-remove-region}
      @about-function{gtk-widget-path-iter-set-name}
      @about-function{gtk-widget-path-iter-set-object-type}
      @about-function{gtk-widget-path-length}
      @about-function{gtk-widget-path-new}
      @about-function{gtk-widget-path-prepend-type}
      @about-function{gtk-widget-path-to-string}
    @end{subsection}
    @begin[GtkIconTheme]{subsection}
      Looking up icons by name

      @about-class{gtk-icon-theme}
      @about-symbol{gtk-icon-info}
      @about-symbol{gtk-icon-lookup-flags}
      @about-symbol{gtk-icon-theme-error}
      @about-function{gtk-icon-theme-new}
      @about-function{gtk-icon-theme-get-default}
      @about-function{gtk-icon-theme-get-for-screen}
      @about-function{gtk-icon-theme-set-screen}
      @about-function{gtk-icon-theme-set-search-path}
      @about-function{gtk-icon-theme-get-search-path}
      @about-function{gtk-icon-theme-append-search-path}
      @about-function{gtk-icon-theme-prepend-search-path}
      @about-function{gtk-icon-theme-set-custom-theme}
      @about-function{gtk-icon-theme-has-icon}
      @about-function{gtk-icon-theme-lookup-icon}
      @about-function{gtk-icon-theme-choose-icon}
      @about-function{gtk-icon-theme-lookup-by-gicon}
      @about-function{gtk-icon-theme-load-icon}
      @about-function{gtk-icon-theme-list-contexts}
      @about-function{gtk-icon-theme-list-icons}
      @about-function{gtk-icon-theme-get-icon-sizes}
      @about-function{gtk-icon-theme-get-example-icon-name}
      @about-function{gtk-icon-theme-rescan-if-needed}
      @about-function{gtk-icon-theme-add-builtin-icon}
      @about-function{gtk-icon-info-copy}
      @about-function{gtk-icon-info-free}
      @about-function{gtk-icon-info-new-for-pixbuf}
      @about-function{gtk-icon-info-get-base-size}
      @about-function{gtk-icon-info-get-filename}
      @about-function{gtk-icon-info-get-builtin-pixbuf}
      @about-function{gtk-icon-info-load-icon}
      @about-function{gtk-icon-info-load-symbolic}
      @about-function{gtk-icon-info-load-symbolic-for-style}
      @about-function{gtk-icon-info-load-symbolic-for-context}
      @about-function{gtk-icon-info-set-raw-coordinates}
      @about-function{gtk-icon-info-get-embedded-rect}
      @about-function{gtk-icon-info-get-attach-points}
      @about-function{gtk-icon-info-get-display-name}
    @end{subsection}
  @end{section}
  @begin[Deprecated]{section}
    @begin[Deprecated since GTK+ 3.0]{subsection}@end{subsection}
    @begin[Resource Files]{subsection}
      Deprecated routines for handling resource files.

      In GTK+ 3.0, resource files have been deprecated and replaced by CSS-like
      style sheets, which are understood by @class{gtk-css-provider}. In the
      Lisp binding the implementation was never very complete. The few symbols
      are not exported.
    @end{subsection}
    @begin[GtkStyle]{subsection}
      Deprecated object that holds style information for widgets.

      In GTK+ 3.0, GtkStyle has been deprecated and replaced by
      @class{gtk-style-context}. In the Lisp binding the implementation was
      never very complete. The few symbols are not exported.
    @end{subsection}

    @begin[Deprecated since GTK+ 3.2]{subsection}@end{subsection}
    @begin[GtkHBox]{subsection}
      A deprecated horizontal container box.

      GtkHBox has been deprecated since GTK+ 3.2. You can use @class{gtk-box}
      with the value @code{:horizontal} for the
      @slot[gtk-orientable]{orientation} property instead, which is a very
      quick and easy change. If you have derived your own classes from GtkHBox,
      you can simply change the inheritance to derive directly from
      @class{gtk-box}. No further changes are needed, since the default value of
      the @slot[gtk-orientable]{orientation} property is @code{:horizontal}.

      If you want your code to be future-proof, the recommendation is to switch
      to @class{gtk-grid}, since @class{gtk-box} is going to be deprecated in
      favor of the more flexible grid widget eventually.

      In the Lisp binding the symbols and functions for GtkHBox are not
      exported.
    @end{subsection}
    @begin[GtkVBox]{subsection}
      A deprecated vertical container box.

      GtkVBox has been deprecated since GTK+ 3.2. You can use @class{gtk-box}
      instead, which is a very quick and easy change. If you have derived your
      own classes from GtkVBox, you can simply change the inheritance to derive
      directly from @class{gtk-box}, and set the
      @slot[gtk-orientable]{orientation} property to @code{:vertical} in your
      instance init function.

      If you want your code to be future-proof, the recommendation is to switch
      to @class{gtk-grid}, since @class{gtk-box} is going to be deprecated in
      favor of the more flexible grid widget eventually.

      In the Lisp binding the symbols and functions for GtkVBox are not
      exported.
    @end{subsection}
    @begin[GtkHButtonBox]{subsection}
      A deprecated container for arranging buttons horizontally.

      GtkHButtonBox has been deprecated since GTK+ 3.2 and should not be used
      in newly-written code. Use @class{gtk-button-box} with the value
      @code{:horizontal} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkHButtonBox are not
      exported.
    @end{subsection}
    @begin[GtkVButtonBox]{subsection}
      A deprecated container for arranging buttons vertically.

      GtkVButtonBox has been deprecated since GTK+ 3.2 and should not be used
      in newly-written code. Use @class{gtk-button-box} with the value
      @code{:vertical} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions of GtkVButtonBox are not
      exported.
    @end{subsection}
    @begin[GtkHPaned]{subsection}
      A deprecated container with two panes arranged horizontally.

      GtkHPaned has been deprecated since GTK+ 3.2 and should not be used
      in newly-written code. Use @class{gtk-paned} with the value
      @code{:horizontal} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkHPaned are not
      exported.
    @end{subsection}
    @begin[GtkVPaned]{subsection}
      A deprecated container with two panes arranged vertically.

      GtkVPaned has been deprecated since GTK+ 3.2 and should not be used
      in newly-written code. Use @class{gtk-paned} with the value
      @code{:vertical} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkVPaned are not
      exported.
    @end{subsection}
    @begin[GtkHScale]{subsection}
      A deprecated horizontal slider widget for selecting a value from a range.

      GtkHScale has been deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-scale} with the value
      @code{:horizontal} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkHScale are not
      exported.
    @end{subsection}
    @begin[GtkVScale]{subsection}
      A deprecated vertical slider widget for selecting a value from a range.

      GtkVScale has been deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-scale} with the value
      @code{:vertical} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkVScale are not
      exported.
    @end{subsection}
    @begin[GtkHSeparator]{subsection}
      A deprecated horizontal separator widget.

      GtkHSeparator has been deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-separator} with the value
      @code{:horizontal} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkHSeparator are not
      exported.
    @end{subsection}
    @begin[GtkVSeparator]{subsection}
      A deprecated vertical separator widget.

      GtkVSeparator has been deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-separator} with the value
      @code{:vertical} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkVSeparator are not
      exported.
    @end{subsection}
    @begin[GtkHScrollbar]{subsection}
      A deprecated horizontal scrollbar.

      GtkHScrollbar has been deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-scrollbar} with the value
      @code{:horizontal} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkHScrollbar are not
      exported.
    @end{subsection}
    @begin[GtkVScrollbar]{subsection}
      A deprecated vertical scrollbar.

      GtkVScrollbar has been deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-scrollbar} with the value
      @code{:vertical} for the @slot[gtk-orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for GtkVScrollbar are not
      exported.
    @end{subsection}
    @begin[GtkFontSelection]{subsection}
      Deprecated widget for selecting fonts.

      GtkFontSelection is deprecated since GTK+ 3.2 and should not be used in
      newly written code. Use @class{gtk-font-chooser} instead.

      In the Lisp binding the symbols and functions for GtkFontSelection are
      not exported.
    @end{subsection}
    @begin[GtkFontSelectionDialog]{subsection}
      Deprecated dialog box for selecting fonts.

      GtkFontSelectionDialog is deprecated since GTK+ 3.2 and should not be
      used in newly written code. Use @class{gtk-font-chooser-dialog} instead.

      In the Lisp binding the symbols and functions for GtkFontSelectionDialog
      are not exported.
    @end{subsection}

    @begin[Deprecated since GTK+ 3.4]{subsection}@end{subsection}
    @begin[GtkHandleBox]{subsection}
      A depreacted widget for detachable window portions.

      @about-class{gtk-handle-box}
      @about-generic{gtk-handle-box-child-detached}
      @about-generic{gtk-handle-box-handle-position}
      @about-generic{gtk-handle-box-shadow-type}
      @about-generic{gtk-handle-box-snap-edge}
      @about-generic{gtk-handle-box-snap-edge-set}
      @about-function{gtk-handle-box-new}
    @end{subsection}
    @begin[GtkTable]{subsection}
      Pack widgets in regular patterns.

      @about-class{gtk-table}
      @about-generic{gtk-table-column-spacing}
      @about-generic{gtk-table-homogeneous}
      @about-generic{gtk-table-n-columns}
      @about-generic{gtk-table-n-rows}
      @about-generic{gtk-table-row-spacing}
      @about-function{gtk-table-child-left-attach}
      @about-function{gtk-table-child-right-attach}
      @about-function{gtk-table-child-top-attach}
      @about-function{gtk-table-child-bottom-attach}
      @about-function{gtk-table-child-x-options}
      @about-function{gtk-table-child-y-options}
      @about-function{gtk-table-child-x-padding}
      @about-function{gtk-table-child-y-padding}
      @about-function{gtk-table-new}
      @about-function{gtk-table-resize}
      @about-function{gtk-table-get-size}
      @about-function{gtk-table-attach}
      @about-function{gtk-table-attach-defaults}
      @about-function{gtk-table-set-row-spacing}
      @about-function{gtk-table-set-col-spacing}
      @about-function{gtk-table-set-row-spacings}
      @about-function{gtk-table-set-col-spacings}
      @about-function{gtk-table-set-homogeneous}
      @about-function{gtk-table-get-default-row-spacing}
      @about-function{gtk-table-get-homogeneous}
      @about-function{gtk-table-get-row-spacing}
      @about-function{gtk-table-get-col-spacing}
      @about-function{gtk-table-get-default-col-spacing}
    @end{subsection}
    @begin[GtkTearoffMenuItem]{subsection}
      A deprecated menu item used to tear off and reattach its menu.

      @about-class{gtk-tearoff-menu-item}
      @about-function{gtk-tearoff-menu-item-new}
    @end{subsection}
    @begin[GtkColorSelection]{subsection}
      A deprecated widget used to select a color.

      @about-class{gtk-color-selection}
      @about-generic{gtk-color-selection-current-alpha}
      @about-generic{gtk-color-selection-current-color}
      @about-generic{gtk-color-selection-current-rgba}
      @about-generic{gtk-color-selection-has-opacity-control}
      @about-generic{gtk-color-selection-has-palette}
      @about-function{gtk-color-selection-child-expand}
      @about-function{gtk-color-selection-child-fill}
      @about-function{gtk-color-selection-child-padding}
      @about-function{gtk-color-selection-child-pack-type}
      @about-function{gtk-color-selection-child-position}
      @about-function{gtk-color-selection-new}
      @about-function{gtk-color-selection-get-previous-alpha}
      @about-function{gtk-color-selection-set-previous-alpha}
      @about-function{gtk-color-selection-get-previous-color}
      @about-function{gtk-color-selection-set-previous-color}
      @about-function{gtk-color-selection-get-previous-rgba}
      @about-function{gtk-color-selection-set-previous-rgba}
      @about-function{gtk-color-selection-is-adjusting}
      @about-function{gtk-color-selection-palette-from-string}
      @about-function{gtk-color-selection-palette-to-string}
      @about-function{gtk-color-selection-set-change-palette-with-screen-hook}
    @end{subsection}
    @begin[GtkColorSelectionDialog]{subsection}
      Deprecated dialog box for selecting a color.

      @about-class{gtk-color-selection-dialog}
      @about-generic{gtk-color-selection-dialog-cancel-button}
      @about-generic{gtk-color-selection-dialog-color-selection}
      @about-generic{gtk-color-selection-dialog-help-button}
      @about-generic{gtk-color-selection-dialog-ok-button}
      @about-function{gtk-color-selection-dialog-new}
    @end{subsection}
    @begin[GtkHSV]{subsection}
      A deprecated \"color wheel\" widget.

      @about-class{gtk-hsv}
      @about-function{gtk-hsv-new}
      @about-function{gtk-hsv-set-color}
      @about-function{gtk-hsv-get-color}
      @about-function{gtk-hsv-set-metrics}
      @about-function{gtk-hsv-get-metrics}
      @about-function{gtk-hsv-is-adjusting}
      @about-function{gtk-hsv-to-rgb}
      @about-function{gtk-rgb-to-hsv}
    @end{subsection}

    @begin[Deprecated since GTK+ 3.8]{subsection}@end{subsection}
    @begin[GtkSymbolicColor]{subsection}
      GtkSymbolicColor is deprecated since version 3.8. Symbolic colors are
      considered an implementation detail of GTK+. In the Lisp binding no
      symbols or functions of GtkSymbolicColor are implemented.
    @end{subsection}
    @begin[GtkGradient]{subsection}
      GtkGradient is deprecated since version 3.8. It was used internally by
      GTK’s CSS engine to represent gradients. As its handling is not conforming
      to modern web standards, it is not used anymore. If you want to use
      gradients in your own code, please use Cairo directly. In the Lisp binding
      no symbols or functions of GtkGradient are implemented.
    @end{subsection}

    @begin[Deprecated since GTK+ 3.10]{subsection}@end{subsection}
    @begin[GtkUIManager]{subsection}
      Constructing menus and toolbars from an XML description.

      @about-class{gtk-ui-manager}
      @about-generic{gtk-ui-manager-add-tearoffs}
      @about-generic{gtk-ui-manager-ui}
      @about-function{gtk-ui-manager-new}
      @about-function{gtk-ui-manager-insert-action-group}
      @about-function{gtk-ui-manager-remove-action-group}
      @about-function{gtk-ui-manager-get-action-groups}
      @about-function{gtk-ui-manager-get-accel-group}
      @about-function{gtk-ui-manager-get-widget}
      @about-function{gtk-ui-manager-get-toplevels}
      @about-function{gtk-ui-manager-get-action}
      @about-function{gtk-ui-manager-add-ui-from-resource}
      @about-function{gtk-ui-manager-add-ui-from-string}
      @about-function{gtk-ui-manager-add-ui-from-file}
      @about-function{gtk-ui-manager-new-merge-id}
      @about-symbol{gtk-ui-manager-item-type}
      @about-function{gtk-ui-manager-add-ui}
      @about-function{gtk-ui-manager-remove-ui}
      @about-function{gtk-ui-manager-ensure-update}
    @end{subsection}
    @begin[GtkActionGroup]{subsection}
      A group of actions.

      @about-class{gtk-action-group}
      @about-generic{gtk-action-group-accel-group}
      @about-generic{gtk-action-group-name}
      @about-generic{gtk-action-group-sensitive}
      @about-generic{gtk-action-group-visible}
      @about-function{gtk-action-group-new}
      @about-function{gtk-action-group-get-action}
      @about-function{gtk-action-group-list-actions}
      @about-function{gtk-action-group-add-action}
      @about-function{gtk-action-group-add-action-with-accel}
      @about-function{gtk-action-group-remove-action}
      @about-symbol{gtk-action-entry}
      @about-function{gtk-action-group-add-actions}
      @about-function{gtk-action-group-add-actions-full}
      @about-symbol{gtk-toggle-action-entry}
      @about-function{gtk-action-group-add-toggle-actions}
      @about-function{gtk-action-group-add-toggle-actions-full}
      @about-symbol{gtk-radio-action-entry}
      @about-function{gtk-action-group-add-radio-actions}
      @about-function{gtk-action-group-add-radio-actions-full}
      @about-function{gtk-action-group-set-translate-func}
      @about-function{gtk-action-group-set-translation-domain}
      @about-function{gtk-action-group-translate-string}
    @end{subsection}
    @begin[GtkAction]{subsection}
      An action which can be triggered by a menu or toolbar item.

      @about-class{gtk-action}
      @about-generic{gtk-action-action-group}
      @about-generic{gtk-action-always-show-image}
      @about-generic{gtk-action-gicon}
      @about-generic{gtk-action-hide-if-empty}
      @about-generic{gtk-action-icon-name}
      @about-generic{gtk-action-is-important}
      @about-generic{gtk-action-label}
      @about-generic{gtk-action-name}
      @about-generic{gtk-action-sensitive}
      @about-generic{gtk-action-short-label}
      @about-generic{gtk-action-stock-id}
      @about-generic{gtk-action-tooltip}
      @about-generic{gtk-action-visible}
      @about-generic{gtk-action-visible-horizontal}
      @about-generic{gtk-action-visible-overflown}
      @about-generic{gtk-action-visible-vertical}
      @about-function{gtk-action-new}
      @about-function{gtk-action-get-name}
      @about-function{gtk-action-is-sensitive}
      @about-function{gtk-action-get-sensitive}
      @about-function{gtk-action-set-sensitive}
      @about-function{gtk-action-is-visible}
      @about-function{gtk-action-get-visible}
      @about-function{gtk-action-set-visible}
      @about-function{gtk-action-activate}
      @about-function{gtk-action-create-icon}
      @about-function{gtk-action-create-menu-item}
      @about-function{gtk-action-create-tool-item}
      @about-function{gtk-action-create-menu}
      @about-function{gtk-action-get-proxies}
      @about-function{gtk-action-connect-accelerator}
      @about-function{gtk-action-disconnect-accelerator}
      @about-function{gtk-action-block-activate}
      @about-function{gtk-action-unblock-activate}
      @about-function{gtk-action-get-always-show-image}
      @about-function{gtk-action-set-always-show-image}
      @about-function{gtk-action-get-accel-path}
      @about-function{gtk-action-set-accel-path}
      @about-function{gtk-action-get-accel-closure}
      @about-function{gtk-action-set-accel-group}
      @about-function{gtk-action-set-label}
      @about-function{gtk-action-get-label}
      @about-function{gtk-action-set-short-label}
      @about-function{gtk-action-get-short-label}
      @about-function{gtk-action-set-tooltip}
      @about-function{gtk-action-get-tooltip}
      @about-function{gtk-action-set-stock-id}
      @about-function{gtk-action-get-stock-id}
      @about-function{gtk-action-set-gicon}
      @about-function{gtk-action-get-gicon}
      @about-function{gtk-action-set-icon-name}
      @about-function{gtk-action-get-icon-name}
      @about-function{gtk-action-set-visible-horizontal}
      @about-function{gtk-action-get-visible-horizontal}
      @about-function{gtk-action-set-visible-vertical}
      @about-function{gtk-action-get-visible-vertical}
      @about-function{gtk-action-set-is-important}
      @about-function{gtk-action-get-is-important}
    @end{subsection}
    @begin[GtkToggleAction]{subsection}
      An action which can be toggled between two states.

      @about-class{gtk-toggle-action}
      @about-generic{gtk-toggle-action-active}
      @about-generic{gtk-toggle-action-draw-as-radio}
      @about-function{gtk-toggle-action-new}
      @about-function{gtk-toggle-action-toggled}
    @end{subsection}
    @begin[GtkRadioAction]{subsection}
      An action of which only one in a group can be active.

      @about-class{gtk-radio-action}
      @about-generic{gtk-radio-action-current-value}
      @about-generic{gtk-radio-action-group}
      @about-generic{gtk-radio-action-value}
      @about-function{gtk-radio-action-new}
      @about-function{gtk-radio-action-get-group}
      @about-function{gtk-radio-action-set-group}
      @about-function{gtk-radio-action-join-group}
    @end{subsection}
    @begin[GtkRecentAction]{subsection}
      An action of which represents a list of recently used files.

      @about-class{gtk-recent-action}
      @about-generic{gtk-recent-action-show-numbers}
      @about-function{gtk-recent-action-new}
      @about-function{gtk-recent-action-new-for-manager}
    @end{subsection}
    @begin[GtkActivatable]{subsection}
      An interface for activatable widgets.

      @about-class{gtk-activatable}
      @about-class{gtk-activatable-iface}
      @about-function{gtk-activatable-do-set-related-action}
      @about-function{gtk-activatable-get-related-action}
      @about-function{gtk-activatable-get-use-action-appearance}
      @about-function{gtk-activatable-sync-action-properties}
      @about-function{gtk-activatable-set-related-action}
      @about-function{gtk-activatable-set-use-action-appearance}
    @end{subsection}
    @begin[GtkImageMenuItem]{subsection}
      A deprecated widget for a menu item with an icon.

      @about-class{gtk-image-menu-item}
      @about-generic{gtk-image-menu-item-accel-group}
      @about-generic{gtk-image-menu-item-always-show-image}
      @about-generic{gtk-image-menu-item-image}
      @about-generic{gtk-image-menu-item-use-stock}
      @about-function{gtk-image-menu-item-new}
      @about-function{gtk-image-menu-item-new-from-stock}
      @about-function{gtk-image-menu-item-new-with-label}
      @about-function{gtk-image-menu-item-new-with-mnemonic}
      @about-function{gtk-image-menu-item-set-accel-group}
    @end{subsection}
    @begin[GtkMisc]{subsection}
      Base class for widgets with alignments and padding.

      @about-class{gtk-misc}
      @about-function{gtk-misc-set-alignment}
      @about-function{gtk-misc-set-padding}
      @about-function{gtk-misc-get-alignment}
      @about-function{gtk-misc-get-padding}
    @end{subsection}
    @begin[Stock items]{subsection}
      Prebuilt common menu/toolbar items and corresponding icons.

      Since GTK+ 3.10, stock items are deprecated. You should instead set up
      whatever labels and/or icons you need using normal widget API, rather
      than relying on GTK+ providing ready-made combinations of these.

      Stock items represent commonly-used menu or toolbar items such as \"Open\"
      or \"Exit\". Each stock item is identified by a stock ID; stock IDs are
      just strings like \"gtk-open\" or \"gtk-exit\". Applications can register
      their own stock items in addition to those built-in to GTK+.

      Each stock ID can be associated with a @code{GtkStockItem}, which contains
      the user-visible label, keyboard accelerator, and translation domain of
      the menu or toolbar item; and/or with an icon stored in a
      @class{gtk-icon-factory}. See @class{gtk-icon-factory} for more
      information on stock icons. The connection between a @code{GtkStockItem}
      and stock icons is purely conventional by virtue of using the same stock
      ID; it's possible to register a stock item but no icon, and vice versa.
      Stock icons may have a RTL variant which gets used for right-to-left
      locales.

      Only the function @fun{gtk-stock-list-ids} is in the Lisp binding
      implemented. For more information see
      @url[https://developer.gnome.org/gtk3/stable/gtk3-Stock-Items.html]{Stock Items}
      in the GTK+ documentation.

      @about-class{gtk-stock-item}
      @about-function{gtk-stock-add}
      @about-function{gtk-stock-add-static}
      @about-function{gtk-stock-item-copy}
      @about-function{gtk-stock-item-free}
      @about-function{gtk-stock-list-ids}
      @about-function{gtk-stock-lookup}
      @about-function{gtk-stock-set-translate-func}
    @end{subsection}
    @begin[Themable Stock Images]{subsection}
      Manipulating stock icons

      @about-class{gtk-icon-source}
      @about-class{gtk-icon-factory}
      @about-class{gtk-icon-set}
      @about-symbol{gtk-icon-size}
      @about-function{gtk-icon-source-copy}
      @about-function{gtk-icon-source-free}
      @about-function{gtk-icon-factory-add}
      @about-function{gtk-icon-factory-add-default}
      @about-function{gtk-icon-factory-lookup}
      @about-function{gtk-icon-factory-lookup-default}
      @about-function{gtk-icon-factory-new}
      @about-function{gtk-icon-factory-remove-default}
      @about-function{gtk-icon-set-add-source}
      @about-function{gtk-icon-set-copy}
      @about-function{gtk-icon-set-new}
      @about-function{gtk-icon-set-new-from-pixbuf}
      @about-function{gtk-icon-set-ref}
      @about-function{gtk-icon-set-render-icon}
      @about-function{gtk-icon-set-render-icon-pixbuf}
      @about-function{gtk-icon-set-unref}
      @about-function{gtk-icon-size-lookup}
      @about-function{gtk-icon-size-lookup-for-settings}
      @about-function{gtk-icon-size-register}
      @about-function{gtk-icon-size-register-alias}
      @about-function{gtk-icon-size-from-name}
      @about-function{gtk-icon-size-get-name}
      @about-function{gtk-icon-size-get-sizes}
      @about-function{gtk-icon-source-get-direction}
      @about-function{gtk-icon-source-get-direction-wildcarded}
      @about-function{gtk-icon-source-get-filename}
      @about-function{gtk-icon-source-get-pixbuf}
      @about-function{gtk-icon-source-get-icon-name}
      @about-function{gtk-icon-source-get-size}
      @about-function{gtk-icon-source-get-size-wildcarded}
      @about-function{gtk-icon-source-get-state}
      @about-function{gtk-icon-source-get-state-wildcarded}
      @about-function{gtk-icon-source-new}
      @about-function{gtk-icon-source-set-direction}
      @about-function{gtk-icon-source-set-direction-wildcarded}
      @about-function{gtk-icon-source-set-filename}
      @about-function{gtk-icon-source-set-pixbuf}
      @about-function{gtk-icon-source-set-icon-name}
      @about-function{gtk-icon-source-set-size}
      @about-function{gtk-icon-source-set-size-wildcarded}
      @about-function{gtk-icon-source-set-state}
      @about-function{gtk-icon-source-set-state-wildcarded}
    @end{subsection}

    @begin[Deprecated since GTK+ 3.14]{subsection}@end{subsection}
    @begin[GtkNumerableIcon]{subsection}
      A @class{g-icon} that allows numbered emblems.

      @about-class{gtk-numerable-icon}
      @about-generic{gtk-numerable-icon-background-icon}
      @about-generic{gtk-numerable-icon-background-icon-name}
      @about-generic{gtk-numerable-icon-count}
      @about-generic{gtk-numerable-icon-label}
      @about-generic{gtk-numerable-icon-style-context}
      @about-function{gtk-numerable-icon-new}
      @about-function{gtk-numerable-icon-new-with-style-context}
    @end{subsection}
    @begin[GtkArrow]{subsection}
      Displays an arrow.

      @about-class{gtk-arrow}
      @about-function{gtk-arrow-new}
      @about-function{gtk-arrow-set}
    @end{subsection}
    @begin[GtkStatusIcon]{subsection}
      Display an icon in the system tray.

      @about-class{gtk-status-icon}
      @about-generic{gtk-status-icon-embedded}
      @about-generic{gtk-status-icon-file}
      @about-generic{gtk-status-icon-gicon}
      @about-generic{gtk-status-icon-has-tooltip}
      @about-generic{gtk-status-icon-icon-name}
      @about-generic{gtk-status-icon-orientation}
      @about-generic{gtk-status-icon-pixbuf}
      @about-generic{gtk-status-icon-screen}
      @about-generic{gtk-status-icon-size}
      @about-generic{gtk-status-icon-stock}
      @about-generic{gtk-status-icon-storage-type}
      @about-generic{gtk-status-icon-title}
      @about-generic{gtk-status-icon-tooltip-markup}
      @about-generic{gtk-status-icon-tooltip-text}
      @about-generic{gtk-status-icon-visible}
      @about-function{gtk-status-icon-new}
      @about-function{gtk-status-icon-new-from-pixbuf}
      @about-function{gtk-status-icon-new-from-file}
      @about-function{gtk-status-icon-new-from-stock}
      @about-function{gtk-status-icon-new-from-icon-name}
      @about-function{gtk-status-icon-new-from-gicon}
      @about-function{gtk-status-icon-set-from-pixbuf}
      @about-function{gtk-status-icon-set-from-file}
      @about-function{gtk-status-icon-set-from-stock}
      @about-function{gtk-status-icon-set-from-icon-name}
      @about-function{gtk-status-icon-set-from-gicon}
      @about-function{gtk-status-icon-set-name}
      @about-function{gtk-status-icon-is-embedded}
      @about-function{gtk-status-icon-position-menu}
      @about-function{gtk-status-icon-get-geometry}
      @about-function{gtk-status-icon-get-x11-window-id}
    @end{subsection}
    @begin[GtkThemingEngine]{subsection}
      Theming renderers

      @about-class{gtk-theming-engine}
      @about-function{gtk-theming-engine-get}
      @about-function{gtk-theming-engine-get-direction}
      @about-function{gtk-theming-engine-get-junction-sides}
      @about-function{gtk-theming-engine-get-path}
      @about-function{gtk-theming-engine-get-property}
      @about-function{gtk-theming-engine-get-screen}
      @about-function{gtk-theming-engine-get-state}
      @about-function{gtk-theming-engine-get-style}
      @about-function{gtk-theming-engine-get-style-property}
      @about-function{gtk-theming-engine-get-style-valist}
      @about-function{gtk-theming-engine-get-valist}
      @about-function{gtk-theming-engine-get-color}
      @about-function{gtk-theming-engine-get-background-color}
      @about-function{gtk-theming-engine-get-border-color}
      @about-function{gtk-theming-engine-get-border}
      @about-function{gtk-theming-engine-get-padding}
      @about-function{gtk-theming-engine-get-margin}
      @about-function{gtk-theming-engine-get-font}
      @about-function{gtk-theming-engine-has-class}
      @about-function{gtk-theming-engine-has-region}
      @about-function{gtk-theming-engine-lookup-color}
      @about-function{gtk-theming-engine-state-is-running}
      @about-function{gtk-theming-engine-load}
      @about-function{gtk-theming-engine-register-property}
    @end{subsection}
    @begin[GtkAlignment]{subsection}
      A deprecated widget which controls the alignment and size of its child.

      @about-class{gtk-alignment}
      @about-generic{gtk-alignment-bottom-padding}
      @about-generic{gtk-alignment-left-padding}
      @about-generic{gtk-alignment-right-padding}
      @about-generic{gtk-alignment-top-padding}
      @about-generic{gtk-alignment-xalign}
      @about-generic{gtk-alignment-xscale}
      @about-generic{gtk-alignment-yalign}
      @about-generic{gtk-alignment-yscale}
      @about-function{gtk-alignment-new}
      @about-function{gtk-alignment-set}
      @about-function{gtk-alignment-get-padding}
      @about-function{gtk-alignment-set-padding}
    @end{subsection}
  @end{section}")

;;; --- End of file gtk.package.lisp -------------------------------------------
