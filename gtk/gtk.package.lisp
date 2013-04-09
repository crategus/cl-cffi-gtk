;;; ----------------------------------------------------------------------------
;;; gtk.package.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defpackage :gtk
  (:use :cl :cl-user :cffi
   :gobject :gdk :gdk-pixbuf :glib :gio :iter :pango :cairo)
  (:export #:cl-cffi-gtk-build-info

           #:define-child-property
           #:container-class-child-properties
           #:generate-child-properties
           #:tree-lisp-store
           #:tree-lisp-store-root
           #:tree-node
           #:make-tree-node
           #:tree-node-tree
           #:tree-node-parent
           #:tree-node-id
           #:tree-node-item
           #:tree-node-children
           #:tree-node-insert-at
           #:tree-node-remove-at
           #:tree-node-child-at
           #:tree-lisp-store-add-column

;           #:gtk-call-aborted
;           #:gtk-call-aborted-condition
           #:let-ui))

(defpackage :gtk-examples
  (:use :cl :gtk :gdk :gobject)
  (:export #:test-dialog))

(in-package :gtk)

#+sbcl (when (and (find-package "SB-EXT")
                  (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
         (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")) :traps nil))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :gtk) t)
 "GTK+ is a library for creating graphical user interfaces. It works on many
  UNIX-like platforms, Windows, and OS X. GTK+ is released under the GNU Library
  General Public License (GNU LGPL), which allows for flexible licensing of
  client applications. GTK+ has a C-based object-oriented architecture that
  allows for maximum flexibility. Bindings for many other languages have been
  written, including C++, Objective-C, Guile/Scheme, Perl, Python, TOM, Ada95,
  Free Pascal, and Eiffel.
  This is the API documentation of a Lisp binding to GTK+.
  @begin[GTK+ Core Reference]{section}
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
    @end{subsection}
    @begin[Main loop and Events]{subsection}
      Library initialization, main event loop, and events.

      Before using GTK+, it needs to be initialized; initialization connects to
      the window system display, and parses some standard command line
      arguments. In the C library the @code{gtk_init()} macro initializes GTK+.
      In the Lisp binding to GTK+, GTk+ is initialized, when loading the
      @code{cl-cffi-gtk} library. Therefore, no functions are exported, which
      initialize GTK+.

      Like all GUI toolkits, GTK+ uses an event-driven programming model. When
      the user is doing nothing, GTK+ sits in the main loop and waits for input.
      If the user performs some action - say, a mouse click - then the main loop
      \"wakes up\" and delivers an event to GTK+. GTK+ forwards the event to one
      or more widgets.

      In the C library the main loop is executed with @code{gtk_main()}. In the
      Lisp binding this function is implemented as @fun{gtk-main}, but in
      general it is not used. The @code{gtk_main()} function is replaced with
      the @fun{within-main-loop} macro, which does all necessary work to run the
      main loop. See the example for a typical main function in the Lisp
      binding.

      When widgets receive an event, they frequently emit one or more signals.
      Signals notify your program that \"something interesting happened\" by
      invoking functions you've connected to the signal with
      @fun{g-signal-connect}. Functions connected to a signal are often termed
      callbacks.

      When your callbacks are invoked, you would typically take some action -
      for example, when an Open button is clicked you might display a
      @class{gtk-file-chooser-dialog}. After a callback finishes, GTK+ will
      return to the main loop and await more user input.

      It is OK to use the GLib main loop directly instead of @sym{gtk-main},
      though it involves slightly more typing. See @type{g-main-loop} in the
      GLib documentation.

      @b{Example.} Typical main function in Lisp for a GTK+ application.
      @begin{pre}
(defun main ()
  (within-main-loop
    (let (;; Create the main window.
          (window (gtk-window-new :toplevel)))

      ;; Set up our GUI elements
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
    @begin[Accelerator Groups]{subsection}
      Groups of global keyboard accelerators for an entire @class{gtk-window}.

      @about-class{gtk-accel-group}
      @about-function{gtk-accel-group-new}
      @about-function{gtk-accel-group-connect}
      @about-function{gtk-accel-group-connect-by-path}
      @about-function{gtk-accel-group-disconnect}
      @about-function{gtk-accel-group-disconnect-key}
      @about-function{gtk-accel-group-query}
      @about-function{gtk-accel-group-activate}
      @about-function{gtk-accel-group-lock}
      @about-function{gtk-accel-group-unlock}
      @about-function{gtk-accel-group-get-is-locked}
      @about-function{gtk-accel-group-from-accel-closure}
      @about-function{gtk-accel-group-get-modifier-mask}
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
      Storing data on clipboards

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
    @begin[Stock items]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkSettings]{subsection}
      Sharing settings between applications

      @about-class{gtk-settings}
      @about-symbol{GtkSettingsValue}
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
      Standard Enumerations

      @about-symbol{gtk-accel-flags}
      @about-symbol{gtk-arrow-placement}
      @about-symbol{gtk-arrow-type}
      @about-symbol{gtk-attach-options}
      @about-symbol{gtk-button-box-style}
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

      @about-class{gtk-selection-data}
      @about-class{gtk-target-entry}
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

      @about-symbol{GTK_STYLE_PROPERTY_BACKGROUND_COLOR}
      @about-symbol{GTK_STYLE_PROPERTY_COLOR}
      @about-symbol{GTK_STYLE_PROPERTY_FONT}
      @about-symbol{GTK_STYLE_PROPERTY_MARGIN}
      @about-symbol{GTK_STYLE_PROPERTY_PADDING}
      @about-symbol{GTK_STYLE_PROPERTY_BORDER_WIDTH}
      @about-symbol{GTK_STYLE_PROPERTY_BORDER_RADIUS}
      @about-symbol{GTK_STYLE_PROPERTY_BORDER_STYLE}
      @about-symbol{GTK_STYLE_PROPERTY_BORDER_COLOR}
      @about-symbol{GTK_STYLE_PROPERTY_BACKGROUND_IMAGE}
      @about-symbol{GTK_STYLE_CLASS_BACKGROUND}
      @about-symbol{GTK_STYLE_CLASS_BUTTON}
      @about-symbol{GTK_STYLE_CLASS_CALENDAR}
      @about-symbol{GTK_STYLE_CLASS_CELL}
      @about-symbol{GTK_STYLE_CLASS_COMBOBOX_ENTRY}
      @about-symbol{GTK_STYLE_CLASS_CHECK}
      @about-symbol{GTK_STYLE_CLASS_DEFAULT}
      @about-symbol{GTK_STYLE_CLASS_ENTRY}
      @about-symbol{GTK_STYLE_CLASS_HEADER}
      @about-symbol{GTK_STYLE_CLASS_MENU}
      @about-symbol{GTK_STYLE_CLASS_RADIO}
      @about-symbol{GTK_STYLE_CLASS_RUBBERBAND}
      @about-symbol{GTK_STYLE_CLASS_SCROLLBAR}
      @about-symbol{GTK_STYLE_CLASS_SCROLLBARS_JUNCTION}
      @about-symbol{GTK_STYLE_CLASS_SLIDER}
      @about-symbol{GTK_STYLE_CLASS_TOOLTIP}
      @about-symbol{GTK_STYLE_CLASS_TROUGH}
      @about-symbol{GTK_STYLE_CLASS_ACCELERATOR}
      @about-symbol{GTK_STYLE_CLASS_DOCK}
      @about-symbol{GTK_STYLE_CLASS_GRIP}
      @about-symbol{GTK_STYLE_CLASS_MENUBAR}
      @about-symbol{GTK_STYLE_CLASS_MENUITEM}
      @about-symbol{GTK_STYLE_CLASS_PROGRESSBAR}
      @about-symbol{GTK_STYLE_CLASS_SPINNER}
      @about-symbol{GTK_STYLE_CLASS_TOOLBAR}
      @about-symbol{GTK_STYLE_CLASS_PRIMARY_TOOLBAR}
      @about-symbol{GTK_STYLE_CLASS_INLINE_TOOLBAR}
      @about-symbol{GTK_STYLE_CLASS_PANE_SEPARATOR}
      @about-symbol{GTK_STYLE_CLASS_SEPARATOR}
      @about-symbol{GTK_STYLE_CLASS_SIDEBAR}
      @about-symbol{GTK_STYLE_CLASS_DND}
      @about-symbol{GTK_STYLE_CLASS_ERROR}
      @about-symbol{GTK_STYLE_CLASS_EXPANDER}
      @about-symbol{GTK_STYLE_CLASS_FRAME}
      @about-symbol{GTK_STYLE_CLASS_HIGHLIGHT}
      @about-symbol{GTK_STYLE_CLASS_IMAGE}
      @about-symbol{GTK_STYLE_CLASS_INFO}
      @about-symbol{GTK_STYLE_CLASS_MARK}
      @about-symbol{GTK_STYLE_CLASS_NOTEBOOK}
      @about-symbol{GTK_STYLE_CLASS_QUESTION}
      @about-symbol{GTK_STYLE_CLASS_SCALE}
      @about-symbol{GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE}
      @about-symbol{GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW}
      @about-symbol{GTK_STYLE_CLASS_SPINBUTTON}
      @about-symbol{GTK_STYLE_CLASS_VIEW}
      @about-symbol{GTK_STYLE_CLASS_WARNING}
      @about-symbol{GTK_STYLE_CLASS_HORIZONTAL}
      @about-symbol{GTK_STYLE_CLASS_VERTICAL}
      @about-symbol{GTK_STYLE_CLASS_TOP}
      @about-symbol{GTK_STYLE_CLASS_BOTTOM}
      @about-symbol{GTK_STYLE_CLASS_LEFT}
      @about-symbol{GTK_STYLE_CLASS_RIGHT}
      @about-symbol{GTK_STYLE_CLASS_LINKED}
      @about-symbol{GTK_STYLE_CLASS_ARROW}
      @about-symbol{GTK_STYLE_REGION_COLUMN}
      @about-symbol{GTK_STYLE_REGION_COLUMN_HEADER}
      @about-symbol{GTK_STYLE_REGION_ROW}
      @about-symbol{GTK_STYLE_REGION_TAB}
      @about-class{gtk-style-context}
      @about-function{gtk-style-context-new}
      @about-function{gtk-style-context-add-provider}
      @about-function{gtk-style-context-add-provider-for-screen}
      @about-function{gtk-style-context-get}
      @about-function{gtk-style-context-get-direction}
      @about-function{gtk-style-context-get-junction-sides}
      @about-function{gtk-style-context-set-parent}
      @about-function{gtk-style-context-get-path}
      @about-function{gtk-style-context-get-property}
      @about-function{gtk-style-context-get-screen}
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
      @about-function{gtk-style-context-set-direction}
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
      @about-function{gtk-style-context-set-screen}
      @about-function{gtk-style-context-set-state}
      @about-class{gtk-border}
      @about-function{gtk-border-new}
      @about-function{gtk-border-copy}
      @about-function{gtk-border-free}
      @about-function{gtk-render-arrow}
      @about-function{gtk-render-background}
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
      @about-function{gtk-render-icon}
      @about-function{gtk-render-insertion-cursor}
    @end{subsection}
    @begin[GtkCssProvider]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkStyleProvider]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkStyleProperties]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkThemingEngine]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkWidgetPath]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkSymbolicColor]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkGradient]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkIconTheme]{subsection}
      Looking up icons by name

      @about-class{gtk-icon-theme}
      @about-symbol{gtk-icon-info}
      @about-symbol{gtk-icon-lookup-flags}
      @about-class{gtk-icon-theme-error}
      @about-function{gtk-icon-theme-error}
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
    @begin[GtkNumerableIcon]{subsection}
      not implemented
    @end{subsection}
    @begin[Resource Files]{subsection}
      Deprecated routines for handling resource files

      @about-class{gtk-rc-style}
      @about-symbol{gtk-rc-flags}
      @about-symbol{gtk-rc-token-type}
      @about-function{gtk-rc-scanner-new}
      @about-function{gtk-rc-get-style}
      @about-function{gtk-rc-get-style-by-paths}
      @about-function{gtk-rc-parse}
      @about-function{gtk-rc-parse-string}
      @about-function{gtk-rc-reparse-all}
      @about-function{gtk-rc-reparse-all-for-settings}
      @about-function{gtk-rc-reset-styles}
      @about-function{gtk-rc-add-default-file}
      @about-function{gtk-rc-get-default-files}
      @about-function{gtk-rc-set-default-files}
      @about-function{gtk-rc-parse-color}
      @about-function{gtk-rc-parse-color-full}
      @about-function{gtk-rc-parse-state}
      @about-function{gtk-rc-parse-priority}
      @about-function{gtk-rc-find-module-in-path}
      @about-function{gtk-rc-find-pixmap-in-path}
      @about-function{gtk-rc-get-module-dir}
      @about-function{gtk-rc-get-im-module-path}
      @about-function{gtk-rc-get-im-module-file}
      @about-function{gtk-rc-get-theme-dir}
      @about-function{gtk-rc-style-new}
      @about-function{gtk-rc-style-copy}
    @end{subsection}
    @begin[GtkStyle]{subsection}
      Functions for drawing widget parts

      @about-class{gtk-style}
      @about-function{gtk-style-new}
      @about-function{gtk-style-copy}
      @about-function{gtk-style-attach}
      @about-function{gtk-style-detach}
      @about-function{gtk-style-ref}
      @about-function{gtk-style-unref}
      @about-function{gtk-style-set-background}
      @about-function{gtk-style-apply-default-background}
      @about-function{gtk-style-apply-default-pixmap}
      @about-function{gtk-style-lookup-color}
      @about-function{gtk-style-lookup-icon-set}
      @about-function{gtk-style-render-icon}
      @about-function{gtk-style-get-font}
      @about-function{gtk-style-set-font}
      @about-function{gtk-style-get-style-property}
      @about-function{gtk-style-get-valist}
      @about-function{gtk-style-get}
      @about-function{gtk-draw-hline}
      @about-function{gtk-draw-vline}
      @about-function{gtk-draw-shadow}
      @about-function{gtk-draw-polygon}
      @about-function{gtk-draw-arrow}
      @about-function{gtk-draw-diamond}
      @about-function{gtk-draw-string}
      @about-function{gtk-draw-box}
      @about-function{gtk-draw-box-gap}
      @about-function{gtk-draw-check}
      @about-function{gtk-draw-extension}
      @about-function{gtk-draw-flat-box}
      @about-function{gtk-draw-focus}
      @about-function{gtk-draw-handle}
      @about-function{gtk-draw-option}
      @about-function{gtk-draw-shadow-gap}
      @about-function{gtk-draw-slider}
      @about-function{gtk-draw-tab}
      @about-function{gtk-draw-expander}
      @about-function{gtk-draw-layout}
      @about-function{gtk-draw-resize-grip}
      @about-function{gtk-paint-arrow}
      @about-function{gtk-paint-box}
      @about-function{gtk-paint-box-gap}
      @about-function{gtk-paint-check}
      @about-function{gtk-paint-diamond}
      @about-function{gtk-paint-extension}
      @about-function{gtk-paint-flat-box}
      @about-function{gtk-paint-focus}
      @about-function{gtk-paint-handle}
      @about-function{gtk-paint-hline}
      @about-function{gtk-paint-option}
      @about-function{gtk-paint-polygon}
      @about-function{gtk-paint-shadow}
      @about-function{gtk-paint-shadow-gap}
      @about-function{gtk-paint-slider}
      @about-function{gtk-paint-spinner}
      @about-function{gtk-paint-string}
      @about-function{gtk-paint-tab}
      @about-function{gtk-paint-vline}
      @about-function{gtk-paint-expander}
      @about-function{gtk-paint-layout}
      @about-function{gtk-paint-resize-grip}
      @about-function{gtk-draw-insertion-cursor}
      @about-symbol{gtk-rc-property}
    @end{subsection}
  @end{section}
  @begin[Windows]{section}
    @begin[GtkDialog]{subsection}
      Create popup windows.

      @about-class{gtk-dialog}
      @about-symbol{gtk-dialog-flags}
      @about-symbol{gtk-response-type}
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
      @about-function{gtk-alternative-dialog-button-order}
      @about-function{gtk-dialog-set-alternative-button-order}
      @about-function{gtk-dialog-set-alternative-button-order-from-array}
    @end{subsection}
    @begin[GtkInvisible]{subsection}
      A widget which is not displayed.

      @about-class{gtk-invisible}
      @about-function{gtk-invisible-new}
      @about-function{gtk-invisible-new-for-screen}
    @end{subsection}
    @begin[GtkMessageDialog]{subsection}
      A convenient message window.

      @about-class{gtk-message-dialog}
      @about-symbol{gtk-message-type}
      @about-symbol{gtk-buttons-type}
      @about-function{gtk-message-dialog-new}
      @about-function{gtk-message-dialog-new-with-markup}
      @about-function{gtk-message-dialog-set-markup}
      @about-function{gtk-message-dialog-set-image}
      @about-function{gtk-message-dialog-get-image}
      @about-function{gtk-message-dialog-format-secondary-text}
      @about-function{gtk-message-dialog-format-secondary-markup}
      @about-function{gtk-message-dialog-get-message-area}
    @end{subsection}
    @begin[GtkWindow]{subsection}
      Toplevel which can contain other widgets.

      @about-class{gtk-window}
      @about-function{gtk-window-new}
      @about-function{gtk-window-set-title}
      @about-function{gtk-window-set-wmclass}
      @about-function{gtk-window-set-resizable}
      @about-function{gtk-window-get-resizable}
      @about-function{gtk-window-add-accel-group}
      @about-function{gtk-window-remove-accel-group}
      @about-function{gtk-window-activate-focus}
      @about-function{gtk-window-activate-default}
      @about-function{gtk-window-set-modal}
      @about-function{gtk-window-set-default-size}
      @about-function{gtk-window-set-default-geometry}
      @about-function{gtk-window-set-geometry-hints}
      @about-function{gtk-window-set-gravity}
      @about-function{gtk-window-get-gravity}
      @about-function{gtk-window-set-position}
      @about-function{gtk-window-set-transient-for}
      @about-function{gtk-window-set-attached-to}
      @about-function{gtk-window-set-destroy-with-parent}
      @about-function{gtk-window-set-hide-titlebar-when-maximized}
      @about-function{gtk-window-set-screen}
      @about-function{gtk-window-get-screen}
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
      @about-function{gtk-window-iconify}
      @about-function{gtk-window-deiconify}
      @about-function{gtk-window-stick}
      @about-function{gtk-window-unstick}
      @about-function{gtk-window-maximize}
      @about-function{gtk-window-unmaximize}
      @about-function{gtk-window-fullscreen}
      @about-function{gtk-window-unfullscreen}
      @about-function{gtk-window-set-keep-above}
      @about-function{gtk-window-set-keep-below}
      @about-function{gtk-window-begin-resize-drag}
      @about-function{gtk-window-begin-move-drag}
      @about-function{gtk-window-set-decorated}
      @about-function{gtk-window-set-deletable}
      @about-function{gtk-window-set-mnemonic-modifier}
      @about-function{gtk-window-set-type-hint}
      @about-function{gtk-window-set-skip-taskbar-hint}
      @about-function{gtk-window-set-skip-pager-hint}
      @about-function{gtk-window-set-urgency-hint}
      @about-function{gtk-window-set-accept-focus}
      @about-function{gtk-window-set-focus-on-map}
      @about-function{gtk-window-set-startup-id}
      @about-function{gtk-window-set-role}
      @about-function{gtk-window-get-decorated}
      @about-function{gtk-window-get-deletable}
      @about-function{gtk-window-get-default-icon-list}
      @about-function{gtk-window-get-default-icon-name}
      @about-function{gtk-window-get-default-size}
      @about-function{gtk-window-get-destroy-with-parent}
      @about-function{gtk-window-get-hide-titlebar-when-maximized}
      @about-function{gtk-window-get-icon}
      @about-function{gtk-window-get-icon-list}
      @about-function{gtk-window-get-icon-name}
      @about-function{gtk-window-get-mnemonic-modifier}
      @about-function{gtk-window-get-modal}
      @about-function{gtk-window-get-position}
      @about-function{gtk-window-get-role}
      @about-function{gtk-window-get-size}
      @about-function{gtk-window-get-title}
      @about-function{gtk-window-get-transient-for}
      @about-function{gtk-window-get-attached-to}
      @about-function{gtk-window-get-type-hint}
      @about-function{gtk-window-get-skip-taskbar-hint}
      @about-function{gtk-window-get-skip-pager-hint}
      @about-function{gtk-window-get-urgency-hint}
      @about-function{gtk-window-get-accept-focus}
      @about-function{gtk-window-get-focus-on-map}
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
      @about-function{gtk-window-set-icon}
      @about-function{gtk-window-set-icon-list}
      @about-function{gtk-window-set-icon-from-file}
      @about-function{gtk-window-set-icon-name}
      @about-function{gtk-window-set-auto-startup-notification}
      @about-function{gtk-window-get-opacity}
      @about-function{gtk-window-set-opacity}
      @about-function{gtk-window-get-mnemonics-visible}
      @about-function{gtk-window-set-mnemonics-visible}
      @about-function{gtk-window-get-focus-visible}
      @about-function{gtk-window-set-focus-visible}
      @about-function{gtk-window-set-has-resize-grip}
      @about-function{gtk-window-get-has-resize-grip}
      @about-function{gtk-window-resize-grip-is-visible}
      @about-function{gtk-window-get-resize-grip-area}
      @about-function{gtk-window-get-application}
      @about-function{gtk-window-set-application}
      @about-function{gtk-window-set-has-user-ref-count}
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
    @begin[GtkAboutDialog]{subsection}
      Display information about an application.

      @about-class{gtk-about-dialog}
      @about-symbol{gtk-license}
      @about-function{gtk-about-dialog-new}
      @about-function{gtk-about-dialog-add-credit-section}
      @about-function{gtk-show-about-dialog}
    @end{subsection}
    @begin[GtkAssistant]{subsection}
      A widget used to guide users through multi-step operations.

      @about-class{gtk-assistant}
      @about-symbol{gtk-assistant-page-type}
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
    @begin[GtkOffscreenWindow]{subsection}
      A toplevel to manage offscreen rendering of child widgets.

      @about-class{gtk-offscreen-window}
      @about-function{gtk-offscreen-window-new}
      @about-function{gtk-offscreen-window-get-surface}
      @about-function{gtk-offscreen-window-get-pixbuf}
    @end{subsection}
  @end{section}
  @begin[Display Widgets]{section}
    @begin[GtkAccelLabel]{subsection}
      A label which displays an accelerator key on the right of the text.

      @about-class{gtk-accel-label}
      @about-function{gtk-accel-label-new}
      @about-function{gtk-accel-label-refetch}
    @end{subsection}
    @begin[GtkImage]{subsection}
      A widget displaying an image.

      @about-class{gtk-image}
      @about-symbol{gtk-image-type}
      @about-function{gtk-image-get-animation}
      @about-function{gtk-image-new-from-file}
      @about-function{gtk-image-new-from-icon-set}
      @about-function{gtk-image-new-from-pixbuf}
      @about-function{gtk-image-new-from-stock}
      @about-function{gtk-image-new-from-animation}
      @about-function{gtk-image-new-from-icon-name}
      @about-function{gtk-image-new-from-gicon}
      @about-function{gtk-image-set-from-file}
      @about-function{gtk-image-set-from-icon-set}
      @about-function{gtk-image-set-from-pixbuf}
      @about-function{gtk-image-set-from-stock}
      @about-function{gtk-image-set-from-animation}
      @about-function{gtk-image-set-from-icon-name}
      @about-function{gtk-image-set-from-gicon}
      @about-function{gtk-image-clear}
      @about-function{gtk-image-new}
    @end{subsection}
    @begin[GtkLabel]{subsection}
      A widget that displays a small to medium amount of text.

      @about-class{gtk-label}
      @about-function{gtk-label-new}
      @about-function{gtk-label-text}
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
    @begin[GtkProgressBar]{subsection}
      A widget which indicates progress visually.

      @about-class{gtk-progress-bar}
      @about-function{gtk-progress-bar-new}
      @about-function{gtk-progress-bar-pulse}
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
    @begin[GtkInfoBar]{subsection}
      Report important messages to the user.

      @about-class{gtk-info-bar}
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
    @begin[GtkStatusIcon]{subsection}
      Display an icon in the system tray.

      @about-class{gtk-status-icon}
      @about-function{gtk-status-icon-new}
      @about-function{gtk-status-new-from-pixbuf}
      @about-function{gtk-status-new-from-file}
      @about-function{gtk-status-new-from-stock}
      @about-function{gtk-status-new-from-icon-name}
      @about-function{gtk-status-new-from-gicon}
      @about-function{gtk-status-set-from-pixbuf}
      @about-function{gtk-status-set-from-file}
      @about-function{gtk-status-set-from-stock}
      @about-function{gtk-status-set-from-icon-name}
      @about-function{gtk-status-set-from-gicon}
      @about-function{gtk-status-is-embedded}
      @about-function{gtk-status-position-menu}
      @about-function{gtk-status-get-geometry}
      @about-function{gtk-status-get-x11-window-id}
    @end{subsection}
    @begin[GtkSpinner]{subsection}
      Show a spinner animation.

      @about-class{gtk-spinner}
      @about-function{gtk-spinner-new}
      @about-function{gtk-spinner-start}
      @about-function{gtk-spinner-stop}
    @end{subsection}
  @end{section}
  @begin[Buttons and Toggles]{section}
    @begin[GtkButton]{subsection}
      A widget that emits a signal when clicked on

      @about-class{gtk-button}
      @about-function{gtk-button-new}
      @about-function{gtk-button-new-with-label}
      @about-function{gtk-button-new-with-mnemonic}
      @about-function{gtk-button-new-from-stock}
      @about-function{gtk-button-pressed}
      @about-function{gtk-button-released}
      @about-function{gtk-button-clicked}
      @about-function{gtk-button-enter}
      @about-function{gtk-button-leave}
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
      @about-function{gtk-radio-button-new}
      @about-function{gtk-radio-button-new-from-widget}
      @about-function{gtk-radio-button-new-with-label}
      @about-function{gtk-radio-button-new-with-label-from-widget}
      @about-function{gtk-radio-button-new-with-mnemonic}
      @about-function{gtk-radio-button-with-mnemonic-from-widget}
      @about-function{gtk-radio-button-join-group}
    @end{subsection}
    @begin[GtkToggleButton]{subsection}
      Create buttons which retain their state.

      @about-class{gtk-toggle-button}
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
      @about-function{gtk-link-button-new}
      @about-function{gtk-link-button-new-with-label}
    @end{subsection}
    @begin[GtkScaleButton]{subsection}
    @end{subsection}
    @begin[GtkVolumeButton]{subsection}
    @end{subsection}
    @begin[GtkSwitch]{subsection}
    @end{subsection}
    @begin[GtkLockButton]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Numeric and Text Data Entry]{section}
    @begin[GtkEntry]{subsection}
    @end{subsection}
    @begin[GtkEntryBuffer]{subsection}
    @end{subsection}
    @begin[GtkEntryCompletion]{subsection}
    @end{subsection}
    @begin[GtkScale]{subsection}
    @end{subsection}
    @begin[GtkSpinButton]{subsection}
    @end{subsection}
    @begin[GtkEditable]{subsection}
    @end{subsection}
  @end{section}
  @begin[Multiline Text Editor]{section}
    @begin[GtkTextIter]{subsection}
    @end{subsection}
    @begin[GtkTextMark]{subsection}
    @end{subsection}
    @begin[GtkTextBuffer]{subsection}
    @end{subsection}
    @begin[GtkTextTag]{subsection}
    @end{subsection}
    @begin[GtkTextTagTable]{subsection}
    @end{subsection}
    @begin[GtkTextView]{subsection}
    @end{subsection}
  @end{section}
  @begin[Tree, List and Icon Grid Widgets]{section}
    @begin[GtkTreeModel]{subsection}
    @end{subsection}
    @begin[GtkTreeSelection]{subsection}
    @end{subsection}
    @begin[GtkTreeViewColumn]{subsection}
    @end{subsection}
    @begin[GtkTreeView]{subsection}
    @end{subsection}
    @begin[GtkTreeView drag and drop]{subsection}
    @end{subsection}
    @begin[GtkCellView]{subsection}
    @end{subsection}
    @begin[GtkIconView]{subsection}
    @end{subsection}
    @begin[GtkTreeSortable]{subsection}
    @end{subsection}
    @begin[GtkTreeModelSort]{subsection}
    @end{subsection}
    @begin[GtkModelFilter]{subsection}
    @end{subsection}
    @begin[GtkCellLayout]{subsection}
    @end{subsection}
    @begin[GtkCellArea]{subsection}
    @end{subsection}
    @begin[GtkCellAreaBox]{subsection}
    @end{subsection}
    @begin[GtkcellAreaContext]{subsection}
    @end{subsection}
    @begin[GtkCellRenderer]{subsection}
    @end{subsection}
    @begin[GtkCellEditable]{subsection}
    @end{subsection}
    @begin[GtkCellRendererAccel]{subsection}
    @end{subsection}
    @begin[GtkCellRendererComo]{subsection}
    @end{subsection}
    @begin[GtkCellRendererPixbuf]{subsection}
    @end{subsection}
    @begin[GtkCellRendererProgress]{subsection}
    @end{subsection}
    @begin[GtkCellRendererSpin]{subsection}
    @end{subsection}
    @begin[GtkCellRendererText]{subsection}
    @end{subsection}
    @begin[GtkCellRendererToggle]{subsection}
    @end{subsection}
    @begin[GtkCellRendererSpinner]{subsection}
    @end{subsection}
    @begin[GtkListStore]{subsection}
    @end{subsection}
    @begin[GtkTreeStore]{subsection}
    @end{subsection}
  @end{section}
  @begin[Menus, Combo Box, Toolbar]{section}
    @begin[GtkComboBox]{subsection}
    @end{subsection}
    @begin[GtkComboBoxText]{subsection}
    @end{subsection}
    @begin[GtkMenu]{subsection}
      A menu widget

      @about-class{gtk-menu}
      @about-function{gtk-menu-new}
      @about-function{gtk-menu-from-model}
      @about-function{gtk-menu-set-screen}
      @about-function{gtk-menu-reorder-child}
      @about-function{gtk-menu-attach}
      @about-function{gtk-menu-popup-for-device}
      @about-function{gtk-menu-popup}
      @about-function{gtk-menu-set-accel-group}
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
      @about-function{gtk-menu-bar-new}
      @about-function{gtk-menu-bar-new-from-model}
      @about-symbol{gtk-pack-direction}
      @about-function{gtk-menu-bar-set-pack-direction}
      @about-function{gtk-menu-bar-get-pack-direction}
      @about-function{gtk-menu-bar-set-child-pack-direction}
      @about-function{gtk-menu-bar-get-child-pack-direction}
    @end{subsection}
    @begin[GtkMenuItem]{subsection}
    @end{subsection}
    @begin[GtkImageMenuItem]{subsection}
    @end{subsection}
    @begin[GtkRadioMenuItem]{subsection}
      A choice from multiple check menu items

      @about-class{gtk-radio-menu-item}
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
    @end{subsection}
    @begin[GtkSeparatorMenuItem]{subsection}
    @end{subsection}
    @begin[GtkTearoffMenuItem]{subsection}
    @end{subsection}
    @begin[GtkToolShell]{subsection}
    @end{subsection}
    @begin[GtkTollbar]{subsection}
    @end{subsection}
    @begin[GtkToolItem]{subsection}
    @end{subsection}
    @begin[GtkToolPalette]{subsection}
    @end{subsection}
    @begin[GtkToolItemGroup]{subsection}
    @end{subsection}
    @begin[GtkSeparatorToolItem]{subsection}
    @end{subsection}
    @begin[GtkToolButton]{subsection}
    @end{subsection}
    @begin[GtkMenuToolButton]{subsection}
    @end{subsection}
    @begin[GtkToogleToolButton]{subsection}
    @end{subsection}
    @begin[GtkRadioToolButton]{subsection}
      A toolbar item that contains a radio button.

      @about-class{gtk-radio-tool-button}
      @about-function{gtk-radio-tool-button-new}
      @about-function{gtk-radio-tool-button-new-from-stock}
      @about-function{gtk-radio-tool-button-new-from-widget}
      @about-function{gtk-radio-tool-button-new-with-stock-from-widget}
      @about-function{gtk-radio-tool-button-get-group}
      @about-function{gtk-radio-tool-button-set-group}
    @end{subsection}
  @end{section}
  @begin[Action-based menus and toolbars]{section}
    @begin[GtkUIManager]{subsection}
    @end{subsection}
    @begin[GtkActionGroup]{subsection}
      A group of actions.

      @about-class{gtk-action-group}
      @about-function{gtk-action-group-new}
      @about-function{gtk-action-group-get-action}
      @about-function{gtk-action-group-list-actions}
      @about-function{gtk-action-group-add-action}
      @about-function{gtk-action-group-add-action-with-accel}
      @about-function{gtk-action-group-remove-action}
      @about-symbol{gtk-action-entry}
      @about-function{gtk-action-group-add-actions}
      @about-function{gtk-action-group-add-actions-full}
      @about-class{gtk-toggle-action-entry}
      @about-function{gtk-action-group-add-toggle-actions}
      @about-function{gtk-action-group-add-toggle-actions-full}
      @about-class{gtk-radio-action-entry}
      @about-function{gtk-action-group-add-radio-actions}
      @about-function{gtk-action-group-add-radio-actions-full}
      @about-function{gtk-action-group-set-translate-func}
      @about-function{gtk-action-group-set-translation-domain}
      @about-function{gtk-action-group-translate-string}
    @end{subsection}
    @begin[GtkAction]{subsection}
      An action which can be triggered by a menu or toolbar item.

      @about-class{gtk-action}
      @about-function{gtk-action-new}
      @about-function{gtk-action-is-sensitive}
      @about-function{gtk-action-is-visible}
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
      @about-function{gtk-action-get-accel-path}
      @about-function{gtk-action-set-accel-path}
      @about-function{gtk-action-get-accel-closure}
      @about-function{gtk-action-set-accel-group}
    @end{subsection}
    @begin[GtkToogleAction]{subsection}
    @end{subsection}
    @begin[GtkRadioAction]{subsection}
    @end{subsection}
    @begin[GtkRecentAction]{subsection}
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
  @end{section}
  @begin[Selectors (Color, File and Font)]{section}
    @begin[GtkColorChooser]{subsection}
    @end{subsection}
    @begin[GtkColorButton]{subsection}
    @end{subsection}
    @begin[GtkColorChooserWidget]{subsection}
    @end{subsection}
    @begin[GtkColorChooserDialog]{subsection}
    @end{subsection}
    @begin[GtkColorSelection]{subsection}
    @end{subsection}
    @begin[GtkColorSelectionDialog]{subsection}
    @end{subsection}
    @begin[GtkHSV]{subsection}
    @end{subsection}
    @begin[GtkFileChooser]{subsection}
    @end{subsection}
    @begin[GtkFileChooserButton]{subsection}
      A button to launch a file selection dialog.

      @about-class{gtk-file-chooser-button}
      @about-function{gtk-file-chooser-button-new}
      @about-function{gtk-file-chooser-button-new-with-dialog}
      @about-function{gtk-file-chooser-button-get-title}
      @about-function{gtk-file-chooser-button-set-title}
      @about-function{gtk-file-chooser-button-get-width-chars}
      @about-function{gtk-file-chooser-button-set-width-chars}
      @about-function{gtk-file-chooser-button-get-focus-on-click}
      @about-function{gtk-file-chooser-button-set-focus-on-click}
    @end{subsection}
    @begin[GtkFileChooserDialog]{subsection}
    @end{subsection}
    @begin[GtkFileChooserWidget]{subsection}
    @end{subsection}
    @begin[GtkFileFilter]{subsection}
    @end{subsection}
    @begin[GtkFontChooser]{subsection}
    @end{subsection}
    @begin[GtkFontButton]{subsection}
    @end{subsection}
    @begin[GtkFontChooserWidget]{subsection}
    @end{subsection}
    @begin[GtkFontChooserDialog]{subsection}
    @end{subsection}
    @begin[GtkSelection]{subsection}
    @end{subsection}
    @begin[GtkFonSelectionDialog]{subsection}
    @end{subsection}
  @end{section}
  @begin[Layout Containers]{section}
    @begin[GtkGrid]{subsection}
      Pack widgets in a rows and columns.

      @about-class{gtk-grid}
      @about-function{gtk-grid-new}
      @about-function{gtk-grid-attach}
      @about-function{gtk-grid-attach-next-to}
      @about-function{gtk-grid-get-child-at}
      @about-function{gtk-grid-insert-row}
      @about-function{gtk-grid-insert-column}
      @about-function{gtk-grid-insert-next-to}
    @end{subsection}
    @begin[GtkAlignment]{subsection}
      A widget which controls the alignment and size of its child.

      @about-class{gtk-alignment}
      @about-function{gtk-alignment-new}
      @about-function{gtk-alignment-set}
      @about-function{gtk-alignment-get-padding}
      @about-function{gtk-alignment-set-padding}
    @end{subsection}
    @begin[GtkAspectFrame]{subsection}
      A frame that constrains its child to a particular aspect ratio.

      @about-class{gtk-aspect-frame}
      @about-function{gtk-aspect-frame-new}
      @about-function{gtk-aspect-frame-set}
    @end{subsection}
    @begin[GtkBox]{subsection}
      A container box.

      @about-class{gtk-box}
      @about-function{gtk-box-new}
      @about-function{gtk-box-pack-start}
      @about-function{gtk-box-pack-end}
      @about-function{gtk-box-reorder-child}
      @about-function{gtk-box-query-child-packing}
      @about-function{gtk-box-set-child-packing}
      A horizontal container box.

      @about-class{gtk-hbox}
      @about-function{gtk-hbox-new}
      A vertical container box.

      @about-class{gtk-vbox}
      @about-function{gtk-vbox-new}
    @end{subsection}
    @begin[GtkButtonBox]{subsection}
      A container for arranging buttons

      @about-class{gtk-button-box}
      @about-function{gtk-button-box-new}
      @about-function{gtk-button-box-get-layout}
      @about-function{gtk-button-box-get-child-secondary}
      @about-function{gtk-button-box-child-non-homogeneous}
      @about-function{gtk-button-box-set-layout}
      @about-function{gtk-button-box-set-child-secondary}
      @about-function{gtk-button-box-set-child-non-homogeneous}

      @b{GtkHButtonBox}

      A container for arranging buttons horizontally

      @about-class{gtk-hbutton-box}
      @about-function{gtk-hbutton-box-new}

      @b{GtkVButtonBox}

      A container for arranging buttons vertically

      @about-class{gtk-vbutton-box}
      @about-function{gtk-vbutton-box-new}
    @end{subsection}
    @begin[GtkFixed]{subsection}
    @end{subsection}
    @begin[GtkPaned]{subsection}
    @end{subsection}
    @begin[GtkLayout]{subsection}
    @end{subsection}
    @begin[GtkNotebook]{subsection}
      A tabbed notebook container.

      @about-class{gtk-notebook}
      @about-function{gtk-notebook-new}
      @about-function{gtk-notebook-append-page}
      @about-function{gtk-notebook-append-page-menu}
      @about-function{gtk-notebook-prepend-page}
      @about-function{gtk-notebook-prepend-page-menu}
      @about-function{gtk-notebook-insert-page}
      @about-function{gtk-notebook-insert-page-menu}
      @about-function{gtk-notebook-remove-page}
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
    @begin[GtkTable]{subsection}
      Pack widgets in regular patterns

      @about-class{gtk-table}
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
    @begin[GtkExpander]{subsection}
    @end{subsection}
    @begin[GtkOverlay]{subsection}
    @end{subsection}
    @begin[GtkOrientable]{subsection}
      An interface for flippable widgets.

      @about-class{gtk-orientable}
    @end{subsection}
  @end{section}
  @begin[Ornaments]{section}
    @begin[GtkFrame]{subsection}
      A bin with a decorative frame and optional label.

      @about-class{gtk-frame}
      @about-function{gtk-frame-new}
     @about-function{gtk-frame-set-label}
     @about-function{gtk-frame-set-label-widget}
     @about-function{gtk-frame-set-label-align}
     @about-function{gtk-frame-set-shadow-type}
     @about-function{gtk-frame-get-label}
     @about-function{gtk-frame-get-label-align}
     @about-function{gtk-frame-get-label-widget}
     @about-function{gtk-frame-get-shadow-type}
    @end{subsection}
    @begin[GtkSeparator]{subsection}
      A separator widget.

      @about-class{gtk-separator}
      @about-function{gtk-separator-new}
      @about-class{gtk-hseparator}
      @about-function{gtk-hseparator-new}
      @about-class{gtk-vseparator}
      @about-function{gtk-vseparator-new}
    @end{subsection}
  @end{section}
  @begin[Scrolling]{section}
    @begin[GtkScrollbar]{subsection}
    @end{subsection}
    @begin[GtkScrolledWindow]{subsection}
    @end{subsection}
    @begin[GtkScrollable]{subsection}
    @end{subsection}
  @end{section}
  @begin[Printing]{section}
    @begin[GtkPrintOperation]{subsection}
    @end{subsection}
    @begin[GtkPrintContext]{subsection}
    @end{subsection}
    @begin[GtkPrintSettings]{subsection}
    @end{subsection}
    @begin[GtkPageSetup]{subsection}
    @end{subsection}
    @begin[GtkPaperSize]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkPrinter]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkPrintJob]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkPrintUnixDialog]{subsection}
    @end{subsection}
    @begin[GtkPageSetupUnixDialog]{subsection}
    @end{subsection}
  @end{section}
  @begin[Miscellaneous]{section}
    @begin[GtkAdjustment]{subsection}
      A representation of an adjustable bounded value.

      @about-class{gtk-adjustment}
      @about-function{gtk-adjustment-new}
      @about-function{gtk-adjustment-clamp-page}
      @about-function{gtk-adjustment-changed}
      @about-function{gtk-adjustment-value-changed}
      @about-function{gtk-adjustment-configure}
      @about-function{gtk-adjustment-get-minimum-increment}
    @end{subsection}
    @begin[GtkArrow]{subsection}
      Displays an arrow.

      @about-class{gtk-arrow}
      @about-function{gtk-arrow-new}
      @about-function{gtk-arrow-set}
    @end{subsection}
    @begin[GtkCalendar]{subsection}
      Displays a calendar and allows the user to select a date.

      @about-class{gtk-calendar}
      @about-symbol{gtk-calendar-display-options}
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
    @end{subsection}
    @begin[GtkEventBox]{subsection}
    @end{subsection}
    @begin[GtkHandleBox]{subsection}
    @end{subsection}
    @begin[GtkIMContexSimple]{subsection}
    @end{subsection}
    @begin[GtkIMMulticontex]{subsection}
    @end{subsection}
    @begin[GtkSizeGroup]{subsection}
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
    @end{subsection}
    @begin[GtkAccessible]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Abstract Base Classes]{section}
    @begin[GtkWidget]{subsection}
      Base class for all widgets.

      @about-class{gtk-widget}
      @about-class{gtk-widget-class}
      @about-struct{gtk-requisition}
      @about-struct{gtk-allocation}
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
      @about-function{gtk-widget-size-request}
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
      @about-function{gtk-widget-grab-focus}
      @about-function{gtk-widget-grab-default}
      @about-function{gtk-widget-set-state}
      @about-function{gtk-widget-get-parent-window}
      @about-function{gtk-widget-set-parent-window}
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
      @about-function{gtk-widget-ensure-style}
      @about-function{gtk-widget-reset-rc-styles}
      @about-function{gtk-widget-get-default-style}
      @about-function{gtk-widget-get-direction}
      @about-function{gtk-widget-set-direction}
      @about-function{gtk-widget-get-default-direction}
      @about-function{gtk-widget-set-default-direction}
      @about-function{gtk-widget-shape-combine-region}
      @about-function{gtk-widget-input-shape-combine-region}
      @about-function{gtk-widget-path}
      @about-function{gtk-widget-class-path}
      @about-function{gtk-widget-get-composite-name}
      @about-function{gtk-widget-set-composite-name}
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
      @about-function{gtk-widget-set-redraw-on-allocate}
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
      @about-function{gtk-widget-set-child-visible}
      @about-function{gtk-widget-get-settings}
      @about-function{gtk-widget-get-clipboard}
      @about-function{gtk-widget-get-display}
      @about-function{gtk-widget-get-root-window}
      @about-function{gtk-widget-get-screen}
      @about-function{gtk-widget-has-screen}
      @about-function{gtk-widget-get-size-request}
      @about-function{gtk-widget-set-size-request}
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
      @about-function{gtk-cairo-should-draw-window}
      @about-function{gtk-cairo-transform-to-window}
      @about-function{gtk-widget-get-allocated-width}
      @about-function{gtk-widget-get-allocated-height}
      @about-function{gtk-widget-get-allocation}
      @about-function{gtk-widget-set-allocation}
      @about-function{gtk-widget-get-has-window}
      @about-function{gtk-widget-set-has-window}
      @about-function{gtk-widget-is-sensitive}
      @about-function{gtk-widget-get-state}
      @about-function{gtk-widget-set-state-flags}
      @about-function{gtk-widget-unset-state-flags}
      @about-function{gtk-widget-get-state-flags}
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
      @about-function{gtk-widget-get-path}
      @about-function{gtk-widget-get-style-context}
      @about-function{gtk-widget-reset-style}
      @about-function{gtk-requisition-new}
      @about-function{gtk-requisition-copy}
      @about-function{gtk-requisition-free}
      @about-symbol{gtk-size-request-mode}
      @about-symbol{gtk-requested-size}
      @about-function{gtk-widget-get-preferred-height}
      @about-function{gtk-widget-get-preferred-width}
      @about-function{gtk-widget-get-preferred-height-for-width}
      @about-function{gtk-widget-get-preferred-width-for-height}
      @about-function{gtk-widget-get-request-mode}
      @about-function{gtk-widget-get-preferred-size}
      @about-function{gtk-distribute-natural-allocation}
      @about-function{gtk-widget-queue-compute-expand}
      @about-function{gtk-widget-compute-expand}
    @end{subsection}
    @begin[GtkContainer]{subsection}
      Base class for widgets which contain other widgets.

      @about-class{gtk-container}
      @about-function{GTK_IS_RESIZE_CONTAINER}
      @about-function{GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID}
      @about-function{gtk-container-add}
      @about-function{gtk-container-remove}
      @about-function{gtk-container-add-with-properties}
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
    @end{subsection}
    @begin[GtkMisc]{subsection}
      Base class for widgets with alignments and padding.

      @about-class{gtk-misc}
    @end{subsection}
    @begin[GtkRange]{subsection}
      Base class for widgets which visualize an adjustment

      @about-class{gtk-range}
      @about-function{gtk-range-get-fill-level}
      @about-function{gtk-range-get-restrict-to-fill-level}
      @about-function{gtk-range-get-show-fill-level}
      @about-function{gtk-range-set-fill-level}
      @about-function{gtk-range-set-restrict-to-fill-level}
      @about-function{gtk-range-set-show-fill-level}
      @about-function{gtk-range-get-adjustment}
      @about-function{gtk-range-set-adjustment}
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
    @end{subsection}
  @end{section}
  @begin[Cross-process Embedding]{section}
    @begin[GtkPlug]{subsection}
    @end{subsection}
    @begin[GtkSocket]{subsection}
    @end{subsection}
  @end{section}
  @begin[Recently Used Documents]{section}
    @begin[GtkRecentManager]{subsection}
    @end{subsection}
    @begin[GtkRecentChooser]{subsection}
    @end{subsection}
    @begin[GtkRecentChooserDialog]{subsection}
    @end{subsection}
    @begin[GtkRecentChooserMenu]{subsection}
    @end{subsection}
    @begin[GtkRecentChooserWidget]{subsection}
    @end{subsection}
    @begin[GtkRecentFilter]{subsection}
    @end{subsection}
  @end{section}
  @begin[Choosing from installed applications]{section}
    @begin[GtkAppChooser]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkAppChooserButton]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkAppChooserDialog]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkAppChooserWidget]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Interface builder]{section}
    @begin[GtkBuildable]{subsection}
    @end{subsection}
    @begin[GtkBuilder]{subsection}
    @end{subsection}
  @end{section}
  @begin[Application support]{section}
    @begin[GtkApplication]{subsection}
      Application class

      @about-class{gtk-application}
      @about-class{gtk-application-inhibit-flags}
      @about-function{gtk-application-new}
      @about-function{gtk-application-add-window}
      @about-function{gtk-application-remove-window}
      @about-function{gtk-application-get-windows}
      @about-function{gtk-application-inhibit}
      @about-function{gtk-application-uninhibit}
      @about-function{gtk-application-is-inhibited}
      @about-function{gtk-application-get-app-menu}
      @about-function{gtk-application-set-app-menu}
      @about-function{gtk-application-get-menubar}
      @about-function{gtk-application-set-menubar}
    @end{subsection}
    @begin[GtkApplicationWindow]{subsection}
      GtkWindow subclass with GtkApplication support

      @about-class{gtk-application-window}
      @about-function{gtk-application-window-new}
    @end{subsection}
    @begin[GtkActionable]{subsection}
      An interface for widgets that can be associated with actions.

      @about-class{gtk-actionable}
      @about-class{gtk-actionable-interface}
      @about-function{gtk-actionable-get-action-target-value}
      @about-function{gtk-actionable-set-action-target-value}
      @about-function{gtk-actionable-set-detailed-action-name}
    @end{subsection}
  @end{section}
")

;;; --- End of file gtk.package.lisp -------------------------------------------
