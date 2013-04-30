;;; ----------------------------------------------------------------------------
;;; gtk.package.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
 "This is the API documentation of a Lisp binding to GTK+.
  GTK+ is a library for creating graphical user interfaces. It works on many
  UNIX-like platforms, Windows, and OS X. GTK+ is released under the GNU Library
  General Public License (GNU LGPL), which allows for flexible licensing of
  client applications. GTK+ has a C-based object-oriented architecture that
  allows for maximum flexibility. Bindings for many other languages have been
  written, including C++, Objective-C, Guile/Scheme, Perl, Python, TOM, Ada95,
  Free Pascal, and Eiffel.
  @begin[GTK+ Core Reference]{section}
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
      @about-variable{+gtk-major-version+}
      @about-variable{+gtk-minor-version+}
      @about-variable{+gtk-micro-version+}
      @about-variable{+gtk-binary-age+}
      @about-variable{+gtk-interface-age+}
    @end{subsection}
    @begin[Accelerator Groups]{subsection}
      Groups of global keyboard accelerators for an entire @class{gtk-window}
      widget.

      @about-class{gtk-accel-group}
      @about-function{gtk-accel-group-new}
      @about-function{gtk-accel-group-connect}
      @about-function{gtk-accel-group-connect-by-path}
      @about-function{gtk-accel-group-disconnect}
      @about-function{gtk-accel-group-disconnect-key}
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
    @begin[Stock items]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkSettings]{subsection}
      Sharing settings between applications.

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

      @about-struct{gtk-selection-data}
      @about-struct{gtk-target-entry}
      @about-struct{gtk-target-list}
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
      @about-symbol{GTK_STYLE_CLASS_OSD}
      @about-symbol{GTK_STYLE_CLASS_LEVEL_BAR}
      @about-symbol{GTK_STYLE_CLASS_CURSOR_HANDLE}
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
      @about-function{gtk-style-context-get-parent}
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
      @about-function{gtk-style-context-set-parent}
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
      @about-struct{gtk-border}
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
      Deprecated routines for handling resource files.

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
      @about-function{gtk-style-set-background}
      @about-function{gtk-style-apply-default-background}
      @about-function{gtk-style-apply-default-pixmap}
      @about-function{gtk-style-lookup-color}
      @about-function{gtk-style-lookup-icon-set}
      @about-function{gtk-style-render-icon}
      @about-function{gtk-style-get-style-property}
      @about-function{gtk-style-get-valist}
      @about-function{gtk-style-get}
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
      @about-function{gtk-invisible-set-screen}
      @about-function{gtk-invisible-get-screen}
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
      @about-function{gtk-about-dialog-get-program-name}
      @about-function{gtk-about-dialog-set-program-name}
      @about-function{gtk-about-dialog-get-version}
      @about-function{gtk-about-dialog-set-version}
      @about-function{gtk-about-dialog-get-copyright}
      @about-function{gtk-about-dialog-set-copyright}
      @about-function{gtk-about-dialog-get-comments}
      @about-function{gtk-about-dialog-set-comments}
      @about-function{gtk-about-dialog-get-license}
      @about-function{gtk-about-dialog-set-license}
      @about-function{gtk-about-dialog-get-wrap-license}
      @about-function{gtk-about-dialog-set-wrap-license}
      @about-function{gtk-about-dialog-get-license-type}
      @about-function{gtk-about-dialog-set-license-type}
      @about-function{gtk-about-dialog-get-website}
      @about-function{gtk-about-dialog-set-website}
      @about-function{gtk-about-dialog-get-website-label}
      @about-function{gtk-about-dialog-set-website-label}
      @about-function{gtk-about-dialog-get-authors}
      @about-function{gtk-about-dialog-set-authors}
      @about-function{gtk-about-dialog-get-artists}
      @about-function{gtk-about-dialog-set-artists}
      @about-function{gtk-about-dialog-get-documenters}
      @about-function{gtk-about-dialog-set-documenters}
      @about-function{gtk-about-dialog-get-translator-credits}
      @about-function{gtk-about-dialog-set-translator-credits}
      @about-function{gtk-about-dialog-get-logo}
      @about-function{gtk-about-dialog-set-logo}
      @about-function{gtk-about-dialog-get-logo-icon-name}
      @about-function{gtk-about-dialog-set-logo-icon-name}
      @about-function{gtk-about-dialog-add-credit-section}
      @about-function{gtk-show-about-dialog}
    @end{subsection}
    @begin[GtkAssistant]{subsection}
      A widget used to guide users through multi-step operations.

      @about-class{gtk-assistant}
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
      @about-symbol{gtk-assistant-page-type}
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
      @about-function{gtk-accel-label-set-accel-closure}
      @about-function{gtk-accel-label-get-accel-widget}
      @about-function{gtk-accel-label-set-accel-widget}
      @about-function{gtk-accel-label-get-accel-width}
      @about-function{gtk-accel-label-set-accel}
      @about-function{gtk-accel-label-refetch}
    @end{subsection}
    @begin[GtkImage]{subsection}
      A widget displaying an image.

      @about-class{gtk-image}
      @about-symbol{gtk-image-type}
      @about-function{gtk-image-get-icon-set}
      @about-function{gtk-image-get-pixbuf}
      @about-function{gtk-image-get-stock}
      @about-function{gtk-image-get-animation}
      @about-function{gtk-image-get-icon-name}
      @about-function{gtk-image-get-gicon}
      @about-function{gtk-image-get-storage-type}
      @about-function{gtk-image-new-from-file}
      @about-function{gtk-image-new-from-icon-set}
      @about-function{gtk-image-new-from-pixbuf}
      @about-function{gtk-image-new-from-stock}
      @about-function{gtk-image-new-from-animation}
      @about-function{gtk-image-new-from-icon-name}
      @about-function{gtk-image-new-from-gicon}
      @about-function{gtk-image-new-from-resource}
      @about-function{gtk-image-set-from-file}
      @about-function{gtk-image-set-from-icon-set}
      @about-function{gtk-image-set-from-pixbuf}
      @about-function{gtk-image-set-from-stock}
      @about-function{gtk-image-set-from-animation}
      @about-function{gtk-image-set-from-icon-name}
      @about-function{gtk-image-set-from-gicon}
      @about-function{gtk-image-set-from-resource}
      @about-function{gtk-image-clear}
      @about-function{gtk-image-new}
      @about-function{gtk-image-set-pixel-size}
      @about-function{gtk-image-get-pixel-size}
    @end{subsection}
    @begin[GtkLabel]{subsection}
      A widget that displays a small to medium amount of text.

      @about-class{gtk-label}
      @about-function{gtk-label-new}
      @about-function{gtk-label-set-text}
      @about-function{gtk-label-set-attributes}
      @about-function{gtk-label-set-markup}
      @about-function{gtk-label-set-markup-with-mnemonic}
      @about-function{gtk-label-set-pattern}
      @about-function{gtk-label-set-justify}
      @about-function{gtk-label-set-ellipsize}
      @about-function{gtk-label-set-width-chars}
      @about-function{gtk-label-set-max-width-chars}
      @about-function{gtk-label-set-line-wrap}
      @about-function{gtk-label-set-line-wrap-mode}
      @about-function{gtk-label-get-layout-offsets}
      @about-function{gtk-label-get-mnemonic-keyval}
      @about-function{gtk-label-get-selectable}
      @about-function{gtk-label-get-text}
      @about-function{gtk-label-new-with-mnemonic}
      @about-function{gtk-label-select-region}
      @about-function{gtk-label-set-mnemonic-widget}
      @about-function{gtk-label-set-selectable}
      @about-function{gtk-label-set-text-with-mnemonic}
      @about-function{gtk-label-get-attributes}
      @about-function{gtk-label-get-justify}
      @about-function{gtk-label-get-ellipsize}
      @about-function{gtk-label-get-width-chars}
      @about-function{gtk-label-get-max-width-chars}
      @about-function{gtk-label-get-label}
      @about-function{gtk-label-get-layout}
      @about-function{gtk-label-get-line-wrap}
      @about-function{gtk-label-get-line-wrap-mode}
      @about-function{gtk-label-get-mnemonic-widget}
      @about-function{gtk-label-get-selection-bounds}
      @about-function{gtk-label-get-use-markup}
      @about-function{gtk-label-get-use-underline}
      @about-function{gtk-label-get-single-line-mode}
      @about-function{gtk-label-get-angle}
      @about-function{gtk-label-set-label}
      @about-function{gtk-label-set-use-markup}
      @about-function{gtk-label-set-use-underline}
      @about-function{gtk-label-set-single-line-mode}
      @about-function{gtk-label-set-angle}
      @about-function{gtk-label-get-current-uri}
      @about-function{gtk-label-set-track-visited-links}
      @about-function{gtk-label-get-track-visited-links}
    @end{subsection}
    @begin[GtkProgressBar]{subsection}
      A widget which indicates progress visually.

      @about-class{gtk-progress-bar}
      @about-function{gtk-progress-bar-new}
      @about-function{gtk-progress-bar-pulse}
      @about-function{gtk-progress-bar-set-fraction}
      @about-function{gtk-progress-bar-get-fraction}
      @about-function{gtk-progress-bar-set-inverted}
      @about-function{gtk-progress-bar-get-inverted}
      @about-function{gtk-progress-bar-set-show-text}
      @about-function{gtk-progress-bar-get-show-text}
      @about-function{gtk-progress-bar-set-text}
      @about-function{gtk-progress-bar-get-text}
      @about-function{gtk-progress-bar-set-ellipsize}
      @about-function{gtk-progress-bar-get-ellipsize}
      @about-function{gtk-progress-bar-set-pulse-step}
      @about-function{gtk-progress-bar-get-pulse-step}
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
      @about-function{gtk-info-bar-set-message-type}
      @about-function{gtk-info-bar-get-message-type}
      @about-function{gtk-info-bar-get-action-area}
      @about-function{gtk-info-bar-get-content-area}
    @end{subsection}
    @begin[GtkStatusIcon]{subsection}
      Display an icon in the system tray.

      @about-class{gtk-status-icon}
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
      @about-function{gtk-status-icon-get-storage-type}
      @about-function{gtk-status-icon-get-pixbuf}
      @about-function{gtk-status-icon-get-stock}
      @about-function{gtk-status-icon-get-icon-name}
      @about-function{gtk-status-icon-get-gicon}
      @about-function{gtk-status-icon-get-size}
      @about-function{gtk-status-icon-set-screen}
      @about-function{gtk-status-icon-get-screen}
      @about-function{gtk-status-icon-set-tooltip-text}
      @about-function{gtk-status-icon-get-tooltip-text}
      @about-function{gtk-status-icon-set-tooltip-markup}
      @about-function{gtk-status-icon-get-tooltip-markup}
      @about-function{gtk-status-icon-set-has-tooltip}
      @about-function{gtk-status-icon-get-has-tooltip}
      @about-function{gtk-status-icon-set-title}
      @about-function{gtk-status-icon-get-title}
      @about-function{gtk-status-icon-set-name}
      @about-function{gtk-status-icon-set-visible}
      @about-function{gtk-status-icon-get-visible}
      @about-function{gtk-status-icon-is-embedded}
      @about-function{gtk-status-icon-position-menu}
      @about-function{gtk-status-icon-get-geometry}
      @about-function{gtk-status-icon-get-x11-window-id}
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
      A widget that emits a signal when clicked on.

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
      @about-function{gtk-button-set-relief}
      @about-function{gtk-button-get-relief}
      @about-function{gtk-button-get-label}
      @about-function{gtk-button-set-label}
      @about-function{gtk-button-get-use-stock}
      @about-function{gtk-button-set-use-stock}
      @about-function{gtk-button-get-use-underline}
      @about-function{gtk-button-set-use-underline}
      @about-function{gtk-button-set-focus-on-click}
      @about-function{gtk-button-get-focus-on-click}
      @about-function{gtk-button-set-alignment}
      @about-function{gtk-button-get-alignment}
      @about-function{gtk-button-set-image}
      @about-function{gtk-button-get-image}
      @about-function{gtk-button-set-image-position}
      @about-function{gtk-button-get-image-position}
      @about-function{gtk-button-set-always-show-image}
      @about-function{gtk-button-get-always-show-image}
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
      @about-function{gtk-radio-button-set-group}
      @about-function{gtk-radio-button-get-group}
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
      @about-function{gtk-toggle-button-get-active}
      @about-function{gtk-toggle-button-set-active}
      @about-function{gtk-toggle-button-get-inconsistent}
      @about-function{gtk-toggle-button-set-inconsistent}
    @end{subsection}
    @begin[GtkLinkButton]{subsection}
      Create buttons bound to a URL.

      @about-class{gtk-link-button}
      @about-function{gtk-link-button-new}
      @about-function{gtk-link-button-new-with-label}
      @about-function{gtk-link-button-get-uri}
      @about-function{gtk-link-button-set-uri}
      @about-function{gtk-link-button-get-visited}
      @about-function{gtk-link-button-set-visited}
    @end{subsection}
    @begin[GtkScaleButton]{subsection}
      A button which pops up a scale.

      @about-class{gtk-scale-button}
      @about-function{gtk-scale-button-new}
      @about-function{gtk-scale-button-set-adjustment}
      @about-function{gtk-scale-button-set-icons}
      @about-function{gtk-scale-button-set-value}
      @about-function{gtk-scale-button-get-adjustment}
      @about-function{gtk-scale-button-get-value}
      @about-function{gtk-scale-button-get-popup}
      @about-function{gtk-scale-button-get-plus-button}
      @about-function{gtk-scale-button-get-minus-button}
    @end{subsection}
    @begin[GtkVolumeButton]{subsection}
      A button which pops up a volume control.

      @about-class{gtk-volume-button}
      @about-function{gtk-volume-button-new}
    @end{subsection}
    @begin[GtkSwitch]{subsection}
      A \"light switch\" style toggle.

      @about-class{gtk-switch}
      @about-function{gtk-switch-new}
      @about-function{gtk-switch-set-active}
      @about-function{gtk-switch-get-active}
    @end{subsection}
    @begin[GtkLockButton]{subsection}
      not implemented
    @end{subsection}
    @begin[GtkMenuButton]{subsection}
      not impplented
    @end{subsection}
  @end{section}
  @begin[Numeric and Text Data Entry]{section}
    @begin[GtkEntry]{subsection}
      A single line text entry field.

      @about-class{gtk-entry}
      @about-function{gtk-entry-new}
      @about-function{gtk-entry-with-buffer}
      @about-function{gtk-entry-get-buffer}
      @about-function{gtk-entry-set-buffer}
      @about-function{gtk-entry-set-text}
      @about-function{gtk-entry-get-text}
      @about-function{gtk-entry-get-text-length}
      @about-function{gtk-entry-get-text-area}
      @about-function{gtk-entry-set-visibility}
      @about-function{gtk-entry-set-invisible-char}
      @about-function{gtk-entry-unset-invisible-char}
      @about-function{gtk-entry-set-max-length}
      @about-function{gtk-entry-get-activates-default}
      @about-function{gtk-entry-get-has-frame}
      @about-function{gtk-entry-get-inner-border}
      @about-function{gtk-entry-get-width-chars}
      @about-function{gtk-entry-set-activates-default}
      @about-function{gtk-entry-set-has-frame}
      @about-function{gtk-entry-set-inner-border}
      @about-function{gtk-entry-set-width-chars}
      @about-function{gtk-entry-get-invisible-char}
      @about-function{gtk-entry-set-alignment}
      @about-function{gtk-entry-get-alignment}
      @about-function{gtk-entry-set-placeholder-text}
      @about-function{gtk-entry-get-placeholder-text}
      @about-function{gtk-entry-set-overwrite-mode}
      @about-function{gtk-entry-get-overwrite-mode}
      @about-function{gtk-entry-get-layout}
      @about-function{gtk-entry-get-layout-offsets}
      @about-function{gtk-entry-layout-index-to-text-index}
      @about-function{gtk-entry-text-index-to-layout-index}
      @about-function{gtk-entry-set-attributes}
      @about-function{gtk-entry-get-attributes}
      @about-function{gtk-entry-get-max-length}
      @about-function{gtk-entry-get-visibility}
      @about-function{gtk-entry-set-completion}
      @about-function{gtk-entry-get-completion}
      @about-function{gtk-entry-set-cursor-hadjustment}
      @about-function{gtk-entry-get-cursor-hadjustment}
      @about-function{gtk-entry-set-progress-fraction}
      @about-function{gtk-entry-get-progress-fraction}
      @about-function{gtk-entry-set-progress-pulse-step}
      @about-function{gtk-entry-get-progress-pulse-step}
      @about-function{gtk-entry-progress-pulse}
      @about-function{gtk-entry-im-context-filter-keypress}
      @about-function{gtk-entry-reset-im-context}
      @about-symbol{gtk-entry-icon-position}
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
      @about-symbol{gtk-input-purpose}
      @about-function{gtk-entry-set-input-purpose}
      @about-function{gtk-entry-get-input-purpose}
      @about-symbol{gtk-input-hints}
      @about-function{gtk-entry-set-input-hints}
      @about-function{gtk-entry-get-input-hints}
    @end{subsection}
    @begin[GtkEntryBuffer]{subsection}
      Text buffer for @class{gtk-entry}.

      @about-class{gtk-entry-buffer}
      @about-function{gtk-entry-buffer-new}
      @about-function{gtk-entry-buffer-get-text}
      @about-function{gtk-entry-buffer-set-text}
      @about-function{gtk-entry-buffer-get-bytes}
      @about-function{gtk-entry-buffer-get-length}
      @about-function{gtk-entry-buffer-get-max-length}
      @about-function{gtk-entry-buffer-set-max-length}
      @about-function{gtk-entry-buffer-insert-text}
      @about-function{gtk-entry-buffer-delete-text}
      @about-function{gtk-entry-buffer-emit-deleted-text}
      @about-function{gtk-entry-buffer-emit-inserted-text}
    @end{subsection}
    @begin[GtkEntryCompletion]{subsection}
      Completion functionality for GtkEntry.

      @about-class{gtk-entry-completion}
      @about-function{gtk-entry-completion-new}
      @about-function{gtk-entry-completion-new-with-area}
      @about-function{gtk-entry-completion-get-entry}
      @about-function{gtk-entry-completion-set-model}
      @about-function{gtk-entry-completion-get-model}
      @about-function{gtk-entry-completion-set-match-func}
      @about-function{gtk-entry-completion-set-minimum-key-length}
      @about-function{gtk-entry-completion-get-minimum-key-length}
      @about-function{gtk-entry-completion-compute-prefix}
      @about-function{gtk-entry-completion-complete}
      @about-function{gtk-entry-completion-get-completion-prefix}
      @about-function{gtk-entry-completion-insert-prefix}
      @about-function{gtk-entry-completion-insert-action-text}
      @about-function{gtk-entry-completion-insert-action-markup}
      @about-function{gtk-entry-completion-delete-action}
      @about-function{gtk-entry-completion-set-text-column}
      @about-function{gtk-entry-completion-get-text-column}
      @about-function{gtk-entry-completion-set-inline-completion}
      @about-function{gtk-entry-completion-get-inline-completion}
      @about-function{gtk-entry-completion-set-inline-selection}
      @about-function{gtk-entry-completion-get-inline-selection}
      @about-function{gtk-entry-completion-set-popup-completion}
      @about-function{gtk-entry-completion-get-popup-completion}
      @about-function{gtk-entry-completion-set-popup-set-width}
      @about-function{gtk-entry-completion-get-popup-set-width}
      @about-function{gtk-entry-completion-set-popup-single-match}
      @about-function{gtk-entry-completion-get-popup-single-match}
    @end{subsection}
    @begin[GtkScale]{subsection}
      A slider widget for selecting a value from a range.

      @about-class{gtk-scale}
      @about-function{gtk-scale-new}
      @about-function{gtk-scale-new-with-range}
      @about-function{gtk-scale-set-digits}
      @about-function{gtk-scale-set-draw-value}
      @about-function{gtk-scale-set-has-origin}
      @about-function{gtk-scale-set-value-pos}
      @about-function{gtk-scale-get-digits}
      @about-function{gtk-scale-get-draw-value}
      @about-function{gtk-scale-get-has-origin}
      @about-function{gtk-scale-get-value-pos}
      @about-function{gtk-scale-get-layout}
      @about-function{gtk-scale-get-layout-offsets}
      @about-function{gtk-scale-add-mark}
      @about-function{gtk-scale-clear-marks}
    @end{subsection}
    @begin[GtkHScale]{subsection}
      A horizontal slider widget for selecting a value from a range.

      @about-class{gtk-hscale}
      @about-function{gtk-hscale-new}
      @about-function{gtk-hscale-new-with-range}
    @end{subsection}
    @begin[GtkVScale]{subsection}
      A vertical slider widget for selecting a value from a range.

      @about-class{gtk-vscale}
      @about-function{gtk-vscale-new}
      @about-function{gtk-vscale-new-with-range}
    @end{subsection}
    @begin[GtkSpinButton]{subsection}
      Retrieve an integer or floating-point number from the user.

      @about-class{gtk-spin-button}
      @about-symbol{gtk-spin-button-update-policy}
      @about-symbol{gtk-spin-type}
      @about-function{gtk-spin-button-configure}
      @about-function{gtk-spin-button-new}
      @about-function{gtk-spin-button-new-with-range}
      @about-function{gtk-spin-button-set-adjustment}
      @about-function{gtk-spin-button-get-adjustment}
      @about-function{gtk-spin-button-set-digits}
      @about-function{gtk-spin-button-set-increments}
      @about-function{gtk-spin-button-set-range}
      @about-function{gtk-spin-button-get-value-as-int}
      @about-function{gtk-spin-button-set-value}
      @about-function{gtk-spin-button-set-update-policy}
      @about-function{gtk-spin-button-set-numeric}
      @about-function{gtk-spin-button-spin}
      @about-function{gtk-spin-button-set-wrap}
      @about-function{gtk-spin-button-set-snap-to-ticks}
      @about-function{gtk-spin-button-update}
      @about-function{gtk-spin-button-get-digits}
      @about-function{gtk-spin-button-get-increments}
      @about-function{gtk-spin-button-get-numeric}
      @about-function{gtk-spin-button-get-range}
      @about-function{gtk-spin-button-get-snap-to-ticks}
      @about-function{gtk-spin-button-get-update-policy}
      @about-function{gtk-spin-button-get-value}
      @about-function{gtk-spin-button-get-wrap}
      @about-symbol{GTK_INPUT_ERROR}
    @end{subsection}
    @begin[GtkSearchEntry]{subsection}
      not implemented
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
      @fun{gtk-text-view-set-buffer}.

    @subheading{Example of Changing Text Attributes}
      There are two ways to affect text attributes in @class{gtk-text-view}. You
      can change the default attributes for a given @class{gtk-text-view}, and
      you can apply tags that change the attributes for a region of text. For
      text features that come from the theme - such as font and foreground color
      - use standard @class{gtk-widget} functions such as
      @fun{gtk-widget-modify-font} or @fun{gtk-widget-override-text}. For other
      attributes there are dedicated methods on @class{gtk-text-view} such as
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
      @about-function{gtk-text-iter-forward-find-char}
      @about-function{gtk-text-iter-backward-find-char}
      @about-symbol{gtk-text-search-flags}
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
      @about-function{gtk-text-mark-new}
      @about-function{gtk-text-mark-set-visible}
      @about-function{gtk-text-mark-get-visible}
      @about-function{gtk-text-mark-get-deleted}
      @about-function{gtk-text-mark-get-name}
      @about-function{gtk-text-mark-get-buffer}
      @about-function{gtk-text-mark-get-left-gravity}
    @end{subsection}
    @begin[GtkTextBuffer]{subsection}
      Stores attributed text for display in a @class{gtk-text-view} widget.

      @about-class{gtk-text-buffer}
      @about-function{gtk-text-buffer-new}
      @about-function{gtk-text-buffer-get-line-count}
      @about-function{gtk-text-buffer-get-char-count}
      @about-function{gtk-text-buffer-get-tag-table}
      @about-function{gtk-text-buffer-insert}
      @about-function{gtk-text-buffer-insert-at-cursor}
      @about-function{gtk-text-buffer-insert-interactive}
      @about-function{gtk-text-buffer-insert-interactive-at-cursor}
      @about-function{gtk-text-buffer-insert-range}
      @about-function{gtk-text-buffer-insert-range-interactive}
      @about-function{gtk-text-buffer-insert-with-tags}
      @about-function{gtk-text-buffer-insert-with-tags-by-name}
      @about-function{gtk-text-buffer-delete}
      @about-function{gtk-text-buffer-delete-interactive}
      @about-function{gtk-text-buffer-backspace}
      @about-function{gtk-text-buffer-set-text}
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
    @end{subsection}
    @begin[GtkTextTagTable]{subsection}
    @end{subsection}
    @begin[GtkTextView]{subsection}
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

   /* Now we can manipulate the view just like any other GTK widget */
   ...
@}
      @end{pre}
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
      @about-function{gtk-widget-is-focus}
      @about-function{gtk-widget-grab-focus}
      @about-function{gtk-widget-grab-default}
      @about-function{gtk-widget-set-name}
      @about-function{gtk-widget-get-name}
      @about-function{gtk-widget-set-state}
      @about-function{gtk-widget-set-sensitive}
      @about-function{gtk-widget-set-parent}
      @about-function{gtk-widget-set-parent-window}
      @about-function{gtk-widget-get-parent-window}
      @about-function{gtk-widget-set-events}
      @about-function{gtk-widget-get-events}
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
      @about-function{gtk-widget-get-parent}
      @about-function{gtk-widget-get-settings}
      @about-function{gtk-widget-get-clipboard}
      @about-function{gtk-widget-get-display}
      @about-function{gtk-widget-get-root-window}
      @about-function{gtk-widget-get-screen}
      @about-function{gtk-widget-has-screen}
      @about-function{gtk-widget-get-size-request}
      @about-function{gtk-widget-set-child-visible}
      @about-function{gtk-widget-set-size-request}
      @about-function{gtk-widget-thaw-child-notify}
      @about-function{gtk-widget-set-no-show-all}
      @about-function{gtk-widget-get-no-show-all}
      @about-function{gtk-widget-list-mnemonic-labels}
      @about-function{gtk-widget-add-mnemonic-label}
      @about-function{gtk-widget-remove-mnemonic-label}
      @about-function{gtk-widget-is-composited}
      @about-function{gtk-widget-error-bell}
      @about-function{gtk-widget-keynav-failed}
      @about-function{gtk-widget-get-tooltip-markup}
      @about-function{gtk-widget-set-tooltip-markup}
      @about-function{gtk-widget-get-tooltip-text}
      @about-function{gtk-widget-set-tooltip-text}
      @about-function{gtk-widget-get-tooltip-window}
      @about-function{gtk-widget-set-tooltip-window}
      @about-function{gtk-widget-get-has-tooltip}
      @about-function{gtk-widget-set-has-tooltip}
      @about-function{gtk-widget-trigger-tooltip-query}
      @about-function{gtk-widget-get-window}
      @about-function{gtk-cairo-should-draw-window}
      @about-function{gtk-cairo-transform-to-window}
      @about-function{gtk-widget-get-allocated-width}
      @about-function{gtk-widget-get-allocated-height}
      @about-function{gtk-widget-get-allocation}
      @about-function{gtk-widget-set-allocation}
      @about-function{gtk-widget-get-app-paintable}
      @about-function{gtk-widget-get-can-default}
      @about-function{gtk-widget-set-can-default}
      @about-function{gtk-widget-get-can-focus}
      @about-function{gtk-widget-set-can-focus}
      @about-function{gtk-widget-get-double-buffered}
      @about-function{gtk-widget-get-has-window}
      @about-function{gtk-widget-set-has-window}
      @about-function{gtk-widget-get-sensitive}
      @about-function{gtk-widget-is-sensitive}
      @about-function{gtk-widget-get-state}
      @about-function{gtk-widget-get-visible}
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
      @about-function{gtk-widget-set-window}
      @about-function{gtk-widget-set-receives-default}
      @about-function{gtk-widget-get-receives-default}
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
      @about-function{gtk-widget-get-halign}
      @about-function{gtk-widget-set-halign}
      @about-function{gtk-widget-get-valign}
      @about-function{gtk-widget-set-valign}
      @about-function{gtk-widget-get-margin-left}
      @about-function{gtk-widget-set-margin-left}
      @about-function{gtk-widget-get-margin-right}
      @about-function{gtk-widget-set-margin-right}
      @about-function{gtk-widget-get-margin-top}
      @about-function{gtk-widget-set-margin-top}
      @about-function{gtk-widget-get-margin-bottom}
      @about-function{gtk-widget-set-margin-bottom}
      @about-function{gtk-widget-get-hexpand}
      @about-function{gtk-widget-set-hexpand}
      @about-function{gtk-widget-get-hexpand-set}
      @about-function{gtk-widget-set-hexpand-set}
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
