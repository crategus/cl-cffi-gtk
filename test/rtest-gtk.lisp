(in-package :gtk-testsuite)

(def-suite gtk-suite :in gtk-testsuite)
(in-suite gtk-suite)

;;;  Application support

(load "rtest-gtk-application.lisp")
(load "rtest-gtk-application-window.lisp")
(load "rtest-gtk-actionable.lisp")

;;;  Interface builder

(load "rtest-gtk-builder.lisp")
(load "rtest-gtk-buildable.lisp")

;;;  Windows

(load "rtest-gtk-window.lisp")
(load "rtest-gtk-dialog.lisp")
;    gtk.message-dialog.lisp
;    gtk.about-dialog.lisp
;    gtk.assistant.lisp
;    gtk.invisible.lisp
;    gtk.offscreen-window.lisp
;    gtk.window-group.lisp

;;;  Layout Containers

(load "rtest-gtk-box.lisp")
(load "rtest-gtk-grid.lisp")
;    gtk.revealer.lisp
(load "rtest-gtk-list-box.lisp")
(load "rtest-gtk-flow-box.lisp")
;    gtk.stack.lisp
;    gtk.stack-switcher.lisp
;    gtk.stack-sidebar.lisp
;    gtk.action-bar.lisp
;    gtk.header-bar.lisp
;    gtk.overlay.lisp
(load "rtest-gtk-button-box.lisp")
(load "rtest-gtk-paned.lisp")
(load "rtest-gtk-layout.lisp")
(load "rtest-gtk-notebook.lisp")
;    gtk.expander.lisp
;    gtk.orientable.lisp
;    gtk.aspect-frame.lisp
;    gtk.fixed.lisp

;;;  Display Widgets

(load "rtest-gtk-label.lisp")
(load "rtest-gtk-image.lisp")
(load "rtest-gtk-spinner.lisp")
(load "rtest-gtk-info-bar.lisp")
(load "rtest-gtk-progress-bar.lisp")
(load "rtest-gtk-level-bar.lisp")
(load "rtest-gtk-statusbar.lisp")
(load "rtest-gtk-accel-label.lisp")

;;;  Buttons and Toggles

(load "rtest-gtk-button.lisp")
;    gtk.check-button.lisp
(load "rtest-gtk-radio-button.lisp")
;    gtk.toggle-button.lisp
;    gtk.link-button.lisp
;    gtk.menu-button.lisp
;    gtk.switch.lisp
;    gtk.scale-button.lisp
;    gtk.volume-button.lisp
;    not implemented
;    gtk.model-button.lisp

;;;  Numeric and Text Data Entry

;    gtk.entry.lisp
(load "rtest-gtk-entry-buffer.lisp")
;    gtk.entry-completion.lisp
;    gtk.scale.lisp
;    gtk.spin-button.lisp
;    gtk.search-entry.lisp
;    gtk.search-bar.lisp
;    gtk.editable.lisp

;;;  Multiline Text Editor

(load "rtest-gtk-text-attributes.lisp")
;(load "rtest-gtk-text-iter.lisp")
;(load "rtest-gtk-text-mark.lisp")
(load "rtest-gtk-text-buffer.lisp")
;(load "rtest-gtk-text-tag.lisp")
;(load "rtest-gtk-text-tag-table.lisp")
;(load "rtest-gtk-text-view.lisp")

;;;  Tree, List and Icon Grid Widgets

(load "rtest-gtk-tree-model.lisp")
(load "rtest-gtk-tree-selection.lisp")
;    gtk.tree-view-column.lisp
(load "rtest-gtk-tree-view.lisp")
;    gtk.tree-view-drag-and-drop.lisp
;    gtk.cell-view.lisp
;    gtk.icon-view.lisp
;    gtk.tree-sortable.lisp
;    gtk.tree-model-sort.lisp
;    gtk.tree-model-filter.lisp
;(load "rtest-gtk-cell-layout.lisp")
;    gtk.cell-area.lisp
;    gtk.cell-area-box.lisp
;    gtk.cell-area-context.lisp
(load "rtest-gtk-cell-renderer.lisp")
;    gtk.cell-editable.lisp
;    gtk.cell-renderer-accel.lisp
;    gtk.cell-renderer-combo.lisp
;    gtk.cell-renderer-pixbuf.lisp
;    gtk.cell-renderer-progress-lisp
;    gtk.cell-renderer-spin.lisp
;    gtk.cell-renderer-text.lisp
;    gtk.cell-renderer-toggle.lisp
;    gtk.cell-renderer-spinner.lisp
;(load "rtest-gtk-list-store.lisp")
;    gtk.tree-store.lisp

;;;  Menus, Combo Box, Toolbar

;    gtk.combo-box.lisp
;    gtk.combo-box-text.lisp
(load "rtest-gtk-menu.lisp")
;    gtk.menu-bar.lisp
;    gtk.menu-item.lisp
(load "rtest-gtk-radio-menu-item.lisp")
;    gtk.check-menu-item.lisp
;    gtk.separator-menu-item.lisp
;    gtk.tool-shell.lisp
;    gtk.toolbar.lisp
;    gtk.tool-item.lisp
(load "rtest-gtk-tool-palette.lisp")
;    gtk.tool-item-group.lisp
;    gtk.separator-tool-item.lisp
;    gtk.tool-button.lisp
;    gtk.menu-tool-button.lisp
;    gtk.toggle-tool-button.lisp
(load "rtest-gtk-radio-tool-button.lisp")
;    gtk.popover.lisp
;    gtk.popover-menu.lisp

;;;  Selector Widgets and Dialogs

;    gtk.color-chooser.lisp
;    gtk.color-button.lisp
;    gtk.color-chooser-widget.lisp
;    gtk.color-chooser-dialog.lisp

(load "rtest-gtk-file-chooser.lisp")
;    gtk.file-chooser-button.lisp
;    gtk.file-chooser-native.lisp
;    gtk.file-chooser-dialog.lisp
(load "rtest-gtk-file-chooser-widget.lisp")
;    gtk.file-filter.lisp

;    gtk.font-chooser.lisp
;    gtk.font-button.lisp
;    gtk.font-chooser-widget.lisp
;    gtk.font-chooser-dialog.lisp
;    gtk.places-sidebar.lisp

;;;  Ornaments

(load "rtest-gtk-frame.lisp")
; gtk.separator.lisp

;;;  Scrolling

(load "rtest-gtk-scrollbar.lisp")
(load "rtest-gtk-scrolled-window.lisp")
(load "rtest-gtk-scrollable.lisp")

;;;  Printing

(load "rtest-gtk-print-operation.lisp")
(load "rtest-gtk-print-context.lisp")
(load "rtest-gtk-print-settings.lisp")
(load "rtest-gtk-page-setup.lisp")
(load "rtest-gtk-paper-size.lisp")
(load "rtest-gtk-printer.lisp")
(load "rtest-gtk-print-job.lisp")
(load "rtest-gtk-print-unix-dialog.lisp")
(load "rtest-gtk-page-setup-unix-dialog.lisp")

;;;  Shortcuts Overview

;    gtk.shortcuts-window
;    gtk-shortcuts-section
;    gtk-shortcuts-group
;    gtk-shortcuts-shortcut

;;;  Miscellaneous

(load "rtest-gtk-adjustment.lisp")
;    gtk.calendar.lisp
;    gtk.drawing-area.lisp
;    not implemented                  - GtkGLArea
;    gtk.event-box.lisp
;    gtk.im-context-simple.lisp
;    gtk.im-multicontext.lisp
;    gtk.size-group.lisp
;    gtk.tooltip.lisp
;    gtk.viewport.lisp
;    not implemented                  - GtkAccessible

;;;  Abstract Base Classes

(load "rtest-gtk-widget.lisp")
(load "rtest-gtk-container.lisp")
;    gtk.bin.lisp
(load "rtest-gtk-menu-shell.lisp")
;    gtk.range.lisp
;    gtk.im-context.lisp
;    not implemented                  - GtkNativeDialog

;;;  Cross-process Embedding

;    gtk.plug.lisp
;    gtk.socket.lisp

;;;  Recently Used Documents

;    gtk.recent-manager-lisp
;    gtk.recent-chooser.lisp
;    gtk.recent-chooser-dialog.lisp
;    gtk.recent-chooser-menu.lisp
;    gtk.recent-chooser-widget.lisp
;    gtk.recent-filter.lisp

;;;  Choosing from installed applications

(load "rtest-gtk-app-chooser.lisp")
(load "rtest-gtk-app-chooser-button.lisp")
(load "rtest-gtk-app-chooser-dialog.lisp")
;    gtk.app-chooser-widget.lisp

;;;  Gestures and event handling

;    gtk.event-controller.lisp
;    gtk.event-controller-key.lisp
;    gtk.event-controller-scroll.lisp
;    gtk.event-controller-motion.lisp
;    gtk.gesture.lisp
;    gtk.gesture-single.lisp
;    gtk.gesture-drag.lisp
;    gtk.gesture-long-press.lisp
;    gtk.gesture-multi-press.lisp
;    gtk.gesture-pan.lisp
;    gtk.gesture-swipe.lisp
;    gtk.gesture-rotate.lisp
;    gtk.gesture-zoom.lisp
;    gtk.gesture-stylus.lisp
;    gtk.pad-controller.lisp

;;;  GTK+ Core Reference

(load "rtest-gtk-main-loop.lisp")
;    gtk.version.lisp
(load "rtest-gtk-accel-group.lisp")
(load "rtest-gtk-accel-map.lisp")
(load "rtest-gtk-clipboard.lisp")
;    gtk.drag-and-drop.lisp
(load "rtest-gtk-settings.lisp")
;    gtk.bindings.lisp
;    gtk.enumerations.lisp
(load "rtest-gtk-selections.lisp")
;    gtk.mount-operation.lisp

;;;  Theming in GTK+

(load "rtest-gtk-style-context.lisp")
(load "rtest-gtk-css-provider.lisp")
(load "rtest-gtk-style-provider.lisp")
(load "rtest-gtk-widget-path.lisp")
(load "rtest-gtk-icon-theme.lisp")

;;;  Deprecated

(load "rtest-gtk-action-group.lisp")
(load "rtest-gtk-action.lisp")

;;; 2021-4-30
