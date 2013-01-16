;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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

(in-package :gtk)

(load "src/atdoc-gtk.package.lisp")

;;; Content from the GTK+ 3.4.3 Reference Manual

;;; GTK+ Core Reference

    (load "src/atdoc-gtk.main-loop.lisp")
#|
    gtk.version.lisp                 - Version Information         - GTK+ 3.4.3
|#
    (load "src/atdoc-gtk.accel-group.lisp")
    (load "src/atdoc-gtk.accel-map.lisp")
#|
    gtk.clipboard.lisp               - GtkClipboard                - GTK+ 3.4.3
    gtk.drag-and-drop.lisp           - Drag and Drop handling      - GTK+ 3.4.3
    
    not implemented                  - Stock Items
    
    gtk.setting.lisp                 - GtkSettings                 - GTK+ 3.4.3
    
    not implemented                  - Bindings
|#
    (load "src/atdoc-gtk.enumerations.lisp")
    (load "src/atdoc-gtk.selections.lisp")
#|
    
    not implemented                  - Testing
    not implemented                  - Filesystem utilities
    
Theming in GTK+

    gtk.style-context.lisp           - GtkStyleContext             - GTK+ 3.4.3

    not implemented                  - GtkCssProvider
    not implemented                  - GtkStyleProvider
    not implemented                  - GtkStyleProperties
    not implemented                  - GtkThemingEngine
    not implemented                  - GtkWidgetPath
    not implemented                  - GtkSymbolicColor
    not implemented                  - GtkGradient

    gtk.icon-theme.lisp              - GtkIconTheme                - GTK+ 3.4.3
    gtk.stock-images.lisp            - Themeable Stock Images      - GTK+ 3.4.3

    not implemented                  - GtkNumerableIcon

    gtk.resource-files.lisp          - Resource Files (**)         - GTK+ 3.2.3*
|#
    (load "src/atdoc-gtk.style.lisp")
#|

GTK+ Widgets and Objects

  Windows

|# 
    (load "src/atdoc-gtk.dialog.lisp")
#|
    gtk.invisible.lisp               - GtkInvisible                - GTK+ 3.4.3
    gtk.message-dialog.lisp          - GtkMessageDialog            - GTK+ 3.4.3
|#
    (load "src/atdoc-gtk.window.lisp")
#|
    gtk.window-group.lisp            - GtkWindowGroup              - GTK+ 3.4.3
|#
    (load "src/atdoc-gtk.about-dialog.lisp")
#|
    gtk.assistant.lisp               - GtkAssistant                - GTK+ 3.4.3
    gtk.offscreen-window.lisp        - GtkOffscreenWindow          - GTK+ 3.4.3
    
  Display Widgets
|#
    (load "src/atdoc-gtk.accel-label.lisp")
#|
    gtk.image.lisp                   - GtkImage                    - GTK+ 3.4.1
    gtk.label.lisp                   - GtkLabel                    - GTK+ 3.4.1
    gtk.progress-bar.lisp            - GtkProgressBar              - GTK+ 3.4.1
    gtk.statusbar                    - GtkStatusbar                - GTK+ 3.4.1
    gtk.info-bar.lisp                - GtkInfoBar                  - GTK+ 3.4.3
    gtk.status-icon.lisp             - GtkStatusIcon               - GTK+ 3.4.3
    gtk.spinner.lisp                 - GtkSpinner                  - GTK+ 3.4.3
    
  Buttons and Toggles
    
    gtk.button.lisp                  - GtkButton                   - GTK+ 3.4.1
    gtk.check-button.lisp            - GtkCheckButton              - GTK+ 3.4.1
    gtk.radio-button.lisp            - GtkRadioButton              - GTK+ 3.4.1
    gtk.toggle-button.lisp           - GtkToggleButton             - GTK+ 3.4.1
    gtk.link-button.lisp             - GtkLinkButton               - GTK+ 3.4.1
    gtk.scale-button.lisp            - GtkScaleButton              - GTK+ 3.4.1
    gtk.volume-button.lisp           - GtkVolumeButton             - GTK+ 3.4.2
    gtk.switch.lisp                  - GtkSwitch                   - GTK+ 3.4.2
    
    not implemented                  - GtkLockButton
    
  Numeric/Text Data Entry
  
    gtk.entry.lisp                   - GtkEntry                    - GTK+ 3.4.3
    gtk.entry-buffer.lisp            - GtkEntryBuffer              - GTK+ 3.4.3
    gtk.entry-completion.lisp        - GtkEntryCompletion          - GTK+ 3.4.3
    gtk.scale.lisp                   - GtkScale, ...               - GTK+ 3.4.3
    gtk.spin-button.lisp             - GtkSpinButton               - GTK+ 3.4.3
    gtk.editable.lisp                - GtkEditable                 - GTK+ 3.4.3
    
  Multiline Text Editor
    
    gtk.text-iter.lisp               - GtkTextIter                 - GTK+ 3.4.3
    gtk.text-mark.lisp               - GtkTextMark                 - GTK+ 3.4.3
    gtk.text-buffer.lisp             - GtkTextBuffer               - GTK+ 3.4.3
    gtk.text-tag.lisp                - GtkTextTag                  - GTK+ 3.4.3
    gtk.text-tag-table.lisp          - GtkTextTagTable             - GTK+ 3.4.3
    gtk.text-view.lisp               - GtkTextView                 - GTK+ 3.4.3
  
  Tree, List and Icon Grid Widgets
    
    gtk.tree-model.lisp              - GtkTreeModel                - GTK+ 3.4.3
    gtk.tree-selection.lisp          - GtkTreeSelection            - GTK+ 3.4.3
    gtk.tree-view-column.lisp        - GtkTreeViewColumn           - GTK+ 3.4.3
    gtk.tree-view.lisp               - GtkTreeView                 - GTK+ 3.4.3
    gtk.tree-view-drag-and-drop.lisp - GtkTreeView drag and drop   - GTK+ 3.4.3
    
    gtk.cell-view.lisp               - GtkCellView                 - GTK+ 3.4.3
    gtk.icon-view.lisp               - GtkIconView                 - GTK+ 3.4.3
    
    gtk.tree-sortable.lisp           - GtkTreeSortable             - GTK+ 3.4.3
    gtk.tree-model-sort.lisp         - GtkTreeModelSort            - GTK+ 3.4.3
    gtk.tree-model-filter.lisp       - GtkTreeModelFilter          - GTK+ 3.4.3
    
    gtk.cell-layout.lisp             - GtkCellLayout               - GTK+ 3.4.3
    
    gtk.cell-area.lisp               - GtkCellArea                 - GTK+ 3.4.3
    gtk.cell-area-box.lisp           - GtkCellAreaBox              - GTK+ 3.4.3
    gtk.cell-area-context.lisp       - GtkCellAreaContext          - GTK+ 3.4.3
    
    gtk.cell-renderer.lisp           - GtkCellRenderer             - GTK+ 3.4.3
    gtk.cell-editable.lisp           - GtkCellEditable             - GTK+ 3.4.3
    gtk.cell-renderer-accel.lisp     - GtkCellRendererAccel        - GTK+ 3.4.3
    gtk.cell-renderer-combo.lisp     - GtkCellRendererCombo        - GTK+ 3.4.3
    gtk.cell-renderer-pixbuf.lisp    - GtkCellRendererPixbuf       - GTK+ 3.4.3
    gtk.cell-renderer-progress-lisp  - GtkCellRendererProgress     - GTK+ 3.4.3
    gtk.cell-renderer-spin.lisp      - GtkCellRendererSpin         - GTK+ 3.4.3
    gtk.cell-renderer-text.lisp      - GtkCellRendererText         - GTK+ 3.4.3
    gtk.cell-renderer-toggle.lisp    - GtkCellRendererToggle       - GTK+ 3.4.3
    gtk.cell-renderer-spinner.lisp   - GtkCellRendererSpinner      - GTK+ 3.4.3
    
    gtk.list-store.lisp              - GtkListStore                - GTK+ 3.4.3
    gtk.tree-store.lisp              - GtkTreeStore                - GTK+ 3.4.3

  Menus, Combo Box, Toolbar
    
    gtk.combo-box.lisp               - GtkComboBox                 - GTK+ 3.4.3
    gtk.combo-box-text.lisp          - GtkComboBoxText             - GTK+ 3.4.3
    
    gtk.menu.lisp                    - GtkMenu                     - GTK+ 3.4.3
    gtk.menu-bar.lisp                - GtkMenuBar                  - GTK+ 3.4.3
    gtk.menu-item.lisp               - GtkMenu                     - GTK+ 3.4.3
    gtk.image-menu-item.lisp         - GtkImageMenuItem            - GTK+ 3.4.3
    gtk.radio-menu-item.lisp         - GtkRadioMenuItem            - GTK+ 3.4.3
    gtk.check-menu-item.lisp         - GtkCheckMenuItem            - GTK+ 3.4.3
    gtk.separator-menu-item.lisp     - GtkSeparatorMenuItem        - GTK+ 3.4.3
    gtk.tearoff-menu-item.lisp       - GtkTearoffMenuItem          - GTK+ 3.4.3
    
    gtk.tool-shell.lisp              - GtkToolShell                - GTK+ 3.4.3
    gtk.toolbar.lisp                 - GtkToolbar                  - GTK+ 3.4.3
    gtk.tool-item.lisp               - GtkToolItem                 - GTK+ 3.4.4
    gtk.tool-palette.lisp            - GtkToolPalette              - GTK+ 3.4.3
    gtk.tool-item-group.lisp         - GtkToolItemGroup            - GTK+ 3.4.3
    gtk.separator-tool-item.lisp     - GtkSeparatorToolItem        - GTK+ 3.4.3
    gtk.tool-button.lisp             - GtkToolButton               - GTK+ 3.4.3
    gtk.menu-tool-button.lisp        - GtkMenuToolButton           - GTK+ 3.4.3
    gtk.toggle-tool-button.lisp      - GtkToggleToolButton         - GTK+ 3.4.3
    gtk.radio-tool-button.lisp       - GtkRadioToolButton          - GTK+ 3.4.3
    
  Action-based menus and toolbars
    
    gtk.ui-manager.lisp              - GtkUIManager                - GTK+ 3.4.3
    gtk.action-group.lisp            - GtkActionGroup              - GTK+ 3.4.3
    gtk.action.lisp                  - GtkAction                   - GTK+ 3.4.3
    gtk.toggle-action.lisp           - GtkToggleAction             - GTK+ 3.4.3
    gtk.radio-action.lisp            - GtkRadioAction              - GTK+ 3.4.3
    gtk.recent-action.lisp           - GtkRecentAction             - GTK+ 3.4.3
    gtk.activatable.lisp             - GtkActivatable              - GTK+ 3.4.3

  Selectors (Color/File/Font)

    gtk.color-chooser.lisp           - GtkColorChooser             - GTK+ 3.4.3
    gtk.color-button.lisp            - GtkColorButton              - GTK+ 3.4.3
    gtk.color-chooser-widget.lisp    - GtkColorChooserWidget       - GTK+ 3.4.3
    gtk.color-chooser-dialog.lisp    - GtkColorChooserDialog       - GTK+ 3.4.3
    gtk.color-selection.lisp         - GtkColorSelection (**)      - GTK+ 3.2.3*
    gtk.color-selection-dialog.lisp  - GtkColorSelectionDialog(**) - GTK+ 3.2.3*
    gtk.hsv.lisp                     - GtkHSV (**)                 - GTK+ 3.2.3*

    gtk.file-chooser.lisp            - GtkFileChooser              - GTK+ 3.4.3
    gtk.file-chooser-button.lisp     - GtkFileChooserButton        - GTK+ 3.4.3
    gtk.file-chooser-dialog.lisp     - GtkFileChooserDialog        - GTK+ 3.4.3
    gtk.file-chooser-widget.lisp     - GtkFileChooserWidget        - GTK+ 3.4.3
    gtk.file-filter.lisp             - GtkFileFilter               - GTK+ 3.4.3

    gtk.font-chooser.lisp            - GtkFontChooser              - GTK+ 3.4.3
    gtk.font-button.lisp             - GtkFontButton               - GTK+ 3.4.3
    gtk.font-chooser-widget.lisp     - GtkFontChooserWidget        - GTK+ 3.4.3
    gtk.font-chooser-dialog.lisp     - GtkFontChooserDialog        - GTK+ 3.4.3
    gtk.font-selection.lisp          - GtkFontSelection (**)       - GTK+ 3.2.3*
    gtk.font-selection-dialog.lisp   - GtkFontSelectionDialog (**) - GTK+ 3.4.3
  
  Layout Containers
|#
    (load "src/atdoc-gtk.grid.lisp")
#|
    gtk.alignment.lisp               - GtkAlignment                - GTK+ 3.4.3
    gtk.aspect-frame.lisp            - GtkAspectFrame              - GTK+ 3.4.3
    gtk.box.lisp                     - GtkBox, GtkHBox, GtkVbox    - GTK+ 3.4.2
    gtk.button-box.lisp              - GtkButtonBox, ...           - GTK+ 3.4.3
    gtk.fixed.lisp                   - GtkFixed                    - GTK+ 3.4.3
    gtk.paned.lisp                   - GtkPaned, ...               - GTK+ 3.4.3
    gtk.layout.lisp                  - GtkLayout                   - GTK+ 3.4.3
    gtk.notebook.lisp                - GtkNotebook                 - GTK+ 3.4.3
    gtk.table.lisp                   - GtkTable                    - GTK+ 3.4.2
    gtk.expander.lisp                - GtkExpander                 - GTK+ 3.4.3
    gtk.overlay.lisp                 - GtkOverlay                  - GTK+ 3.4.3
    gtk.orientable.lisp              - GtkOrientable               - GTK+ 3.4.3
  
  Ornaments
  
    gtk.frame.lisp                   - GtkFrame                    - GTK+ 3.4.3
    gtk.separator.lisp               - GtkSeparator, ...           - GTK+ 3.4.3
  
  Scrolling
  
    gtk.scrollbar.lisp               - GtkScrollbar, ...           - GTK+ 3.4.3
    gtk.scrolled-window.lisp         - GtkScrolledWindow           - GTK+ 3.4.3    
    gtk.scrollable.lisp              - GtkScrollable               - GTK+ 3.4.3

  Printing

    gtk.print-operation.lisp         - GtkPrintOperation           - GTK+ 3.4.3
    gtk.print-context.lisp           - GtkPrintContext             - GTK+ 3.4.3
    gtk.print-settings.lisp          - GtkPrintSettings            - GTK+ 3.4.3
    gtk.page-setup.lisp              - GtkPageSetup                - GTK+ 3.4.3

    not implemented                  - GtkPaperSize
    not implemented                  - GtkPrinter
    not implemented                  - GtkPrintJob

    gtk.print-unix-dialog.lisp       - GtkPrintUnixDialog          - GTK+ 3.4.3
    gtk.page-setup-unix-dialog.lisp  - GtkPageSetupUnixDialog      - GTK+ 3.4.3
  
  Miscellaneous
  
    gtk.adjustment.lisp              - GtkAdjustment               - GTK+ 3.4.2
    gtk.arrow.lisp                   - GtkArrow                    - GTK+ 3.4.3
    gtk.calendar.lisp                - GtkCalendar                 - GTK+ 3.4.3
    gtk.drawing-area.lisp            - GtkDrawingArea              - GTK+ 3.4.3
    gtk.event-box.lisp               - GtkEventBox                 - GTK+ 3.4.3
    gtk.handle-box.lisp              - GtkHandleBox (**)           - GTK+ 3.4.3
    gtk.im-context-simple.lisp       - GtkIMContextSimple          - GTK+ 3.4.3
    gtk.im-multicontext.lisp         - GtkIMMulticontext           - GTK+ 3.4.3
    gtk.size-group.lisp              - GtkSizeGroup                - GTK+ 3.4.3
|#
    (load "src/atdoc-gtk.tooltip.lisp")
#|
    gtk.viewport.lisp                - GtkViewport                 - GTK+ 3.4.3
    
    not implemented                  - GtkAccessible
  
  Abstract Base Classes
|#
    (load "src/atdoc-gtk.widget.lisp")
    (load "src/atdoc-gtk.container.lisp")
    (load "src/atdoc-gtk.bin.lisp")
#|
    gtk.menu-shell.lisp              - GtkMenuShell                - GTK+ 3.4.3
|#
    (load "src/atdoc-gtk.misc.lisp")
#|
    gtk.range.lisp                   - GtkRange                    - GTK+ 3.4.1
    gtk.im-context.lisp              - GtkIMContext                - GTK+ 3.4.3
    
  Cross-process Embedding
    
    gtk.plug.lisp                    - GtkPlug                     - GTK+ 3.4.3
    gtk.socket.lisp                  - GtkSocket                   - GTK+ 3.4.3
    
  Recently Used Documents
    
    gtk.recent-manager-lisp          - GtkRecentManager            - GTK+ 3.4.3
    gtk.recent-chooser.lisp          - GtkRecentChooser            - GTK+ 3.4.3
    gtk.recent-chooser-dialog.lisp   - GtkRecentChooserDialog      - GTK+ 3.4.3
    gtk.recent-chooser-menu.lisp     - GtkRecentChooserMenu        - GTK+ 3.4.3
    gtk.recent-chooser-widget.lisp   - GtkRecentChooserWidget      - GTK+ 3.4.3
    gtk.recent-filter.lisp           - GtkRecentFilter             - GTK+ 3.4.3
    
  Choosing from installed applications
    
    not implemented                  - GtkAppChooser
    not implemented                  - GtkAppChooserButton
    not implemented                  - GtkAppChooserDialog
    not implemented                  - GtkAppChooserWidget
  
  Interface builder
|#
    (load "src/atdoc-gtk.buildable.lisp")
#|
    gtk.builder.lisp                 - GtkBuilder                  - GTK+ 3.4.3
    
  Application support
  
    gtk.application.lisp             - GtkApplication              - GTK+ 3.4.3
    gtk.application-window.lisp      - GtkApplicationWindow        - GTK+ 3.4.3
    gtk.actionable.lisp              - GtkActionable               - GTK+ 3.4.1

|#

;;; --- End of file atdoc-gtk.lisp ---------------------------------------------
