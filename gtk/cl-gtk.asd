;;; ----------------------------------------------------------------------------
;;; cl-gtk.asd
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
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

(defpackage #:cl-gtk-system
  (:use #:cl #:asdf))

(in-package #:cl-gtk-system)

(defclass plain-file (static-file)
  ((type :initarg :type :reader plain-file-type :initform nil)))

(defmethod source-file-type ((c plain-file) (s module))
  (plain-file-type c))

(defsystem :cl-gtk
  :name :cl-gtk
  :version "0.0.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components
  ((:file "gtk.package")
   (:file "gtk.misc-lisp")
   (:file "gtk.child-properties")
   
   ;; Gtk+ Core
   (:file "gtk.version")               ; Version Information
   (:file "gtk.enumerations")          ; Standard Enumerations
   (:file "gtk.main-loop")             ; Main event loop, and events
   (:file "gtk.accel-group")           ; Accelerator Groups
   (:file "gtk.accel-map")             ; Loadable keyboard accelerator
   (:file "gtk.clipboard")             ; Storing data on clipboards
   (:file "gtk.drag-and-drop")         ; Controlling drag and drop
   (:file "gtk.settings")              ; Sharing settings
   (:file "gtk.selections")            ; Inter-process communication
                                       
   ;; Interface builder                
   (:file "gtk.buildable")             ; GtkBuildable
   (:file "gtk.builder")               ; Build an interface
                                       
   ;; More Inferfaces                  
   (:file "atk.implementor-iface")     ; AtkImplementorIface
   (:file "gtk.orientable")            ; Interface for flippable widgets
   (:file "gtk.activatable")           ; Interface for activatable widgets
   (:file "gtk.recent-chooser")        ; Displaying recently used files
                                       
   ;; Theming in Gtk+                  
   (:file "gtk.style-context")         ; Rendering UI elements
   (:file "gtk.stock-images")          ; Manipulating stock icons
   (:file "gtk.icon-theme")            ; Looking up icons by name
                                       
   ;; Abstract Base Classes            
   (:file "gtk.object")                ; GtkObject
   (:file "gtk.widget")                ; Base class for all widgets
   (:file "gtk.misc")                  ; Base class for alignments
   (:file "gtk.container")             ; GtkContainer
   (:file "gtk.bin")                   ; Container with just one child
   (:file "gtk.range")                 ; Base class for adjustments
   (:file "gtk.menu-shell")            ; Base class for menu objects
   (:file "gtk.im-context")            ; Base class for input contexts
                                       
   ;; Layout Containers                
   (:file "gtk.box")                   ; GtkBox, GtkHBox, GtkVBox
   (:file "gtk.table")                 ; GtkTable
   (:file "gtk.layout")                ; Infinite scrollable
   (:file "gtk.fixed")                 ; Widgets at fixed coordinates
   (:file "gtk.notebook")              ; Tabbed notebook container
   (:file "gtk.paned")                 ; Two adjustable panes
   (:file "gtk.expander")              ; Container which can hide childs
   (:file "gtk.alignment")             ; GtkAlignment
   (:file "gtk.button-box")            ; Container for arranging buttons
                                       
   ;; Ornaments                        
   (:file "gtk.separator")             ; Separator widget
   (:file "gtk.frame")                 ; Decorative frame
   (:file "gtk.aspect-frame")          ; Constrain childs to a aspect ratio
   
   ;; Scrolling
   (:file "gtk.scrollbar")             ; GtkScrollbar
   (:file "gtk.scrolled-window")       ; Adds scrollbars
   
   ;; Windows
   (:file "gtk.dialog")                ; GtkDialog
   (:file "gtk.invisible")             ; GtkInvisible
   (:file "gtk.message-dialog")        ; GtkMessageDialog
   (:file "gtk.window")                ; GtkWindow
   (:file "gtk.window-group")          ; GtkWindowGroup
   (:file "gtk.about-dialog")          ; GtkAboutDialog
   (:file "gtk.assistant")             ; GtkAssistant
   ;; GtkOffscreenWindow not implemented
   
   ;; Display Widgets
   (:file "gtk.accel-label")           ; GtkAccelLabel
   (:file "gtk.image")                 ; GtkImage
   (:file "gtk.label")                 ; GtkLabel
   (:file "gtk.progress-bar")          ; GtkProgessBar
   (:file "gtk.statusbar")             ; GTKStatusbar
   ;; GtkInfoBar not implemented     
   (:file "gtk.status-icon")           ; GtkStatusIcon
   ;; GtkSpinner not implemented
   
   ;; Buttons and Toggles
   (:file "gtk.button")                ; GtkButton
   (:file "gtk.toggle-button")         ; GtkToggleButton
   (:file "gtk.check-button")          ; GtkCheckButton
   (:file "gtk.radio-button")          ; GtkRadioButton
   (:file "gtk.link-button")           ; GtkLinkButton
   (:file "gtk.scale-button")          ; GtkScaleButton
   (:file "gtk.volume-button")         ; GtkVolumeButton
   ;; GtkSwitch not implemented
   ;; GtkLockButton not implemented
   
   ;; Multiline Text Editor
   (:file "gtk.text-iter")             ; GtkTextIter
   (:file "gtk.text-mark")             ; GtkTextMark
   (:file "gtk.text-tag")              ; GtkTextTag
   (:file "gtk.text-buffer")           ; GtkTextBuffer
   (:file "gtk.text-tag-table")        ; GtkTextTagTable
   (:file "gtk.text-view")             ; GtkTextView
   
   ;; Tree, List and Icon Grid Widgets
   (:file "gtk.tree-model")            ; Tree interface
   (:file "gtk.cell-layout")           ; Interface for packing cells
   (:file "gtk.tree-sortable")         
   (:file "gtk.tree-view-drag-and-drop")
   (:file "gtk.tree-model-sort")       ; GtkTreeModelSort
   (:file "gtk.tree-model-filter")     ; GtkTreeModelFilter
   (:file "gtk.tree-view")             ; Displaying both trees and lists
   (:file "gtk.tree-view-column")      ; Visible column in GtkTreeView
   (:file "gtk.tree-store")            ; A tree-like data structure
   (:file "gtk.tree-selection")        ; Selection object for GtkTreeView
   (:file "gtk.cell-editable")         ; GtkCellEditable
   (:file "gtk.cell-renderer")         ; Object for rendering a cell
   (:file "gtk.cell-renderer-text")    ; Renders text in a cell
   (:file "gtk.cell-renderer-pixbuf")  ; Renders a pixbuf in a cell
   (:file "gtk.cell-renderer-progress"); Renders numbers as progress bars
   (:file "gtk.cell-renderer-accel")   ; Renders a keyboard accelerator
   (:file "gtk.cell-renderer-combo")   ; Renders a combobox in a cell
   (:file "gtk.cell-renderer-spin")    ; Renders a spin button in a cell
   (:file "gtk.cell-renderer-toggle")  ; Renders a toggle button in a cell
   (:file "gtk.cell-view")             ; Displaying a single row
   (:file "gtk.icon-view")             ; List of icons in a grid
   (:file "gtk.list-store")            ; list-like data structure
   
   ;; Numeric/Text Data Entry
   (:file "gtk.editable")              ; GtkEditable
   (:file "gtk.entry")                 ; GtkEntry
   ;; GtkEntryBuffer not implemented
   (:file "gtk.entry-completion")      ; GtkEntryCompletion
   (:file "gtk.scale")                 ; GtkScale
   (:file "gtk.h-scale")               ; GtkHScale
   (:file "gtk.v-scale")               ; GtkVScale
   (:file "gtk.spin-button")           ; GtkSpinButton
   
   ;; Menus, Combo Box, Toolbar
   (:file "gtk.item")                  ; Abstract base class GtkItem
   (:file "gtk.menu-item")             ; Widget used for item in menus
   (:file "gtk.menu")                  ; Menu widget
   (:file "gtk.menu-bar")              ; Subclass for GtkMenuItem widgets
   (:file "gtk.combo-box")             ; GtkComboBox
   (:file "gtk.combo-box-entry")       ; Text entry field with a dropdown list
   (:file "gtk.tool-shell")            ; Interface for GtkToolItem
   (:file "gtk.tool-item")             ; GtkToolItem
   (:file "gtk.toolbar")               ; Create bars of buttons
   (:file "gtk.tool-button")           ; GtkToolButton
   (:file "gtk.toggle-tool-button")    ; GtkToggleToolButton
   (:file "gtk.radio-tool-button")     ; GtkRadioToolButton
   (:file "gtk.menu-tool-button")      ; GtkMenuToolButton
   
   ;; Action-based menus and toolbars
   (:file "gtk.ui-manager")            ; Constructing menus and toolbars
   (:file "gtk.action-group")          ; Group of actions
   (:file "gtk.action")                ; GtkAction
   (:file "gtk.toggle-action")         ; GtkToggleAction
   (:file "gtk.radio-action")          ; GtkRadioAction
   (:file "gtk.recent-action")         ; List of recently used files
   
   ;; Selectors
   (:file "gtk.color-button")          ; Launch a color selection dialog
   (:file "gtk.color-selection")       ; Widget used to select a color
   (:file "gtk.color-selection-dialog"); Widget used to select a color
   (:file "gtk.hsv")                   ; GtkHSV
   (:file "gtk.file-chooser")          ; File chooser interface
   (:file "gtk.file-chooser-widget")   ; File chooser widget
   (:file "gtk.file-chooser-button")   ; Button to launch a file selection
   (:file "gtk.file-chooser-dialog")   ; File chooser dialog
   (:file "gtk.file-filter")           ; Selecting a file subset
   (:file "gtk.font-button")           ; Button to launch a font chooser dialog
   (:file "gtk.font-selection")        ; Deprecated widget for selecting fonts
   
   ;; Miscellaneous
   (:file "gtk.adjustment")            ; Representation of a bounded value
   (:file "gtk.drawing-area")          ; Custom user interface elements
   (:file "gtk.event-box")             ; Widget used to catch events
   (:file "gtk.calendar")              ; Displays a calendar
   (:file "gtk.size-group")            ; Grouping widgets to the same size
   (:file "gtk.tooltip")               ; Add tips to your widgets
   (:file "gtk.arrow")                 ; Displays an arrow
   (:file "gtk.viewport")              ; Adapter which makes widgets scrollable
   (:file "gtk.handle-box")            ; Widget for detachable window portions
   
   ;; Cross-process Embedding
   (:file "gtk.plug")                  ; Embedding into other processes
   (:file "gtk.socket")                ; For widgets from other processes
   
   ;; Recently Used Documents
   (:file "gtk.recent-manager")        ; Managing recently used files
   (:file "gtk.recent-chooser-dialog") ; Displays recently used files
   (:file "gtk.recent-chooser-menu")   ; Displays recently used files in a menu
   (:file "gtk.recent-chooser-widget") ; Displays recently used files
   (:file "gtk.recent-filter")         ; Selecting recently used files
   
   ;; Printing
   (:file "gtk.print-operation")       ; High-level Printing API
   (:file "gtk.print-context")         ; Encapsulates context for drawing pages
   (:file "gtk.print-settings")        ; Stores print settings
   (:file "gtk.page-setup")            ; Stores page setup information
   ; GtkPaperSize — Support for named paper sizes
   ; GtkPrinter — Represents a printer
   ; GtkPrintJob — Represents a print job
   (:file "gtk.print-unix-dialog")     ; A print dialog
   (:file "gtk.page-setup-unix-dialog"); A page setup dialog
   
   ;; Deprecated
   (:file "gtk.ruler")                 ; Class for horizontal or vertical rulers
   (:file "gtk.curve")                 ; Allows direct editing of a curve
   (:file "gtk.item-factory")          ; A factory for menus
   (:file "gtk.style")                 ; Functions for drawing widget parts
   (:file "gtk.resource-files")        ; Routines for handling resource files
   (:file "gtk.input-dialog")          ; Configure devices for XInput extension
   (:file "gtk.font-selection-dialog") ; Widget for selecting fonts
   
   ;; More definitions. The documentation is not completed.
   (:file "gtk.generated-classes")
   (:file "gtk.selectors")
   (:file "gtk.timer")
   
   ;; Lisp
   (:file "gtk.high-level")
   (:file "gtk.init"))
  :depends-on (:cl-gtk-glib
               :cl-gtk-gobject
               :cl-gtk-gdk
               :cl-gtk-pango
               :cffi
               :bordeaux-threads
               :iterate))

;;; --- End of file cl-gtk.asd -------------------------------------------------
