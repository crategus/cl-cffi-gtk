;;; ----------------------------------------------------------------------------
;;; gtk-demo.lisp
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-demo
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-demo)

(load "alignment.lisp")
(load "app-chooser-button.lisp")
(load "app-chooser-dialog.lisp")
(load "arrows.lisp")
(load "aspect-frame.lisp")
(load "application.lisp")
(load "application-window.lisp")
(load "assistant.lisp")
(load "box-packing.lisp")
(load "button.lisp")
(load "button-box.lisp")
(load "buttons.lisp")
(load "calendar.lisp")
(load "clipboard.lisp")
(load "color-button.lisp")
(load "color-chooser-dialog.lisp")
(load "combo-box.lisp")
(load "combo-box-text.lisp")
(load "drag-and-drop.lisp")
(load "css-accordion.lisp")
(load "css-pixbufs.lisp")
(load "dialog-alternative-order.lisp")
(load "dialog-toplevel.lisp")
(load "dialogs.lisp")
(load "drawing.lisp")
(load "entry-completion.lisp")
(load "entry-buffer.lisp")
(load "event-box.lisp")
(load "file-chooser-button.lisp")
(load "fixed.lisp")
(load "font-button.lisp")
(load "frame.lisp")
(load "grid-packing.lisp")
(load "image.lisp")
(load "info-bar.lisp")
(load "labels.lisp")
(load "link-button.lisp")
(load "menu.lisp")
(load "more-labels.lisp")
(load "notebook.lisp")
(load "paned-window.lisp")
(load "pixbuf-scale.lisp")
(load "pixbufs.lisp")
(load "progress-bar.lisp")
(load "scale-widgets.lisp")
(load "scrolled-window.lisp")
(load "search-entry.lisp")
(load "selections-1.lisp")
(load "size-management.lisp")
(load "simple-file-chooser.lisp")
(load "simple-message.lisp")
(load "simple-text-view.lisp")
(load "simple-tree-view.lisp")
(load "simple-window.lisp")
(load "spin-button.lisp")
(load "statusbar.lisp")
(load "switch.lisp")
(load "table-packing.lisp")
(load "text-entry.lisp")
(load "text-view-attributes.lisp")
(load "toggle-buttons.lisp")
(load "tool-palette.lisp")

;;; ----------------------------------------------------------------------------

(defvar info-buffer (make-instance 'gtk-text-buffer))
(defvar source-buffer (make-instance 'gtk-text-buffer))

;;; ----------------------------------------------------------------------------

(defun load-file (filename)
  (with-open-file (stream filename)
    ;; Read the info-header of the file
    (multiple-value-bind (start end)
        (gtk-text-buffer-get-bounds info-buffer)
      (gtk-text-buffer-delete info-buffer start end)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((or (null line)
               (not (>= (length line) 4))
               (not (string= line ";;;;" :start1 0 :end1 4))))
        (gtk-text-buffer-insert info-buffer (string-left-trim ";" line))
        (gtk-text-buffer-insert info-buffer (format nil "~%"))))
    ;; Read the source code of the file
    (multiple-value-bind (start end)
        (gtk-text-buffer-get-bounds source-buffer)
      (gtk-text-buffer-delete source-buffer start end)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (gtk-text-buffer-insert source-buffer (string-left-trim ";" line))
        (gtk-text-buffer-insert source-buffer (format nil "~%"))))))

;;; ----------------------------------------------------------------------------

(defun create-text (buffer is-source)
  (let* ((scrolled (make-instance 'gtk-scrolled-window
                                  :shadow-type :in
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
         (view (make-instance 'gtk-text-view
                              :buffer buffer
                              :editable nil
                              :cursor-visible nil)))
    (gtk-container-add scrolled view)
    (if is-source
        (progn
          (gtk-widget-override-font
              view
              (pango-font-description-from-string "monospace"))
          (gtk-text-buffer-set-text buffer "This is the place for the source."))
        (gtk-text-buffer-set-text buffer "This is the place for the info."))
    ;; return the scrolled window
    scrolled))

;;; ----------------------------------------------------------------------------

(defun create-and-fill-model ()
  (let ((model (make-instance 'gtk-tree-store
                              :column-types '("gchararray" ; Title
                                              "gchararray" ; Filename
                                              "gchararray" ; Function name
                                              "guint"))))
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Theming in GTK+" "" "" 0)))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "CSS Accordion"
                                "css-accordion.lisp" "DEMO-CSS-ACCORDION" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "CSS Pixbufs"
                                "css-pixbufs.lisp" "DEMO-CSS-PIXBUFS" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Windows" "" "" 0)))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Window"
                                "simple-window.lisp" "EXAMPLE-SIMPLE-WINDOW" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Message"
                                "simple-message.lisp" "EXAMPLE-SIMPLE-MESSAGE" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Dialogs"
                                "dialogs.lisp" "EXAMPLE-DIALOG" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Assistant"
                                "assistant.lisp" "DEMO-ASSISTANT" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Display Widgets")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Labels"
                                "labels.lisp" "EXAMPLE-LABELS" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Images"
                                "image.lisp" "DEMO-IMAGE" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "More Labels"
                                "more-labels.lisp" "EXAMPLE-MORE-LABELS" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Progress Bar Widget"
                                "progress-bar.lisp" "EXAMPLE-PROGRESS-BAR" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Statusbar"
                                "statusbar.lisp" "EXAMPLE-STATUSBAR" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Info Bar"
                                "info-bar.lisp" "EXAMPLE-INFO-BAR" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Button Widgets")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Button"
                                "button.lisp" "EXAMPLE-BUTTON" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "More Buttons"
                                "buttons.lisp" "EXAMPLE-BUTTONS" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Toggle Buttons"
                                "toggle-buttons.lisp" "EXAMPLE-TOGGLE-BUTTONS" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Link Button"
                                "link-button.lisp" "EXAMPLE-LINK-BUTTON" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Switch"
                                "switch.lisp" "EXAMPLE-SWITCH" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Numeric/Text Data Entry")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text Entry"
                                "text-entry.lisp" "EXAMPLE-TEXT-ENTRY" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text Entry Buffer"
                                "entry-buffer.lisp" "EXAMPLE-ENTRY-BUFFER" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text Entry Completion"
                                "entry-completion.lisp" "EXAMPLE-ENTRY-COMPLETION" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Scale Widgets"
                                "scale-widgets.lisp" "EXAMPLE-SCALE-WIDGETS" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Spin Button"
                                "spin-button.lisp" "EXAMPLE-SPIN-BUTTON" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                             "Multiline Text Editor")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Text View"
                                "simple-text-view.lisp"
                                "EXAMPLE-SIMPLE-TEXT-VIEW" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Attributes"
                                "text-view-attributes.lisp"
                                "EXAMPLE-TEXT-VIEW-ATTRIBUTES" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Tree, List and Icon Grid Widgets")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Tree View"
                                "simple-tree-view.lisp"
                                "EXAMPLE-SIMPLE-TREE-VIEW" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Menus, Combo Box, Toolbar")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Combo Box"
                                "combo-box.lisp"
                                "EXAMPLE-COMBO-BOX" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Combo Box Text"
                                "combo-box-text.lisp"
                                "EXAMPLE-COMBO-BOX-TEXT" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Menu"
                                "menu.lisp" "EXAMPLE-MENU" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Tool Palette"
                                "tool-palette.lisp" "DEMO-TOOL-PALETTE" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Selectors (Color/File/Font)")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Button"
                                "color-button.lisp" "EXAMPLE-COLOR-BUTTON" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Chooser Dialog"
                                "color-chooser-dialog.lisp"
                                "EXAMPLE-COLOR-CHOOSER-DIALOG" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                 "Simple File Chooser Dialog"
                                 "simple-file-chooser.lisp"
                                 "EXAMPLE-SIMPLE-FILE-CHOOSER-DIALOG" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "File Chooser Button"
                                "file-chooser-button.lisp"
                                "EXAMPLE-FILE-CHOOSER-BUTTON" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Font Button"
                                "font-button.lisp" "EXAMPLE-FONT-BUTTON" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Layout Containers" "" "" 0)))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Box packing"
                                "box-packing.lisp" "EXAMPLE-BOX-PACKING" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Table packing"
                                "table-packing.lisp" "EXAMPLE-TABLE-PACKING" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Grid packing"
                                "grid-packing.lisp" "EXAMPLE-GRID-PACKING" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Button Boxes"
                                "button-box.lisp" "EXAMPLE-BUTTON-BOX" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Alignment"
                                "alignment.lisp" "EXAMPLE-ALIGNMENT" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Fixed Container"
                                "fixed.lisp" "EXAMPLE-FIXED" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Frame Container"
                                "frame.lisp" "DEMO-FRAME" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Aspect Frame"
                                "aspect-frame.lisp" "EXAMPLE-ASPECT-FRAME" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Paned Window"
                                "paned-window.lisp" "EXAMPLE-PANED-WINDOW" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Notebook"
                                "notebook.lisp" "EXAMPLE-NOTEBOOK" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Scrolling")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Scrolled Window"
                                "scrolled-window.lisp"
                                "EXAMPLE-SCROLLED-WINDOW" 0)
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Miscellaneous")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Drawing in response to input"
                                "drawing.lisp" "EXAMPLE-DRAWING" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Arrows"
                                "arrows.lisp" "EXAMPLE-ARROWS" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Calendar"
                                "calendar.lisp" "EXAMPLE-CALENDAR" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "The Event Box"
                                "event-box.lisp" "EXAMPLE-EVENT-BOX" 0)
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Demo Pixbufs"
                                "pixbufs.lisp" "DEMO-PIXBUFS" 0)

    )
    model))

(defun create-view-and-model ()
  (let* ((model (create-and-fill-model))
         (view (make-instance 'gtk-tree-view
                              :model model))
         (selection (gtk-tree-view-get-selection view)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Example"
                                                             renderer
                                                             "text" 0)))
      (gtk-tree-view-append-column view column))
    (g-signal-connect view "row-activated"
       (lambda (tree-view path column)
         (declare (ignore column))
         (let* ((model (gtk-tree-view-get-model tree-view))
                (iter (gtk-tree-model-get-iter model path))
                (func-name (gtk-tree-model-get-value model iter 2))
                (func (find-symbol func-name :gtk-demo)))
           (if func
               (funcall func)
               (format t "~%No function.~%")))))
    (gtk-tree-selection-set-mode selection :browse)
    (g-signal-connect selection "changed"
       (lambda (tree-selection)
         (let* ((iter (gtk-tree-selection-get-selected tree-selection))
                (filename (gtk-tree-model-get-value model iter 1)))
           (if (> (length filename) 0)
               (load-file filename)))))
      view))

;;; ----------------------------------------------------------------------------

(defun main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "GTK+ Lisp Code Demos"
                                 :default-width 800
                                 :default-height 600
                                 :border-width 6))
          ;; A horizontal pane
          (content (make-instance 'gtk-paned
                                  :orientation :horizontal))
          ;; A scrollable
          (scroller (make-instance 'gtk-scrolled-window
                                   :hscrollbar-policy :never
                                   :vscrollbar-policy :automatic
                                   :hexpand t
                                   :vexpand t))
          ;; A notebook
          (notebook (make-instance 'gtk-notebook
                                   :scrollable t))
          (view (create-view-and-model)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set an icon for the application
      (let ((pixbuf (gdk-pixbuf-new-from-file "gtk-logo-rgb.gif")))
        (setq pixbuf (gdk-pixbuf-add-alpha pixbuf t 255 255 255))
        (gtk-window-set-default-icon-list (list pixbuf)))
      ;; Add the widgets to the content of the window
      (gtk-container-add scroller view)
      (gtk-paned-add1 content scroller)
      (gtk-paned-add2 content notebook)
      ;; Add the notebook pages to the notebook
      (gtk-notebook-append-page notebook
                                (create-text info-buffer nil)
                                (gtk-label-new-with-mnemonic "_Info"))
      (gtk-notebook-append-page notebook
                                (create-text source-buffer t)
                                (gtk-label-new-with-mnemonic "_Source"))
      ;; Add the content to the window
      (gtk-container-add window content)
      (gtk-widget-show-all window)))
  (join-gtk-main))

(export 'main)

;;; --- End of file gtk-demo.lisp ----------------------------------------------
