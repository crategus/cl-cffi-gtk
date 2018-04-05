;;; ----------------------------------------------------------------------------
;;; gtk-demo.lisp
;;;
;;; Copyright (C) 2013, 2014 Dieter Kaiser
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

;;(asdf:load-system :cl-cffi-gtk)

(in-package :gtk-demo)

;;; ----------------------------------------------------------------------------

(defvar info-buffer (make-instance 'gtk-text-buffer))
(defvar source-buffer (make-instance 'gtk-text-buffer))

;;; ----------------------------------------------------------------------------

(defun load-file (filename)
  (with-open-file (stream (asdf:system-relative-pathname :cl-cffi-gtk-demo-gtk filename))
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

(defstruct (demo-entry (:constructor make-demo-entry (title filename &optional function)))
  title
  filename
  function)

(defun create-and-fill-model ()
  (let ((model (make-instance 'tree-lisp-store)))
    (tree-lisp-store-add-column
     model "gchararray"
     #'demo-entry-title)
    (tree-lisp-store-add-column
     model "gchararray"
     (lambda (demo-entry)
       (namestring (demo-entry-filename demo-entry))))
    (tree-lisp-store-add-column
     model "gchararray"
     (lambda (demo-entry)
       (let ((function (demo-entry-function demo-entry)))
         (or (and function (symbol-name function)) ""))))
    (flet ((demo (parent index title &optional (filename #P"") function)
             (let ((node (make-tree-node :item (make-demo-entry title filename function))))
               (tree-node-insert-at parent node index)
               node)))
      (let ((root (tree-lisp-store-root model)))
        (let ((parent (demo root 0 "Theming in GTK+")))
          (demo parent 0 "CSS Accordion" #P"css-accordion.lisp" 'demo-css-accordion)
          (demo parent 1 "CSS Pixbufs" #P"css-pixbufs.lisp" 'demo-css-pixbufs)
          (demo parent 2 "Numerable Icons" #P"numerable-icon.lisp" 'demo-numerable-icon))
        (let ((parent (demo root 1 "Windows""")))
          (demo parent 0 "Simple Window" #P"simple-window.lisp" 'example-simple-window)
          (demo parent 1 "Simple Message" #P"simple-message.lisp" 'example-simple-message)
          (demo parent 2 "Dialogs" #P"dialogs.lisp" 'example-dialog)
          (demo parent 3 "Assistant" #P"assistant.lisp" 'demo-assistant)
          (demo parent 4 "Application Window" #P"application-window.lisp" 'demo-application-window))
        (let ((parent (demo root 2 "Display Widgets")))
          (demo parent 0 "Labels" #P"labels.lisp" 'example-labels)
          (demo parent 1 "Images" #P"image.lisp" 'demo-image)
          (demo parent 2 "More Labels" #P"more-labels.lisp" 'example-more-labels)
          (demo parent 3 "Progress Bar Widget" #P"progress-bar.lisp" 'example-progress-bar)
          (demo parent 4 "Statusbar" #P"statusbar.lisp" 'example-statusbar)
          (demo parent 5 "Info Bar" #P"info-bar.lisp" 'example-info-bar))
        (let ((parent (demo root 3 "Button Widgets")))
          (demo parent 0 "Labels" #P"labels.lisp" 'example-labels)
          (demo parent 1 "Button" #P"button.lisp" 'example-button)
          (demo parent 2 "More Buttons" #P"buttons.lisp" 'example-buttons)
          (demo parent 3 "Toggle Buttons" #P"toggle-buttons.lisp" 'example-toggle-buttons)
          (demo parent 4 "Link Button" #P"link-button.lisp" 'example-link-button)
          (demo parent 5 "Switch" #P"switch.lisp" 'example-switch))
        (let ((parent (demo root 4 "Numeric/Text Data Entry")))
          (demo parent 0 "Labels" #P"labels.lisp" 'example-labels)
          (demo parent 1 "Text Entry" "text-entry.lisp" 'example-text-entry)
          (demo parent 2 "Text Entry Buffer" "entry-buffer.lisp" 'example-entry-buffer)
          (demo parent 3 "Text Entry Completion" "entry-completion.lisp" 'example-entry-completion)
          (demo parent 4 "Scale Widgets" "scale-widgets.lisp" 'example-scale-widgets)
          (demo parent 5 "Spin Button" "spin-button.lisp" 'example-spin-button))
        (let ((parent (demo root 5 "Multiline Text Editor")))
          (demo parent 0 "Simple Text View" "simple-text-view.lisp" 'example-simple-text-view)
          (demo parent 1 "Text View Attributes" "text-view-attributes.lisp" 'example-text-view-attributes))
        (let ((parent (demo root 6 "Tree, List and Icon Grid Widgets")))
          (demo parent 0 "Simple Tree View" "simple-tree-view.lisp" 'example-simple-tree-view))
        (let ((parent (demo root 7 "Menus, Combo Box, Toolbar")))
          (demo parent 0 "Combo Box" "combo-box.lisp" 'example-combo-box)
          (demo parent 1 "Combo Box Text" "combo-box-text.lisp" 'example-combo-box-text)
          (demo parent 2 "Menu" "menu.lisp" 'example-menu)
          (demo parent 3 "Tool Palette" "tool-palette.lisp" 'demo-tool-palette))
        (let ((parent (demo root 8 "Selectors (Color/File/Font)")))
          (demo parent 0 "Color Button" "color-button.lisp" 'example-color-button)
          (demo parent 1 "Color Chooser Dialog" "color-chooser-dialog.lisp" 'example-color-chooser-dialog)
          (demo parent 2 "Simple File Chooser Dialog" "simple-file-chooser.lisp" 'example-simple-file-chooser-dialog)
          (demo parent 3 "File Chooser Button" "file-chooser-button.lisp" 'example-file-chooser-button)
          (demo parent 4 "Font Button" "font-button.lisp" 'example-font-button))
        (let ((parent (demo root 9 "Layout Containers")))
          (demo parent 0 "Box packing" "box-packing.lisp" 'example-box-packing)
          (demo parent 1 "Table packing" "table-packing.lisp" 'example-table-packing)
          (demo parent 2 "Grid packing" "grid-packing.lisp" 'example-grid-packing)
          (demo parent 3 "Button Boxes" "button-box.lisp" 'example-button-box)
          (demo parent 4 "Alignment" "alignment.lisp" 'demo-alignment)
          (demo parent 5 "Fixed Container" "fixed.lisp" 'example-fixed)
          (demo parent 6 "Frame Container" "frame.lisp" 'demo-frame)
          (demo parent 7 "Aspect Frame" "aspect-frame.lisp" 'example-aspect-frame)
          (demo parent 8 "Paned Window" "paned-window.lisp" 'example-paned-window)
          (demo parent 9 "Notebook" "notebook.lisp" 'example-notebook)
          (demo parent 10 "Header Bar" "header-bar.lisp" 'example-header-bar)
          (demo parent 11 "Action Bar" "action-bar.lisp" 'example-action-bar)
          (demo parent 12 "Title Bar" "header-bar.lisp" 'example-title-bar))
        (let ((parent (demo root 10 "Scrolling")))
          (demo parent 0 "Scrolled Window" "scrolled-window.lisp" 'example-scrolled-window))
        (let ((parent (demo root 11 "Miscellaneous")))
          (demo parent 0 "Drawing in response to input" "drawing.lisp" 'example-drawing)
          (demo parent 1 "Arrows" "arrows.lisp" 'example-arrows)
          (demo parent 2 "Calendar" "calendar.lisp" 'example-calendar)
          (demo parent 3 "The Event Box" "event-box.lisp" 'example-event-box)
          (demo parent 4 "Demo Pixbufs" "pixbufs.lisp" 'demo-pixbufs)
          (demo parent 5 "Popover Demo" "popover.lisp" 'popover-demo)
          (demo parent 6 "Custom Subclass" "subclass-window.lisp" 'custom-window-demo)
          (demo parent 7 "Simple Drag And Drop" "simple-drag-and-drop.lisp" 'demo-simple-drag-and-drop)
          (demo parent 8 "Drag And Drop" "drag-and-drop.lisp" 'demo-drag-and-drop)
          (demo parent 9 "Glade and GtkBuilder" "builder.lisp" 'example-builder))))
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
                (func (and func-name (find-symbol func-name :gtk-demo))))
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
      (setf *demo-window* window)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set an icon for the application
      (let ((pixbuf (gdk-pixbuf-new-from-file (rel-path "gtk-logo-rgb.gif"))))
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
