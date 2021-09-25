;;; ----------------------------------------------------------------------------
;;; gtk-demo.lisp
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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

(in-package :gtk-demo)

;;; ----------------------------------------------------------------------------

(defvar info-buffer (make-instance 'gtk-text-buffer))
(defvar source-buffer (make-instance 'gtk-text-buffer))
(defvar ui-buffer (make-instance 'gtk-text-buffer))
(defvar css-buffer (make-instance 'gtk-text-buffer))

;;; ----------------------------------------------------------------------------

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk-text-buffer-bounds buffer)
    (gtk-text-buffer-delete buffer start end)))

(defun load-file (filename)
  (with-open-file (stream (rel-path filename))
    ;; Read the info-header of the file
    (clear-buffer info-buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((or (null line)
             (not (>= (length line) 4))
             (not (string= line ";;;;" :start1 0 :end1 4))))
      (gtk-text-buffer-insert info-buffer (string-left-trim ";" line))
      (gtk-text-buffer-insert info-buffer (format nil "~%")))
    ;; Read the source code of the file
    (clear-buffer source-buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk-text-buffer-insert source-buffer line)
      (gtk-text-buffer-insert source-buffer (format nil "~%")))))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream (rel-path filename))
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk-text-buffer-insert buffer line)
      (gtk-text-buffer-insert buffer (format nil "~%")))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;;; ----------------------------------------------------------------------------

(defun create-text (buffer is-source)
  (let* ((scrolled (make-instance 'gtk-scrolled-window
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
         (view (make-instance 'gtk-text-view
                              :buffer buffer
                              :editable nil
                              :cursor-visible nil)))
    (gtk-container-add scrolled view)
    (when is-source
      (gtk-widget-override-font view
                                (pango-font-description-from-string "monospace")))
    ;; return the scrolled window
    scrolled))

;;; ----------------------------------------------------------------------------

(defparameter *tree-model*
   '(("gchararray"        ; Title
      "gchararray"        ; Filename
      "gchararray"        ; Function name
      "gchararray"        ; UI Definition
      "gchararray")       ; CSS Definition

     "Theming in GTK"
     (("CSS Basics"
        "css-basics.lisp"
        "DO-CSS-BASICS"
        ""
        "css-basics.css")
      ("CSS Accordion"
       "css-accordion.lisp"
       "DEMO-CSS-ACCORDION"
       ""
       "css-accordion.css")
      ("CSS Blend Modes"
       "css-blendmodes.lisp"
       "DO-CSS-BLENDMODES"
       "css-blendmodes.ui"
       "css-blendmodes.css")
      ("CSS Pixbufs"
       "css-pixbufs.lisp"
       "DEMO-CSS-PIXBUFS"
       ""
       "css-pixbufs.css")
      ("Custom Drawing"
       "../gtk-example/custom-drawing.lisp"
       "EXAMPLE-CUSTOM-DRAWING"))

     "Windows"
     (("Simple Window"
       "../gtk-example/window-simple.lisp"
       "EXAMPLE-WINDOW-SIMPLE")
      ("Simple Message"
       "simple-message.lisp"
       "EXAMPLE-SIMPLE-MESSAGE")
      ("Dialog Windows"
       "../gtk-example/dialog.lisp"
       "EXAMPLE-DIALOG")
      ("Assistant"
       "assistant.lisp"
       "DEMO-ASSISTANT"))

     "Display Widgets"
     (("Labels"
       "../gtk-example/label.lisp"
       "EXAMPLE-LABEL")
      ("More Labels"
       "../gtk-example/label-more.lisp"
       "EXAMPLE-LABEL-MORE")
      ("Images"
       "../gtk-example/image.lisp"
       "EXAMPLE-IMAGE")
      ("Info Bar"
       "../gtk-example/info-bar.lisp"
       "EXAMPLE-INFO-BAR")
      ("Progress Bar"
       "../gtk-example/progress-bar.lisp"
       "EXAMPLE-PROGRESS-BAR")
      ("Level Bar"
       "../gtk-example/level-bar.lisp"
       "EXAMPLE-LEVEL-BAR")
      ("Statusbar"
       "../gtk-example/statusbar.lisp"
       "EXAMPLE-STATUSBAR"))

     "Button Widgets"
     (("Simple Button"
       "../gtk-example/button-image.lisp"
       "EXAMPLE-BUTTON-IMAGE")
      ("More Buttons"
       "../gtk-example/button-more.lisp"
       "EXAMPLE-BUTTON-MORE")
      ("Toggle Buttons"
       "../gtk-example/toggle-buttons.lisp"
       "EXAMPLE-TOGGLE-BUTTONS")
      ("Link Button"
       "../gtk-example/link-button.lisp"
       "EXAMPLE-LINK-BUTTON")
      ("Switch"
       "../gtk-example/switch.lisp"
       "EXAMPLE-SWITCH")
      ("Scale Button"
       "../gtk-example/scale-button.lisp"
       "EXAMPLE-SCALE-BUTTON"))

     "Numeric/Text Data Entry"
     (("Text Entry"
       "../gtk-example/text-entry.lisp"
       "EXAMPLE-TEXT-ENTRY")
      ("Text Entry Buffer"
       "../gtk-example/text-entry-buffer.lisp"
       "EXAMPLE-TEXT-ENTRY-BUFFER")
      ("Text Entry Completion"
       "../gtk-example/text-entry-completion.lisp"
       "EXAMPLE-TEXT-ENTRY-COMPLETION")
      ("Scale Widget"
       "../gtk-example/scale-widget.lisp"
       "EXAMPLE-SCALE-WIDGET")
      ("Spin Button"
       "../gtk-example/spin-button.lisp"
       "EXAMPLE-SPIN-BUTTON"))

     "Multiline Text Editor"
     (("Simple Text View"
       "../gtk-example/text-view-simple.lisp"
       "EXAMPLE-TEXT-VIEW-SIMPLE")
      ("Text View Attributes"
       "../gtk-example/text-view-attributes.lisp"
       "EXAMPLE-TEXT-VIEW-ATTRIBUTES")
      ("Text View Tags"
       "../gtk-example/text-view-tags.lisp"
       "EXAMPLE-TEXT-VIEW-TAGS")
      ("Text View Search"
       "../gtk-example/text-view-find-next.lisp"
       "EXAMPLE-TEXT-VIEW-FIND-NEXT")
      ("Text View Insert"
       "../gtk-example/text-view-insert.lisp"
       "EXAMPLE-TEXT-VIEW-INSERT")
      ("Text View Insert Image"
       "../gtk-example/text-view-insert-image.lisp"
       "EXAMPLE-TEXT-VIEW-INSERT-IMAGE")
      ("Text View Insert Widget"
       "../gtk-example/text-view-insert-widget.lisp"
       "EXAMPLE-TEXT-VIEW-INSERT-WIDGET")
      ("Text View Tooltip"
       "../gtk-example/text-view-tooltip.lisp"
       "EXAMPLE-TEXT-VIEW-TOOLTIP"))

     "Tree, List and Icon Grid Widgets"
     (("Simple Tree View"
       "../gtk-example/tree-view-simple.lisp"
       "EXAMPLE-TREE-VIEW-SIMPLE")
      ("Tree View Path"
       "../gtk-example/tree-view-path.lisp"
       "EXAMPLE-TREE-VIEW-PATH")
      ("Tree View Content Type"
       "../gtk-example/tree-view-content-type.lisp"
       "EXAMPLE-TREE-VIEW-CONTENT-TYPE")
      ("Icon View"
       "../gtk-example/icon-view.lisp"
       "EXAMPLE-ICON-VIEW"))

     "Menus, Combo Box, Toolbar"
     (("Combo Box"
       "../gtk-example/combo-box.lisp"
       "EXAMPLE-COMBO-BOX")
      ("Combo Box Text"
       "../gtk-example/combo-box-text.lisp"
       "EXAMPLE-COMBO-BOX-TEXT")
      ("Menu"
       "../gtk-example/menu.lisp"
       "EXAMPLE-MENU")
      ("Tool Palette"
       "../gtk-example/tool-palette.lisp"
       "EXAMPLE-TOOL-PALETTE")
      ("Popover"
       "popover.lisp"
       "DO-POPOVER"
       "popover.ui"))

     "Selectors (Color/File/Font)"
     (("Color Button"
       "../gtk-example/color-button.lisp"
       "EXAMPLE-COLOR-BUTTON")
      ("Color Button Label"
       "../gtk-example/color-button-label.lisp"
       "EXAMPLE-COLOR-BUTTON-LABEL")
      ("Color Chooser Widget"
       "color-chooser-widget.lisp"
       "EXAMPLE-COLOR-CHOOSER-WIDGET")
      ("Color Chooser Dialog"
       "../gtk-example/color-chooser-dialog.lisp"
       "EXAMPLE-COLOR-CHOOSER-DIALOG")
      ("Color Chooser Palette"
       "color-chooser-palette.lisp"
       "EXAMPLE-COLOR-CHOOSER-PALETTE")
      ("File Chooser Button"
       "../gtk-example/file-chooser-button.lisp"
       "EXAMPLE-FILE-CHOOSER-BUTTON")
      ("File Chooser Dialog"
       "../gtk-example/file-chooser-dialog.lisp"
       "CREATE-FILE-CHOOSER-DIALOG")
      ("File Chooser Preview"
       "../gtk-example/file-chooser-preview.lisp"
       "CREATE-FILE-CHOOSER-PREVIEW")
      ("File Chooser Widget"
       "../gtk-example/file-chooser-widget.lisp"
       "CREATE-FILE-CHOOSER-WIDGET")
      ("File Chooser Custom Filter"
       "../gtk-example/file-chooser-custom-filter.lisp"
       "CREATE-FILE-CHOOSER-CUSTOM-FILTER")
      ("Font Button"
       "../gtk-example/font-button.lisp"
       "EXAMPLE-FONT-BUTTON")
      ("Font Button Label"
       "../gtk-example/font-button-label.lisp"
       "EXAMPLE-FONT-BUTTON-LABEL"))

     "Layout Containers"
     (("Simple Box"
       "../gtk-example/box-simple.lisp"
       "EXAMPLE-BOX-SIMPLE")
      ("Box packing"
       "../gtk-example/box-packing.lisp"
       "EXAMPLE-BOX-PACKING")
      ("Grid packing"
       "../gtk-example/grid-packing.lisp"
       "EXAMPLE-GRID-PACKING")
      ("Button Boxes"
       "../gtk-example/button-box.lisp"
       "EXAMPLE-BUTTON-BOX")
      ("Paned Window"
       "../gtk-example/paned-window.lisp"
       "EXAMPLE-PANED-WINDOW")
      ("Revealer"
       "../gtk-example/revealer.lisp"
       "EXAMPLE-REVEALER")
      ("Revealer Icon"
       "../gtk-example/revealer-icon.lisp"
       "EXAMPLE-REVEALER-ICON"
       "../gtk-example/revealer-icon.ui")
      ("Fixed Container"
       "../gtk-example/fixed.lisp"
       "EXAMPLE-FIXED")
      ("Frame Widget"
       "../gtk-example/frame.lisp"
       "EXAMPLE-FRAME")
      ("Aspect Frame"
       "../gtk-example/aspect-frame.lisp"
       "EXAMPLE-ASPECT-FRAME")
      ("Notebook"
       "../gtk-example/notebook.lisp"
       "EXAMPLE-NOTEBOOK"))

     ;; Scrolling
     "Scrolling"
     (("Scrolled Window"
       "../gtk-example/scrolled-window.lisp"
       "EXAMPLE-SCROLLED-WINDOW"))

     ;; Printing
     "Printing"
     (("Page Setup Dialog"
       "../gtk-example/page-setup-dialog.lisp"
       "CREATE-PAGE-SETUP-DIALOG")
      ("Print Dialog"
       "../gtk-example/print-dialog.lisp"
       "CREATE-PRINT-DIALOG")
      ("Print Operation"
       "../gtk-example/print-operation.lisp"
       "DO-PRINT-OPERATION"))

     ;; Cairo demos
     "Cairo"
     (("Cairo Stroke"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-STROKE")
      ("Cairo Fill"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-FILL")
      ("Cairo Text"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-TEXT")
      ("Cairo Paint"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-PAINT")
      ("Cairo Mask"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-MASK")
      ("Cairo Source RGBA"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-SET-SOURCE-RGBA")
      ("Cairo Source Gradient"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-SET-SOURCE-GRADIENT")
      ("Cairo Path"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-PATH")
      ("Cairo Dash"
       "../cairo-demo/cairo-demo.lisp"
       "DEMO-CAIRO-DASH")
      ("Cairo Clock"
       "../cairo-demo/cairo-clock.lisp"
       "DEMO-CAIRO-CLOCK"))

     "Miscellaneous"
     (("Drawing in response to input"
       "../gtk-example/drawing-area-input.lisp"
       "EXAMPLE-DRAWING-AREA-INPUT")
      ("Calendar"
       "../gtk-example/calendar.lisp"
       "EXAMPLE-CALENDAR")
      ("Cursor"
       "cursor.lisp"
       "DEMO-CURSOR")
      ("Event Box"
       "../gtk-example/event-box.lisp"
       "EXAMPLE-EVENT-BOX")
      ("Demo Pixbufs"
       "pixbufs.lisp"
       "DEMO-PIXBUFS")
      ("Emblemed Icons"
       "../gio-example/emblemed-icon.lisp"
       "EXAMPLE-EMBLEMED-ICON")
      ("Align Widget"
       "../gtk-example/widget-align.lisp"
       "EXAMPLE-WIDGET-ALIGN"))

     ;; Examples from the tutorial
     "GTK 3 Tutorial for Lisp"
      ;; Chapter: Getting started
      ("Chapter: Getting started"
       (("Simple Window"
         "../gtk-example/window-simple.lisp"
         "EXAMPLE-WINDOW-SIMPLE")
        ("Getting started"
         "../gtk-example/getting-started.lisp"
         "EXAMPLE-GETTING-STARTED")
        ("Hello World"
         "../gtk-example/hello-world.lisp"
         "EXAMPLE-HELLO-WORLD")
        ("Hello World Upgraded"
         "../gtk-example/hello-world-upgraded.lisp"
         "EXAMPLE-HELLO-WORLD-UPGRADED")
        ("Hello World Upgraded (more Lisp like)"
         "../gtk-example/hello-world-upgraded-2.lisp"
         "EXAMPLE-HELLO-WORLD-UPGRADED-2")
        ("Drawing in response to input"
         "../gtk-example/drawing-area-input.lisp"
         "EXAMPLE-DRAWING-AREA-INPUT")))

      ;; Chapter: Packing Widgets
      ("Chapter: Packing Widgets"
       (("Simple Box"
         "../gtk-example/box-simple.lisp"
         "EXAMPLE-BOX-SIMPLE")
        ("Box Packing"
         "../gtk-example/box-packing.lisp"
         "EXAMPLE-BOX-PACKING")
        ("Simple Grid"
         "../gtk-example/grid-simple.lisp"
         "EXAMPLE-GRID-SIMPLE")
        ("Simple Grid more Spacing"
         "../gtk-example/grid-spacing.lisp"
         "EXAMPLE-GRID-SPACING")
        ("Packing using GtkGrid"
         "../gtk-example/grid-packing.lisp"
         "EXAMPLE-GRID-PACKING")))

      ;; Chapter: Button Widgets
      ("Chapter: Button Widgets"
       (("Simple Button"
         "../gtk-example/button-image.lisp"
         "EXAMPLE-BUTTON-IMAGE")
        ("More Buttons"
         "../gtk-example/button-more.lisp"
         "EXAMPLE-BUTTON-MORE")
        ("Radio Button"
         "../gtk-example/radio-button.lisp"
         "EXAMPLE-RADIO-BUTTON")
        ("Toggle Buttons"
         "../gtk-example/toggle-buttons.lisp"
         "EXAMPLE-TOGGLE-BUTTONS")
        ("Link Button"
         "../gtk-example/link-button.lisp"
         "EXAMPLE-LINK-BUTTON")
        ("Switch"
         "../gtk-example/switch.lisp"
         "EXAMPLE-SWITCH")
        ("Scale Button"
         "../gtk-example/scale-button.lisp"
         "EXAMPLE-SCALE-BUTTON")))

      ;; Chapter: Display Widgets
      ("Chapter: Display Widgets"
       (("Labels"
         "../gtk-example/label.lisp"
         "EXAMPLE-LABEL")
        ("More Labels"
         "../gtk-example/label-more.lisp"
         "EXAMPLE-LABEL-MORE")
        ("Images"
         "../gtk-example/image.lisp"
         "EXAMPLE-IMAGE")
        ("Info Bar"
         "../gtk-example/info-bar.lisp"
         "EXAMPLE-INFO-BAR")
        ("Progress Bar"
         "../gtk-example/progress-bar.lisp"
         "EXAMPLE-PROGRESS-BAR")
        ("Statusbar"
         "../gtk-example/statusbar.lisp"
         "EXAMPLE-STATUSBAR")))

      ;; Chapter: Layout Widgets
      ("Chapter: Layout Widgets"
       (("Button Boxes"
         "../gtk-example/button-box.lisp"
         "EXAMPLE-BUTTON-BOX")
        ("Paned Window"
         "../gtk-example/paned-window.lisp"
         "EXAMPLE-PANED-WINDOW")
        ("Layout Widget"
         "../gtk-example/layout.lisp"
         "EXAMPLE-LAYOUT")
        ("Notebook"
         "../gtk-example/notebook.lisp"
         "EXAMPLE-NOTEBOOK")
        ("Frame Widget"
         "../gtk-example/frame.lisp"
         "EXAMPLE-FRAME")
        ("Aspect Frame"
         "../gtk-example/aspect-frame.lisp"
         "EXAMPLE-ASPECT-FRAME")
        ("Fixed Container"
         "../gtk-example/fixed.lisp"
         "EXAMPLE-FIXED")))

     "Deprecated"
     (("Table Packing"
       "../gtk-example/table-packing.lisp"
       "EXAMPLE-TABLE-PACKING")
      ("Table Packing more spacing"
       "../gtk-example/table-packing-2.lisp"
       "EXAMPLE-TABLE-PACKING-2")
      ("Application Window"
       "../gtk-example/window-application.lisp"
       "EXAMPLE-WINDOW-APPLICATION")
      ("Numerable Icons"
       "../gtk-example/numerable-icon.lisp"
       "EXAMPLE-NUMERABLE-ICON")
      ("Arrow Button"
       "../gtk-example/arrow-button.lisp"
       "EXAMPLE-ARROW-BUTTON")
      ("Alignment"
       "../gtk-example/alignment.lisp"
       "EXAMPLE-ALIGNMENT")
      ("Alignment Interactive"
       "../gtk-example/alignment-interactive.lisp"
       "EXAMPLE-ALIGNMENT-INTERACTIVE"))
))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun create-and-fill-tree-store (data &optional (model nil) (iter nil))
  (unless model
    (setf model (apply #'gtk-tree-store-new (mklist (first data))))
    (setf data (rest data)))
  (let ((parent iter))
    (dolist (entry (mklist data))
      (cond ((or (atom entry) (every #'atom entry))
             (setf parent
                   (apply #'gtk-tree-store-set model
                                               (gtk-tree-store-append model
                                                                      iter)
                                               (mklist entry))))
            ((some #'listp entry)
             (create-and-fill-tree-store entry
                                         model
                                         parent)))))
  model)

;;; ----------------------------------------------------------------------------

(defun create-view-and-model ()
  (let* ((model (create-and-fill-tree-store *tree-model*))
         (view (make-instance 'gtk-tree-view
                              :model model))
         (selection (gtk-tree-view-selection view)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk-cell-renderer-text-new))
           (column (gtk-tree-view-column-new-with-attributes "Example"
                                                             renderer
                                                             "text" 0)))
      (gtk-tree-view-append-column view column))
    (g-signal-connect view "row-activated"
       (lambda (tree-view path column)
         (declare (ignore column))
         (let* ((model (gtk-tree-view-model tree-view))
                (iter (gtk-tree-model-iter model path))
                (func-name (gtk-tree-model-value model iter 2))
                (func (or (find-symbol func-name :gtk-demo)
                          (find-symbol func-name :cairo-demo)
                          (find-symbol func-name :cairo-clock))))
           (if func
               (funcall func)
               (format t "~%No function.~%")))))
    (setf (gtk-tree-selection-mode selection) :browse)
    ;; The selection has changed.
    (g-signal-connect selection "changed"
       (lambda (tree-selection)
         (let* ((iter (gtk-tree-selection-selected tree-selection))
                (filename (gtk-tree-model-value model iter 1))
                (ui-file (gtk-tree-model-value model iter 3))
                (css-file (gtk-tree-model-value model iter 4)))
           (if (> (length filename) 0)
               (load-file filename))
           (if (> (length ui-file) 0)
               (load-file-buffer ui-buffer ui-file)
               (clear-buffer ui-buffer))
           (if (> (length css-file) 0)
               (load-file-buffer css-buffer css-file)
               (clear-buffer css-buffer)))))
      view))

(defun gtk-demo-activate (application)
  (let ((window (make-instance 'gtk-application-window
                               :application application
                               :type :toplevel
                               :title "GTK Lisp Code Demos"
                               :default-width 1000
                               :default-height 800))
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
                        (leave-gtk-main)
                        (g-application-quit application)))
    ;; Set an icon for the application
    (let ((pixbuf (gdk-pixbuf-new-from-file (rel-path "gtk-logo-rgb.gif"))))
      (setq pixbuf (gdk-pixbuf-add-alpha pixbuf t 255 255 255))
      (setf (gtk-window-default-icon-list) (list pixbuf)))
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
    (gtk-notebook-append-page notebook
                              (create-text ui-buffer t)
                              (gtk-label-new-with-mnemonic "_UI Definition"))
    (gtk-notebook-append-page notebook
                              (create-text css-buffer t)
                              (gtk-label-new-with-mnemonic "_CSS Definition"))
    ;; Add the content to the window
    (gtk-container-add window content)
    (gtk-widget-show-all window)))

(defun activate-about-dialog ()
  (let (;; Create an about dialog
        (dialog (make-instance 'gtk-about-dialog
                               :program-name "GTK Lisp Demo"
                               :version "0.9"
                               :copyright "(c) Dieter Kaiser"
                               :website
                               "github.com/crategus/cl-cffi-gtk"
                               :website-label "Project web site"
                               :license "LLGPL"
                               :authors '("Dieter Kaiser")
                               :documenters '("Dieter Kaiser")
                               :artists '("None")
                               :logo-icon-name
                               "applications-development"
                               :wrap-license t)))
    ;; Run the about dialog
    (gtk-dialog-run dialog)
    ;; Destroy the about dialog
    (gtk-widget-destroy dialog)))

(defvar *appmenu*
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<interface>
  <!-- interface-requires gtk+ 3.10 -->
  <menu id=\"appmenu\">
    <section>
      <item>
        <attribute name=\"label\" translatable=\"yes\">About</attribute>
        <attribute name=\"action\">app.about</attribute>
      </item>
    </section>
    <section>
      <item>
        <attribute name=\"label\" translatable=\"yes\">_Quit</attribute>
        <attribute name=\"action\">app.quit</attribute>
        <attribute name=\"accel\">&lt;Primary&gt;q</attribute>
      </item>
    </section>
  </menu>
</interface>")

(defun gtk-demo-startup (application)
  ;; Load the application menu
  (let ((builder (make-instance 'gtk-builder)))
    (gtk-builder-add-from-string builder *appmenu*)
    (setf (gtk-application-app-menu application)
          (gtk-builder-object builder "appmenu")))
  ;; Add action "about" to the application
  (let ((action (g-simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (activate-about-dialog)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))
  ;; Add action "quit" to the application
  (let ((action (g-simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application
         (dolist (window (gtk-application-windows application))
           (gtk-widget-destroy window))
         ;; Quit the main loop
         (leave-gtk-main)
         ;; Quit the application
         (g-application-quit application)))
    ;; Add the action to action map of the application
    (g-action-map-add-action application action)))

(defun gtk-demo (&rest argv)
  (within-main-loop
    (unless (string= "GTK Lisp Demo" (g-application-name))
      (setf (g-application-name) "GTK Lisp Demo"))
    (let ((gtk-demo (make-instance 'gtk-application
                                   :application-id "com.crategus.gtk-demo"
                                   :register-session t)))
      ;; Connect signal handlers to the application
      (g-signal-connect gtk-demo "activate" #'gtk-demo-activate)
      (g-signal-connect gtk-demo "startup" #'gtk-demo-startup)
      ;; Start the application
      (g-application-run gtk-demo argv)))
  (join-gtk-main))

;;; --- End of file gtk-demo.lisp ----------------------------------------------
