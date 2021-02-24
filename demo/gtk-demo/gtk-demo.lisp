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
                                  :shadow-type :in
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

(defun create-and-fill-model ()
  (let ((model (make-instance 'gtk-tree-store
                              :column-types '("gchararray" ; Title
                                              "gchararray" ; Filename
                                              "gchararray" ; Function name
                                              "gchararray" ; UI Definition
                                              "gchararray" ; CSS Definition
                                              ))))
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Theming in GTK+")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "CSS Basics"
                                "css-basics.lisp"
                                "DO-CSS-BASICS"
                                ""
                                "css-basics.css")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "CSS Accordion"
                                "css-accordion.lisp"
                                "DEMO-CSS-ACCORDION"
                                ""
                                "css-accordion.css")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "CSS Blend Modes"
                                "css-blendmodes.lisp"
                                "DO-CSS-BLENDMODES"
                                "css-blendmodes.ui"
                                "css-blendmodes.css")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "CSS Pixbufs"
                                "css-pixbufs.lisp"
                                "DEMO-CSS-PIXBUFS"
                                ""
                                "css-pixbufs.css")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Numerable Icons"
                                "numerable-icon.lisp"
                                "DEMO-NUMERABLE-ICON")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Windows")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Window"
                                "simple-window.lisp"
                                "EXAMPLE-SIMPLE-WINDOW")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Message"
                                "simple-message.lisp"
                                "EXAMPLE-SIMPLE-MESSAGE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Dialogs"
                                "dialogs.lisp"
                                "EXAMPLE-DIALOG")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Assistant"
                                "assistant.lisp"
                                "DEMO-ASSISTANT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Application Window"
                                "application-window.lisp"
                                "DEMO-APPLICATION-WINDOW")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Display Widgets")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Labels"
                                "labels.lisp"
                                "EXAMPLE-LABELS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Images"
                                "image.lisp"
                                "DEMO-IMAGE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "More Labels"
                                "more-labels.lisp"
                                "EXAMPLE-MORE-LABELS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Progress Bar Widget"
                                "progress-bar.lisp"
                                "EXAMPLE-PROGRESS-BAR")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Statusbar"
                                "statusbar.lisp"
                                "EXAMPLE-STATUSBAR")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Info Bar"
                                "info-bar.lisp"
                                "EXAMPLE-INFO-BAR")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Button Widgets")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Button"
                                "button.lisp"
                                "EXAMPLE-BUTTON")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "More Buttons"
                                "buttons.lisp"
                                "EXAMPLE-MORE-BUTTONS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Toggle Buttons"
                                "toggle-buttons.lisp"
                                "EXAMPLE-TOGGLE-BUTTONS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Link Button"
                                "link-button.lisp"
                                "EXAMPLE-LINK-BUTTON")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Switch"
                                "switch.lisp"
                                "EXAMPLE-SWITCH")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Numeric/Text Data Entry")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text Entry"
                                "text-entry.lisp"
                                "EXAMPLE-TEXT-ENTRY")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text Entry Buffer"
                                "entry-buffer.lisp"
                                "EXAMPLE-ENTRY-BUFFER")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text Entry Completion"
                                "entry-completion.lisp"
                                "EXAMPLE-ENTRY-COMPLETION")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Scale Widgets"
                                "scale-widgets.lisp"
                                "EXAMPLE-SCALE-WIDGETS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Spin Button"
                                "spin-button.lisp"
                                "EXAMPLE-SPIN-BUTTON")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                             "Multiline Text Editor")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Text View"
                                "../../tutorial/src/text-view-simple.lisp"
                                "EXAMPLE-TEXT-VIEW-SIMPLE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Attributes"
                                "../../tutorial/src/text-view-attributes.lisp"
                                "EXAMPLE-TEXT-VIEW-ATTRIBUTES")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Tags"
                                "../../tutorial/src/text-view-tags.lisp"
                                "EXAMPLE-TEXT-VIEW-TAGS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Search"
                                "../../tutorial/src/text-view-find-next.lisp"
                                "EXAMPLE-TEXT-VIEW-FIND-NEXT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Insert"
                                "../../tutorial/src/text-view-insert.lisp"
                                "EXAMPLE-TEXT-VIEW-INSERT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Insert Image"
                                "../../tutorial/src/text-view-insert-image.lisp"
                                "EXAMPLE-TEXT-VIEW-INSERT-IMAGE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Insert Widget"
                                "../../tutorial/src/text-view-insert-widget.lisp"
                                "EXAMPLE-TEXT-VIEW-INSERT-WIDGET")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Text View Tooltip"
                                "../../tutorial/src/text-view-tooltip.lisp"
                                "EXAMPLE-TEXT-VIEW-TOOLTIP")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Tree, List and Icon Grid Widgets")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Tree View"
                                "../../tutorial/src/tree-view-simple.lisp"
                                "EXAMPLE-TREE-VIEW-SIMPLE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Tree View Path"
                                "../../tutorial/src/tree-view-path.lisp"
                                "EXAMPLE-TREE-VIEW-PATH")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Menus, Combo Box, Toolbar")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Combo Box"
                                "combo-box.lisp"
                                "EXAMPLE-COMBO-BOX")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Combo Box Text"
                                "combo-box-text.lisp"
                                "EXAMPLE-COMBO-BOX-TEXT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Menu"
                                "menu.lisp"
                                "EXAMPLE-MENU")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Tool Palette"
                                "tool-palette.lisp"
                                "DEMO-TOOL-PALETTE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Popover"
                                "popover.lisp"
                                "DO-POPOVER"
                                "popover.ui")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Selectors (Color/File/Font)")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Button"
                                "color-button.lisp"
                                "EXAMPLE-COLOR-BUTTON")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Button Label"
                                "../../tutorial/src/color-button-label.lisp"
                                "EXAMPLE-COLOR-BUTTON-LABEL")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Chooser Widget"
                                "color-chooser-widget.lisp"
                                "EXAMPLE-COLOR-CHOOSER-WIDGET")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Chooser Dialog"
                                "../../tutorial/src/color-chooser-dialog.lisp"
                                "EXAMPLE-COLOR-CHOOSER-DIALOG")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Color Chooser Palette"
                                "color-chooser-palette.lisp"
                                "EXAMPLE-COLOR-CHOOSER-PALETTE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "File Chooser Button"
                                "../../tutorial/src/file-chooser-button.lisp"
                                "EXAMPLE-FILE-CHOOSER-BUTTON")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                 "File Chooser Dialog"
                                 "file-chooser-dialog.lisp"
                                 "CREATE-FILE-CHOOSER-DIALOG")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                 "File Chooser Preview"
                                 "file-chooser-preview.lisp"
                                 "CREATE-FILE-CHOOSER-PREVIEW")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                 "File Chooser Widget"
                                 "file-chooser-widget.lisp"
                                 "CREATE-FILE-CHOOSER-WIDGET")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                 "File Chooser Custom Filter"
                                 "file-chooser-custom-filter.lisp"
                                 "CREATE-FILE-CHOOSER-CUSTOM-FILTER")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Font Button"
                                "font-button.lisp"
                                "EXAMPLE-FONT-BUTTON")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Font Button Label"
                                "../../tutorial/src/font-button-label.lisp"
                                "EXAMPLE-FONT-BUTTON-LABEL")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Layout Containers")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Simple Box"
                                "box-simple.lisp"
                                "DEMO-BOX-SIMPLE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Box packing"
                                "box-packing.lisp"
                                "EXAMPLE-BOX-PACKING")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Table packing"
                                "table-packing.lisp"
                                "EXAMPLE-TABLE-PACKING")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Grid packing"
                                "grid-packing.lisp"
                                "EXAMPLE-GRID-PACKING")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Button Boxes"
                                "button-box.lisp"
                                "EXAMPLE-BUTTON-BOX")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Alignment"
                                "alignment.lisp"
                                "DEMO-ALIGNMENT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Fixed Container"
                                "fixed.lisp"
                                "EXAMPLE-FIXED")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Frame Container"
                                "frame.lisp"
                                "DEMO-FRAME")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Aspect Frame"
                                "aspect-frame.lisp"
                                "EXAMPLE-ASPECT-FRAME")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Paned Window"
                                "paned-window.lisp"
                                "EXAMPLE-PANED-WINDOW")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Notebook"
                                "notebook.lisp"
                                "EXAMPLE-NOTEBOOK")
    )
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Scrolling")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Scrolled Window"
                                "scrolled-window.lisp"
                                "EXAMPLE-SCROLLED-WINDOW")
    )
    ;; Cairo demos
    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Cairo")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Stroke"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-STROKE")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Fill"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-FILL")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Text"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-TEXT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Paint"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-PAINT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Mask"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-MASK")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Source RGBA"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-SET-SOURCE-RGBA")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Source Gradient"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-SET-SOURCE-GRADIENT")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Path"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-PATH")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Dash"
                                "../cairo-demo/cairo-demo.lisp"
                                "DEMO-CAIRO-DASH")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cairo Clock"
                                "../cairo-demo/cairo-clock.lisp"
                                "DEMO-CAIRO-CLOCK")
    )

    (let ((parent (gtk-tree-store-set model (gtk-tree-store-append model nil)
                                            "Miscellaneous")))
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Drawing in response to input"
                                "drawing.lisp"
                                "EXAMPLE-DRAWING")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Arrows"
                                "arrows.lisp"
                                "EXAMPLE-ARROWS")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Calendar"
                                "calendar.lisp"
                                "EXAMPLE-CALENDAR")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Cursor"
                                "cursor.lisp"
                                "DEMO-CURSOR")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Event Box"
                                "event-box.lisp"
                                "EXAMPLE-EVENT-BOX")
      (gtk-tree-store-set model (gtk-tree-store-append model parent)
                                "Demo Pixbufs"
                                "pixbufs.lisp"
                                "DEMO-PIXBUFS")
    )
    model))

(defun create-view-and-model ()
  (let* ((model (create-and-fill-model))
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
                                 :title "GTK+ Lisp Code Demos"
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

(defun gtk-demo (&optional (argv nil))
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
      (g-application-run gtk-demo argv))))

;;; --- End of file gtk-demo.lisp ----------------------------------------------
