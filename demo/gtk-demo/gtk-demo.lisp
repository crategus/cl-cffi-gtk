(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-demo
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-demo)

(load "assistant.lisp")
(load "box-packing.lisp")
(load "table-packing.lisp")
(load "grid-packing.lisp")
(load "button.lisp")
(load "buttons.lisp")
(load "toggle-buttons.lisp")
(load "link-button.lisp")
(load "switch.lisp")
(load "simple-message.lisp")
(load "simple-window.lisp")
(load "dialogs.lisp")
(load "simple-text-view.lisp")
(load "text-view-attributes.lisp")
(load "simple-file-chooser.lisp")
(load "drawing.lisp")
(load "labels.lisp")
(load "more-labels.lisp")
(load "progress-bar.lisp")
(load "statusbar.lisp")
(load "info-bar.lisp")
(load "scale-widgets.lisp")
(load "alignment.lisp")
(load "fixed.lisp")
(load "aspect-frame.lisp")
(load "paned-window.lisp")
(load "scrolled-window.lisp")
(load "button-box.lisp")
(load "notebook.lisp")
(load "simple-tree-view.lisp")
(load "color-button.lisp")
(load "color-chooser-dialog.lisp")
(load "file-chooser-button.lisp")
(load "font-button.lisp")
(load "arrows.lisp")
(load "calendar.lisp")
(load "event-box.lisp")
(load "text-entry.lisp")
(load "spin-button.lisp")
(load "combo-box.lisp")
(load "combo-box-text.lisp")
(load "menu.lisp")
(load "pixbufs.lisp")

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
        (gtk-text-buffer-insert source-buffer (format nil "~%"))))
))

      

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
          (gtk-widget-override-font view
                                    (pango-font-description-from-string "monospace"))
          (gtk-text-buffer-set-text buffer "This is the place for the source."))
        (gtk-text-buffer-set-text buffer "This is the place for the info."))
    ;; return the scrolled window
    scrolled
  )
)

;;; ----------------------------------------------------------------------------

(defun create-and-fill-model ()
  (let ((model (make-instance 'gtk-tree-store
                              :column-types '("gchararray" ; Title
                                              "gchararray" ; Filename
                                              "gchararray" ; Function name
                                              "guint"))))
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
           (format t "~&ROW in GtkTreeView is activated for ~A.~%" func-name)
           (if func
               (funcall func)
               (format t "~%No function.~%")))))

    (gtk-tree-selection-set-mode selection :browse)
    (g-signal-connect selection "changed"
       (lambda (tree-selection)
         (let* ((iter (gtk-tree-selection-get-selected tree-selection))
                (filename (gtk-tree-model-get-value model iter 1)))
           (format t "~&Selection get \"changed\" signal: ~&~A ~A~%" (length filename) filename)
           (if (> (length filename) 0)
               (load-file filename))
)))


      view))

;;; ----------------------------------------------------------------------------

(defun main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "GTK+ Lisp Code Demos"
                                 :default-width 800
                                 :default-height 600))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal))
          (notebook (make-instance 'gtk-notebook
                                   :scrollable t))
          (view (create-view-and-model))
         )
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((pixbuf (gdk-pixbuf-new-from-file "gtk-logo-rgb.gif")))
        (setq pixbuf (gdk-pixbuf-add-alpha pixbuf t 255 255 255))
        (gtk-window-set-default-icon-list (list pixbuf)))

;        (gtk-window-set-default-icon pixbuf))

      (gtk-box-pack-start hbox view :expand nil :fill nil :padding 0)
      (gtk-box-pack-start hbox notebook :expand t :fill t :padding 0)
      (gtk-notebook-append-page notebook
                                (create-text info-buffer nil)
                                (gtk-label-new-with-mnemonic "_Info"))
      (gtk-notebook-append-page notebook
                                (create-text source-buffer t)
                                (gtk-label-new-with-mnemonic "_Source"))
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))

