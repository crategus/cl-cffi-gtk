;;;; Example Tool Palette (2021-3-15)
;;;;
;;;; A tool palette widget shows groups of toolbar items as a grid of icons or
;;;; a list of names.

(in-package :gtk-example)

(defun load-icon-items (palette)
  (let* ((max-icons 24) ; Do not load too much icons.
         (icon-theme (gtk-icon-theme-for-screen (gtk-widget-screen palette)))
         (contexts (gtk-icon-theme-list-contexts icon-theme)))
    (dolist (context contexts)
      (let ((group (gtk-tool-item-group-new context))
            (icons (gtk-icon-theme-list-icons icon-theme context)))
        (dolist (icon-name (subseq icons 0 (min max-icons (length icons))))
          (let ((item (make-instance 'gtk-tool-button
                                     :icon-name icon-name
                                     :label icon-name
                                     :tooltip-text icon-name)))
            (gtk-tool-item-group-insert group item -1)))
         (gtk-container-add palette group)))))

;; Palette DnD

(defun palette-drop-item (drag-item drop-group x y)
  (let ((drag-group (gtk-widget-parent drag-item))
        (drop-item (gtk-tool-item-group-drop-item drop-group x y))
        (drop-position -1))

    (format t "PALETTE-DROP-ITEM~%")
    (format t "   drag-group : ~A~%" drag-group)
    (format t "   drop-item  : ~A~%" drop-item)

    (when drop-item
      (setf drop-position
            (gtk-tool-item-group-child-position drop-group drop-item)))

    (format t "   drop-pos   : ~A~%" drop-position)

    (if (not (equal drag-group drop-group))
        (progn
          (format t "DRAG-GROUP and DROP-GROUP are different~%")
          (let ((child-props (gtk-container-child-get drag-group
                                                      drag-item
                                                      "homogeneous"
                                                      "expand"
                                                      "fill"
                                                      "new-row")))

            (format t "   child-props : ~A~%" child-props)

            (gtk-container-remove drag-group drag-item)
            (gtk-tool-item-group-insert drop-group drag-item drop-position)
            ;; FIXME: Does not work. Tries to assign NIL to a pointer.
            ;; This need to be fixed more general.
;            (gtk-container-child-set drop-group
;                                     drag-item
;                                     "homogenous" (pop child-props)
;                                     "expand" (pop child-props)
;                                     "fill" (pop child-props)
;                                     "new-row" (pop child-props))))
          ))
        (setf (gtk-tool-item-group-child-position drop-group drag-item)
              drop-position))))


(defun palette-drop-group (palette drag-group drop-group)
  (let ((drop-position -1))

    (format t "pos drag-group  : ~a~%"
              (gtk-tool-palette-group-position palette drag-group))
    (format t "pos drop-group  : ~a~%"
              (gtk-tool-palette-group-position palette drop-group))

    (when drop-group
      (setf drop-position
            (gtk-tool-palette-group-position palette drop-group)))

    (format t "drop-position   : ~a~%" drop-position)
    ;; FIXME: This does not work. The position is not changed.
    ;; Is this a bug in C Library? Does not work for the C gtk3-demo, too.
    (setf (gtk-tool-palette-group-position palette drag-group) drop-position)))


(defun palette-drag-date-received (widget context x y selection info time)
  (let ((drag-palette (gtk-drag-source-widget context)))

    (format t "DRAG-DATA-RECEIVED~%")
    (format t "   widget       : ~A~%" widget)
    (format t "   context      : ~A~%" context)
    (format t "   x,y          : ~A, ~A~%" x y)
    (format t "   selection    : ~A~%" selection)
    (format t "   info         : ~A~%" info)
    (format t "   time         : ~A~%" time)

    ;; Get the tool palette CONTEXT belongs to, its a parent widget
    (loop while (and drag-palette
                     (not (g-type-is-a (g-object-type drag-palette)
                                       "GtkToolPalette")))
          do (setf drag-palette
                   (gtk-widget-parent drag-palette)))

    (format t "   drag-palette : ~A~%~%" drag-palette)

    (when drag-palette
      (let ((drag-item (gtk-tool-palette-drag-item drag-palette selection))
            (drop-group (gtk-tool-palette-drop-group widget x y)))

        (format t "   drag-item    : ~A~%" drag-item)
        (format t "   drop-group   : ~A~%" drop-group)

        (cond ((g-type-is-a (g-object-type drag-item) "GtkToolItemGroup")
               (format t "PALETTE DROP GROUP~%")
               (palette-drop-group drag-palette drag-item drop-group))
              ((and drop-group
                    (g-type-is-a (g-object-type drag-item) "GtkToolItem"))
               (format t "PALETTE DROP ITEM~%")
               (let ((allocation (gtk-widget-allocation drop-group)))
                 (format t "   allocation = ~A~%" allocation)
                 (palette-drop-item drag-item
                                    drop-group
                                    (- x (gdk-rectangle-x allocation))
                                    (- y (gdk-rectangle-y allocation)))))
              (t
               (format t "NO VALID DRAG~%")))))))

;; Passive DnD

(defvar *canvas-items* nil)

(defstruct canvas-item
  pixbuf
  x
  y)

(defun canvas-item-new (widget button x y)
  (let* ((item (make-canvas-item))
         (icon-theme (gtk-icon-theme-for-screen (gtk-widget-screen widget)))
         (icon-name (gtk-tool-button-icon-name button))
         (icon-size 48) ; workaround, better implementation needed
         (pixbuf (gtk-icon-theme-load-icon icon-theme
                                           icon-name
                                           icon-size
                                           :generic-fallback)))
    (when pixbuf
      (setf (canvas-item-pixbuf item) pixbuf)
      (setf (canvas-item-x item) x)
      (setf (canvas-item-y item) y))
    item))

(defun canvas-item-draw (item cr preview)
  (let ((cx (gdk-pixbuf-width (canvas-item-pixbuf item)))
        (cy (gdk-pixbuf-height (canvas-item-pixbuf item))))

    (gdk-cairo-set-source-pixbuf cr (canvas-item-pixbuf item)
                                    (- (canvas-item-x item) (* 0.5d0 cx))
                                    (- (canvas-item-y item) (* 0.5d0 cy)))

    (if preview
        (cairo-paint-with-alpha cr 0.6d0)
        (cairo-paint cr))))

(defun canvas-draw (widget cr)
  (declare (ignore widget))
  (let ((cr (pointer cr)))

    (cairo-set-source-rgb cr 1.0 1.0 1.0)
    (cairo-paint cr)

    (dolist (item *canvas-items*)
      (canvas-item-draw item cr nil))

    (cairo-destroy cr)))


(defun passive-canvas-drag-data-received (widget context x y selection info time)
  (let ((palette (gtk-drag-source-widget context))
        (tool-item nil))

    (format t "DRAG-DATA-RECEIVED~%")
    (format t "   widget       : ~A~%" widget)
    (format t "   context      : ~A~%" context)
    (format t "   x,y          : ~A, ~A~%" x y)
    (format t "   selection    : ~A~%" selection)
    (format t "   info         : ~A~%" info)
    (format t "   time         : ~A~%" time)

    ;; Get the tool palette CONTEXT belongs to, its a parent widget
    (loop while (and palette
                     (not (g-type-is-a (g-object-type palette)
                                       "GtkToolPalette")))
          do (setf palette
                   (gtk-widget-parent palette)))

    (when palette
      (setf tool-item
            (gtk-tool-palette-drag-item palette selection)))

    (when tool-item
      (let ((canvas-item (canvas-item-new widget tool-item x y)))
        (when canvas-item
          (push canvas-item *canvas-items*)
          (gtk-widget-queue-draw widget))))))


(defun example-tool-palette ()
  (within-main-loop
    (let* (;; Create a toplevel window.
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Tool Palette"
                                  :border-width 12))
           ;; A horizontal Box for the content of the window.
           (content (make-instance 'gtk-grid
                                   :orientation :horizontal
                                   :column-spacing 24))
           ;; A scrollable
           (scroller (make-instance 'gtk-scrolled-window
                                    :hscrollbar-policy :never
                                    :vscrollbar-policy :automatic
                                    :hexpand t
                                    :vexpand t
                                    :default-width 300))
           ;; A tool palette
           (palette (make-instance 'gtk-tool-palette
                                   :default-width 300))
           ;; A vertical Grid for the actions.
           (action (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :row-spacing 6))
           (contents (make-instance 'gtk-drawing-area
                                    :app-paintable t))
           (notebook (make-instance 'gtk-notebook
                                    :border-width 6))
           (page-1 (make-instance 'gtk-grid
                                  :border-width 12
                                  :orientation :vertical
                                  :row-spacing 6))
;           (page-2 (make-instance 'gtk-grid
;                                  :border-width 12
;                                  :orientation :vertical
;                                  :row-spacing 6))
)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Orientation combo box
      (let ((combo (make-instance 'gtk-combo-box-text)))
        (g-signal-connect combo "changed"
           (lambda (combobox)
             (let* ((text (gtk-combo-box-text-active-text combobox))
                    (orientation (cdr (assoc text
                                             '(("Vertical" . :vertical)
                                               ("Horizontal" . :horizontal))
                                             :test #'string=))))
               (setf (gtk-orientable-orientation palette) orientation)
               (if (eq orientation :horizontal)
                   (setf (gtk-scrolled-window-policy scroller)
                         '(:automatic :never))
                   (setf (gtk-scrolled-window-policy scroller)
                         '(:never :automatic))))))
        (gtk-combo-box-text-append-text combo "Vertical")
        (gtk-combo-box-text-append-text combo "Horizontal")
        (setf (gtk-combo-box-active combo) 0)
        (gtk-container-add page-1
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label "<b>Orientation</b>"))
        (gtk-container-add page-1 combo))

      ;; Style combo box
      (let ((combo (make-instance 'gtk-combo-box-text)))
        (g-signal-connect combo "changed"
           (lambda (combobox)
             (let* ((text (gtk-combo-box-text-active-text combobox))
                    (style (cdr (assoc text
                                             '(("Icons" . :icons)
                                               ("Text" . :text)
                                               ("Both" . :both)
                                               ("Both Horizontal" . :both-horiz)
                                               ("Default" . :default))
                                             :test #'string=))))
               (if (eq style :default)
                   ;; FIXME: This seems not to work as accepted.
                   (gtk-tool-palette-unset-style palette)
                   (setf (gtk-tool-palette-toolbar-style palette) style)))))
        (gtk-combo-box-text-append-text combo "Icons")
        (gtk-combo-box-text-append-text combo "Text")
        (gtk-combo-box-text-append-text combo "Both")
        (gtk-combo-box-text-append-text combo "Both Horizontal")
        (gtk-combo-box-text-append-text combo "Default")
        (setf (gtk-combo-box-active combo) 0)
        (gtk-container-add page-1
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Style</b>"))
        (gtk-container-add page-1 combo))

      ;; Icon size combo box
      (let ((combo (make-instance 'gtk-combo-box-text)))
        (g-signal-connect combo "changed"
           (lambda (combobox)
             (let* ((text (gtk-combo-box-text-active-text combobox))
                    (size (cdr (assoc text
                                      '(("Menu" . :menu)
                                        ("Small Toolbar" . :small-toolbar)
                                        ("Large Toolbar" . :large-toolbar)
                                        ("Button" . :button)
                                        ("Dnd" . :dnd)
                                        ("Dialog" . :dialog))
                                      :test #'equal))))
               (format t "Signal CHANGED text = ~A, size = ~A~%" text size)
               (setf (gtk-tool-palette-icon-size palette) size))))
        (gtk-combo-box-text-append-text combo "Menu")
        (gtk-combo-box-text-append-text combo "Small Toolbar")
        (gtk-combo-box-text-append-text combo "Large Toolbar")
        (gtk-combo-box-text-append-text combo "Button")
        (gtk-combo-box-text-append-text combo "Dnd")
        (gtk-combo-box-text-append-text combo "Dialog")
        (setf (gtk-combo-box-active combo) 1)
        (gtk-container-add page-1
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Icon Size</b>"))
        (gtk-container-add page-1 combo))

      (gtk-notebook-append-page notebook
                                page-1
                                (make-instance 'gtk-label
                                               :label "Properties"))

      ;; Fill the tool palette
      (load-icon-items palette)
      ;; Add the palette to the scrolled window
      (gtk-container-add scroller palette)
      ;; Add the scrolled window to the content
      (gtk-container-add content scroller)

      ;; DnD for tool items
      (gtk-tool-palette-add-drag-dest palette
                                      palette
                                      '(:all)
                                      '(:items :groups)
                                      '(:move))
      (g-signal-connect palette
                        "drag-data-received"
                        #'palette-drag-date-received)


      ;; Passive DnD dest

      (g-signal-connect contents "draw" #'canvas-draw)
      (g-signal-connect contents "drag-data-received"
                                 #'passive-canvas-drag-data-received)

      (gtk-tool-palette-add-drag-dest palette contents :all :items :copy)

      (let ((page-2 (make-instance 'gtk-scrolled-window
                                   :border-width 6
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always)))
        (gtk-container-add page-2 contents)

        (gtk-notebook-append-page notebook
                                  page-2
                                  (make-instance 'gtk-label
                                                 :label "Passive DnD Mode")))

      ;; Add the notebook to the action container
      (gtk-container-add action notebook)

      ;; A Quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit"
                                   :margin-top 12)))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-widget-destroy window)))
        ;; Add the quit button to the action container
        (gtk-container-add action button))

      ;; Add frame, content, and action to the window.
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))
