;;;; Demo GdkCursor

(in-package :gtk-demo)

(defun load-tool-items (palette)
  (let ((group (make-instance 'gtk-tool-item-group
                              :label "Available Standard Cursors"))
        (cursor-types (gobject::get-enum-items "GdkCursorType"))
       )

    (dolist (cursor-type cursor-types)

      (let ((name (gobject::enum-item-name cursor-type))
            (value (gobject::enum-item-value cursor-type))
            (nick (gobject::enum-item-nick cursor-type)))

       (when (and (>= value 0)
                  (< value (foreign-enum-value 'gdk-cursor-type :last-cursor)))
         (let* ((cursor (gdk-cursor-new-for-display (gdk-display-default) value))
                (image (gtk-image-new-from-pixbuf (gdk-cursor-image cursor)))
                (button (gtk-tool-button-new image nick))
               )

          (format t "name  : ~a~%" name)
          (format t "value : ~a~%" value)
          (format t "nick  : ~a~%" nick)

          (setf (gtk-widget-tooltip-text button) nick)
          (gtk-tool-item-group-insert group button -1)

    ))))

    (gtk-container-add palette group))

  (let ((group (make-instance 'gtk-tool-item-group
                              :label "Cursors created from name"))
        (names (list
 "default" "help" "context-menu" "pointer" "progress" "wait"
 "cell"
"crosshair"
"text"
"vertical-text"
"alias"
"copy"
"move"
"no-drop"
"dnd-ask"
"not-allowed"
"grab"
"grabbing"
"all-scroll"
"col-resize"
"row-resize"
"n-resize"
"e-resize"
"s-resize"
"w-resize"
"ne-resize"
"nw-resize"
"se-resize"
"sw-resize"
"ew-resize"
"ns-resize"
"nesw-resize"
"nwse-resize"
"zoom-in"
"zoom-out"
)


))

    (dolist (name names)

      (let* ((cursor (gdk-cursor-new-from-name (gdk-display-default) name))
             (image (gtk-image-new-from-pixbuf (gdk-cursor-image cursor)))
             (button (gtk-tool-button-new image name)))

        (setf (gtk-widget-tooltip-text button) name)
        (gtk-tool-item-group-insert group button -1)

    ))

    (gtk-container-add palette group)))

(defun demo-gdk-cursor ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo GdkCursor"
                                 :default-width 320
                                 :default-heigt 240
                                 :border-width 12))
          ;; A GtkGrid for the content of the window.
          (content (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-spacing 24))
          ;; A scrollable
          (scroller (make-instance 'gtk-scrolled-window
                                   :hscrollbar-policy :never
                                   :vscrollbar-policy :automatic
                                   :hexpand t
                                   :vexpand t
                                   :default-height 320
                                   :default-width 480))
          ;; A GtkGrid for the actions.
          (action (make-instance 'gtk-grid
                                 :orientation :vertical
                                 :row-spacing 6))


          (palette (make-instance 'gtk-tool-palette)))


      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))


      ;; Show the Global Settings
      (let* ((settings (gtk-settings-default))
             (blink (gtk-settings-gtk-cursor-blink settings))
             (blink-time (gtk-settings-gtk-cursor-blink-time settings))
             (blink-timeout (gtk-settings-gtk-cursor-blink-timeout settings))
             (theme-name (gtk-settings-gtk-cursor-theme-name settings))
             (theme-size (gtk-settings-gtk-cursor-theme-size settings)))

        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Settings</b>"))

        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "Theme Name    : ~a" theme-name)))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "Theme Size    : ~a" theme-size)))

        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "Blink         : ~a" blink)))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "Blink Time    : ~a" blink-time)))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "Blink Timeout : ~a" blink-timeout)))


      )

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
                                             :test #'equal))))
               (format t "Signal CHANGED text = ~A, style = ~A~%" text style)
               (if (eq style :default)
                   ;; TODO: This seems to not work.
                   (gtk-tool-palette-unset-style palette)
                   (gtk-tool-palette-set-style palette style))
             )))

        (gtk-combo-box-text-append-text combo "Icons")
        (gtk-combo-box-text-append-text combo "Text")
        (gtk-combo-box-text-append-text combo "Both")
        (gtk-combo-box-text-append-text combo "Both Horizontal")
        (gtk-combo-box-text-append-text combo "Default")
        (setf (gtk-combo-box-active combo) 0)
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Style</b>"))
        (gtk-container-add action combo))

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
               (setf (gtk-tool-palette-icon-size palette) size)
             )))

        (gtk-combo-box-text-append-text combo "Menu")
        (gtk-combo-box-text-append-text combo "Small Toolbar")
        (gtk-combo-box-text-append-text combo "Large Toolbar")
        (gtk-combo-box-text-append-text combo "Button")
        (gtk-combo-box-text-append-text combo "Dnd")
        (gtk-combo-box-text-append-text combo "Dialog")
        (setf (gtk-combo-box-active combo) 1)
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Icon Size</b>"))
        (gtk-container-add action combo))

      ;; Load icons into the tool palette
      (load-tool-items palette)
      (gtk-container-add scroller palette)
      (gtk-container-add content scroller)
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))

