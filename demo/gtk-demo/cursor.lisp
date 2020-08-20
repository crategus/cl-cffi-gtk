;;;; Demo Cursor
;;;;
;;;; Demonstrates a useful set of available cursors.

(in-package :gtk-demo)

(defun cursor-add-button (section name)
  (let* ((display (gtk-widget-display section))
         (cursor (gdk-cursor-new-from-name display name))
         (image (if cursor
                    (gtk-image-new-from-pixbuf (gdk-cursor-image cursor))
                    (gtk-image-new-from-icon-name "image-missing" :menu)))
         (button (make-instance 'gtk-button
                                :tooltip-text name)))
    ;; Signal handler to set the clicked cursor
    (g-signal-connect button "clicked"
        (lambda (widget)
          (declare (ignore widget))
          (let ((window (gtk-widget-window (gtk-widget-toplevel button))))
            (setf (gdk-window-cursor window) cursor))))
    ;; Put the image of the cursor in the button
    (gtk-container-add button image)
    ;; Add the button to the section
    (gtk-container-add section button)))

(defun cursor-add-section (box heading)
  (let ((label (make-instance 'gtk-label
                              :use-markup t
                              :label (format nil "<b>~a</b>" heading)
                              :xalign 0.0
                              :margin-top 12
                              :margin-bottom 6))
        (section (make-instance 'gtk-flow-box
                                :halign :start
                                :selection-mode :none
                                :min-children-per-line 2
                                :max-children-per-line 20)))
    (gtk-box-pack-start box label :expand nil)
    (gtk-box-pack-start box section :expand nil)
    section))

(defun demo-cursor ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cursor"
                                 :default-height 480
                                 :default-width 360
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
                                   :vexpand t))
          ;; A GtkGrid for the actions
          (action (make-instance 'gtk-grid
                                 :orientation :vertical
                                 :row-spacing 6
                                 :margin-end 24))
          ;; A GtkBox for the cursors
          (cursors (make-instance 'gtk-box
                                  :orientation :vertical
                                  :margin-end 24)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the Global Settings related to cursors
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
                                          :margin-bottom 6
                                          :label
                                          "<b>Global Settings</b>"))
        (let ((grid (make-instance 'gtk-grid
                                   :column-spacing 12
                                   :row-spacing 6)))
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label "Theme Name :"
                                          :xalign 1.0)
                           0 0 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "~a" theme-name)
                                          :xalign 0.0)
                           1 0 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label "Theme Size :"
                                          :xalign 1.0)
                           0 1 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label (format nil "~a" theme-size)
                                          :xalign 0.0)
                           1 1 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label "Blink :"
                                          :xalign 1.0)
                           0 2 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label (format nil "~a" blink)
                                          :xalign 0.0)
                           1 2 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label "Blink Time :"
                                          :xalign 1.0)
                           0 3 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "~a" blink-time)
                                          :xalign 0.0)
                           1 3 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label "Blink Timeout :"
                                          :xalign 1.0)
                           0 4 1 1)
          (gtk-grid-attach grid
                           (make-instance 'gtk-label
                                          :label
                                          (format nil "~a" blink-timeout)
                                          :xalign 0.0)
                           1 4 1 1)
        (gtk-container-add action grid)))
      ;; Create and add the sections for the cursors
      (let ((section (cursor-add-section cursors "General")))
        (cursor-add-button section "default"))
      (let ((section (cursor-add-section cursors "Link &amp; Status")))
        (cursor-add-button section "context-menu")
        (cursor-add-button section "help")
        (cursor-add-button section "pointer")
        (cursor-add-button section "progress")
        (cursor-add-button section "wait"))
      (let ((section (cursor-add-section cursors "Selection")))
        (cursor-add-button section "cell")
        (cursor-add-button section "crosshair")
        (cursor-add-button section "text")
        (cursor-add-button section "vertical-text"))
      (let ((section (cursor-add-section cursors "Drag &amp; Drop")))
        (cursor-add-button section "alias")
        (cursor-add-button section "copy")
        (cursor-add-button section "move")
        (cursor-add-button section "no-drop")
        (cursor-add-button section "not-allowed")
        (cursor-add-button section "grab")
        (cursor-add-button section "grabbing"))
      (let ((section (cursor-add-section cursors "Resize &amp; Scrolling")))
        (cursor-add-button section "all-scroll")
        (cursor-add-button section "col-resize")
        (cursor-add-button section "row-resize")
        (cursor-add-button section "n-resize")
        (cursor-add-button section "e-resize")
        (cursor-add-button section "s-resize")
        (cursor-add-button section "w-resize")
        (cursor-add-button section "ne-resize")
        (cursor-add-button section "nw-resize")
        (cursor-add-button section "se-resize")
        (cursor-add-button section "sw-resize")
        (cursor-add-button section "ew-resize")
        (cursor-add-button section "ns-resize")
        (cursor-add-button section "nesw-resize")
        (cursor-add-button section "nwse-resize"))
      (let ((section (cursor-add-section cursors "Zoom")))
        (cursor-add-button section "zoom-in")
        (cursor-add-button section "zoom-out"))
      ;; Put the widgets into the window
      (gtk-container-add scroller cursors)
      (gtk-container-add content scroller)
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))

