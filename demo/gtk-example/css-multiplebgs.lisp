;;;; Theming/Multiple Backgrounds - 2021-11-27
;;;;
;;;; GTK themes are written using CSS. Every widget is build of multiple items
;;;; that you can style very similarly to a regular website.

(in-package :gtk-example)

(defun example-css-multiplebgs (&optional application)
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :application application
                                  :title "Example CSS Multiple Backgrounds"
                                  :default-height 420
                                  :default-width 600))
           (overlay (make-instance 'gtk-overlay))
           (text (make-instance 'gtk-text-buffer))
           (area (make-instance 'gtk-drawing-area
                                 :name "canvas"))
           (button (make-instance 'gtk-button
                                  :name "bricks-button"
                                  :margin 12
                                  :halign :center
                                  :valign :start
                                  :width-request 270
                                  :height-request 96))
           (paned (make-instance 'gtk-paned
                                 :orientation :vertical))
           (box (make-instance 'gtk-box
                               :orientation :vertical
                               :height-request 150))
           (scrolled (make-instance 'gtk-scrolled-window))
           (view (make-instance 'gtk-text-view
                                :buffer text
                                :monospace t))
           (provider (make-instance 'gtk-css-provider)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect area "draw"
          (lambda (widget cr)
            (let ((context (gtk-widget-style-context widget))
                  (cr (pointer cr))
                  (width (gtk-widget-allocated-width widget))
                  (height (gtk-widget-allocated-height widget)))
              (gtk-render-background context cr 0 0 width height)
              (gtk-render-frame context cr 0 0 width height)
              +gdk-event-propagate+)))
      (g-signal-connect text "changed"
          (lambda (buffer)
            (let ((start (gtk-text-buffer-start-iter buffer))
                  (end (gtk-text-buffer-end-iter buffer)))
              (gtk-text-buffer-remove-all-tags buffer start end)
              (gtk-css-provider-load-from-data
                                provider
                                (gtk-text-buffer-get-text buffer start end nil))
              (apply-css-to-widget provider window)
              (gtk-style-context-reset-widgets (gdk-screen-default)))))
      (g-signal-connect provider "parsing-error"
          (lambda (provider section err)
            (declare (ignore provider err))
            (let ((start (gtk-text-buffer-iter-at-line-index
                             text
                             (gtk-css-section-start-line section)
                             (gtk-css-section-start-position section)))
                  (end (gtk-text-buffer-iter-at-line-index
                           text
                           (gtk-css-section-end-line section)
                           (gtk-css-section-end-position section))))
              (gtk-text-buffer-apply-tag text "error" start end)
              +gdk-event-stop+)))
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table text)
                              (make-instance 'gtk-text-tag
                                             :name "error"
                                             :underline :error))
      (setf (gtk-text-buffer-text text)
            (read-file (sys-path "css-multiplebgs.css")))
      ;; Add the widgets to the window
      (gtk-container-add overlay area)
      (gtk-overlay-add-overlay overlay button)
      (gtk-container-add paned box)
      (gtk-container-add scrolled view)
      (gtk-container-add paned scrolled)
      (gtk-overlay-add-overlay overlay paned)
      (gtk-container-add window overlay)
      ;; Apply the provider to the window
      (apply-css-to-widget provider window)
      ;; Show the window
      (gtk-widget-show-all window))))
