;;;; Theming/Shadows - 2021-11-27
;;;;
;;;; This demo shows how to use CSS shadows.

(in-package :gtk-example)

(defun create-toolbar ()
  (let ((toolbar (make-instance 'gtk-toolbar)))
    (gtk-toolbar-insert toolbar
                        (make-instance 'gtk-tool-button
                                       :icon-name "go-next")
                        -1)
    (gtk-toolbar-insert toolbar
                        (make-instance 'gtk-tool-button
                                       :icon-name "go-previous")
                        -1)
    (gtk-toolbar-insert toolbar
                        (make-instance 'gtk-tool-button
                                       :label "Hello World")
                        -1)
    toolbar))

(defun example-css-shadows (&optional application)
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :application application
                                  :title "Example CSS Shadows"
                                  :default-height 420
                                  :default-width 600))
           (paned (make-instance 'gtk-paned
                                 :orientation :vertical))
           (toolbar (create-toolbar))
           (scrolled (make-instance 'gtk-scrolled-window))
           (text (make-instance 'gtk-text-buffer))
           (view (make-instance 'gtk-text-view
                                :monospace t
                                :buffer text))
           (provider (make-instance 'gtk-css-provider)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect text "changed"
          (lambda (buffer)
            (let ((start (gtk-text-buffer-start-iter buffer))
                  (end (gtk-text-buffer-end-iter buffer)))
              (gtk-text-buffer-remove-all-tags buffer start end)
              (gtk-css-provider-load-from-data
                                provider
                                (gtk-text-buffer-get-text buffer start end nil))
              (apply-css-to-widget provider window))))
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
            (read-file (sys-path "css-shadows.css")))
      ;; Add the widgets to the window
      (gtk-container-add paned toolbar)
      (gtk-container-add scrolled view)
      (gtk-container-add paned scrolled)
      (gtk-container-add window paned)
      ;; Apply the provider to the window
      (apply-css-to-widget provider window)
      ;; Show the window
      (gtk-widget-show-all window))))
