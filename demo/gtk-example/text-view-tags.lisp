;;;; Text View Tags (2021-6-4)

(in-package :gtk-example)

(defun on-toggle-tool-button-clicked (button buffer tag)
  (when (gtk-text-buffer-has-selection buffer)
    (multiple-value-bind (start end)
        (gtk-text-buffer-selection-bounds buffer)
      (if (gtk-toggle-tool-button-active button)
          (gtk-text-buffer-apply-tag buffer tag start end)
          (gtk-text-buffer-remove-tag buffer tag start end)))))

(defun example-text-view-tags ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Example Text View Tags"
                                  :type :toplevel
                                  :default-width 350
                                  :default-height 250))
           (vbox (make-instance 'gtk-box
                                :orientation :vertical))
           (textview (make-instance 'gtk-text-view
                                    :top-margin 6
                                    :left-margin 6
                                    :right-margin 6))
           (buffer (gtk-text-view-buffer textview))
           (toolbar (make-instance 'gtk-toolbar)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signal handler for cursor movements in the text buffer
      (g-signal-connect buffer "notify::cursor-position"
          (lambda (object pspec)
            (declare (ignore pspec))
            (let* ((cursor (gtk-text-buffer-cursor-position object))
                   (iter (gtk-text-buffer-iter-at-offset buffer cursor))
                   (tags (mapcar #'gtk-text-tag-name
                                 (gtk-text-iter-tags iter))))
              ;; Iterate over the toggle tool buttons
              (dotimes (item (gtk-toolbar-n-items toolbar))
                (let* ((button (gtk-toolbar-nth-item toolbar item))
                       (label (gtk-tool-button-label button)))
                  ;; Activate/Deactivate the buttons
                  (if (member label tags :test #'string=)
                      (setf (gtk-toggle-tool-button-active button) t)
                      (setf (gtk-toggle-tool-button-active button) nil)))))))
      ;; Create toggle tool button for Bold
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-text-bold"
                                   :label "Bold")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-tool-button-clicked widget buffer "Bold")))
        (gtk-container-add toolbar button))
      ;; Create toogle tool button for Italic
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-text-italic"
                                   :label "Italic")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-tool-button-clicked widget buffer "Italic")))
        (gtk-container-add toolbar button))
      ;; Create toggle tool button for Underline
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-text-underline"
                                   :label "Underline")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-tool-button-clicked widget buffer "Underline")))
        (gtk-container-add toolbar button))
      ;; Create toggle tool button for Strikethrough
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-text-strikethrough"
                                   :label "Strikethrough")))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (on-toggle-tool-button-clicked widget buffer "Strikethrough")))
        (gtk-container-add toolbar button))
      ;; Create tags associated with the text buffer
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "Bold"
                                             :weight 700))
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "Italic"
                                             :style :italic))
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "Underline"
                                             :underline :single))
      (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer)
                              (make-instance 'gtk-text-tag
                                             :name "Strikethrough"
                                             :strikethrough t))
      ;; Pack and show the widgets
      (gtk-box-pack-start vbox toolbar :expand nil)
      (gtk-box-pack-start vbox textview :expand t)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
