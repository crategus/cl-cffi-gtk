;;;; Text View Attributes (2021-6-4)

(in-package :gtk-example)

(defun example-text-view-attributes ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text View Attributes"
                                  :default-width 350
                                  :default-height 200))
           (provider (gtk-css-provider-new))
           (textview (make-instance 'gtk-text-view
                                    ;; Change left margin throughout the widget
                                    :left-margin 24
                                    ;; Change top margin
                                    :top-margin 12))
           (buffer (gtk-text-view-buffer textview)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (setf (gtk-text-buffer-text buffer) "Hello, this is some text.")
      ;; Change default font and color throughout the widget
      (gtk-css-provider-load-from-data provider
                                       "textview, text {
                                          color : Green;
                                          font : 20px Purisa; }")
      (gtk-style-context-add-provider (gtk-widget-style-context textview)
                                      provider
                                      +gtk-style-provider-priority-application+)
      ;; Use a tag to change the color for just one part of the widget
      (let ((tag (gtk-text-buffer-create-tag buffer
                                             "blue_foreground"
                                             :foreground "blue"))
            (start (gtk-text-buffer-iter-at-offset buffer 7))
            (end (gtk-text-buffer-iter-at-offset buffer 12)))
        ;; Apply the tag to a region of the text in the buffer
        (gtk-text-buffer-apply-tag buffer tag start end))
      ;; Add the text view to the window and show all
      (gtk-container-add window textview)
      (gtk-widget-show-all window))))
