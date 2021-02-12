;;;; Text View Attributes

(in-package #:gtk-demo)

(defun example-text-view-attributes ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text View Attributes"
                                  :default-width 350))
           (provider (gtk-css-provider-new))
           (view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-buffer view)))
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
      (gtk-style-context-add-provider (gtk-widget-style-context view)
                                      provider
                                      +gtk-style-provider-priority-application+)
      ;; Change left margin throughout the widget
      (setf (gtk-text-view-left-margin view) 30)
      ;; Use a tag to change the color for just one part of the widget
      (let ((tag (gtk-text-buffer-create-tag buffer
                                             "blue_foreground"
                                             :foreground "blue"))
            (start (gtk-text-buffer-iter-at-offset buffer 7))
            (end (gtk-text-buffer-iter-at-offset buffer 12)))
        ;; Apply the tag to a region of the text in the buffer
        (gtk-text-buffer-apply-tag buffer tag start end))
      ;; Add the view to the window and show all
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

;;; 2021-2-12
