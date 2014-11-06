;;;; Text View Attributes

(in-package #:gtk-demo)

(defun example-text-view-attributes ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Text View Attributes"
                                  :default-width 350))
           (view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-get-buffer view)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-text-buffer-set-text buffer "Hello, this is some text.")
      ;; Change default font throughout the widget
      (gtk-widget-override-font
                             view
                             (pango-font-description-from-string "Serif 20"))
      ;; Change default color throughout the widget
      (gtk-widget-override-color view
                                 :normal
                                 (gdk-rgba-parse "red"))
      ;; Change left margin throughout the widget
      (gtk-text-view-set-left-margin view 30)
      ;; Use a tag to change the color for just one part of the widget
      (let ((tag (make-instance 'gtk-text-tag
                                :name "blue_foreground"
                                :foreground "blue"))
            (start (gtk-text-buffer-get-iter-at-offset buffer 7))
            (end (gtk-text-buffer-get-iter-at-offset buffer 12)))
        ;; Add the tag to the tag table of the buffer
        (gtk-text-tag-table-add (gtk-text-buffer-get-tag-table buffer) tag)
        ;; Apply the tag to a region of the text in the buffer
        (gtk-text-buffer-apply-tag buffer tag start end))
      ;; Add the view to the window and show all
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

