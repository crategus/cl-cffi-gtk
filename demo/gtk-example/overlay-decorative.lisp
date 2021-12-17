;;;; Example Decorative Overlay - 2021-12-7
;;;;
;;;; Another example of an overlay with some decorative and some interactive
;;;; controls.

(in-package :gtk-example)

(defun example-overlay-decorative (&optional (application nil))
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Example Overlay Decorative"
                                  :type :toplevel
                                  :application application
                                  :default-width 500
                                  :default-height 510))
           (overlay (make-instance 'gtk-overlay))
           (scrolled (make-instance 'gtk-scrolled-window
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :automatic))
           (view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-buffer view))
           (tag (gtk-text-buffer-create-tag buffer
                                            "top-margin"
                                            :pixels-above-lines 0))
           (adjustment (gtk-adjustment-new 0 0 200 1 1 0)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (g-signal-connect adjustment "value-changed"
          (lambda (adjustment)
            (let ((value (truncate (gtk-adjustment-value adjustment))))
              (setf (gtk-text-view-left-margin view) value)
              (setf (gtk-text-tag-pixels-above-lines tag) value))))
      (setf (gtk-text-buffer-text buffer) "Dear diary...")
      (let* ((start (gtk-text-buffer-start-iter buffer))
             (end (gtk-text-iter-copy start)))
        (gtk-text-iter-move end :by :word)
        (gtk-text-buffer-apply-tag buffer tag start end))
      (let ((image (gtk-image-new-from-file (sys-path "decor1.png"))))
        (gtk-overlay-add-overlay overlay image)
        (setf (gtk-overlay-child-pass-through overlay image) t)
        (setf (gtk-widget-halign image) :start)
        (setf (gtk-widget-valign image) :start))
      (let ((image (gtk-image-new-from-file (sys-path "decor2.png"))))
        (gtk-overlay-add-overlay overlay image)
        (setf (gtk-overlay-child-pass-through overlay image) t)
        (setf (gtk-widget-halign image) :end)
        (setf (gtk-widget-valign image) :end))
      (let ((scale (make-instance 'gtk-scale
                                  :orientation :horizontal
                                  :adjustment adjustment
                                  :draw-value nil
                                  :width-request 120
                                  :height-request -1
                                  :margin-start 20
                                  :margin-end 20
                                  :margin-bottom 20
                                  :halign :start
                                  :valign :end
                                  :tooltip-text "Margin")))
        (gtk-overlay-add-overlay overlay scale)
        (setf (gtk-adjustment-value adjustment) 100))
      (gtk-container-add scrolled view)
      (gtk-container-add overlay scrolled)
      (gtk-container-add window overlay)
      (gtk-widget-show-all window))))
