;;;; Example Action Bar - 2021-12-8

(in-package :gtk-example)

(defun example-action-bar (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Action Bar"
                                 :type :toplevel
                                 :application application
                                 :default-width 320
                                 :default-height 180))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical))
          (actionbar (make-instance 'gtk-action-bar)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (let* ((button (make-instance 'gtk-button))
             (icon (g-themed-icon-new "media-skip-backward"))
             (image (gtk-image-new-from-gicon icon :button)))
        (gtk-container-add button image)
        (gtk-action-bar-pack-start actionbar button))
      (let* ((button (make-instance 'gtk-button))
             (icon (g-themed-icon-new "media-playback-start"))
             (image (gtk-image-new-from-gicon icon :button)))
        (gtk-container-add button image)
        (gtk-action-bar-pack-start actionbar button))
      (let* ((button (make-instance 'gtk-button))
             (icon (g-themed-icon-new "media-skip-forward"))
             (image (gtk-image-new-from-gicon icon :button)))
        (gtk-container-add button image)
        (gtk-action-bar-pack-start actionbar button))
      (gtk-box-pack-start vbox (make-instance 'gtk-text-view))
      (gtk-box-pack-start vbox actionbar :expand nil)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
