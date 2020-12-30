(in-package :gtk-example)

(defclass custom-window (gtk-window)
  ((label :initform (make-instance 'gtk-label :label "A label text")
          :reader custom-window-label)
   (button :initform (make-instance 'gtk-button :label "Click me!")
           :reader custom-window-button))
  (:metaclass gobject-class)
  (:default-initargs :title "Custom window"
                     :default-width 320
                     :default-height 240))

(defmethod initialize-instance :after
    ((window custom-window) &key &allow-other-keys)
  (g-signal-connect (custom-window-button window) "clicked"
                    (lambda (button)
                      (declare (ignore button))
                      (format t "Button clicked~%")))
  (let ((box (make-instance 'gtk-box :orientation :vertical)))
    (gtk-container-add window box)
    (gtk-box-pack-start box (custom-window-label window))
    (gtk-box-pack-start box (custom-window-button window) :expand nil)))

;; Now we can use the custom-window as a composite widget.

(defun example-custom-window ()
  (within-main-loop
    (let ((window (make-instance 'custom-window)))
      (gtk-widget-show-all window))))

;;; 2020-12-19
