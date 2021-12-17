;;;; Example Interactive Overlay - 2021-12-17
;;;;
;;;; Shows widgets in static positions over a main widget. The overlayed widgets
;;;; can be interactive controls such as the entry in this example, or just
;;;; decorative, like the big blue label.

(in-package :gtk-example)

(defun example-overlay-interactive (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Interactive Overlay"
                                 :type :toplevel
                                 :application application
                                 :default-width 500
                                 :default-height 510))
          (overlay (make-instance 'gtk-overlay))
          (grid (make-instance 'gtk-grid))
          (entry (make-instance 'gtk-entry))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :halign :center
                               :valign :center)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (dotimes (j 5)
        (dotimes (i 5)
          (let ((button (make-instance 'gtk-button
                                       :label
                                       (format nil "~a" (+ 1 i (* 5 j)))
                                       :hexpand t
                                       :vexpand t)))
            (g-signal-connect button "clicked"
                 (lambda (button)
                   (setf (gtk-entry-text entry)
                         (gtk-button-label button))))
            (gtk-grid-attach grid button i j 1 1))))
      (let ((label (make-instance 'gtk-label
                                  :use-markup t
                                  :margin-bottom 18
                                  :label
                                  "<span foreground='orange'
                                         weight='ultrabold'
                                         font='32'>Choose a Number</span>")))
        (gtk-box-pack-start vbox label :expand nil))
      (gtk-box-pack-start vbox entry :expand nil)
      (gtk-overlay-add-overlay overlay vbox)
      (setf (gtk-overlay-child-pass-through overlay vbox) t)
      (gtk-container-add overlay grid)
      (gtk-container-add window overlay)
      (gtk-widget-show-all window))))
