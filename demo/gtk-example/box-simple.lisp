;;;; Example Simple Box - 2021-11-19
;;;;
;;;; The example shows three buttons with colored labels. The red button
;;;; shows the start position in the box, the green button the end position,
;;;; and the yellow button is a center widget.
;;;;
;;;; In addition, this example demonstrate how to use CSS style information to
;;;; change the appearance of a widget.

(in-package :gtk-example)

(defparameter +css-button+
"button {
   padding: 3px; }
 button > label {
   color: black;
   background-color: yellow; }
 button:first-child > label {
   background-color: red; }
 button:last-child > label {
   background-color : green; }")

(defun example-box-simple (&optional application)
  (within-main-loop
    (let (;; Create a toplevel window
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :title "Example Simple Box"
                                 :border-width 12))
          ;; Create a box
          (box (make-instance 'gtk-box
                              :orientation :horizontal
                              :homogeneous nil
                              :spacing 0
                              :halign :fill
                              :valign :center
                              :width-request 480))
          (provider (make-instance 'gtk-css-provider)))

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Add Start button
      (let ((button (make-instance 'gtk-button
                                   :label "START")))
        (setf (gtk-widget-width-request (gtk-bin-child button)) 120)
        (setf (gtk-widget-height-request (gtk-bin-child button)) 120)
        (gtk-box-pack-start box button :expand nil))
      ;; Add Center button
      (let ((button (make-instance 'gtk-button
                                   :label "CENTER")))
        (setf (gtk-widget-width-request (gtk-bin-child button)) 80)
        (setf (gtk-box-center-widget box) button))
      ;; Add End button
      (let ((button (make-instance 'gtk-button
                                   :label "END")))
        (setf (gtk-widget-width-request (gtk-bin-child button)) 60)
        (gtk-box-pack-end box button :expand nil))

      ;; Add the box to the window.
      (gtk-container-add window box)
      ;; Load CSS from data into the provider
      (gtk-css-provider-load-from-data provider +css-button+)
      ;; Apply CSS to the widgets
      (apply-css-to-widget provider box)
      ;; Show the window.
      (gtk-widget-show-all window))))
