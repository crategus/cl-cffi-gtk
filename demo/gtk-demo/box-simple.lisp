;;;; A demo for GtkBox
;;;;
;;;; This demo shows three buttons with colored labels. The green button
;;;; shows the start position in the box, the red button the end position,
;;;; and the yellow button is a center widget.
;;;;
;;;; In addition this demo demonstrate how to use CSS style information to
;;;; change the appearance of a widget.

(in-package :gtk-demo)

(defparameter +css-button+
"button label {
  color: black;
  background-color: yellow;
}

button:first-child label {
    background-color: red;
}

button:last-child label {
    background-color : green;
}")

(defun apply-css-to-widget (widget provider)
  (gtk-style-context-add-provider (gtk-widget-style-context widget)
                                  provider
                                  +gtk-style-provider-priority-user+)
  (when (g-type-is-a (g-type-from-instance widget) "GtkContainer")
    (gtk-container-forall widget
                          (lambda (widget)
                            (apply-css widget provider)))))

(defun demo-box-simple ()
  (within-main-loop
    (let (;; Create a toplevel window
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo GtkBox"
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
        (setf (gtk-widget-width-request (gtk-bin-child button)) 80)
        (setf (gtk-widget-height-request (gtk-bin-child button)) 80)
        (gtk-box-pack-start box button :expand nil))
      ;; Add Center button
      (let ((button (make-instance 'gtk-button
                                   :label "CENTER")))
        (setf (gtk-widget-width-request (gtk-bin-child button)) 80)
        (setf (gtk-box-center-widget box) button))
      ;; Add End button
      (let ((button (make-instance 'gtk-button
                                   :label "END")))
        (setf (gtk-widget-width-request (gtk-bin-child button)) 120)
        (setf (gtk-widget-height-request (gtk-bin-child button)) 120)
        (gtk-box-pack-end box button :expand nil))

      ;; Add the box to the window.
      (gtk-container-add window box)
      ;; Load CSS from data into the provider
      (gtk-css-provider-load-from-data provider +css-button+)
      ;; Apply CSS to the widgets
      (apply-css-to-widget box provider)
      ;; Show the window.
      (gtk-widget-show-all window))))

;;; 2020-9-28
