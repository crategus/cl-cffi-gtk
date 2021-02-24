;;;; Color Button Label
;;;;
;;;; The example shows a color button. The button is initialized with the color
;;;; "Black". The handler for the "color-set" signal changes the color of the
;;;; "Color button" label. To change the color of the label the CSS color is
;;;; loaded in a CSS provider and the style context of the label is updated.

(in-package :gtk-tutorial)

(defun example-color-button-label ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Color Button"
                                 :border-width 12
                                 :default-width 300
                                 :default-height 200))
          (provider (gtk-css-provider-new))
          (box (make-instance 'gtk-box
                              :orientation :horizontal))
          (label (make-instance 'gtk-label
                                :label "<big><b>Color button</b></big>"
                                :use-markup t))
          (button (make-instance 'gtk-color-button
                                 :rgba (gdk-rgba-parse "Black"))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Handler for the "color-set" signal
      (g-signal-connect button "color-set"
         (lambda (widget)
           (let* ((rgba (gtk-color-chooser-rgba widget))
                  (css-label (format nil "label { color : ~a }"
                                         (gdk-rgba-to-string rgba)))
                  (context (gtk-widget-style-context label)))
             ;; Update the color of the label
             (gtk-css-provider-load-from-data provider css-label)
             (gtk-style-context-add-provider context
                                             provider
                                             +gtk-style-provider-priority-user+))))
      ;; Pack and show the widgets
      (gtk-box-pack-start box button)
      (gtk-box-pack-start box label)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
