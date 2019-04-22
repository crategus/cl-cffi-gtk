;;;; Color Chooser Dialog

(in-package #:gtk-demo)

(let ((color (gdk-rgba-parse "Blue"))
      ;; Color palette with 4 rgba colors
      (palette1 (list (gdk-rgba-parse "Red")
                      (gdk-rgba-parse "Yellow")
                      (gdk-rgba-parse "Blue")
                      (gdk-rgba-parse "Green")))
      ;; Gray palette with 3 rgba grays
      (palette2 (list (gdk-rgba-parse "White")
                      (gdk-rgba-parse "Gray")
                      (gdk-rgba-parse "Black"))))
  (defun drawing-area-event (widget event area)
    (declare (ignore widget))
    (let ((handled nil))
      (when (eql (gdk-event-type event) :button-press)
        (let ((dialog (make-instance 'gtk-color-chooser-dialog
                                      :color color
                                      :use-alpha nil)))
          (setq handled t)
          ;; Add a custom palette to the dialog
          (gtk-color-chooser-add-palette dialog :vertical 1 palette1)
          ;; Add a second coustom palette to the dialog
          (gtk-color-chooser-add-palette dialog :vertical 1 palette2)
          ;; Run the color chooser dialog
          (let ((response (gtk-dialog-run dialog)))
            (when (eql response :ok)
              (setq color (gtk-color-chooser-rgba dialog)))
            ;; Set the color of the area widget
            (gtk-widget-override-background-color area :normal color)
            ;; Destroy the color chooser dialog
            (gtk-widget-destroy dialog))))
      handled))

  (defun example-color-chooser-dialog ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Color Chooser Dialog"
                                   :default-width 300))
            (area (make-instance 'gtk-drawing-area)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (gtk-widget-override-background-color area :normal color)
        (setf (gtk-widget-events area) :button-press-mask)
        (g-signal-connect area "event"
                          (lambda (widget event)
                            (drawing-area-event widget event area)))
        (gtk-container-add window area)
        (gtk-widget-show-all window)))))

