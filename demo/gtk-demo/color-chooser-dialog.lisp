;;;; Color Chooser Dialog
;;;;
;;;; Clicking on the drawing area opens a color chooser dialog to select a
;;;; background color for the drawing area. The default palettes are replaced
;;;; for this color chooser dialog.

(in-package #:gtk-demo)

(let ((message "Click to change the background color.")
      (bg-color (gdk-rgba-parse "White"))
      ;; Color palette with 9 Red RGBA colors
      (palette1 (list (gdk-rgba-parse "IndianRed")
                      (gdk-rgba-parse "LightCoral")
                      (gdk-rgba-parse "Salmon")
                      (gdk-rgba-parse "DarkSalmon")
                      (gdk-rgba-parse "LightSalmon")
                      (gdk-rgba-parse "Crimson")
                      (gdk-rgba-parse "Red")
                      (gdk-rgba-parse "FireBrick")
                      (gdk-rgba-parse "DarkRed")))
      ;; Gray palette with 9 gray RGBA colors
      (palette2 (list (gdk-rgba-parse "Gainsboro")
                      (gdk-rgba-parse "LightGray")
                      (gdk-rgba-parse "Silver")
                      (gdk-rgba-parse "DarkGray")
                      (gdk-rgba-parse "Gray")
                      (gdk-rgba-parse "DimGray")
                      (gdk-rgba-parse "LightSlateGray")
                      (gdk-rgba-parse "SlateGray")
                      (gdk-rgba-parse "DarkSlateGray"))))

  (defun example-color-chooser-dialog ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Color Chooser Dialog"
                                   :default-width 400))
            (area (make-instance 'gtk-drawing-area)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Draw the background color and a hint on the drawing area
        (g-signal-connect area "draw"
            (lambda (widget cr)
              (declare (ignore widget))
              (let ((cr (pointer cr))
                    (red (gdk-rgba-red bg-color))
                    (green (gdk-rgba-green bg-color))
                    (blue (gdk-rgba-blue bg-color)))
                    ;; Paint the current color on the drawing area
                    (cairo-set-source-rgb cr red green blue)
                    (cairo-paint cr)
                    ;; Print a hint on the drawing area
                    (cairo-set-source-rgb cr (- 1 red) (- 1 green) (- 1 blue))
                    (cairo-select-font-face cr "Sans")
                    (cairo-set-font-size cr 12)
                    (cairo-move-to cr 12 24)
                    (cairo-show-text cr message)
                    (cairo-destroy cr))))
        ;; Create and run a color chooser dialog to select a background color
        (g-signal-connect area "event"
            (lambda (widget event)
              (declare (ignore widget))
              (when (eq (gdk-event-type event) :button-press)
                (let ((dialog (make-instance 'gtk-color-chooser-dialog
                                             :use-alpha nil)))
                  ;; Add a custom palette to the dialog
                  (gtk-color-chooser-add-palette dialog :vertical 1 palette1)
                  ;; Add a second coustom palette to the dialog
                  (gtk-color-chooser-add-palette dialog :vertical 1 palette2)
                  ;; Set the actual background color for the color chooser
                  (setf (gtk-color-chooser-rgba dialog) bg-color)
                  ;; Run the color chooser dialog
                  (let ((response (gtk-dialog-run dialog)))
                    (when (eq response :ok)
                      ;; Change the background color for the drawing area
                      (setf bg-color (gtk-color-chooser-rgba dialog)))
                      ;; Destroy the color chooser dialog
                      (gtk-widget-destroy dialog))))))
        ;; Set the event mask for the drawing area
        (setf (gtk-widget-events area) :button-press-mask)
        ;; Add the drawing area to the window
        (gtk-container-add window area)
        (gtk-widget-show-all window)))))

;;; 2021-2-4
