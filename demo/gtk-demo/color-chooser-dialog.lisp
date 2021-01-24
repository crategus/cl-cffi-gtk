;;;; Color Chooser Dialog

(in-package #:gtk-demo)

(let ((message "Click to change the background color.")
      (bg-color (gdk-rgba-parse "White"))
      ;; Color palette with 4 rgba colors
      (palette1 (list (gdk-rgba-parse "Red")
                      (gdk-rgba-parse "Yellow")
                      (gdk-rgba-parse "Blue")
                      (gdk-rgba-parse "Green")))
      ;; Gray palette with 3 rgba grays
      (palette2 (list (gdk-rgba-parse "White")
                      (gdk-rgba-parse "Gray")
                      (gdk-rgba-parse "Black"))))

  (defun drawing-area-event (widget event)
    (declare (ignore widget))
    (when (eq (gdk-event-type event) :button-press)
      (let ((dialog (make-instance 'gtk-color-chooser-dialog
                                   :color bg-color
                                   :use-alpha nil)))
        ;; Add a custom palette to the dialog
        (gtk-color-chooser-add-palette dialog :vertical 1 palette1)
        ;; Add a second coustom palette to the dialog
        (gtk-color-chooser-add-palette dialog :vertical 1 palette2)
        ;; Run the color chooser dialog
        (let ((response (gtk-dialog-run dialog)))
          (when (eq response :ok)
            ;; Change the background color for the drawing area
            (setf bg-color (gtk-color-chooser-rgba dialog)))
          ;; Destroy the color chooser dialog
          (gtk-widget-destroy dialog)))))

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
        (g-signal-connect area "draw"
                          (lambda (widget cr)
                            (declare (ignore widget))
                            (let ((cr (pointer cr)))
                              ;; Paint the current color on the drawing area
                              (cairo-set-source-rgb cr
                                                    (gdk-rgba-red bg-color)
                                                    (gdk-rgba-green bg-color)
                                                    (gdk-rgba-blue bg-color))
                              (cairo-paint cr)
                              ;; Print a hint on the drawing area
                              (cairo-set-source-rgb cr 0.1 0.1 0.1)
                              (cairo-select-font-face cr "Sans"
                                                         :normal :normal)
                              (cairo-set-font-size cr 12)
                              (cairo-move-to cr 12 24)
                              (cairo-show-text cr message)
                              (cairo-destroy cr))))
        (g-signal-connect area "event"
                          (lambda (widget event)
                            (drawing-area-event widget event)))
        ;; Set the event mask for the drawing area
        (setf (gtk-widget-events area) :button-press-mask)
        ;; Add the drawing area to the window
        (gtk-container-add window area)
        (gtk-widget-show-all window)))))

;;; 2021-1-23
