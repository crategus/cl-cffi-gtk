(in-package :pango-example)

(defparameter *example-text*
"Ägypten
Ägypten")

(defun drawing-text-metrics (widget cr)
  (let* ((cr (pointer cr))
         (layout (pango-cairo-create-layout cr))
         (font nil)
         (font-size (truncate (/ (gtk-widget-allocated-height widget) 6)))
         (factor 1.5)
         (line-spacing 0)
         (metrics nil)
         (ascent 0)
         (descent 0)
         (height 0)
         (text-x 0)
         (text-y 0)
         (baseline 0)
         (indent (truncate (/ font-size 3)))
         (indent2 (truncate (/ indent 3))))
    ;; Set the font
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string "Verdana Bold"))
    ;; Set the font size in Pango units
    (setf (pango-font-description-size (pango-layout-font-description layout))
          (* font-size +pango-scale+))
    ;; Set the line spacing
    (setf (pango-layout-line-spacing layout) factor)
    ;; Set the text
    (setf (pango-layout-text layout) *example-text*)

    ;; Get the font
    (setf font
          (pango-font-map-load-font (pango-cairo-font-map-default)
                                    (pango-layout-context layout)
                                    (pango-layout-font-description layout)))
    ;; Get the font metrics
    (setf metrics (pango-font-metrics font (pango-language-default)))
    (setf ascent (pango-pixels (pango-font-metrics-ascent metrics)))
    (setf descent (pango-pixels (pango-font-metrics-descent metrics)))
    (setf height (pango-pixels (pango-font-metrics-height metrics)))

    ;; Print information about the font metrics
    (format t "~%~%")
    (format t "        type : ~a~%"
              (pango-cairo-font-map-font-type (pango-cairo-font-map-default)))
    (format t "  resolution : ~a~%"
              (pango-cairo-font-map-resolution (pango-cairo-font-map-default)))
    (format t "        size : ~a~%" font-size)
    (format t "      ascent : ~a~%"
              (pango-pixels (pango-font-metrics-ascent metrics)))
    (format t "     descent : ~a~%"
              (pango-pixels (pango-font-metrics-descent metrics)))
    (format t "      height : ~a~%"
              (pango-pixels (pango-font-metrics-height metrics)))

    ;; Set the baseline and line spacing
    (setf baseline (pango-pixels (pango-layout-baseline layout)))
    (setf line-spacing (* factor height))

    ;; Extents of the Pango layout in pixel units
    (multiple-value-bind (text-width text-height)
        (pango-layout-pixel-size layout)

      ;; Start point for the text
      (setf text-x  (- (/ (gtk-widget-allocated-width widget) 2)
                       (/ text-width 2)))
      (setf text-y (- (/ (gtk-widget-allocated-height widget) 2)
                      (/ text-height 2)))

      ;; Print information about the layout
      (format t "    baseline : ~a~%"
                (pango-pixels (pango-layout-baseline layout)))
      (format t "    spacing  : ~a~%"
                (pango-pixels (pango-layout-spacing layout)))
      (format t "line-spacing : ~a~%" (pango-layout-line-spacing layout))

      ;; Set source and line-width
      (cairo-set-source-rgb cr 1.0 0.0 0.0)
      (cairo-set-line-width cr 0.5)

      ;; Draw first baseline
      (cairo-move-to cr (- text-x indent)
                        (+ text-y baseline))
      (cairo-line-to cr (+ text-x text-width indent)
                        (+ text-y baseline))
      (cairo-stroke cr)

      ;; Draw first ascent line
      (cairo-move-to cr (- text-x indent)
                        (+ text-y baseline (- ascent)))
      (cairo-line-to cr (+ text-x text-width indent)
                        (+ text-y baseline (- ascent)))
      (cairo-stroke cr)

      ;; Draw first descent line
      (cairo-move-to cr (- text-x indent)
                        (+ text-y baseline descent))
      (cairo-line-to cr (+ text-x text-width indent)
                        (+ text-y baseline descent))
      (cairo-stroke cr)

      ;; Draw second baseline
      (cairo-move-to cr (- text-x indent)
                        (+ text-y baseline line-spacing))
      (cairo-line-to cr (+ text-x text-width indent)
                        (+ text-y baseline line-spacing))
      (cairo-stroke cr)

      ;; Draw second ascent line
      (cairo-move-to cr (- text-x indent)
                        (+ text-y baseline line-spacing (- ascent)))
      (cairo-line-to cr (+ text-x text-width indent)
                        (+ text-y baseline line-spacing (- ascent)))
      (cairo-stroke cr)

      ;; Second descent line
      (cairo-move-to cr (- text-x indent)
                        (+ text-y baseline line-spacing descent))
      (cairo-line-to cr (+ text-x text-width indent)
                        (+ text-y baseline line-spacing descent))
      (cairo-stroke cr)

      ;; Vertical left line
      (cairo-move-to cr (+ text-x (- indent) indent2)
                        (+ text-y baseline (- indent2)))
      (cairo-line-to cr (+ text-x (- indent) indent2)
                        (+ text-y baseline line-spacing indent2))
      (cairo-stroke cr)
      ;; Circles at crossings on the left line
      (cairo-arc cr
                 (+ text-x (- indent) indent2)
                 (+ text-y baseline)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo-fill cr)
      (cairo-arc cr
                 (+ text-x (- indent) indent2)
                 (+ text-y baseline line-spacing)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo-fill cr)

      ;; Vertical right line
      (cairo-move-to cr (+ text-x text-width indent (- indent2))
                        (- (+ baseline text-y) ascent indent2))
      (cairo-line-to cr (+ text-x text-width indent (- indent2))
                        (+ text-y line-spacing indent2))
      (cairo-stroke cr)
      ;; Circles at crosssings on the right line
      (cairo-arc cr
                 (+ text-x text-width indent (- indent2))
                 (+ baseline text-y (- ascent))
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo-fill cr)
      (cairo-arc cr
                 (+ text-x text-width indent (- indent2))
                 (+ baseline text-y)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo-fill cr)
      (cairo-arc cr
                 (+ text-x text-width indent (- indent2))
                 (+ baseline text-y descent)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo-fill cr)
      (cairo-arc cr
                 (+ text-x text-width indent (- indent2))
                 (+ text-y line-spacing)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo-fill cr)

      ;; Set the color
      (cairo-set-source-rgb cr 0.5 0.5 0.5)
      ;; Move to the start position of the text
      (cairo-move-to cr text-x text-y)
      ;; Print the example text on the Cario context
      (pango-cairo-show-layout cr layout)

      ;; Label Ascent
      (setf (pango-layout-font-description layout)
            (pango-font-description-from-string "Courier Normal"))
      ;; Set the font size in Pango units
      (setf (pango-font-description-size (pango-layout-font-description layout))
            (max (* 10 +pango-scale+)
                 (* (truncate (/ font-size 10)) +pango-scale+)))
      (cairo-set-source-rgb cr 0.0 0.0 0.0)
      (setf (pango-layout-text layout) "Ascent")
      (pango-cairo-update-layout cr layout)

      (cairo-move-to cr (+ text-x text-width (* 1.5 indent))
                        (+ text-y baseline (- (/ ascent 2))))
      (pango-cairo-show-layout cr layout)

      ;; Label Descent
      (setf (pango-layout-text layout) "Descent")
      (pango-cairo-update-layout cr layout)
      (cairo-move-to cr (+ text-x text-width (* 1.5 indent))
                        (+ text-y baseline (/ descent 2)))
      (pango-cairo-show-layout cr layout)

      ;; Label Spacing
      (setf (pango-layout-text layout) "Spacing")
      (pango-cairo-update-layout cr layout)
      (cairo-move-to cr (+ text-x text-width (* 1.5 indent))
                        (+ text-y baseline descent
                                  (/ (- line-spacing ascent descent) 2)))
      (pango-cairo-show-layout cr layout)

      ;; Label Line Spacing
      (setf (pango-layout-text layout) "Line Spacing")
      (pango-cairo-update-layout cr layout)
      (cairo-move-to cr (- text-x indent
                                  indent2 (pango-layout-pixel-size layout))
                        (+ text-y baseline (/ line-spacing 2)))
      (pango-cairo-show-layout cr layout)
      ;; Destroy the Cairo context
      (cairo-destroy cr))))

(defun example-text-metrics ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Pango Text Metrics"
                                 :default-width 650
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-text-metrics)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2021-1-19
