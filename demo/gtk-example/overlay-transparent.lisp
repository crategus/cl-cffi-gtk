;;;; Example Overlay Transparent - 2021-12-17
;;;;
;;;; Use transparent background on GdkWindows to create a shadow effect on a
;;;; GtkOverlay widget.
;;;;
;;;; TODO:
;;;;
;;;; This example does not seem to work as expected.

(in-package :gtk-example)

(defparameter +shadow-radius+ 5)
(defparameter +shadow-offset-x+ 14)
(defparameter +shadow-offset-y+ 7)

(defun draw-shadow-box (cr rect radius transparency)
  (let* ((x (gdk-rectangle-x rect))
         (y (gdk-rectangle-y rect))
         (width (gdk-rectangle-width rect))
         (height (gdk-rectangle-height rect))
         (x0 x)
         (x1 (+ x radius))
         (x2 (+ x width (- radius)))
         (x3 (+ x width))
         (y0 y)
         (y1 (+ y radius))
         (y2 (+ y height (- radius)))
         (y3 (+ y height)))
    ;; Fill non-border part
    (cairo-set-source-rgba cr 0.5 0.5 0.5 transparency)
    (cairo-rectangle cr x1 y1 (- x2 x1) (- y2 y1))
    (cairo-fill cr)
    ;; Upper border
    (let ((pattern (cairo-pattern-create-linear 0 y0 0 y1)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 0)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 transparency)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x1 y0 (- x2 x1) (- y1 y0))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
    ;; Bottom border
    (let ((pattern (cairo-pattern-create-linear 0 y2 0 y3)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 transparency)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 0)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x1 y2 (- x2 x1) (- y3 y2))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
   ;; Left border
    (let ((pattern (cairo-pattern-create-linear x0 0 x1 0)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 0)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 transparency)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x0 y1 (- x1 x0) (- y2 y1))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
    ;; Right border
    (let ((pattern (cairo-pattern-create-linear x2 0 x3 0)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 transparency)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 0)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x2 x1 (- x3 x2) (- y2 y1))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
    ;; NW corner
    (let ((pattern (cairo-pattern-create-radial x1 y1 0 x1 y1 radius)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 transparency)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 0)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x0 y0 (- x1 x0) (- y1 y0))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
    ;; NE corner
    (let ((pattern (cairo-pattern-create-radial x2 y1 0 x2 y1 radius)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 transparency)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 0)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x2 y0 (- x3 x2) (- y1 y0))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
   ;; SW corner
    (let ((pattern (cairo-pattern-create-radial x1 y2 0 x1 y2 radius)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 transparency)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 0)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x0 y2 (- x1 x0) (- y3 y2))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))
    ;;; SE corner
    (let ((pattern (cairo-pattern-create-radial x2 y2 0 x2 y2 radius)))
      (cairo-pattern-add-color-stop-rgba pattern 0 0 0 0 transparency)
      (cairo-pattern-add-color-stop-rgba pattern 1 0 0 0 0)
      (cairo-set-source cr pattern)
      (cairo-rectangle cr x2 y2 (- x3 x2) (- y3 y2))
      (cairo-fill cr)
      (cairo-pattern-destroy pattern))))

(defun example-overlay-transparent (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Overlay Transparent"
                                 :type :toplevel
                                 :application application
                                 :default-width 380
                                 :default-height 380))
          (view (make-instance 'gtk-text-view))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :hscroll-policy :automatic
                                   :vscroll-policy :automatic))
          (overlay (make-instance 'gtk-overlay))
          (entry (make-instance 'gtk-entry
                                :halign :center
                                :valign :center))
          (provider (make-instance 'gtk-css-provider))
          (css (format nil "* { border-width:0px ~apx ~apx 0px; }"
                           +shadow-offset-x+
                           +shadow-offset-y+)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (g-signal-connect entry "draw"
          (lambda (entry cr)
            (let ((cr (pointer cr))
                  (rect (gtk-widget-allocation entry)))
              (setf (gdk-rectangle-x rect)
                    (+ (gdk-rectangle-x rect) +shadow-offset-x+))
              (setf (gdk-rectangle-y rect)
                    (+ (gdk-rectangle-y rect) +shadow-offset-y+))
              (setf (gdk-rectangle-width rect)
                    (- (gdk-rectangle-width rect) +shadow-offset-x+))
              (setf (gdk-rectangle-height rect)
                    (- (gdk-rectangle-height rect) +shadow-offset-y+))
              (draw-shadow-box cr rect +shadow-radius+ 0.4))))
      (gtk-css-provider-load-from-data provider css)
      (gtk-style-context-add-provider (gtk-widget-style-context entry)
                                      provider
                                      +gtk-style-provider-priority-application+)
      (gtk-overlay-add-overlay overlay entry)
      (gtk-container-add scrolled view)
      (gtk-container-add overlay scrolled)
      (gtk-container-add window overlay)
      (gtk-widget-show-all window))))
