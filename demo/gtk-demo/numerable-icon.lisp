;;;; This small demo shows how numerable icons are created.
;;;;
;;;; gtk-numerable-icon is a subclass of g-emblemed-icon that can show a number
;;;; or short string as an emblem. The number can be overlayed on top of
;;;; another emblem, if desired.
;;;;
;;;; In this demo only the properties GICON and COUNT are in use to number
;;;; the icons in the grid from 1 ... 25. The icon is a stock icon.
;;;;
;;;; Last update: 1-2-2014

(in-package #:gtk-demo)

(defun demo-numerable-icon ()
  (within-main-loop
    (let (;; Create a toplevel window
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Numerable Icons"
                                 :border-width 12))
          ;; Create a grid for the numerable icons
          (grid (make-instance 'gtk-grid
                               :row-spacing 12
                               :column-spacing 12)))
      ;; Signal handler for the window to handle the signal "destroy
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Fill a 5 x 5 grid with numerable icons
      (dotimes (i 5)
        (dotimes (j 5)
          (gtk-grid-attach grid
                           (gtk-image-new-from-gicon
                             (make-instance 'gtk-numerable-icon
                                            :gicon (g-themed-icon-new "gtk-ok")
                                            :count (+ (* j 5)(1+ i)))
                           :dialog)
                       i j 1 1)))
      ;; Show the window
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

