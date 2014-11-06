;;;; pixbufs
;;;;
;;;; A GdkPixbuf represents an image, normally in RGB or RGBA format.
;;;; Pixbufs are normally used to load files from disk and perform
;;;; image scaling.
;;;;
;;;; This demo is not all that educational, but looks cool. It was written
;;;; by Extreme Pixbuf Hacker Federico Mena Quintero and was translated to Lisp
;;;; by Crategus. It also shows off how to use GtkDrawingArea to do a
;;;  simple animation.
;;;;
;;;; Look at the Image demo for additional pixbuf usage examples.

(in-package #:gtk-demo)

(defvar *image-files*
        '("apple-red.png"
          "gnome-applets.png"
          "gnome-calendar.png"
          "gnome-foot.png"
          "gnome-gmush.png"
          "gnome-gimp.png"
          "gnome-gsame.png"
          "gnu-keys.png"))

(defvar *image-pixbufs* (make-array 8))

(let ((timeout-id 0)
      (cycle-len 60)
      (frame nil)
      (frame-num 0)
      (area (make-instance 'gtk-drawing-area))
      (background nil)
      (back-height nil)
      (back-width nil)
      (back-rect (make-gdk-rectangle))
      (src-rect (make-gdk-rectangle))
      (surface nil))

  (defun load-pixbufs ()
    (setf background (gdk-pixbuf-new-from-file "background.jpg"))
    (setf back-width (gdk-pixbuf-get-width background)
          back-height (gdk-pixbuf-get-height background))
    (setf back-rect (make-gdk-rectangle :x 0 :y 0
                                        :width back-width :height back-height))
    (loop for i from 0 and file-name in *image-files*
          do (setf (aref *image-pixbufs* i)
                   (gdk-pixbuf-new-from-file (rel-path file-name)))))

  (defun timeout ()
    (let* ((cr (cairo-create surface))
           (f (* 1.0d0 (/ (mod frame-num cycle-len) cycle-len)))
           (xmid (/ back-width 2.0d0))
           (ymid (/ back-height 2.0d0))
           (radius (/ (min xmid ymid) 2.0d0)))
      (gdk-pixbuf-copy-area background 0 0 back-width back-height frame 0 0)
      (dotimes (i 8)
        (let* ((ang (* 2.0d0 3.14d0 (- (/ i 8) f)))
               (iw (gdk-pixbuf-get-width (aref *image-pixbufs* i)))
               (ih (gdk-pixbuf-get-height (aref *image-pixbufs* i)))
               (r (+ radius (* (/ radius 3.0d0) (sin (* f 2.0d0 3.14)))))
               (xpos (floor (+ xmid (* r (cos ang)) (* -1 (/ iw 2.0d0)) 0.5)))
               (ypos (floor (+ ymid (* r (sin ang)) (* -1 (/ ih 2.0d0)) 0.5)))
               (k (max 0.25d0
                       (* 2.0 (expt (if (eql i (* 2 (truncate (/ i 2))))
                                        (sin (* f 2.0d0 3.14))
                                        (cos (* f 2.0d0 3.14)))
                                    2)))))
          (setf (gdk-rectangle-x src-rect) xpos
                (gdk-rectangle-y src-rect) ypos
                (gdk-rectangle-width src-rect) (floor (* k iw))
                (gdk-rectangle-height src-rect) (floor (* k ih)))
          (let ((dest-rect (gdk-rectangle-intersect src-rect back-rect)))
              (gdk-pixbuf-composite (aref *image-pixbufs* i)
                                    frame
                                    (gdk-rectangle-x dest-rect)
                                    (gdk-rectangle-y dest-rect)
                                    (gdk-rectangle-width dest-rect)
                                    (gdk-rectangle-height dest-rect)
                                    (coerce xpos 'double-float)
                                    (coerce ypos 'double-float)
                                    (coerce k 'double-float)
                                    (coerce k 'double-float)
                                    :nearest 255))))
    (gdk-cairo-set-source-pixbuf cr frame 0.0d0 0.0d0)
    (cairo-paint cr)
    (cairo-destroy cr)
    (gtk-widget-queue-draw-area area
                                0 0
                                (gdk-rectangle-width back-rect)
                                (gdk-rectangle-height back-rect))
    (incf frame-num 1)
    t))

  (defun demo-pixbufs ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Demo Pixbufs"
                                   :resizable nil
                                   :default-width 300)))
        (g-signal-connect window "destroy"
                                 (lambda (widget)
                                   (declare (ignore widget))
                                   (g-source-remove timeout-id)
                                   (setf timeout-id 0)
                                   (leave-gtk-main)))
        (setf frame-num 0)
        (g-signal-connect area "draw"
           (lambda (widget cr)
             (declare (ignore widget))
             (cairo-set-source-surface (pointer cr) surface 0.0d0 0.0d0)
             (cairo-paint (pointer cr))
             ;; We must destroy the Cairo Context
             (cairo-destroy (pointer cr))
             t))
        (g-signal-connect area "configure-event"
           (lambda (widget event)
             (declare (ignore event))
             (when surface
               (cairo-surface-destroy surface))
             (setq surface
                   (gdk-window-create-similar-surface
                                   (gtk-widget-window widget)
                                   :color
                                   (gtk-widget-get-allocated-width widget)
                                   (gtk-widget-get-allocated-height widget)))
             ;; Clear surface
             (let ((cr (cairo-create surface)))
               (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
               (cairo-paint cr)
               (cairo-destroy cr))
             t))

        ;; Load the background and the images.
        (load-pixbufs)

        (setf (gtk-widget-size-request window) (list back-width back-height))
        (setf frame (gdk-pixbuf-new :rgb nil 8 back-width back-height))

        (gtk-container-add window area)
        (setf timeout-id (g-timeout-add 100 #'timeout))

        (gtk-widget-show-all window)))))

