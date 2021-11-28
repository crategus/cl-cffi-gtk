;;;; pixbufs - 2021-11-11
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

;; TODO: The center of the rotation is no longer at the (200,200) point? Why?

(in-package #:gtk-example)

(defvar *pixbufs-files*
        '("apple-red.png"
          "gnome-applets.png"
          "gnome-calendar.png"
          "gnome-foot.png"
          "gnome-gmush.png"
          "gnome-gimp.png"
          "gnome-gsame.png"
          "gnu-keys.png"
          "background.jpg"))

(defvar *pixbufs-image* (make-array 9))
(defvar *pixbufs-cycle* 60)
(defvar *pixbufs-frame* 0)

(defun pixbufs-timeout (surface area frame)
  (let* ((cr (cairo-create surface))
         (f (* 1.0d0 (/ (mod *pixbufs-frame* *pixbufs-cycle*) *pixbufs-cycle*)))
         (background (aref *pixbufs-image* 8))
         (width (gdk-pixbuf-width background))
         (height (gdk-pixbuf-height background))
         (rect (gdk-rectangle-new :x 0 :y 0 :width width :height height))
         (xmid (/ width 2.0d0))
         (ymid (/ height 2.0d0))
         (radius (/ (min xmid ymid) 2.0d0)))

         (format t "~%")
         (format t " width : ~a~%" width)
         (format t "height : ~a~%" height)
         (format t "  xmid : ~a~%" xmid)
         (format t "  ymid : ~a~%" ymid)
         (format t "radius : ~a~%" radius)

    (gdk-pixbuf-copy-area background 0 0 width height frame 0 0)
    (dotimes (i 8)
      (let* ((ang (* 2.0d0 3.14d0 (- (/ i 8.0d0) f)))
             (iw (gdk-pixbuf-width (aref *pixbufs-image* i)))
             (ih (gdk-pixbuf-height (aref *pixbufs-image* i)))
             (r (+ radius (* (/ radius 3.0d0) (sin (* f 2.0d0 3.14d0)))))
             (xpos (floor (+ xmid (* r (cos ang))
                                  (* -1.0d0 (/ iw 2.0d0))
                                  0.5d0)))
             (ypos (floor (+ ymid (* r (sin ang))
                                  (* -1.0d0 (/ ih 2.0d0))
                                  0.5d0)))
             (k (max 0.25d0
                     (* 2.0d0 (expt (if (= i (* 2 (truncate (/ i 2))))
                                        (sin (* f 2.0d0 3.14d0))
                                        (cos (* f 2.0d0 3.14d0)))
                                  2.0d0))))
             (src (gdk-rectangle-new :x xpos
                                     :y ypos
                                     :width (floor (* k iw))
                                     :height (floor (* k ih))))
            (dest (gdk-rectangle-intersect src rect)))
        (gdk-pixbuf-composite (aref *pixbufs-image* i)
                              frame
                              (gdk-rectangle-x dest)
                              (gdk-rectangle-y dest)
                              (gdk-rectangle-width dest)
                              (gdk-rectangle-height dest)
                              xpos
                              ypos
                              k
                              k
                              :bilinear
                              255)))
    (gdk-cairo-set-source-pixbuf cr frame 0.0d0 0.0d0)
    (cairo-paint cr)
    (cairo-destroy cr)
    (gtk-widget-queue-draw-area area
                                0 0
                                (gdk-rectangle-width rect)
                                (gdk-rectangle-height rect))
    (incf *pixbufs-frame* 1)
    t))

(defun example-pixbufs (&optional application)
  (within-main-loop
    (let* ((background (gdk-pixbuf-new-from-file (sys-path "background.jpg")))
           (width (gdk-pixbuf-width background))
           (height (gdk-pixbuf-height background))
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :application application
                                  :title "Example Pixbufs"
                                  :resizable nil
                                  :width-request width
                                  :height-request height))
           (area (make-instance 'gtk-drawing-area))
           (frame (gdk-pixbuf-new :rgb nil 8 width height))
           (surface nil)
           (timeout-id 0))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (when (not (= 0 timeout-id))
                                   (g-source-remove timeout-id)
                                   (setf timeout-id 0))
                                 (leave-gtk-main)))
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
                                  (gtk-widget-allocated-width widget)
                                  (gtk-widget-allocated-height widget)))
            ;; Clear surface
            (let ((cr (cairo-create surface)))
              (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
              (cairo-paint cr)
              (cairo-destroy cr))
            t))
      ;; Load the images
      (loop for i from 0
            for filename in *pixbufs-files*
            do (setf (aref *pixbufs-image* i)
                     (gdk-pixbuf-new-from-file (sys-path filename))))
      ;; Start the timer for the animation
      (setf timeout-id
            (g-timeout-add 100
                           (lambda ()
                             (pixbufs-timeout surface area frame))))
      ;; Pack and show the widgets
      (gtk-container-add window area)
      (gtk-widget-show-all window))))
