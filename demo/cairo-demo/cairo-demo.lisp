(defpackage :cairo-demo
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :common-lisp)
  (:export #:demo-cairo-stroke #:demo-cairo-fill #:demo-cairo-text
           #:demo-cairo-paint #:demo-cairo-mask #:demo-cairo-set-source-rgba
           #:demo-cairo-set-source-gradient
           #:demo-cairo-path #:demo-cairo-dash))

(in-package :cairo-demo)

(defun demo-cairo-stroke ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Stroke"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (cairo-set-line-width cr 0.1)
           (cairo-set-source-rgb cr 1.0 0.0 0.0)
           (cairo-rectangle cr 0.25 0.25 0.5 0.5)
           (cairo-stroke cr)
           ;; Destroy the Cario context
           (cairo-destroy cr)
           t)))
      (gtk-widget-show-all window))))

(defun demo-cairo-fill ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Fill"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (cairo-set-source-rgb cr 1.0 0.0 0.0)
           (cairo-rectangle cr 0.25 0.25 0.5 0.5)
           (cairo-fill cr)
           ;; Destroy the Cairo context
           (cairo-destroy cr)
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-text ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Text"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (cairo-set-source-rgb cr 0.0 0.0 0.0)
           (cairo-select-font-face cr "Georgia" :normal :bold)
           (cairo-set-font-size cr 1.2)
           (let ((text-extents (cairo-text-extents cr "a")))
             (cairo-move-to cr
                            (- 0.5
                               (/ (cairo-text-extents-t-width text-extents) 2)
                               (cairo-text-extents-t-x-bearing text-extents))
                            (- 0.5
                               (/ (cairo-text-extents-t-height text-extents) 2)
                               (cairo-text-extents-t-y-bearing text-extents)))
             (cairo-show-text cr "a"))
           ;; Destroy the Cairo context
           (cairo-destroy cr)
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-paint ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Paint"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (cairo-set-source-rgb cr 0.0 0.0 0.0)
           (cairo-paint-with-alpha cr 0.5d0)
           ;; Destroy the Cairo context
           (cairo-destroy cr)
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-mask ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Mask"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (let ((linpat (cairo-pattern-create-linear 0 0 1 1))
                 (radpat (cairo-pattern-create-radial 0.5 0.5 0.25
                                                      0.5 0.5 0.75)))
             (cairo-pattern-add-color-stop-rgb linpat 0 0 0.3 0.8)
             (cairo-pattern-add-color-stop-rgb linpat 1 0 0.8 0.3)
             (cairo-pattern-add-color-stop-rgba radpat 0 0 0 0 1)
             (cairo-pattern-add-color-stop-rgba radpat 0.5 0 0 0 0)
             (cairo-set-source cr linpat)
             (cairo-mask cr radpat))
           (cairo-destroy cr)
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-set-source-rgba ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Set Source RGBA"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (cairo-set-source-rgb cr 0 0 0)
           (cairo-move-to cr 0 0)
           (cairo-line-to cr 1 1)
           (cairo-move-to cr 1 0)
           (cairo-line-to cr 0 1)
           (cairo-set-line-width cr 0.2)
           (cairo-stroke cr)
           (cairo-rectangle cr 0 0 0.5 0.5)
           (cairo-set-source-rgba cr 1 0 0 0.80)
           (cairo-fill cr)
           (cairo-rectangle cr 0 0.5 0.5 0.5)
           (cairo-set-source-rgba cr 0 1 0 0.60)
           (cairo-fill cr)
           (cairo-rectangle cr 0.5 0 0.5 0.5)
           (cairo-set-source-rgba cr 0 0 1 0.40)
           (cairo-fill cr)
           ;; Destroy the Cairo context
           (cairo-destroy cr)
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-set-source-gradient ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Set Source Gradient"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (let ((radpat (cairo-pattern-create-radial 0.25 0.25 0.10
                                                      0.50 0.50 0.50))
                 (linpat (cairo-pattern-create-linear 0.25 0.35 0.75 0.65)))
             (cairo-pattern-add-color-stop-rgb radpat 0.00 1.00 0.80 0.80)
             (cairo-pattern-add-color-stop-rgb radpat 1.00 0.90 0.00 0.00)
             (iter (for i from 1 below 10)
                   (iter (for j from 1 below 10)
                         (cairo-rectangle cr
                                          (- (/ i 10.0) 0.04)
                                          (- (/ j 10.0) 0.04)
                                          0.08
                                          0.08)))
             (cairo-set-source cr radpat)
             (cairo-fill cr)
             (cairo-pattern-add-color-stop-rgba linpat 0.00 1.0 1.0 1.0 0.0)
             (cairo-pattern-add-color-stop-rgba linpat 0.25 0.0 1.0 0.0 0.5)
             (cairo-pattern-add-color-stop-rgba linpat 0.50 1.0 1.0 1.0 0.0)
             (cairo-pattern-add-color-stop-rgba linpat 0.75 0.0 0.0 1.0 0.5)
             (cairo-pattern-add-color-stop-rgba linpat 1.00 1.0 1.0 1.0 0.0)
             (cairo-rectangle cr 0.0 0.0 1.0 1.0)
             (cairo-set-source cr linpat)
             (cairo-fill cr))
           ;; Destroy the Cairo context
           (cairo-destroy cr)
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-path ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Path"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let (;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (cairo-set-line-width (pointer cr) 0.01d0)
           (cairo-set-source-rgb (pointer cr) 1.0d0 0.0d0 0.0d0)
           (cairo-move-to (pointer cr) 0.25 0.25)
           (cairo-line-to (pointer cr) 0.5 0.375)
           (cairo-rel-line-to (pointer cr) 0.25 -0.125)
           (cairo-arc (pointer cr)
                      0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
           (cairo-rel-curve-to (pointer cr) -0.25 -0.125 -0.25 0.125 -0.5 0)
           (cairo-close-path (pointer cr))
           (cairo-stroke (pointer cr))
           ;; Destroy the Cairo context
           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

;;; --- Cairo Examples ---------------------------------------------------------

(defun demo-cairo-dash ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo Cairo Stroke"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect window "draw"
         (lambda (widget cr)
           (let ((cr (pointer cr))
                 ;; Get the GdkWindow for the widget
                 (window (gtk-widget-window widget)))
           ;; Clear surface
           (cairo-set-source-rgb cr 1.0 1.0 1.0)
           (cairo-paint cr)
           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale cr
                        (gdk-window-width window)
                        (gdk-window-height window))
           ;; Drawing code goes here
           (let* ((scale 500)
                  (dashes (list (/ 50.0 scale)
                                (/ 10.0 scale)
                                (/ 10.0 scale)
                                (/ 10.0 scale)))
                  (offset (/ -50.0 scale)))
             (cairo-set-source-rgb cr 0.0 0.0 0.0)
             (cairo-set-dash cr dashes offset)
             (cairo-set-line-width cr (/ 10.0 scale))
             (cairo-move-to cr (/ 128.0 scale) (/ 25.6 scale))
             (cairo-line-to cr (/ 230.4 scale) (/ 230.4 scale))
             (cairo-rel-line-to cr (/ -102.4 scale) 0.0)
             (cairo-curve-to cr (/ 51.2 scale)
                                (/ 230.4 scale)
                                (/ 51.2 scale)
                                (/ 128.0 scale)
                                (/ 128.0 scale)
                                (/ 128.0 scale))
              (cairo-stroke cr))
           ;; Destroy the Cario context
           (cairo-destroy cr)
           t)))
      (gtk-widget-show-all window))))

;;; 2020-11-26
