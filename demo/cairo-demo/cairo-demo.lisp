(asdf:load-system :cl-cffi-gtk)

(defpackage :demo-cairo
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :common-lisp)
  (:export #:demo-cairo))

(in-package :demo-cairo)

(defun demo-cairo ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

           ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (cairo-set-line-width (pointer cr) 0.1d0)
           (cairo-set-source-rgb (pointer cr) 1.0d0 0.0d0 0.0d0)
           (cairo-rectangle (pointer cr) 0.25d0 0.25d0 0.5d0 0.5d0)
           (cairo-stroke (pointer cr))

           (cairo-destroy (pointer cr))
           t)))
      (gtk-widget-show-all window))))

(defun demo-cairo-1 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (cairo-set-source-rgb (pointer cr) 1.0d0 0.0d0 0.0d0)
           (cairo-rectangle (pointer cr) 0.25d0 0.25d0 0.5d0 0.5d0)
           (cairo-fill (pointer cr))

           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))


(defun demo-cairo-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (cairo-set-source-rgb (pointer cr) 0.0d0 0.0d0 0.0d0)
           (cairo-select-font-face (pointer cr) "Georgia" :normal :bold)
           (cairo-set-font-size (pointer cr) 1.2d0)
           (let ((text-extents (cairo-text-extents (pointer cr) "a")))
             (cairo-move-to (pointer cr)
                            (- 0.5d0
                               (/ (cairo-text-extents-t-width text-extents) 2)
                               (cairo-text-extents-t-x-bearing text-extents))
                            (- 0.5d0
                               (/ (cairo-text-extents-t-height text-extents) 2)
                               (cairo-text-extents-t-y-bearing text-extents)))
             (cairo-show-text (pointer cr) "a"))
           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-3 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (cairo-set-source-rgb (pointer cr) 0.0d0 0.0d0 0.0d0)
           (cairo-paint-with-alpha (pointer cr) 0.5d0)

           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-4 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (let ((linpat (cairo-pattern-create-linear 0 0 1 1))
                 (radpat (cairo-pattern-create-radial 0.5 0.5 0.25 0.5 0.5 0.75)))

             (cairo-pattern-add-color-stop-rgb linpat 0 0 0.3 0.8)
             (cairo-pattern-add-color-stop-rgb linpat 1 0 0.8 0.3)

             (cairo-pattern-add-color-stop-rgba radpat 0 0 0 0 1)
             (cairo-pattern-add-color-stop-rgba radpat 0.5 0 0 0 0)

             (cairo-set-source (pointer cr) linpat)
             (cairo-mask (pointer cr) radpat)
           )
;           (cairo-paint (pointer cr))
           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-5 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (cairo-set-source-rgb (pointer cr) 0 0 0)
           (cairo-move-to (pointer cr) 0 0)
           (cairo-line-to (pointer cr) 1 1)
           (cairo-move-to (pointer cr) 1 0)
           (cairo-line-to (pointer cr) 0 1)
           (cairo-set-line-width (pointer cr) 0.2)
           (cairo-stroke (pointer cr))

           (cairo-rectangle (pointer cr) 0 0 0.5 0.5)
           (cairo-set-source-rgba (pointer cr) 1 0 0 0.80)
           (cairo-fill (pointer cr))

           (cairo-rectangle (pointer cr) 0 0.5 0.5 0.5)
           (cairo-set-source-rgba (pointer cr) 0 1 0 0.60)
           (cairo-fill (pointer cr))

           (cairo-rectangle (pointer cr) 0.5 0 0.5 0.5)
           (cairo-set-source-rgba (pointer cr) 0 0 1 0.40)
           (cairo-fill (pointer cr))

           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-6 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (let ((radpat (cairo-pattern-create-radial 0.25d0 0.25d0 0.10d0 0.50d0 0.50d0 0.50d0))
                 (linpat (cairo-pattern-create-linear 0.25d0 0.35d0 0.75d0 0.65d0)))
             (cairo-pattern-add-color-stop-rgb radpat 0.00d0 1.00d0 0.80d0 0.80d0)
             (cairo-pattern-add-color-stop-rgb radpat 1.00d0 0.90d0 0.00d0 0.00d0)

             (iter (for i from 1 below 10)
                   (iter (for j from 1 below 10)
                         (cairo-rectangle (pointer cr)
                                          (- (/ i 10.0d0) 0.04d0)
                                          (- (/ j 10.0d0) 0.04d0)
                                          0.08d0
                                          0.08d0)))
             (cairo-set-source (pointer cr) radpat)
             (cairo-fill (pointer cr))

             (cairo-pattern-add-color-stop-rgba linpat 0.00 1.0 1.0 1.0 0.0)
             (cairo-pattern-add-color-stop-rgba linpat 0.25 0.0 1.0 0.0 0.5)
             (cairo-pattern-add-color-stop-rgba linpat 0.50 1.0 1.0 1.0 0.0)
             (cairo-pattern-add-color-stop-rgba linpat 0.75 0.0 0.0 1.0 0.5)
             (cairo-pattern-add-color-stop-rgba linpat 1.00 1.0 1.0 1.0 0.0)

             (cairo-rectangle (pointer cr) 0.0 0.0 1.0 1.0)
             (cairo-set-source (pointer cr) linpat)
             (cairo-fill (pointer cr))

           )
#|
int i, j;
cairo_pattern_t *radpat, *linpat;

radpat = cairo_pattern_create_radial (0.25, 0.25, 0.1,  0.5, 0.5, 0.5);
cairo_pattern_add_color_stop_rgb (radpat, 0,  1.0, 0.8, 0.8);
cairo_pattern_add_color_stop_rgb (radpat, 1,  0.9, 0.0, 0.0);

for (i=1; i<10; i++)
    for (j=1; j<10; j++)
        cairo_rectangle (cr, i/10.0 - 0.04, j/10.0 - 0.04, 0.08, 0.08);
cairo_set_source (cr, radpat);
cairo_fill (cr);

linpat = cairo_pattern_create_linear (0.25, 0.35, 0.75, 0.65);
cairo_pattern_add_color_stop_rgba (linpat, 0.00,  1, 1, 1, 0);
cairo_pattern_add_color_stop_rgba (linpat, 0.25,  0, 1, 0, 0.5);
cairo_pattern_add_color_stop_rgba (linpat, 0.50,  1, 1, 1, 0);
cairo_pattern_add_color_stop_rgba (linpat, 0.75,  0, 0, 1, 0.5);
cairo_pattern_add_color_stop_rgba (linpat, 1.00,  1, 1, 1, 0);

cairo_rectangle (cr, 0.0, 0.0, 1, 1);
cairo_set_source (cr, linpat);
cairo_fill (cr);
|#

           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

(defun demo-cairo-7 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"
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
                 (window (gtk-widget-get-window widget)))
             (format t "in DRAW~%")
             (format t " window = ~A (~A, ~A)~%" window
                                                 (gdk-window-get-width window)
                                                 (gdk-window-get-height window))
           ;; Clear surface
           (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
           (cairo-paint (pointer cr))

	   ;; Example is in 1.0 x 1.0 coordinate space
           (cairo-scale (pointer cr)
                        (gdk-window-get-width window)
                        (gdk-window-get-height window))

           ;; Drawing code goes here
           (cairo-set-line-width (pointer cr) 0.01d0)
           (cairo-set-source-rgb (pointer cr) 1.0d0 0.0d0 0.0d0)           

           (cairo-move-to (pointer cr) 0.25 0.25)

           (cairo-line-to (pointer cr) 0.5 0.375)
           (cairo-rel-line-to (pointer cr) 0.25 -0.125)

           (cairo-arc (pointer cr) 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
           (cairo-rel-curve-to (pointer cr) -0.25 -0.125 -0.25 0.125 -0.5 0)

           (cairo-close-path (pointer cr))

           (cairo-stroke (pointer cr))

           (cairo-destroy (pointer cr))
           t)))
        (gtk-widget-show-all window))))

