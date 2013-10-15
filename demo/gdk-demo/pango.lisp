(asdf:load-system :cl-cffi-gtk)

(defpackage :demo-pango
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :common-lisp)
  (:export #:demo-events))

(in-package :demo-pango)

#|
#define RADIUS 100
#define N_WORDS 10
#define FONT "Sans Bold 18"

PangoContext *context;
PangoLayout *layout;
PangoFontDescription *desc;

double radius;
int width, height;
int i;

/* Set up a transformation matrix so that the user space coordinates for
 * where we are drawing are [-RADIUS, RADIUS], [-RADIUS, RADIUS]
 * We first center, then change the scale */

width = gdk_window_get_width (window);
height = gdk_window_get_height (window);
radius = MIN (width, height) / 2.;

cairo_translate (cr,
                 radius + (width - 2 * radius) / 2,
                 radius + (height - 2 * radius) / 2);
cairo_scale (cr, radius / RADIUS, radius / RADIUS);

/* Create a PangoLayout, set the font and text */
context = gdk_pango_context_get_for_screen (screen);
layout = pango_layout_new (context);
pango_layout_set_text (layout, "Text", -1);
desc = pango_font_description_from_string (FONT);
pango_layout_set_font_description (layout, desc);
pango_font_description_free (desc);

/* Draw the layout N_WORDS times in a circle */
for (i = 0; i < N_WORDS; i++)
  {
    double red, green, blue;
    double angle = 2 * G_PI * i / n_words;

    cairo_save (cr);

    /* Gradient from red at angle == 60 to blue at angle == 300 */
    red = (1 + cos (angle - 60)) / 2;
    green = 0;
    blue = 1 - red;

    cairo_set_source_rgb (cr, red, green, blue);
    cairo_rotate (cr, angle);

    /* Inform Pango to re-layout the text with the new transformation matrix */
    pango_cairo_update_layout (cr, layout);

    pango_layout_get_size (layout, &width, &height);

    cairo_move_to (cr, - width / 2 / PANGO_SCALE, - DEFAULT_TEXT_RADIUS);
    pango_cairo_show_layout (cr, layout);

    cairo_restore (cr);
  }

g_object_unref (layout);
g_object_unref (context);
|#

(let ((radius 150)
      (n-words 8))
  (defun demo-pango ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Example Using Pango with Cairo"
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
             (let* (;; Get the GdkWindow for the widget
                    (window (gtk-widget-get-window widget))
                    (width (gdk-window-get-width window))
                    (height (gdk-window-get-height window)))
               (format t "in DRAW~%")
               (format t " window = ~A (~A, ~A)~%" window
                                                   (gdk-window-get-width window)
                                                   (gdk-window-get-height window))
             ;; Clear surface
             (cairo-set-source-rgb (pointer cr) 1.0d0 1.0d0 1.0d0)
             (cairo-paint (pointer cr))

             ;; Create a PangoLayout, set the font and text
             (let* ((layout (pango-cairo-create-layout (pointer cr)))
                    (desc (pango-font-description-from-string "Sans Bold 20")))
               (cairo-set-source-rgb (pointer cr) 0.0d0 1.0d0 0.0d0)
               (pango-layout-set-text layout "Text")
               (pango-layout-set-font-description layout desc)

               (multiple-value-bind (layout-width layout-height)
                   (pango-layout-get-size layout)

                 (cairo-translate (pointer cr)
                                  (- (/ width 2.0d0) (/ layout-width 1024 2))
                                  (- (/ height 2.0d0) (/ layout-height 1024)) )
               )
               (pango-cairo-update-layout (pointer cr) layout)

;;;   cairo_translate (cr,
;;;                    radius + (width - 2 * radius) / 2,
;;;                    radius + (height - 2 * radius) / 2);
;;;                    cairo_scale (cr, radius / RADIUS, radius / RADIUS);

;               (cairo-translate (pointer cr) (+ 50 radius) (+ 50 radius))
               (pango-cairo-show-layout (pointer cr) layout)

;;;       int width, height;
;;;       double angle = (360. * i) / N_WORDS;
;;;       double red;
;;;       cairo_save (cr);
;;;       /* Gradient from red at angle == 60 to blue at angle == 240 */
;;;       red   = (1 + cos ((angle - 60) * G_PI / 180.)) / 2;
;;;       cairo_set_source_rgb (cr, red, 0, 1.0 - red);
;;;       cairo_rotate (cr, angle * G_PI / 180.);
;;;       /* Inform Pango to re-layout the text with the new transformation */
;;;       pango_cairo_update_layout (cr, layout);
;;;       pango_layout_get_size (layout, &width, &height);
;;;       cairo_move_to (cr, - ((double)width / PANGO_SCALE) / 2, - RADIUS);
;;;       pango_cairo_show_layout (cr, layout);
;;;       cairo_restore (cr);


               (do* ((i 0 (+ i 1))
                     (angle 0.0d0 (/ (* 360.0d0 i) n-words))
                     (red (/ (+ 1.0d0 (cos (* (/ pi 180.0d0) (- angle 60)))) 2.0d0)
                          (/ (+ 1.0d0 (cos (* (/ pi 180.0d0) (- angle 60)))) 2.0d0))
                    )
                    ((>= i n-words))
                 (format t " i=~A, angle=~A~%" i angle)
                 (cairo-set-source-rgb (pointer cr) red 0.0d0 (- 1.0d0 red))
                 (cairo-rotate (pointer cr) (/ (* angle pi) 180d0))
                 (pango-cairo-update-layout (pointer cr) layout)
                 
                 (cairo-move-to (pointer cr)
                                (* 1.0d0 (/ width 2.0d0 1024))
;                                ( + (* -1.0d0 (/ (pango-layout-get-size layout) 2 +pango-scale+))
;                                    (/ (pango-layout-get-size layout) 1024 2))
                                (* -1.0d0 radius))

                 (pango-cairo-show-layout (pointer cr) layout)
                    


             ))

             (cairo-destroy (pointer cr))
             t)))
        (gtk-widget-show-all window)))))

