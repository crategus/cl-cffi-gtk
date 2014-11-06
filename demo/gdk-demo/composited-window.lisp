(defpackage :demo-composited-window
  (:use :gtk :gdk :gobject :glib :cairo :cffi :common-lisp)
  (:export #:demo-composited-window))

(in-package :demo-composited-window)

(defun demo-composited-window ()
  (within-main-loop
    (let (;; Make the widgets
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :border-width 12))
          (button (make-instance 'gtk-button
                                 :label "A Button"))
          (event  (make-instance 'gtk-event-box))
         )
      ;; Put a red background on the window
      (gtk-widget-modify-bg window :normal (gdk-color-parse "red"))
      ;; Set the colourmap for the event box.
      ;; Must be done before the event box is realised.
      (gtk-widget-set-visual event
        (gdk-screen-get-rgba-visual (gtk-widget-get-screen event)))

      ;; Set our event box to have a fully-transparent background
      ;; drawn on it. Currently there is no way to simply tell GTK+
      ;; that "transparency" is the background colour for a widget.
      (setf (gtk-widget-app-paintable event) t)
      (g-signal-connect event "draw"
         ;; The expose event handler for the event box.
         ;; This function simply draws a transparency onto a widget on the area
         ;; for which it receives expose events.  This is intended to give the
         ;; event box a "transparent" background.
         ;; In order for this to work properly, the widget must have an RGBA
         ;; colormap.  The widget should also be set as app-paintable since it
         ;; does not make sense for GTK+ to draw a background if we are drawing
         ;; it and because GTK+ might actually replace our transparency with its
         ;;default background colour.
         (lambda (widget cr)
           (let ((cr (pointer cr)))
             (cairo-set-operator cr :clear)
;             (gdk-cairo-region cr (gdk-event-expose-region event))
             (cairo-fill cr)
             (cairo-destroy cr)
             nil)))

      ;; Put them inside one another
      (gtk-container-add window event)
      (gtk-container-add event button)

      ;; Realise and show everything
      (gtk-widget-show-all window)

      ;; Set the event box GdkWindow to be composited.
      ;; Obviously must be performed after event box is realised.
      (gdk-window-set-composited (gtk-widget-window event) t)

      ;; Set up the compositing handler.
      ;; Note that we do _after_ so that the normal (red) background is drawn
      ;; by gtk before our compositing occurs.
      (g-signal-connect-after window "draw"
         ;; The expose event handler for the window.
         ;; This function performs the actual compositing of the event box onto
         ;; the already-existing background of the window at 50% normal opacity.
         ;; In this case we do not want app-paintable to be set on the widget
         ;; since we want it to draw its own (red) background. Because of this,
         ;; however, we must ensure that we use g_signal_connect_after so that
         ;; this handler is called after the red has been drawn. If it was
         ;; called before then GTK would just blindly paint over our work.
         ;; Note: if the child window has children, then you need a cairo 1.6
         ;; feature to make this work correctly.
         (lambda (widget cr)
           (let* (;; get our child (in this case, the event box)
                  (child (gtk-bin-get-child widget))
                  ;; create a cairo context to draw to the window
                  (cr (pointer cr))
;                  (cr (gdk-cairo-create (gtk-widget-window widget)))
)
             ;; the source data is the (composited) event box
             (gdk-cairo-set-source-window cr
                                          (gtk-widget-window child)
                                          (coerce (gdk-rectangle-x (gtk-widget-get-allocation child))
                                                  'long-float)
                                          (coerce (gdk-rectangle-y (gtk-widget-get-allocation child))
                                                  'long-float))
             ;; draw no more than our expose event intersects our child
;             (let ((region (cairo-region-create-rectangle (gtk-widget-get-allocation child))))
;               (cairo-region-intersect region (gdk-event-expose-region event))
;               (gdk-cairo-region cr region)
;               (cairo-clip cr)
;               (cairo-region-destroy region))

             ;; composite, with a 50 % opacity
             (cairo-set-operator cr :over)
             (cairo-paint-with-alpha cr 0.5d0)
             ;; We are done
             (cairo-destroy cr)
             nil)))
)))


