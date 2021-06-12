;;;; Example Event Box (2021-6-11)
;;;;
;;;; Example Event Box demonstrates both uses of a GtkEventBox widget - a label
;;;; is created that is clipped to a small box, and set up so that a mouse-click
;;;; on the label causes the program to exit. Resizing the window reveals
;;;; varying amounts of the label.
;;;;
;;;; In addition, example Event Box shows how to change the cursor over a
;;;; window. Every widget has an associated window of type GdkWindow, which can
;;;; be get with the function gtk-widget-window. The function gdk-window-cursor
;;;; sets a cursor for this GdkWindow. A new cursor is created with the function
;;;; gdk-cursor-new-from-name. The function takes two arguments. The first
;;;; argument is the GdkDisplay object for which the cursor will be created.
;;;; The second argument is a string with the name of the cursor. Look at the
;;;; documentation of the function gdk-cursor-new-from-name for available cursor
;;;; names. In example Event Box the cursor with the name "pointer" is chosen.
;;;; This cursor is associated to the GdkWindow with the function
;;;; gdk-window-cursor.

(in-package :gtk-example)

(defun example-event-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Event Box"
                                 :default-height 150
                                 :border-width 24))
          (eventbox (make-instance 'gtk-event-box))
          (label (make-instance 'gtk-label
                                :ellipsize :end
                                :label
                                "Click here to quit this Example Event Box.")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set the available events for the event box
      (setf (gtk-widget-events eventbox) :button-press-mask)
      ;; Connect a signal handler to the eventbox
      (g-signal-connect eventbox "button-press-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gtk-widget-destroy window)))
      ;; Add the label to the event box and the event box to the window
      (gtk-container-add eventbox label)
      (gtk-container-add window eventbox)
      ;; Realize the event box
      (gtk-widget-realize eventbox)
      ;; Set a new cursor for the event box
      (setf (gdk-window-cursor (gtk-widget-window eventbox))
            (gdk-cursor-new-from-name (gdk-display-default) "pointer"))
      ;; Show the window
      (gtk-widget-show-all window))))
