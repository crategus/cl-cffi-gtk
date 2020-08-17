;;;; Event Box
;;;;
;;;; Example Event Box demonstrates both uses of a GtkEventBox widget - a label
;;;; is created that is clipped to a small box, and set up so that a mouse-click
;;;; on the label causes the program to exit. Resizing the window reveals
;;;; varying amounts of the label.
;;;;
;;;; In addition, example Event Box shows how to change the mouse pointer over a
;;;; window. Every widget has an associated window of type GdkWindow, which can
;;;; be get with the function gtk-widget-window. The function gdk-window-cursor
;;;; sets a mouse pointer for this GdkWindow. A new mouse pointer is created
;;;; with the function gdk-cursor-new. The functions takes one argument, which
;;;; is a keyword of the enumeration type GdkCursorType for a predefined mouse
;;;; pointer. GTK+ has about 150 predefined mouse pointers. Look at the
;;;; implementation of GdkCursorType in the file gdk.cursor.lisp to see all
;;;; possible keywords. In example Event Box the mouse pointer :hand1 is chosen.
;;;; This mouse pointer is than associated to the GdkWindow with the function
;;;; gdk-window-cursor.

(in-package :gtk-demo)

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
      ;; Set the events for the event box
      (setf (gtk-widget-events eventbox) :button-press-mask)
      ;; Connect a signal to the eventbox
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
            (gdk-cursor-new :hand1))
      ;; Show the window
      (gtk-widget-show-all window))))

