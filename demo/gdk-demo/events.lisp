(defpackage :demo-events
  (:use :gtk :gdk :gobject :glib :cairo :cffi :common-lisp)
  (:export #:demo-events))

(in-package :demo-events)

(let ((notebook (make-instance 'gtk-notebook :show-tabs nil))
      ;; Labels for Button events
      (event-type (make-instance 'gtk-label :xalign 0.1))
      (event-window (make-instance 'gtk-label :xalign 0.1))
      (event-button-time (make-instance 'gtk-label :xalign 0.1))
      (event-button-x (make-instance 'gtk-label :xalign 0.1))
      (event-button-y (make-instance 'gtk-label :xalign 0.1))
      (event-button-axes (make-instance 'gtk-label :xalign 0.1))
      (event-button-state (make-instance 'gtk-label :xalign 0.1))
      (event-button-button (make-instance 'gtk-label :xalign 0.1))
      (event-button-device (make-instance 'gtk-label :xalign 0.1))
      (event-button-x-root (make-instance 'gtk-label :xalign 0.1))
      (event-button-y-root (make-instance 'gtk-label :xalign 0.1))
      ;; Labels for Key events
      (event-key-time (make-instance 'gtk-label :xalign 0.1))
      (event-key-state (make-instance 'gtk-label :xalign 0.1))
      (event-key-keyval (make-instance 'gtk-label :xalign 0.1))
      (event-key-length (make-instance 'gtk-label :xalign 0.1))
      (event-key-string (make-instance 'gtk-label :xalign 0.1))
      (event-key-hardware-keycode (make-instance 'gtk-label :xalign 0.1))
      (event-key-group (make-instance 'gtk-label :xalign 0.1))
      (event-key-is-modifier (make-instance 'gtk-label :xalign 0.1))
      ;; Labels for Crossing events
      (event-crossing-subwindow (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-time (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-x (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-y (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-x-root (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-y-root (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-mode (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-detail (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-focus (make-instance 'gtk-label :xalign 0.1))
      (event-crossing-state (make-instance 'gtk-label :xalign 0.1))

     )

  (defun event-handler (event)
    (format t "Event ~A~%" event)
    (when (not (eq :expose (gdk-event-type event)))
      (setf (gtk-label-text event-type)
            (format nil "~A" (gdk-event-type event)))
      (setf (gtk-label-text event-window)
            (format nil "~A" (gdk-event-window event)))
      (case (gdk-event-type event)
        ((:button-release :button-press :2button-press :3button-press)
         (format t "Button relase and press event~%")
         (setf (gtk-label-text event-button-time)
               (format nil "~A" (gdk-event-button-time event)))
         (setf (gtk-label-text event-button-x)
               (format nil "~A" (gdk-event-button-x event)))
         (setf (gtk-label-text event-button-y)
               (format nil "~A" (gdk-event-button-y event)))
         (setf (gtk-label-text event-button-axes)
               (format nil "~A" (gdk-event-button-axes event)))
         (setf (gtk-label-text event-button-state)
               (format nil "~A"
                       (foreign-bitfield-symbols 'gdk-modifier-type
                                                 (gdk-event-button-state event))))
         (setf (gtk-label-text event-button-button)
               (format nil "~A" (gdk-event-button-button event)))
         (setf (gtk-label-text event-button-device)
               (format nil "~A" (gdk-event-button-device event)))
         (setf (gtk-label-text event-button-x-root)
               (format nil "~A" (gdk-event-button-x-root event)))
         (setf (gtk-label-text event-button-y-root)
               (format nil "~A" (gdk-event-button-y-root event)))
         (setf (gtk-notebook-current-page notebook) 0))
        ((:key-press :key-release)
         (setf (gtk-label-text event-key-time)
               (format nil "~A" (gdk-event-key-time event)))
         (setf (gtk-label-text event-key-state)
               (format nil "~A" (gdk-event-key-state event)))
         (setf (gtk-label-text event-key-keyval)
               (format nil "~A" (gdk-event-key-keyval event)))
         (setf (gtk-label-text event-key-length)
               (format nil "~A" (gdk-event-key-length event)))
         (setf (gtk-label-text event-key-string)
               (format nil "~A" (gdk-event-key-string event)))
         (setf (gtk-label-text event-key-hardware-keycode)
               (format nil "~A" (gdk-event-key-hardware-keycode event)))
         (setf (gtk-label-text event-key-group)
               (format nil "~A" (gdk-event-key-group event)))
         (setf (gtk-label-text event-key-is-modifier)
               (format nil "~A" (gdk-event-key-is-modifier event)))
         (setf (gtk-notebook-current-page notebook) 1))
        ((:enter-notify :leave-notify)
         (setf (gtk-label-text event-crossing-subwindow)
               (format nil "~A" (gdk-event-crossing-subwindow event)))
         (setf (gtk-label-text event-crossing-time)
               (format nil "~A" (gdk-event-crossing-time event)))
         (setf (gtk-label-text event-crossing-x)
               (format nil "~A" (gdk-event-crossing-x event)))
         (setf (gtk-label-text event-crossing-y)
               (format nil "~A" (gdk-event-crossing-y event)))
         (setf (gtk-label-text event-crossing-x-root)
               (format nil "~A" (gdk-event-crossing-x-root event)))
         (setf (gtk-label-text event-crossing-y-root)
               (format nil "~A" (gdk-event-crossing-y-root event)))
         (setf (gtk-label-text event-crossing-mode)
               (format nil "~A" (gdk-event-crossing-mode event)))
         (setf (gtk-label-text event-crossing-detail)
               (format nil "~A" (gdk-event-crossing-detail event)))
         (setf (gtk-label-text event-crossing-focus)
               (format nil "~A" (gdk-event-crossing-focus event)))
         (setf (gtk-label-text event-crossing-state)
               (format nil "~A" (gdk-event-crossing-state event)))
         (setf (gtk-notebook-current-page notebook) 2))
))
    (gtk-main-do-event event)
    +gdk-event-stop+)

  (defun demo-events ()
    (within-main-loop
      (let (;; Create a toplevel window.
            (window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Demo Events"
                                   :border-width 12))
            (hgrid (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-spacing 12))
            (vgrid (make-instance 'gtk-grid
                                  :orientation :verticaL))
            (frame (make-instance 'gtk-frame
                                  :label "Area to move Mouse"
                                  :shadow-type :in))
            (area (make-instance 'gtk-drawing-area
                                 :width-request 200
                                 :height-request 200))
  )
        ;; Signal handler for the window to handle the signal "destroy".
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))

        (setf (gtk-widget-events area)
              (append (gtk-widget-events area)
                      '(:all-events-mask)))

        (gtk-container-add vgrid (make-instance 'gtk-label
                                                :xalign 0.0
                                                :use-markup t
                                                :margin-top 6
                                                :margin-bottom 3
                                                :label "<b>Last Event Type</b>"))
        (gtk-container-add vgrid event-type)
        (gtk-container-add vgrid (make-instance 'gtk-label
                                                :xalign 0.0
                                                :use-markup t
                                                :margin-top 6
                                                :margin-bottom 3
                                                :label "<b>Window</b>"))
        (gtk-container-add vgrid event-window)

        ;; Add a page to the notebook for button events
        (let ((page (make-instance 'gtk-grid
                                   :orientation :vertical)))
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Time</b>"))
          (gtk-container-add page event-button-time)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>X</b>"))
          (gtk-container-add page event-button-x)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Y</b>"))
          (gtk-container-add page event-button-y)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Axes</b>"))
          (gtk-container-add page event-button-axes)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>State</b>"))
          (gtk-container-add page event-button-state)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Button</b>"))
          (gtk-container-add page event-button-button)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Device</b>"))
          (gtk-container-add page event-button-device)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>X-Root</b>"))
          (gtk-container-add page event-button-x-root)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Y-Root</b>"))
          (gtk-container-add page event-button-y-root)
          (gtk-widget-show-all page)
          (gtk-notebook-add-page notebook page nil))

        ;; Add a page to the notebook for key events
        (let ((page (make-instance 'gtk-grid
                                   :orientation :vertical)))
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Time</b>"))
          (gtk-container-add page event-key-time)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>State</b>"))
          (gtk-container-add page event-key-state)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Keyval</b>"))
          (gtk-container-add page event-key-keyval)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Length</b>"))
          (gtk-container-add page event-key-length)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>String</b>"))
          (gtk-container-add page event-key-string)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Hardware Keycode</b>"))
          (gtk-container-add page event-key-hardware-keycode)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Group</b>"))
          (gtk-container-add page event-key-group)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Is Modifier</b>"))
          (gtk-container-add page event-key-is-modifier)
          (gtk-widget-show-all page)
          (gtk-notebook-add-page notebook page nil))

        ;; Add a page to the notebook for crossing events
        (let ((page (make-instance 'gtk-grid
                                   :orientation :vertical)))
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Subwindow</b>"))
          (gtk-container-add page event-crossing-subwindow)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>time</b>"))
          (gtk-container-add page event-crossing-time)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>X</b>"))
          (gtk-container-add page event-crossing-x)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Y</b>"))
          (gtk-container-add page event-crossing-y)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>X-Root</b>"))
          (gtk-container-add page event-crossing-x-root)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Y-Root</b>"))
          (gtk-container-add page event-crossing-y-root)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Mode</b>"))
          (gtk-container-add page event-crossing-mode)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Detail</b>"))
          (gtk-container-add page event-crossing-detail)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>Focus</b>"))
          (gtk-container-add page event-crossing-focus)
          (gtk-container-add page
                             (make-instance 'gtk-label
                                            :xalign 0.0
                                            :use-markup t
                                            :margin-top 6
                                            :margin-bottom 3
                                            :label "<b>State</b>"))
          (gtk-container-add page event-crossing-state)


          (gtk-widget-show-all page)
          (gtk-notebook-add-page notebook page nil))

        (gtk-container-add vgrid notebook)

        (gtk-container-add frame area)
        (gtk-container-add hgrid frame)
        (gtk-container-add hgrid vgrid)
        (gtk-container-add window hgrid)

        (gdk-event-handler-set #'event-handler)

        ;; Show the window.
        (gtk-widget-show-all window))))
)
