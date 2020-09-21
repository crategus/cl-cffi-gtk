;;;; Popovers
;;;;
;;;; A bubble-like window containing contextual information or options.
;;;; GtkPopovers can be attached to any widget, and will be displayed
;;;; within the same window, but on top of all its content.

(in-package #:gtk-demo)

(defun create-popover (parent child pos)
  (let ((popover (make-instance 'gtk-popover
                                :relative-to parent
                                :position pos)))
    (gtk-container-add popover child)
    (setf (gtk-container-border-width popover) 6)
    (gtk-widget-show-all child)
    popover))

(defun create-complex-popover (parent pos)
  (let* ((builder (gtk-builder-new-from-file (rel-path "popover.ui")))
         (window (gtk-builder-object builder "window"))
         (content (gtk-bin-child window)))
    (gtk-container-remove (gtk-widget-parent content) content)
    (gtk-widget-destroy window)
    (create-popover parent content pos)))

(defun do-popover ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo Popover"))
           (box (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 24
                               :border-width 24)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window box)
      ;; Show a button with a popover
      (let* ((button (make-instance 'gtk-toggle-button
                                    :label "Press me to show Popover"))
             (label (make-instance 'gtk-label
                                   :label "This popover does not grab input."))
             (popover (create-popover button label :top)))
        (setf (gtk-popover-modal popover) nil)
        (g-signal-connect button "toggled"
                          (lambda (widget)
                            (setf (gtk-widget-visible popover)
                                  (gtk-toggle-button-active widget))))
        (gtk-container-add box button))
      ;; Show an entry with a popover
      (let* ((entry (make-instance 'gtk-entry))
             (popover (create-complex-popover entry :top)))
        (gtk-entry-set-icon-from-icon-name entry :primary "edit-find")
        (gtk-entry-set-icon-from-icon-name entry :secondary "edit-clear")
        (g-signal-connect entry "icon-press"
            (lambda (entry icon-pos event)
              (declare (ignore event))
              (let ((rect (gtk-entry-icon-area entry icon-pos)))
                (setf (gtk-popover-pointing-to popover) rect)
                      (gtk-widget-show popover))))
        (gtk-container-add box entry))
      ;; Show a calendar with a popover
      (let ((calendar (make-instance 'gtk-calendar)))
        (g-signal-connect calendar "day-selected"
            (lambda (calendar)
              (let ((event (gtk-current-event)))
                (when (eq :button-press (gdk-event-type event))
                  (multiple-value-bind (x y)
                    (gdk-window-coords-to-parent (gdk-event-button-window event)
                                                 (gdk-event-button-x event)
                                                 (gdk-event-button-y event))
                    (let ((rect (gtk-widget-allocation calendar)))
                      (setf (gdk-rectangle-x rect)
                            (- (truncate x) (gdk-rectangle-x rect)))
                      (setf (gdk-rectangle-y rect)
                            (- (truncate y) (gdk-rectangle-y rect)))
                      (setf (gdk-rectangle-width rect) 1)
                      (setf (gdk-rectangle-height rect) 1)
                      (let ((popover (create-popover calendar
                                                     (make-instance 'gtk-entry)
                                                     :bottom)))
                        (setf (gtk-popover-pointing-to popover) rect)
                              (gtk-widget-show popover))))))))
        (gtk-container-add box calendar))
      ;; Show the window.
      (gtk-widget-show-all window))))

