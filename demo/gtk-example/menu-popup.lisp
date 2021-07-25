;;;; Example Menu Popup (2021-7-17)

(in-package :gtk-example)

(defun example-menu-popup ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 300
                                 :default-height 180
                                 :title "Example Popup Menu"))
          (button (gtk-button-new-with-label "Click me")))
      ;; Create pop-up menu for button
      (let ((popupmenu (gtk-menu-new))
            (bigitem (gtk-menu-item-new-with-label "Larger"))
            (smallitem (gtk-menu-item-new-with-label "Smaller")))
        (gtk-menu-shell-append popupmenu bigitem)
        (gtk-menu-shell-append popupmenu smallitem)
        (gtk-widget-show-all popupmenu)
        ;; Signal handler to pop up the menu
        (g-signal-connect button "button-press-event"
           (lambda (widget event)
             (declare (ignore widget))
             (gtk-menu-popup popupmenu
                             :button (gdk-event-button-button event)
                             :activate-time (gdk-event-button-time event))
             t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))
