;;;; Example Menu Popup (2021-6-1)

(in-package :gtk-example)

(defun example-menu-popup ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 150
                                 :title "Example Popup Menu"))
          (button (gtk-button-new-with-label "Click me")))
      ;; Create pop-up menu for button
      (let ((popup-menu (gtk-menu-new))
            (big-item (gtk-menu-item-new-with-label "Larger"))
            (small-item (gtk-menu-item-new-with-label "Smaller")))
        (gtk-menu-shell-append popup-menu big-item)
        (gtk-menu-shell-append popup-menu small-item)
        (gtk-widget-show-all popup-menu)
        ;; Signal handler to pop up the menu
        (g-signal-connect button "button-press-event"
           (lambda (widget event)
             (declare (ignore widget))
             (gtk-menu-popup popup-menu
                             :button (gdk-event-button-button event)
                             :activate-time (gdk-event-button-time event))
             t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))
