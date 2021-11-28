;;;; Example Menu Popup - 2021-11-14

(in-package :gtk-example)

(defun example-menu-popup (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :default-width 300
                                 :default-height 180
                                 :title "Example Popup Menu"))
          (button (gtk-button-new-with-label "Click me")))
      ;; Create pop-up menu for button
      (let ((popup (make-instance 'gtk-menu))
            (bigitem (gtk-menu-item-new-with-label "Larger"))
            (smallitem (gtk-menu-item-new-with-label "Smaller")))
        (gtk-menu-shell-append popup bigitem)
        (gtk-menu-shell-append popup smallitem)
        (gtk-widget-show-all popup)
        ;; Signal handler to pop up the menu
        (g-signal-connect button "button-press-event"
           (lambda (widget event)
             (declare (ignore widget))
             (gtk-menu-popup-at-pointer popup event)
             t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))
