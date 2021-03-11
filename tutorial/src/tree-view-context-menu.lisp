;;;; Tree View Context Menu

(in-package :gtk-tutorial)

(defun create-popup-menu (view event)
  (let ((menu (gtk-menu-new))
        (item (gtk-menu-item-new-with-label "Do something")))
    (g-signal-connect item "activate"
                      (lambda (widget)
                        (declare (ignore widget))
                        (format t "Do something.~%")))
    (gtk-menu-shell-append menu item)
    (gtk-widget-show-all menu)
    (gtk-menu-popup-at-pointer menu event)))

(defun example-tree-view-context-menu ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Tree View Context Menu"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 200))
          (view (create-view-and-model-simple)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signal handler for button right clicked
      (g-signal-connect view "button-press-event"
          (lambda (widget event)
            (when (and (eq :button-press (gdk-event-type event))
                       (= 3 (gdk-event-button event)))
              (format t "Single right click on the tree view.~%")
              (create-popup-menu widget event))))
      ;; Signal handler for keyboard Shift F10
      (g-signal-connect view "popup-menu"
          (lambda (widget)
            (format t "Popup Menu is activated with Shift F10.~%")
            (create-popup-menu widget nil)))
      ;; Pack and show widgets
      (gtk-container-add window view)
      (gtk-widget-show-all window))))
