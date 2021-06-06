;;;; Example Menu by hand (2021-6-1)

(in-package :gtk-example)

(defun example-menu-by-hand ()
  (within-main-loop
    ;; We set the "gtk-shell-shows-menubar" property to NIL to display the
    ;; menubar by the application itself and not by the desktop environment.
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-default))
          nil)
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 425
                                 :default-height 250
                                 :title "Example Menu by Hand"))
          ;; A vbox to put a menu in
          (vbox (gtk-box-new :vertical 0)))
      ;; Create a menu bar and the menu items for the menu bar
      (let ((menu-bar (gtk-menu-bar-new))
            (file-item (gtk-menu-item-new-with-label "File"))
            (view-item (gtk-menu-item-new-with-label "View"))
            (tools-item (gtk-menu-item-new-with-label "Tools"))
            (help-item (gtk-menu-item-new-with-label "Help")))
        ;; Add the menu bar to the main container
        (gtk-container-add vbox menu-bar)
        ;; Add the menu items to the menu bar
        (gtk-menu-shell-append menu-bar file-item)
        (gtk-menu-shell-append menu-bar view-item)
        (gtk-menu-shell-append menu-bar tools-item)
        (gtk-menu-shell-append menu-bar help-item)
        ;; Create the menus for the menu items in the menu bar
        (let ((file-menu (gtk-menu-new))
              (view-menu (gtk-menu-new))
              (tools-menu (gtk-menu-new))
              (help-menu (gtk-menu-new)))
          ;; Attach the submenus to the items of the menu bar
          (setf (gtk-menu-item-submenu file-item) file-menu)
          (setf (gtk-menu-item-submenu view-item) view-menu)
          (setf (gtk-menu-item-submenu tools-item) tools-menu)
          (setf (gtk-menu-item-submenu help-item) help-menu)
          ;; Create items to put into the File menu
          (let ((open-item (gtk-menu-item-new-with-label "Open"))
                (close-item (gtk-menu-item-new-with-label "Close"))
                (exit-item (gtk-menu-item-new-with-label "Exit")))
            ;; Append the items to the File menu
            (gtk-menu-shell-append file-menu open-item)
            (gtk-menu-shell-append file-menu close-item)
            (gtk-menu-shell-append file-menu (gtk-separator-menu-item-new))
            (gtk-menu-shell-append file-menu exit-item)
            ;; Add a signal handler for exit-item
            (g-signal-connect exit-item "activate"
                              (lambda (widget)
                                (declare (ignore widget))
                                (gtk-widget-destroy window))))

          ;; The view and tools menus will be empty for now

          ;; Create items to put into the Help menu
          (let ((query-item (gtk-menu-item-new-with-label "What's this?"))
                (about-help-item (gtk-menu-item-new-with-label "Info")))
            ;; Append the items to the About Help Menu
            (gtk-menu-shell-append help-menu query-item)
            (gtk-menu-shell-append help-menu (gtk-separator-menu-item-new))
            (gtk-menu-shell-append help-menu about-help-item)
            ;; Create a submenu and items for about-help-item
            (let ((about-help-menu (gtk-menu-new))
                  (about-tool-item (gtk-menu-item-new-with-label "About This"))
                  (about-more-item (gtk-menu-item-new-with-label "About That")))
              ;; Attach the submenu to the about-help-item
              (setf (gtk-menu-item-submenu about-help-item) about-help-menu)
              ;; Append the items to the about-help-menu
              (gtk-menu-shell-append about-help-menu about-tool-item)
              (gtk-menu-shell-append about-help-menu about-more-item)))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
