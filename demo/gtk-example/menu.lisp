;;;; Example Menu - 2021-11-13

(in-package :gtk-example)

(defun example-menu (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :title "Example Menu"
                                 :default-width 320
                                 :default-height 180))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create the menu-bar and the items of the menu-bar
      (let ((menubar (make-instance 'gtk-menu-bar
                                    :visible t
                                    :can-focus nil))
            ;; Item file of the menu-bar
            (item-file (make-instance 'gtk-menu-item
                                      :label "_File"
                                      :use-underline t))
            ;; Item edit of the menu-bar
            (item-edit (make-instance 'gtk-menu-item
                                      :label "_Edit"
                                      :use-underline t))
            ;; Item help of the menu-bar
            (item-help (make-instance 'gtk-menu-item
                                      :label "_Help"
                                      :use-underline t)))
        ;; Create submenu for the item file
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-file-new (make-instance 'gtk-menu-item
                                            :label "New"))
              (item-file-open (make-instance 'gtk-menu-item
                                             :label "Open ..."))
              (item-file-save (make-instance 'gtk-menu-item
                                             :label "Save"))
              (item-file-save-as (make-instance 'gtk-menu-item
                                                :label "Save as ..."))
              (item-file-quit (make-instance 'gtk-menu-item
                                             :label "Quit")))
          ;; Add an accelerator to the QUIT menu item
          (let ((group (make-instance 'gtk-accel-group)))
            (gtk-window-add-accel-group window group)
            (gtk-widget-add-accelerator item-file-quit
                                        "activate"
                                        group
                                        (gdk-keyval-from-name "q")
                                        :control-mask
                                        :visible))
          ;; Add the items to to the submenu.
          (gtk-menu-shell-append submenu item-file-new)
          (gtk-menu-shell-append submenu item-file-open)
          (gtk-menu-shell-append submenu item-file-save)
          (gtk-menu-shell-append submenu item-file-save-as)
          ;; Insert a GtkSeparatorMenuItem.
          (gtk-menu-shell-append submenu (gtk-separator-menu-item-new))
          ;; Add the item file quit to the submenu
          (gtk-menu-shell-append submenu item-file-quit)
          ;; Set the submenu of the item file.
          (setf (gtk-menu-item-submenu item-file) submenu))
        ;; Create submenu for the item edit.
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-edit-cut (make-instance 'gtk-menu-item
                                            :label "Cut"))
              (item-edit-copy (make-instance 'gtk-menu-item
                                             :label "Copy"))
              (item-edit-paste (make-instance 'gtk-menu-item
                                              :label "Paste"))
              (item-edit-delete (make-instance 'gtk-menu-item
                                               :label "Delete")))
          ;; Add the items to to the submenu.
          (gtk-menu-shell-append submenu item-edit-cut)
          (gtk-menu-shell-append submenu item-edit-copy)
          (gtk-menu-shell-append submenu item-edit-paste)
          (gtk-menu-shell-append submenu item-edit-delete)
          ;; Set the submenu of the item edit.
          (setf (gtk-menu-item-submenu item-edit) submenu))
        ;; Create submenu for the item help.
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-help-about (make-instance 'gtk-menu-item
                                              :label "About ...")))
          ;; Add the items to to the submenu.
          (gtk-menu-shell-append submenu item-help-about)
          ;; Set the submenu of the item help.
          (setf (gtk-menu-item-submenu item-help) submenu))
        ;; Add the items file, edit, and help into the menubar.
        (gtk-menu-shell-append menubar item-file)
        (gtk-menu-shell-append menubar item-edit)
        (gtk-menu-shell-append menubar item-help)
        ;; Pack the menubar into the vbox.
        (gtk-box-pack-start vbox menubar :expand nil))
        ;; Pack a text view into the vbox
        (gtk-box-pack-start vbox (make-instance 'gtk-text-view))
      ;; Pack the vbox into the window.
      (gtk-container-add window vbox)
      ;; Show the window.
      (gtk-widget-show-all window))))
