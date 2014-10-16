;;;; Menu

(in-package #:gtk-demo)

(defun example-menu ()
  (within-main-loop
    (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-get-default))
          nil)
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
          nil)
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 200
                                 :title "Example Menu Widget"))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create the menu-bar and the items of the menu-bar.
      (let ((menubar (make-instance 'gtk-menu-bar
                                    :visible t
                                    :can-focus nil))
            ;; Item file of the menu-bar.
            (item-file (make-instance 'gtk-menu-item
                                      :label "_Datei"
                                      :use-underline t))
            ;; Item edit of the menu-bar.
            (item-edit (make-instance 'gtk-menu-item
                                      :label "_Bearbeiten"
                                      :use-underline t))
            ;; Item help of the menu-bar.
            (item-help (make-instance 'gtk-menu-item
                                      :label "_Hilfe"
                                      :use-underline t)))
        ;; Create submenu for the item file.
        (let ((submenu (make-instance 'gtk-menu
                                      :visible t
                                      :can-focus nil))
              (item-file-new (make-instance 'gtk-image-menu-item
                                       :label "gtk-new"
                                       :use-underline t
                                       :use-stock t))
              (item-file-open (make-instance 'gtk-image-menu-item
                                        :label "gtk-open"
                                        :use-underline t
                                        :use-stock t))
              (item-file-save (make-instance 'gtk-image-menu-item
                                        :label "gtk-save"
                                        :user-underline t
                                        :use-stock t))
              (item-file-save-as (make-instance 'gtk-image-menu-item
                                                :label "gtk-save-as"
                                                :user-underline t
                                                :use-stock t))
              (item-file-quit (make-instance 'gtk-image-menu-item
                                             :label "gtk-quit"
                                             :user-underline t
                                             :use-stock t)))
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
              (item-edit-cut (make-instance 'gtk-image-menu-item
                                            :label "gtk-cut"
                                            :use-underline t
                                            :use-stock t))
              (item-edit-copy (make-instance 'gtk-image-menu-item
                                             :label "gtk-copy"
                                             :use-underline t
                                             :use-stock t))
              (item-edit-paste (make-instance 'gtk-image-menu-item
                                              :label "gtk-paste"
                                              :user-underline t
                                              :use-stock t))
              (item-edit-delete (make-instance 'gtk-image-menu-item
                                               :label "gtk-delete"
                                               :user-underline t
                                               :use-stock t)))
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
              (item-help-about (make-instance 'gtk-image-menu-item
                                              :label "gtk-about"
                                              :use-underline t
                                              :use-stock t)))
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
