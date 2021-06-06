;;;; Example Toolbar by hand (2021-6-5)

(in-package :gtk-example)

(defun example-toolbar-by-hand ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :default-width 250
                                 :default-height 150
                                 :title "Example Toolbar"))
          ;; A vbox to put a menu and a button in
          (vbox (gtk-box-new :vertical 0)))
      (let ((toolbar (gtk-toolbar-new))
            (new-button (gtk-tool-button-new-from-stock "gtk-new"))
            (open-button (gtk-tool-button-new-from-stock "gtk-open"))
            (save-button (gtk-tool-button-new-from-stock "gtk-save"))
            (quit-button (gtk-tool-button-new-from-stock "gtk-quit"))
            (separator (make-instance 'gtk-separator-tool-item
                                      :draw nil)))
        (gtk-toolbar-insert toolbar new-button -1)
        (gtk-toolbar-insert toolbar open-button -1)
        (gtk-toolbar-insert toolbar save-button -1)
        (gtk-toolbar-insert toolbar separator -1)
        (gtk-toolbar-insert toolbar quit-button -1)
        (setf (gtk-tool-item-expand separator) t)
        (gtk-box-pack-start vbox toolbar :fill nil :expand nil :padding 3)
        ;; Connect a signal handler to the quit button
        (g-signal-connect quit-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
