;;;; Box packing
;;;;
;;;; Packing Demonstration Program

(defun make-box (homogeneous spacing expand fill padding)
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
                            :homogeneous homogeneous
                            :spacing spacing)))
    (gtk-box-pack-start box
                        (gtk-button-new-with-label "gtk-box-pack")
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (gtk-button-new-with-label "box")
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (gtk-button-new-with-label "button")
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (if expand
                            (gtk-button-new-with-label "T")
                            (gtk-button-new-with-label "NIL"))
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (if fill
                            (gtk-button-new-with-label "T")
                            (gtk-button-new-with-label "NIL"))
                        :expand expand
                        :fill fill
                        :padding padding)
    (gtk-box-pack-start box
                        (gtk-button-new-with-label (format nil "~A" padding))
                        :expand expand
                        :fill fill
                        :padding padding)
    box))

(defun example-box-packing (&optional (spacing 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Box Packing"
                                 :type :toplevel
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 6))
          (button (make-instance 'gtk-button
                                 :label "Quit"))
          (quitbox (make-instance 'gtk-box
                                  :orientation :horizontal)))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                          "GtkBox   ~
                                           :orientation :horizontal   ~
                                           :homogeneous nil   ~
                                           :spacing ~A"
                                          spacing)
                                         :xalign 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing nil nil 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t nil 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t t 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                            "GtkBox   ~
                                             :orientation :horizontal   ~
                                             :homogeneous t   ~
                                             :spacing ~A"
                                            spacing)
                                         :xalign 0)
                          :expand nil
                          :padding 6)
      (gtk-box-pack-start vbox
                          (make-box t spacing t nil 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box t spacing t t 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (gtk-separator-new :horizontal)
                          :expand nil
                          :padding 6)
      ;; Align the quit-button on the right side
      (gtk-box-pack-end quitbox button :expand nil)
      (gtk-box-pack-start vbox quitbox :expand nil)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

