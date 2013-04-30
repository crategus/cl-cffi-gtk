;;;; More Labels

(defun example-more-labels ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "GTK+ 3.4 Example More Labels"
                                 :default-width 300
                                 :border-width 6))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 6))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :homogeneous nil
                               :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-box-pack-start hbox
                          (make-instance 'gtk-label
                                         :label "Angle 90°"
                                         :angle 90))
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label "Angel 45°"
                                         :angle 45))
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :label "Angel 315°"
                                         :angle 315))
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start hbox
                          (make-instance 'gtk-label
                                         :label "Angel 270°"
                                         :angle 270))
      (gtk-box-pack-start vbox2 hbox)
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-hseparator))
      (gtk-box-pack-start vbox2
                          (gtk-label-new "Normal Label"))
      (gtk-box-pack-start vbox2
                          (gtk-label-new-with-mnemonic "With _Mnemonic"))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label "This Label is Selectable"
                                         :selectable t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label
                                         "<small>Small text</small>"
                                          :use-markup t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :label
                                         "<b>Bold text</b>"
                                          :use-markup t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :label
                                         (format nil
                                         "Go to the ~
                                         <a href=\"http://gtk.org/\">~
                                         GTK+ Website</a> for more ...")))
      (gtk-container-add window vbox2)
      (gtk-widget-show-all window))))

