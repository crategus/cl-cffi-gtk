;;;; Example Button Boxes (2021-4-30)

(in-package :gtk-example)

(defun create-bbox (orientation title style)
  (let ((box (make-instance 'gtk-box
                            :orientation :vertical))
        (bbox (make-instance 'gtk-button-box
                             :orientation orientation
                             :border-width 6
                             :layout-style style
                             :spacing 6)))
    (gtk-container-add bbox
                       (make-instance 'gtk-button
                                      :label "OK"
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-ok")))
    (gtk-container-add bbox
                       (make-instance 'gtk-button
                                      :label "Cancel"
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-cancel")))
    (gtk-container-add bbox
                       (make-instance 'gtk-button
                                      :label "Help"
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-help")))

    (gtk-box-pack-start box (make-instance 'gtk-label
                                           :xalign 0.0
                                           :label title)
                            :expand nil)
    (gtk-box-pack-start box (make-instance 'gtk-separator
                                           :margin-top 6
                                           :margin-bottom 6
                                           :orientation :horizontal)
                            :expand nil)
    (gtk-box-pack-start box bbox)
    box))

(defun example-button-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Button Box"
                                 :default-width 420
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :homogeneous nil
                               :spacing 12)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :margin-top 6
                                         :margin-bottom 6
                                         :xalign 0
                                         :use-markup t
                                         :label
                                         "<b>Horizontal Button Boxes</b>")
                          :expand nil)
      (gtk-box-pack-start vbox
                          (create-bbox :horizontal "Spread" :spread))
      (gtk-box-pack-start vbox
                          (create-bbox :horizontal "Edge" :edge))
      (gtk-box-pack-start vbox
                          (create-bbox :horizontal "Start" :start))
      (gtk-box-pack-start vbox
                          (create-bbox :horizontal "End" :end))
      (gtk-box-pack-start vbox
                          (create-bbox :horizontal "Center" :center))
      (gtk-box-pack-start vbox
                          (create-bbox :horizontal "Expand" :expand))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
