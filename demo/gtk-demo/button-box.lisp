;;;; Button Boxes

(in-package #:gtk-demo)

(defun create-bbox (orientation title spacing layout)
  (let ((frame (make-instance 'gtk-frame
                              :label title))
        (bbox (make-instance 'gtk-button-box
                             :orientation orientation
                             :border-width 6
                             :layout-style layout
                             :spacing spacing)))
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
  (gtk-container-add frame bbox)
  frame))

(defun example-button-box ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Button Box"
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :homogeneous nil
                               :spacing 12)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create Horizontal Button Boxes
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :ypad 6
                                         :xalign 0
                                         :use-markup t
                                         :label
                                         "<b>Horizontal Button Boxes</b>")
                          :expand nil
                          :fill nil)
      ;; Create the first Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "Spread (spacing 12)"
                                       12
                                       :spread))
      ;; Create the second Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "Edge (spacing 12)"
                                       12
                                       :edge))
      ;; Create the third Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "Start (spacing 6)"
                                       6
                                       :start))
      ;; Create the fourth Horizontal Box
      (gtk-box-pack-start vbox2
                          (create-bbox :horizontal
                                       "End (spacing 6)"
                                       6
                                       :end))
      (gtk-box-pack-start vbox1 vbox2)
      ;; Create Vertical Button Boxes
      (gtk-box-pack-start vbox1
                          (make-instance 'gtk-label
                                         :ypad 12
                                         :xalign 0
                                         :use-markup t
                                         :label
                                         "<b>Vertical Button Boxes</b>")
                          :expand nil
                          :fill nil)
      ;; Create the first Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "Spread (spacing 12)"
                                       12
                                       :spread))
      ;; Create the second Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "Edge (spacing 12)"
                                       12
                                       :edge))
      ;; Create the third Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "Start (spacing 6)"
                                       6
                                       :start))
      ;; Create the fourth Vertical Box
      (gtk-box-pack-start hbox
                          (create-bbox :vertical
                                       "End (spacing 6)"
                                       6
                                       :end))
      (gtk-box-pack-start vbox1 hbox)
      (gtk-container-add window vbox1)
      (gtk-widget-show-all window))))

;;; 2020-11-18
