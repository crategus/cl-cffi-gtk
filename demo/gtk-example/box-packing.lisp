;;;; Example Box Packing - 2021-12-4

(in-package :gtk-example)

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
                        (gtk-button-new-with-label "child")
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
    box))

(defun example-box-packing (&optional (application nil) (spacing 6) (padding 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Box Packing"
                                 :application application
                                 :type :toplevel
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 12)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Non-homogenous boxes
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :label "<b>Non-homogeneous boxes</b>"
                                         :xalign 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing nil nil padding)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t nil padding)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box nil spacing t t padding)
                          :expand nil)
      ;; Homogeneous boxes
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :label "<b>Homogeneous boxes</b>"
                                         :xalign 0)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box t spacing t nil padding)
                          :expand nil)
      (gtk-box-pack-start vbox
                          (make-box t spacing t t padding)
                          :expand nil)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
