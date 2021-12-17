;;;; Paned Window Widgets - 2021-12-7

(in-package :gtk-example)

(defun example-paned-window (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Paned Window"
                                 :application application
                                 :default-width 320
                                 :default-height 280
                                 :border-width 12))
          (paned1 (make-instance 'gtk-paned
                                 :position 100
                                 :orientation :vertical))
          (paned2 (make-instance 'gtk-paned
                                 :position 100
                                 :orientation :horizontal)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-paned-add1 paned2
                      (make-instance 'gtk-frame
                                     :label "Pane 1"
                                     :label-xalign 0
                                     :label-yalign 0))
      (gtk-paned-add2 paned2
                      (make-instance 'gtk-frame
                                     :label "Pane 2"
                                     :label-xalign 0
                                     :label-yalign 0))
      (gtk-paned-add2 paned1
                      (make-instance 'gtk-frame
                                     :label "Pane 3"
                                     :label-xalign 0
                                     :label-yalign 0))
      (gtk-paned-add1 paned1 paned2)
      (gtk-container-add window paned1)
      (gtk-widget-show-all window))))
