;;;; Example Revealer - 2021-12-4

(in-package :gtk-example)

(defun example-revealer (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Revealer"
                                 :application application
                                 :default-width 300
                                 :default-height 300
                                 :border-width 12))
          (grid (make-instance 'gtk-grid
                               :column-homogeneous t
                               :row-homogeneous t
                               :column-spacing 12
                               :row-spacing 12)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (gtk-grid-attach grid
                       (make-instance 'gtk-label
                                      :use-markup t
                                      :label "<b>Press a Button</b>")
                       2 2 1 1)
      (let ((toggle (make-instance 'gtk-toggle-button :label "None"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :center
                                     :valign :center
                                     :transition-type :none
                                     :transition-duration 4000))
            (label (make-instance 'gtk-label
                                  :label "No transation")))
        (gtk-container-add revealer label)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 0 0 1 1)
        (gtk-grid-attach grid revealer 1 0 1 1))
      (let ((toggle (make-instance 'gtk-toggle-button :label "Fade"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :center
                                     :valign :center
                                     :transition-type :crossfade
                                     :transition-duration 4000))
            (label (make-instance 'gtk-label
                                  :label "Fade in the label")))
        (gtk-container-add revealer label)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 4 4 1 1)
        (gtk-grid-attach grid revealer 3 4 1 1))
      (let ((toggle (make-instance 'gtk-toggle-button :label "Right"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :start
                                     :hexpand t
                                     :transition-type :slide-right
                                     :transition-duration 4000))
            (label (make-instance 'gtk-label
                                  :label "Slide in the label")))
        (gtk-container-add revealer label)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 3 2 1 1)
        (gtk-grid-attach grid revealer 4 2 1 1))
      (let ((toggle (make-instance 'gtk-toggle-button :label "Down"))
            (revealer (make-instance 'gtk-revealer
                                     :valign :start
                                     :vexpand t
                                     :transition-type :slide-down
                                     :transition-duration 4000))
            (label (make-instance 'gtk-label
                                  :label "Slide in the label")))
        (gtk-container-add revealer label)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle   2 3 1 1)
        (gtk-grid-attach grid revealer 2 4 1 1))
      (let ((toggle (make-instance 'gtk-toggle-button :label "Left"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :end
                                     :hexpand t
                                     :transition-type :slide-left
                                     :transition-duration 4000))
            (label (make-instance 'gtk-label
                                  :label "Slide in the label")))
        (gtk-container-add revealer label)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 1 2 1 1)
        (gtk-grid-attach grid revealer 0 2 1 1))
      (let ((toggle (make-instance 'gtk-toggle-button :label "Up"))
            (revealer (make-instance 'gtk-revealer
                                     :valign :end
                                     :vexpand t
                                     :transition-type :slide-up
                                     :transition-duration 4000))
            (label (make-instance 'gtk-label
                                  :label "Slide in the label")))
        (gtk-container-add revealer label)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle   2 1 1 1)
        (gtk-grid-attach grid revealer 2 0 1 1))
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
