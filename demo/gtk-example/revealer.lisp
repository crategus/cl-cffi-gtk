;;;; Example Revealer (2021-4-28)

(in-package :gtk-example)

(defun example-revealer ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Revealer"
                                 :default-width 300
                                 :default-height 300
                                 :border-width 12))
          (grid (make-instance 'gtk-grid
                               :column-spacing 12
                               :row-spacing 12)))

      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))

      ;; Placeholder to reserve space
      (gtk-grid-attach grid (make-instance 'gtk-fixed
                                           :margin-left 180
                                           :margin-top 48)
                            1 1 1 1)
      (gtk-grid-attach grid (make-instance 'gtk-fixed
                                           :margin-left 180
                                           :margin-top 48)
                            3 3 1 1)

      (gtk-grid-attach grid
                       (make-instance 'gtk-label
                                      :use-markup t
                                      :label
                                      (format nil
                                              "<b>Push the buttons to show~% ~
                                               different transition types</b>"))
                       2 2 1 1)

      (let ((toggle (make-instance 'gtk-toggle-button :label "None"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :start
                                     :valign :start
                                     :transition-type :none
                                     :transition-duration 4000))
            (entry (make-instance 'gtk-entry
                                  :text "No transation.")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 0 0 1 1)
        (gtk-grid-attach grid revealer 1 0 1 1))

      (let ((toggle (make-instance 'gtk-toggle-button :label "Fade"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :end
                                     :valign :end
                                     :transition-type :crossfade
                                     :transition-duration 4000))
            (entry (make-instance 'gtk-entry
                                  :text "Fade in the entry")))
        (gtk-container-add revealer entry)
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
            (entry (make-instance 'gtk-entry
                                  :text "Slide in from the left")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 0 2 1 1)
        (gtk-grid-attach grid revealer 1 2 1 1))

      (let ((toggle (make-instance 'gtk-toggle-button :label "Down"))
            (revealer (make-instance 'gtk-revealer
                                     :valign :start
                                     :vexpand t
                                     :transition-type :slide-down
                                     :transition-duration 4000))
            (entry (make-instance 'gtk-entry
                                  :text "Slide in from the top")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 2 0 1 1)
        (gtk-grid-attach grid revealer 2 1 1 1))

      (let ((toggle (make-instance 'gtk-toggle-button :label "Left"))
            (revealer (make-instance 'gtk-revealer
                                     :halign :end
                                     :hexpand t
                                     :transition-type :slide-left
                                     :transition-duration 4000))
            (entry (make-instance 'gtk-entry :text "Slide in from the right")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 4 2 1 1)
        (gtk-grid-attach grid revealer 3 2 1 1))

      (let ((toggle (make-instance 'gtk-toggle-button :label "Up"))
            (revealer (make-instance 'gtk-revealer
                                     :valign :end
                                     :vexpand t
                                     :transition-type :slide-up
                                     :transition-duration 4000))
            (entry (make-instance 'gtk-entry :text "Slide in from the bottom")))
        (gtk-container-add revealer entry)
        (g-object-bind-property toggle "active"
                                revealer "reveal-child"
                                :default)
        (gtk-grid-attach grid toggle 2 4 1 1)
        (gtk-grid-attach grid revealer 2 3 1 1))

      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
