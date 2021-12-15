;;;; Example Grid Packing - 2021-12-4

(in-package :gtk-example)

(defun make-grid (homogeneous spacing expand align margin)
  (let ((grid (make-instance 'gtk-grid
                             :orientation :horizontal
                             :column-homogeneous homogeneous
                             :column-spacing spacing)))
    (gtk-container-add grid
                       (make-instance 'gtk-button
                                      :label "gtk-container-add"
                                      :hexpand expand
                                      :halgin align
                                      :margin margin))
    (gtk-container-add grid
                       (make-instance 'gtk-button
                                      :label "grid"
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add grid
                       (make-instance 'gtk-button
                                      :label "child"
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add grid
                       (make-instance 'gtk-button
                                      :label (if expand "T" "NIL")
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add grid
                       (make-instance 'gtk-button
                                      :label (format nil "~A" align)
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    grid))

(defun example-grid-packing (&optional (application nil) (spacing 6) (margin 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Grid Packing"
                                 :application application
                                 :type :toplevel
                                 :border-width 12
                                 :default-height 200
                                 :default-width 300))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 12)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add grid
                         (make-instance 'gtk-label
                                        :use-markup t
                                        :label "<b>Non-homogeneus Grids</b>"
                                        :xalign 0
                                        :yalign 0
                                        :vexpand nil
                                        :valign :start))
      (gtk-container-add grid (make-grid nil spacing nil :center margin))
      (gtk-container-add grid (make-grid nil spacing t :center margin))
      (gtk-container-add grid (make-grid nil spacing t :fill margin))
      (gtk-container-add grid
                         (make-instance 'gtk-label
                                        :use-markup t
                                        :label "<b>Homogeneous Grids</b>"
                                         :xalign 0
                                         :yalign 0
                                         :vexpand nil
                                         :valign :start
                                         :margin 6))
      (gtk-container-add grid (make-grid t spacing t :center margin))
      (gtk-container-add grid (make-grid t spacing t :fill margin))
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
