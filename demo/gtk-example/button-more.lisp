;;;; Example More Buttons (2021-5-19)

(in-package :gtk-example)

(defun example-button-more ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example More Buttons"
                                 :type :toplevel
                                 :default-width 300
                                 :default-height 180
                                 :border-width 12))
          (grid (make-instance 'gtk-grid
                               :halign :center
                               :valign :center
                               :column-spacing 9
                               :row-spacing 9)))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; These are the standard functions to create a button
      (gtk-grid-attach grid
                       (gtk-button-new-with-label "Label")
                       0 0 1 1)
      (gtk-grid-attach grid
                       (gtk-button-new-with-mnemonic "_Mnemonic")
                       0 1 1 1)
      (gtk-grid-attach grid
                       (gtk-button-new-from-icon-name "gtk-apply" :button)
                       0 2 1 1)

      ;; Create some buttons with make-instance
      (gtk-grid-attach grid
                       (make-instance 'gtk-button
                                      :image-position :left
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-edit")
                                      :label "Bearbeiten")
                       1 0 1 1)
      (gtk-grid-attach grid
                       (make-instance 'gtk-button
                                      :image-position :top
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-cut")
                                      :label "Ausschneiden")
                       1 1 1 1)
      (gtk-grid-attach grid
                       (make-instance 'gtk-button
                                      :image-position :bottom
                                      :always-show-image t
                                      :image
                                      (make-instance 'gtk-image
                                                     :icon-name "gtk-cancel")
                                      :label "Abbrechen")
                       1 2 1 1)

      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
