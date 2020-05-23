;;;; More Buttons

(in-package #:gtk-demo)

(defun example-buttons ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Buttons"
                                 :type :toplevel
                                 :default-width 250
                                 :border-width 12))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (hbox  (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Set gtk-button-images to T. This allows buttons with text and image.
      (setf (gtk-settings-gtk-button-images (gtk-settings-get-default)) t)

      ;; These are the standard functions to create a button.
      (gtk-box-pack-start vbox1
                          (gtk-button-new-with-label "Label"))

      (let ((button (gtk-button-new-with-mnemonic "_Mnemonic")))
        (setf (gtk-button-use-underline button) t)
        (gtk-box-pack-start vbox1 button))

      (let ((button (gtk-button-new-from-icon-name "edit-copy" :button)))
        (setf (gtk-button-label button) "Kopieren")
        (gtk-box-pack-start vbox1 button))

      (gtk-box-pack-start vbox1
                          (gtk-button-new-from-stock "gtk-apply"))

      ;; Create some buttons with make-instance.
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :right
                                         :image
                                         (gtk-image-new-from-stock "gtk-edit"
                                                                   :button)
                                         :label "gtk-edit"
                                         :use-stock t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :top
                                         :image
                                         (gtk-image-new-from-stock "gtk-cut"
                                                                   :button)
                                         :label "gtk-cut"
                                         :use-stock t))
      (gtk-box-pack-start vbox2
                          (make-instance 'gtk-button
                                         :image-position :bottom
                                         :image
                                         (gtk-image-new-from-stock "gtk-cancel"
                                                                   :button)
                                         :label "gtk-cancel"
                                         :use-stock t))
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start hbox vbox2)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
