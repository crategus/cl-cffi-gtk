;;;; Text View Insert Widget (2021-6-4)

(in-package :gtk-example)

(defun example-text-view-insert-widget ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Text View Insert Widget"
                                 :default-width 350
                                 :default-height 200))
          (text-view (make-instance 'gtk-text-view
                                    :top-margin 6
                                    :left-margin 6
                                    :right-margin 6))
          (button (make-instance 'gtk-button
                                 :label "Insert Widget"))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical)))
    (g-signal-connect window "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        (leave-gtk-main)))
    ;; Signal handler to insert a widget at the current cursor position.
    (g-signal-connect button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (let* ((buffer (gtk-text-view-buffer text-view))
                (cursor (gtk-text-buffer-get-insert buffer))
                (iter (gtk-text-buffer-iter-at-mark buffer cursor))
                (anchor (gtk-text-buffer-create-child-anchor buffer iter))
                (button (gtk-button-new-with-label "New Button")))
           (gtk-text-view-add-child-at-anchor text-view button anchor)
           (gtk-widget-show button))))
    (gtk-box-pack-start vbox text-view)
    (gtk-box-pack-start vbox button :expand nil)
    (gtk-container-add window vbox)
    (gtk-widget-show-all window))))
