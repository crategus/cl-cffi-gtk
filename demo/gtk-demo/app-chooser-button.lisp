;;;; App Chooser Button

(in-package #:gtk-demo)

(defun demo-app-chooser-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo Application Chooser Button"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 100))
          (button (make-instance 'gtk-app-chooser-button
                                 :content-type "*"
                                 :show-dialog-item t)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
;      (g-signal-connect button "font-set"
;         (lambda (widget)
;           (declare (ignore widget))
;           (format t "Font is set:~%")
;           (format t "   Font name   : ~A~%"
;                   (gtk-font-chooser-font button))
;           (format t "   Font family : ~A~%"
;                   (pango-font-family-name
;                     (gtk-font-chooser-font-family button)))
;           (format t "   Font face   : ~A~%"
;                   (pango-font-face-face-name
;                     (gtk-font-chooser-font-face button)))
;           (format t "   Font size   : ~A~%"
;                   (gtk-font-chooser-font-size button))))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))
