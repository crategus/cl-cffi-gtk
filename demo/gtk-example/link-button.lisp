;;;; Link button (2021-5-20)

(in-package :gtk-example)

(defun example-link-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Link Button"
                                 :default-width 280
                                 :border-width 18))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 6
                               :column-homogeneous t)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (gtk-container-add grid
                         (make-instance 'gtk-label
                                        :use-markup t
                                        :label
                                        "<b>Link Button with url</b>"))
      (gtk-container-add grid
                         (gtk-link-button-new "http://www.gtk.org/"))
      (gtk-container-add grid
                         (make-instance 'gtk-label
                                        :margin-top 12
                                        :use-markup t
                                        :label
                                        "<b>Link Button with Label</b>"))
      (gtk-container-add grid
                         (gtk-link-button-new-with-label "http://www.gtk.org/"
                                                         "Project WebSite"))
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
