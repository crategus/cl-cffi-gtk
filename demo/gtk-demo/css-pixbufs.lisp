;;;; Animated backgrounds
;;;;
;;;; This demo is done in honour of the Pixbufs demo further down.
;;;; It is done exclusively with CSS as the background of the window.

(in-package #:gtk-demo)

(defun demo-css-pixbufs ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo CSS Pixbufs"
                                 :default-height 400
                                 :default-width 400))
          (container (make-instance 'gtk-box
                                    :orientation :horizontal
                                    :halign :center
                                    :valign :center))
          (provider (make-instance 'gtk-css-provider)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Add container to window
      (gtk-container-add window container)
      ;; Load CSS from file into the provider
      (gtk-css-provider-load-from-path provider (namestring (asdf:system-relative-pathname :cl-cffi-gtk-demo-gtk "css-pixbufs.css")))
      ;; Apply CSS to the widgets
      (apply-css window provider)
      ;; Show the window.
      (gtk-widget-show-all window))))
