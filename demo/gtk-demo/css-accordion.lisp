;;;; CSS Accordion
;;;;
;;;; A simple accordion demo written using CSS transitions and multiple
;;;; backgrounds.

(in-package #:gtk-demo)

(defun apply-css (widget provider)
  (gtk-style-context-add-provider (gtk-widget-get-style-context widget)
                                  provider
                                  +gtk-style-provider-priority-user+)
  (when (g-type-is-a (g-type-from-instance widget) "GtkContainer")
    (gtk-container-forall widget
                          (lambda (widget)
                            (apply-css widget provider)))))

(defun demo-css-accordion ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Demo CSS Accordion"
                                 :default-height 300
                                 :default-width 600))
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
      ;; Add buttons to the container
      (gtk-container-add container
                         (gtk-button-new-with-label "This"))
      (gtk-container-add container
                         (gtk-button-new-with-label "Is"))
      (gtk-container-add container
                         (gtk-button-new-with-label "A"))
      (gtk-container-add container
                         (gtk-button-new-with-label "CSS"))
      (gtk-container-add container
                         (gtk-button-new-with-label "Accordion"))
      (gtk-container-add container
                         (gtk-button-new-with-label "."))
      ;; Add container to window
      (gtk-container-add window container)
      ;; Load CSS from file into the provider
      (gtk-css-provider-load-from-path provider (asdf:system-relative-pathname :cl-cffi-gtk-demo-gtk "css-accordion.css"))
      ;; Apply CSS to the widgets
      (apply-css window provider)
      ;; Show the window.
      (gtk-widget-show-all window))))

