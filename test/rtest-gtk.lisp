
(in-package :gtk-testsuite)

(def-suite gtk-suite :in gtk-testsuite)
(in-suite gtk-suite)

(def-fixture simple-window (widget title)
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title title
                                 :default-width 300
                                 :default-height 200
                                 :border-width 12)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (&body)
      ;; Add the widget to the window.
      (gtk-container-add window widget)
      ;; Show the window.
      (gtk-widget-show-all window)
      ;; Destroy the window
;      (gtk-widget-destroy window)
))
  (join-gtk-main))

(def-fixture action-window (content action title)
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title title
                                 :default-width 300
                                 :default-height 200
                                 :border-width 12))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :spacing 12))
         )
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (&body)
      ;; Add the content and action to the window.
      (gtk-box-pack-start hbox content)
      (gtk-box-pack-start hbox action)
      (gtk-container-add window hbox)
      ;; Show the window.
      (gtk-widget-show-all window)
      ;; Destroy the window
;      (gtk-widget-destroy window)
  ))
  (join-gtk-main))

(load "rtest-gtk-accel-map.lisp")
(load "rtest-gtk-action.lisp")
(load "rtest-gtk-action-group.lisp")
(load "rtest-gtk-app-chooser.lisp")
(load "rtest-gtk-dialog.lisp")
(load "rtest-gtk-entry-buffer.lisp")
(load "rtest-gtk-frame.lisp")
(load "rtest-gtk-list-store.lisp")
(load "rtest-gtk-main-loop.lisp")
(load "rtest-gtk-paper-size.lisp")
(load "rtest-gtk-print-settings.lisp")
(load "rtest-gtk-selections.lisp")
(load "rtest-gtk-text-buffer.lisp")
(load "rtest-gtk-text-iter.lisp")
(load "rtest-gtk-tool-palette.lisp")
(load "rtest-gtk-widget.lisp")

