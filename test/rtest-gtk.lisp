(in-package :gtk-testsuite)

(def-suite gtk-suite :in gtk-testsuite)
(in-suite gtk-suite)

#|
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
|#

;;;  Application support

(load "rtest-gtk-application.lisp")
(load "rtest-gtk-application-window.lisp")
(load "rtest-gtk-actionable.lisp")

;;;  Interface builder

(load "rtest-gtk-builder.lisp")
(load "rtest-gtk-buildable.lisp")

;    gtk.buildable.lisp

;;;  Windows

(load "rtest-gtk-window.lisp")
(load "rtest-gtk-dialog.lisp")

;    gtk.message-dialog.lisp
;    gtk.about-dialog.lisp
;    gtk.assistant.lisp
;    gtk.invisible.lisp
;    gtk.offscreen-window.lisp
;    gtk.window-group.lisp

;;;  Layout Containers

(load "rtest-gtk-box.lisp")

;    gtk.grid.lisp
;    gtk.revealer.lisp
;    gtk.list-box.lisp
;    gtk.flow-box.lisp
;    gtk.stack.lisp
;    gtk.stack-switcher.lisp
;    gtk.stack-sidebar.lisp
;    gtk.action-bar.lisp
;    gtk.header-bar.lisp
;    gtk.overlay.lisp
;    gtk.button-box.lisp
;    gtk.paned.lisp
;    gtk.layout.lisp
;    gtk.notebook.lisp
;    gtk.expander.lisp
;    gtk.orientable.lisp
;    gtk.aspect-frame.lisp
;    gtk.fixed.lisp

;;;  Scrolling

(load "rtest-gtk-scrollable.lisp")

;;;  Printing

(load "rtest-gtk-print-operation.lisp")
(load "rtest-gtk-paper-size.lisp")
(load "rtest-gtk-print-settings.lisp")

;;;  Miscellaneous

(load "rtest-gtk-adjustment.lisp")

;;;  Abstract Base Classes

(load "rtest-gtk-widget.lisp")
(load "rtest-gtk-container.lisp")

;;;  Choosing from installed applications

(load "rtest-gtk-app-chooser.lisp")

;;;  Gestures and event handling

;;;  GTK+ Core Reference

(load "rtest-gtk-main-loop.lisp")

(load "rtest-gtk-accel-group.lisp")
(load "rtest-gtk-accel-map.lisp")

(load "rtest-gtk-selections.lisp")

;;;  Theming in GTK+

(load "rtest-gtk-style-context.lisp")
(load "rtest-gtk-css-provider.lisp")
(load "rtest-gtk-widget-path.lisp")

;;;  Deprecated

(load "rtest-gtk-action.lisp")
(load "rtest-gtk-action-group.lisp")



(load "rtest-gtk-entry-buffer.lisp")
(load "rtest-gtk-frame.lisp")
(load "rtest-gtk-list-store.lisp")
(load "rtest-gtk-text-buffer.lisp")
(load "rtest-gtk-text-iter.lisp")
(load "rtest-gtk-tool-palette.lisp")

