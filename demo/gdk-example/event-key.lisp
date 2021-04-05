;;;; Example GdkEventKey - 2021-4-3

(in-package #:gdk-example)

(defun example-event-key ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Example GdkEventKey"
                                  :type :toplevel
                                  :border-width 6
                                  :default-width 380
                                  :default-height 460))
           (vbox (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 12))
           (entry (make-instance 'gtk-entry))
           (buffer (make-instance 'gtk-text-buffer))
           (view (make-instance 'gtk-text-view
                                :buffer buffer
                                :monospace t
                                :editable nil))
           (scrolled (make-instance 'gtk-scrolled-window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect entry "key-press-event"
          (lambda (widget event)
            (declare (ignore widget))
            (gtk-text-buffer-insert buffer (format nil "~%~a~%" event))
            (gtk-text-view-scroll-to-mark view
                                          (gtk-text-buffer-get-insert buffer))))
      (g-signal-connect entry "key-release-event"
          (lambda (widget event)
            (declare (ignore widget))
            (format t "~%")
            (format t "        type : ~a~%" (gdk-event-key-type event))
            (format t "      window : ~a~%" (gdk-event-key-window event))
            (format t "  send-event : ~a~%" (gdk-event-key-send-event event))
            (format t "        time : ~a~%" (gdk-event-key-time event))
            (format t "       state : ~a~%" (gdk-event-key-state event))
            (format t "      keyval : ~a~%" (gdk-event-key-keyval event))
            (format t "      length : ~a~%" (gdk-event-key-length event))
            (format t "      string : ~a~%" (gdk-event-key-string event))
            (format t "     keycode : ~a~%" (gdk-event-key-hardware-keycode event))
            (format t "       group : ~a~%" (gdk-event-key-group event))
            (format t " is-modifier : ~a~%" (gdk-event-key-is-modifier event))
            (gtk-text-buffer-insert buffer (format nil "~a~%" event))
            (gtk-text-view-scroll-to-mark view
                                          (gtk-text-buffer-get-insert buffer))))
      ;; Pack and  show the widgets
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :label "<b>Enter text:</b>")
                          :expand nil)
      (gtk-box-pack-start vbox entry :expand nil)
      (gtk-container-add scrolled view)
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :label "<b>GdkEventKey:</b>")
                          :expand nil)
      (gtk-box-pack-start vbox scrolled)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
