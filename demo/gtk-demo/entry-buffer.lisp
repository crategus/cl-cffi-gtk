;;;; Entry Buffer
;;;;
;;;; GtkEntryBuffer provides the text content in a GtkEntry.

(defun example-entry-buffer ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Entry Buffer"
                                 :border-width 12
                                 :default-width 400))
          (hbox (make-instance 'gtk-grid
                               :orientation :horizontal))
          (vbox (make-instance 'gtk-grid
                               :orientation :vertical))
          ;; The entry buffer for the entries of this example
          (buffer (make-instance 'gtk-entry-buffer)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label "<b>First Entry</b>"
                                        :halign :start
                                        :margin-bottom 3
                                        :use-markup t))
      (gtk-container-add vbox
                         (make-instance 'gtk-entry
                                        :buffer buffer))

      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label "<b>Second Entry</b>"
                                        :halign :start
                                        :margin-top 12
                                        :margin-bottom 3
                                        :use-markup t))

      (gtk-container-add vbox
                         (make-instance 'gtk-entry
                                        :buffer buffer))

      (gtk-container-add hbox vbox)

      (gtk-container-add hbox
                         (make-instance 'gtk-label
                                        :valign :start
                                        :margin-top 12
                                        :margin-left 12
                                        :label
                                        (format nil 
                                                "Both entries have the same ~%~
                                                 entry buffer object.~%~%~
                                                 Typ in some text in one of ~%~
                                                 the entries to see the ~
                                                 effect.")))
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))

