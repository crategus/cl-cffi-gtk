;;;; Text View Search

(in-package :gtk-tutorial)

(defun example-text-view-search ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Text View Search"
                                 :type :toplevel
                                 :default-width 350
                                 :default-height 250))
          (entry (make-instance 'gtk-search-entry))
          (toolitem (make-instance 'gtk-tool-item))
          (toolbar (make-instance 'gtk-toolbar))
          (scrolled (make-instance 'gtk-scrolled-window))
          (textview (make-instance 'gtk-text-view
                                    :wrap-mode :word
                                    :top-margin 6
                                    :left-margin 6
                                    :right-margin 6))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Search and select the text in the text buffer
      (g-signal-connect entry "search-changed"
          (lambda (widget)
            (let* ((text (gtk-entry-text widget))
                   (buffer (gtk-text-view-buffer textview))
                   (iter (gtk-text-buffer-start-iter buffer)))
              (multiple-value-bind (found start end)
                  (gtk-text-iter-search iter text)
                (when found
                      (gtk-text-buffer-select-range buffer start end))))))
      ;; Set some text into the text buffer
      (setf (gtk-text-buffer-text (gtk-text-view-buffer textview))
            *some-text*)
      ;; Pack and show the widgets
      (gtk-container-add toolitem entry)
      (gtk-container-add toolbar toolitem)
      (gtk-box-pack-start vbox toolbar :expand nil)
      (gtk-container-add scrolled textview)
      (gtk-box-pack-start vbox scrolled :expand t)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
