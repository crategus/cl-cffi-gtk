;;;; Text View Find Next (2021-6-4)

(in-package :gtk-example)

(defun find-next-match (textview text iter &key (direction :forward))
  (let ((buffer (gtk-text-view-buffer textview)))
    (multiple-value-bind (found start end)
        (gtk-text-iter-search iter text :direction direction)
      (when found
        (gtk-text-buffer-select-range buffer start end)
        (gtk-text-buffer-create-mark buffer "last-start" start)
        (let ((last-end (gtk-text-buffer-create-mark buffer "last-end" end)))
          (gtk-text-view-scroll-mark-onscreen textview last-end))))))

(defun example-text-view-find-next ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Text View Find Next"
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
              (find-next-match textview text iter))))
      ;; Find the next match
      (g-signal-connect entry "next-match"
          (lambda (widget)
             (let* ((text (gtk-entry-text widget))
                    (buffer (gtk-text-view-buffer textview))
                    (last-end (gtk-text-buffer-mark buffer "last-end"))
                    (iter (gtk-text-buffer-iter-at-mark buffer last-end)))
               (when last-end
                 (find-next-match textview text iter)))))
      ;; Find the previous match
      (g-signal-connect entry "previous-match"
          (lambda (widget)
             (let* ((text (gtk-entry-text widget))
                    (buffer (gtk-text-view-buffer textview))
                    (last-start (gtk-text-buffer-mark buffer "last-start"))
                    (iter (gtk-text-buffer-iter-at-mark buffer last-start)))
               (when last-start
                 (find-next-match textview text iter :direction :backward)))))
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
