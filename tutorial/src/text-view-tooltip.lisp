;;;; Text View Tooltip

(in-package :gtk-tutorial)

(let ((tooltip nil)
      (provider (make-instance 'gtk-css-provider))
      (css-tooltip "label {
                      color: white;
                      background-color: black;
                      font: 14px 'Monospace'; }
                    textview {
                      font: 14px 'Monospace'; }"))

  (defun get-tip (word)
    (cdr (assoc word
                '(("format" . "destination control-string &rest args")
                  ("gtk-text-iter-move" ."iter &key count by direction")
                  ("gtk-text-iter-copy" ."iter")
                  ("gtk-text-iter-find-char" .
                   "iter predicate &key limit direction")
                  ("gtk-text-buffer-insert" .
                   "buffer text &key position interactive editable"))
                :test #'equal)))

  (defun tip-window-new (tip-text)
    (let ((win (make-instance 'gtk-window
                              :type :popup
                              :border-width 0))
          (event-box (make-instance 'gtk-event-box
                                    :border-width 1))
          (label (make-instance 'gtk-label
                                :label tip-text)))
      (gtk-container-add event-box label)
      (gtk-container-add win event-box)
      ;; Apply CSS to the label
      (apply-css-to-widget label provider)
      win))

  (defun check-for-tooltip (window textview location)
    (declare (ignore window))
    (let ((start (gtk-text-iter-copy location)))
      (when (gtk-text-iter-find-char start
                                     (lambda (ch) (eq ch #\())
                                     :direction :backward)
        (gtk-text-iter-move start)
        (let* ((word (string-trim " " (gtk-text-iter-text start location)))
               (tip-text (get-tip word)))
          (when tip-text
            (let* ((rect (gtk-text-view-iter-location textview location))
                   (buffer-x (gdk-rectangle-x rect))
                   (buffer-y (gdk-rectangle-y rect))
                   (text-height (gdk-rectangle-height rect))
                   (win (gtk-text-view-window textview :widget)))
              (multiple-value-bind (window-x window-y)
                  (gtk-text-view-buffer-to-window-coords textview
                                                         :widget
                                                         buffer-x
                                                         buffer-y)
                (multiple-value-bind (screen-x screen-y)
                    (gdk-window-origin win)
                  ;; Destroy any previous tool tip window
                  (when tooltip
                    (gtk-widget-destroy tooltip)
                    (setf tooltip nil))
                  ;; Create a new tool tip window
                  (setf tooltip (tip-window-new tip-text))
                  ;; Place it at the calculated position.
                  (gtk-window-move tooltip
                                   (+ window-x screen-x)
                                   (+ window-y screen-y text-height))
                  (gtk-widget-show-all tooltip)))))))))

  (defun example-text-view-tooltip ()
    (within-main-loop
      (let* ((window (make-instance 'gtk-window
                                    :title "Text View Tooltip"
                                    :type :toplevel
                                    :default-width 450
                                    :default-height 200))
             (scrolled (make-instance 'gtk-scrolled-window))
             (textview (make-instance 'gtk-text-view
                                      :top-margin 6
                                      :left-margin 6
                                      :right-margin 6))
             (buffer (gtk-text-view-buffer textview))
             (in-open-brace-p nil)
             (open-brace-count 0))
        (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (when tooltip
                            (gtk-widget-destroy tooltip)
                            (setf tooltip nil))
                          (leave-gtk-main)))
        ;; Signal handler for the text buffer of the text view
        (g-signal-connect buffer "insert-text"
           (lambda (buffer location text len)
             (declare (ignore buffer len))
             (cond ((string= text "(")
                    (setf open-brace-count (1+ open-brace-count))
                    (setf in-open-brace-p t))
                   ((string= text ")")
                    (setf open-brace-count (max 0 (- open-brace-count 1)))
                    (when (= 0 open-brace-count)
                      (setf in-open-brace-p nil))
                    (when tooltip
                      (gtk-widget-destroy tooltip)
                      (setf tooltip nil)))
                   ((and in-open-brace-p (string= " " text))
                    (check-for-tooltip window textview location))
                   (t nil))))
        ;; Pack the widgets
        (gtk-container-add scrolled textview)
        (gtk-container-add window scrolled)
        ;; Load CSS from data into the provider
        (gtk-css-provider-load-from-data provider css-tooltip)
        (apply-css-to-widget textview provider)
        ;; show the window and child widgets
        (gtk-widget-show-all window)))))
