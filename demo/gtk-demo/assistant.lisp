;;;; Assistant
;;;;
;;;; Demonstrates a sample multi-step assistant. Assistants are used to divide
;;;; an operation into several simpler sequential steps, and to guide the user
;;;; through these steps.

(let ((assistant nil)
      (progress-bar nil))

  (defun apply-changes-gradually ()
    (let ((fraction (+ 0.025d0 (gtk-progress-bar-fraction progress-bar)))
          (return-value nil))
      (cond ((< fraction 1.0d0)
             (setf (gtk-progress-bar-fraction progress-bar) fraction)
             (setf return-value t))
            (t
             (gtk-widget-destroy assistant)
             (setf return-value nil)))
      return-value))

  (defun create-page-1 (assistant)
    (let ((box (make-instance 'gtk-box
                              :orientation :horizontal
                              :spacing 12
                              :border-width 12))
          (label (gtk-label-new "You must fill out this entry to continue:"))
          (entry (make-instance 'gtk-entry
                                :activate-defaults t)))
      (g-signal-connect entry "changed"
         (lambda (widget)
           (let* ((page-number (gtk-assistant-get-current-page assistant))
                  (current-page (gtk-assistant-get-nth-page assistant page-number))
                  (text (gtk-entry-text widget)))
             (if text
                 (gtk-assistant-set-page-complete assistant current-page t)
                 (gtk-assistant-set-page-complete assistant current-page nil)))))

      (gtk-box-pack-start box label :expand nil :fill nil :padding 0)
      (gtk-box-pack-start box entry :expand t   :fill t   :padding 0)

      (gtk-widget-show-all box)
      (gtk-assistant-append-page assistant box)
      (gtk-assistant-set-page-title assistant box "Page 1")
      (gtk-assistant-set-page-type assistant box :intro)))

  (defun create-page-2 (assistant)
    (let ((box (make-instance 'gtk-box
                              :orientation :vertical
                              :spacing 12
                              :border-width 12))
          (checkbutton (make-instance 'gtk-check-button
                                      :label
                                      (format nil
                                        "This is optonal data, you may continue ~
                                         even if yout do not check this"))))
      (gtk-box-pack-start box checkbutton :expand nil :fill nil :padding 0)
      (gtk-widget-show-all box)
      (gtk-assistant-append-page assistant box)
      (gtk-assistant-set-page-complete assistant box t)
      (gtk-assistant-set-page-title assistant box "Page 2")))

  (defun create-page-3 (assistant)
    (let ((label (gtk-label-new "This is a confirmation page, press 'Apply' to apply changes")))
      (gtk-widget-show label)
      (gtk-assistant-append-page assistant label)
      (gtk-assistant-set-page-type assistant label :confirm)
      (gtk-assistant-set-page-complete assistant label t)
      (gtk-assistant-set-page-title assistant label "Confirmation")))

  (defun create-page-4 (assistant)
    (setf progress-bar
          (make-instance 'gtk-progress-bar :halign :center :valign :center))
    (gtk-widget-show progress-bar)
    (gtk-assistant-append-page assistant progress-bar)
    (gtk-assistant-set-page-type assistant progress-bar :progress)
    (gtk-assistant-set-page-title assistant progress-bar "Applying changes")
    ;; This prevents the assistant window from being closed while we are
    ;; busy applying changes.
    (gtk-assistant-set-page-complete assistant progress-bar nil))

  (defun demo-assistant ()
    (within-main-loop
      (setf assistant (make-instance 'gtk-assistant))
      ;; Signal handlers for the assistant
      (g-signal-connect assistant "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect assistant "close"
                        (lambda (widget)
                          (gtk-widget-destroy widget)))
      (g-signal-connect assistant "cancel"
                        (lambda (widget)
                          (gtk-widget-destroy widget)))
      (g-signal-connect assistant "prepare"
        (lambda (assistant page)
          (declare (ignore page))
          (let ((current-page (gtk-assistant-get-current-page assistant))
                (n-pages (gtk-assistant-get-n-pages assistant)))
            (setf (gtk-window-title assistant)
                  (format nil "Sample assistant (~A of ~A)"
                              (+ current-page 1)
                              n-pages))
            ;; The fourth page (counting from zero) is the progress page. The
            ;; user clicked Apply to get here so we tell the assistant to
            ;; commit, which means the changes up to this point are permanent
            ;; and cannot be cancelled or revisited.
            (when (eql current-page 3)
              (gtk-assistant-commit assistant)))))
      (g-signal-connect assistant "apply"
         (lambda (assistant)
           (declare (ignore assistant))
           ;; Start a timer to simulate changes taking a few seconds to apply.
           (g-timeout-add 150 #'apply-changes-gradually)))

      ;; Create the pages for the assistant
      (create-page-1 assistant)
      (create-page-2 assistant)
      (create-page-3 assistant)
      (create-page-4 assistant)

      ;; Show the assistant.
      (gtk-widget-show-all assistant))))

