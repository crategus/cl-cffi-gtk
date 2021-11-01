;;;; Assistant - 2021-11-1
;;;;
;;;; Demonstrates a sample multi-step assistant. Assistants are used to divide
;;;; an operation into several simpler sequential steps, and to guide the user
;;;; through these steps.

(in-package #:gtk-example)

(defun apply-changes-gradually (assistant pbar)
  (let ((fraction (+ 0.025d0 (gtk-progress-bar-fraction pbar))))
    (cond ((< fraction 1.0d0)
           (setf (gtk-progress-bar-fraction pbar) fraction)
           +g-source-continue+)
          (t
           (gtk-widget-destroy assistant)
           +g-source-remove+))))

(defun create-page1 (assistant)
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
                            :spacing 12
                            :border-width 12))
        (label (make-instance 'gtk-label
                              :use-markup t
                              :label "<b>Fill out the entry to continue:</b>"))
        ;; TODO: The entry widget expands horizontally. Why?
        (entry (make-instance 'gtk-entry
                              :activate-defaults t)))
    ;; Mark the page complete if we have text in the entry
    (g-signal-connect entry "changed"
        (lambda (widget)
          (let* ((num (gtk-assistant-current-page assistant))
                 (page (gtk-assistant-nth-page assistant num)))
            (if (> (length (gtk-entry-text widget)) 0)
                (setf (gtk-assistant-page-complete assistant page) t)
                (setf (gtk-assistant-page-complete assistant page) nil)))))
    ;; Append the first page to the assistant
    (gtk-assistant-append-page assistant box)
    (setf (gtk-assistant-page-title assistant box) "Page 1")
    (setf (gtk-assistant-page-type assistant box) :intro)
    ;; Pack and show the widgets
    (gtk-box-pack-start box label :expand nil :fill nil)
    (gtk-box-pack-start box entry :expand nil :fill nil)
    (gtk-widget-show-all box)))

(defun create-page2 (assistant)
  (let ((box (make-instance 'gtk-box
                            :orientation :vertical
                            :spacing 12
                            :border-width 12))
        (check (make-instance 'gtk-check-button
                              :label
                              "<b>Optional data, you may continue.</b>")))
    ;; Use markup for the label inside the check button
    (setf (gtk-label-use-markup (gtk-bin-child check)) t)
    ;; Append the second page to the assistant
    (gtk-assistant-append-page assistant box)
    (setf (gtk-assistant-page-complete assistant box) t)
    (setf (gtk-assistant-page-title assistant box) "Page 2")
    ;; Pack and show the widgets
    (gtk-box-pack-start box check :expand nil :fill nil)
    (gtk-widget-show-all box)))

(defun create-page3 (assistant)
  (let ((label (make-instance 'gtk-label
                              :use-markup t
                              :label
                               "<b>Confirmation page, press Apply</b>")))
    ;; Append the third page to the assistant
    (gtk-assistant-append-page assistant label)
    (setf (gtk-assistant-page-title assistant label) "Confirmation")
    (setf (gtk-assistant-page-type assistant label) :confirm)
    (setf (gtk-assistant-page-complete assistant label) t)
    ;; Show the widget
    (gtk-widget-show label)))

(defun create-page4 (assistant pbar)
  ;; Append the fourth page to the assistant
  (gtk-assistant-append-page assistant pbar)
  (setf (gtk-assistant-page-type assistant pbar) :progress)
  (setf (gtk-assistant-page-title assistant pbar) "Applying changes")
  ;; Prevent the assistant from being closed while applying changes.
  (setf (gtk-assistant-page-complete assistant pbar) nil)
  ;; Show the widget
  (gtk-widget-show pbar))

(defun example-assistant ()
  (within-main-loop
    (let* ((assistant (make-instance 'gtk-assistant
                                     ;; TODO: Always shows a header bar!?
                                     :use-header-bar -1
                                     :default-height 300))
           (pbar (make-instance 'gtk-progress-bar
                                :halign :center
                                :valign :center))
           (provider (gtk-css-provider-new))
           (context (gtk-widget-style-context pbar))
           (css-data "progressbar > trough,
                      progressbar > trough > progress {
                          min-height : 24px; }"))
      ;; Change the appearance of the progress bar
      (gtk-css-provider-load-from-data provider css-data)
      (gtk-style-context-add-provider context
                                      provider
                                      +gtk-style-provider-priority-application+)
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
          (let ((page (gtk-assistant-current-page assistant))
                (n-pages (gtk-assistant-n-pages assistant)))
            (setf (gtk-window-title assistant)
                  (format nil "Sample assistant (~A of ~A)"
                              (+ page 1)
                              n-pages))
            ;; The fourth page (counting from zero) is the progress page. The
            ;; user clicked Apply to get here so we tell the assistant to
            ;; commit, which means the changes up to this point are permanent
            ;; and cannot be cancelled or revisited.
            (when (= 3 page)
              (gtk-assistant-commit assistant)))))
      (g-signal-connect assistant "apply"
         (lambda (assistant)
           ;; Start a timer to simulate changes taking a few seconds to apply.
           (g-timeout-add 150
                          (lambda ()
                            (apply-changes-gradually assistant pbar)))))
      ;; Create the pages for the assistant
      (create-page1 assistant)
      (create-page2 assistant)
      (create-page3 assistant)
      (create-page4 assistant pbar)
      ;; Show the assistant.
      (gtk-widget-show-all assistant))))
