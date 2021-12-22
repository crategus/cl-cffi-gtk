;;;; Example Progress Bar- 2021-12-22

(in-package :gtk-example)

(defstruct pbar
  widget
  timer
  mode)

(defun progress-bar-timeout (pdata)
  (if (pbar-mode pdata)
      (gtk-progress-bar-pulse (pbar-widget pdata))
      (let ((val (+ (gtk-progress-bar-fraction (pbar-widget pdata))
                    0.01)))
        (when (> val 1.0) (setq val 0.0))
        (setf (gtk-progress-bar-fraction (pbar-widget pdata)) val)))
  +g-source-continue+)

(defun example-progress-bar (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Progress Bar"
                                 :application application
                                 :default-width 320))
          (pdata (make-pbar :widget (make-instance 'gtk-progress-bar)))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :border-width 12
                               :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (g-source-remove (pbar-timer pdata))
                          (setf (pbar-timer pdata) 0)
                          (leave-gtk-main)))
      (let ((provider (gtk-css-provider-new))
            (context (gtk-widget-style-context (pbar-widget pdata)))
            (css-data "progressbar > trough,
                       progressbar > trough > progress {
                           min-height : 24px; }"))
         (gtk-css-provider-load-from-data provider css-data)
         (gtk-style-context-add-provider context
                                         provider
                                         +gtk-style-provider-priority-application+))
      (setf (pbar-timer pdata)
            (g-timeout-add 100
                           (lambda ()
                             (progress-bar-timeout pdata))))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :label
                                         "<b>Progress bar</b>"))
      (gtk-box-pack-start vbox (pbar-widget pdata))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :margin-top 12
                                         :label
                                         "<b>Change properties</b>"))
      (let ((check (gtk-check-button-new-with-label "Show text")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (if (gtk-toggle-button-active check)
                 (setf (gtk-progress-bar-text (pbar-widget pdata))
                       (if (pbar-mode pdata)
                           "Progress bar is in activity mode"
                           "Progress bar is in normal mode"))
                 (setf (gtk-progress-bar-text (pbar-widget pdata)) ""))
             (setf (gtk-progress-bar-show-text (pbar-widget pdata))
                   (gtk-toggle-button-active check))))
        (gtk-box-pack-start vbox check))
      (let ((check (gtk-check-button-new-with-label "Activity mode")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (setf (pbar-mode pdata)
                   (not (pbar-mode pdata)))
             (if (pbar-mode pdata)
                 (progn
                   (gtk-progress-bar-pulse (pbar-widget pdata))
                   (setf (gtk-progress-bar-text (pbar-widget pdata))
                         "Progress bar is in activity mode"))
                 (progn
                   (setf (gtk-progress-bar-text (pbar-widget pdata))
                         "Progress bar is in normal mode")
                   (setf (gtk-progress-bar-fraction (pbar-widget pdata))
                         0.0)))))
        (gtk-box-pack-start vbox check))
      (let ((check (gtk-check-button-new-with-label "Inverted")))
        (g-signal-connect check "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (setf (gtk-progress-bar-inverted (pbar-widget pdata))
                   (gtk-toggle-button-active check))))
        (gtk-box-pack-start vbox check))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
