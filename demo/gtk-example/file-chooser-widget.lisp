;;;; Example File Chooser Widget (2021-6-6)
;;;;
;;;; In this example the options "Local only", "Select Multiple" and
;;;; "Show hidden" can be toggled with check buttons which are put in an extra
;;;; widget for the file chooser.

(in-package :gtk-example)

(defun create-file-chooser-widget ()
  (let ((response nil)
        (chooser (gtk-file-chooser-dialog-new "Example File Chooser Widget"
                                              nil
                                              :open
                                              "gtk-open" :accept
                                              "gtk-cancel" :cancel))
        (extra-widget (make-instance 'gtk-box
                                     :orientation :horizontal
                                     :spacing 12))
        (local-only (gtk-check-button-new-with-label "Local only"))
        (select-multiple (gtk-check-button-new-with-label "Select Multiple"))
        (show-hidden (gtk-check-button-new-with-label "Show hidden")))
    ;; Connect signal handlers to the toggle buttons
    (g-signal-connect local-only "toggled"
                      (lambda (button)
                        (setf (gtk-file-chooser-local-only chooser)
                              (gtk-toggle-button-active button))))
    (g-signal-connect select-multiple "toggled"
                      (lambda (button)
                        (setf (gtk-file-chooser-select-multiple chooser)
                              (gtk-toggle-button-active button))))
    (g-signal-connect show-hidden "toggled"
                      (lambda (button)
                        (setf (gtk-file-chooser-show-hidden chooser)
                              (gtk-toggle-button-active button))))
    ;; Put the extra widgets in a box
    (gtk-box-pack-start extra-widget local-only)
    (setf (gtk-toggle-button-active local-only) t) ; default is true
    (gtk-box-pack-start extra-widget select-multiple)
    (gtk-box-pack-start extra-widget show-hidden)
    (setf (gtk-file-chooser-extra-widget chooser) extra-widget)
    ;; Show the extra widgets
    (gtk-widget-show-all extra-widget)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk-dialog-run chooser)))
      (format t "Open file ~A~%"
                (gtk-file-chooser-filename chooser)))
    (gtk-widget-destroy chooser)
    response))
