;;;; Retrieving Selections

(in-package #:gtk-demo)

(defun selection-received (widget selection-data time)
  (declare (ignore widget time))
  (format t "Event 'selection-received' event: ~A~%" selection-data)
  (cond ((< (gtk-selection-data-get-length selection-data) 0)
         (format t "Selection retrieval failed.~%"))
        ((not (equal (gtk-selection-data-get-data-type selection-data)
                     +gdk-selection-type-atom+))
         (format t "Selection 'Targets' was not returned as atoms.~%"))
        (t
          (format t "All is fine: ~A~%" (gtk-selection-data-get-data selection-data))
          (format t " targets = ~A~%" (gtk-selection-data-get-targets selection-data))))

  )


(defun demo-selections-1 ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :title "Retrieving Selections"
                                 :type :toplevel
                                 :border-width 12))
          (button (make-instance 'gtk-button
                                 :label "Get Targets")))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Signal hanlder for the button "Get Targets"
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (format t "Event 'clicked' for button~%")
           ;; Request the "TARGETS" target for the primary selection
           (gtk-selection-convert window
                                  "PRIMARY"
                                  "TARGETS"
                                  +gdk-current-time+)))

      (g-signal-connect window "selection-received" #'selection-received)

      (gtk-container-add window button)

      ;; Show the window.
      (gtk-widget-show-all window))))

