(in-package :gtk-example)

(defun create-file-chooser-dialog-open (window)
  (let ((dialog (gtk-file-chooser-dialog-new "Open File"
                                             window
                                             :open
                                             "gtk-cancel" :cancel
                                             "gtk-open" :accept)))
    (if (eq (gtk-dialog-run dialog)
            (foreign-enum-value 'gtk-response-type :accept))
      (let ((filename (gtk-file-chooser-filename dialog)))
        (format t "~& OPEN THE FILE ~A~%" filename)
      ))

    (gtk-widget-destroy dialog)))

(defun create-file-chooser-dialog-save (window filename)
  (let ((dialog (gtk-file-chooser-dialog-new "Save File"
                                             window
                                             :save
                                             "gtk-cancel" :cancel
                                             "gtk-save" :accept)))
    (setf (gtk-file-chooser-do-overwrite-confirmation dialog) t)

    (if filename
        (setf (gtk-file-chooser-filename dialog) filename)
        (setf (gtk-file-chooser-current-name dialog) "Untitled document"))

    (if (eq (gtk-dialog-run dialog)
            (foreign-enum-value 'gtk-response-type :accept))
      (let ((filename (gtk-file-chooser-filename dialog)))
        (format t "~& SAVW TO FILE ~A~%" filename)
      ))

    (gtk-widget-destroy dialog)))


(defun example-file-chooser-dialog ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example File Chooser Dialog"
                                 :default-width 250
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window vbox)
      (let ((button (make-instance 'gtk-button
                                   :label "Open a File")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the dialog
             (create-file-chooser-dialog-open window))))
      (let ((button (make-instance 'gtk-button
                                   :label "Save a File")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the message dialog
             (create-file-chooser-dialog-save window nil))))
      ;; Create a quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-widget-show-all window))))

