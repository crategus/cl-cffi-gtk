;;;; Examples for GtkDialog

(in-package :gtk-example)

(defun create-dialog ()
  (let ((dialog (make-instance 'gtk-dialog
                               :title "Dialog Window"
                               :has-separator t)))
    ;; Add a border width to the vbox of the content area
    (setf (gtk-container-border-width (gtk-dialog-content-area dialog)) 12)
    ;; Add a label widget with text to the content area
    (let ((vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :border-width 12))
          (label (make-instance 'gtk-label
                                :wrap t
                                :label
                                (format nil
                                        "The content area is the place to ~
                                         put in the widgets.~%~% ~
                                         The action area contains ~
                                         the buttons."))))
      (gtk-box-pack-start vbox label)
      (gtk-box-pack-start (gtk-dialog-content-area dialog) vbox)
      ;; Show the content area of the dialog
      (gtk-widget-show-all (gtk-dialog-content-area dialog)))
    ;; Add buttons with a stock id to the action area
    (gtk-dialog-add-button dialog "gtk-yes" :yes)
    (gtk-dialog-add-button dialog "gtk-no" :no)
    (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
    (gtk-dialog-set-default-response dialog :cancel)
    ;; Create a button
    (let ((button (gtk-button-new-with-label "User Button")))
      (gtk-container-add (gtk-dialog-action-area dialog) button)
      (gtk-widget-show button)
      (g-signal-connect button "clicked"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (gtk-dialog-response dialog 9))))
    ;; Run the dialog and print the message on the console
    (format t "Response was: ~S~%" (gtk-dialog-run dialog))
    ;; Destroy the dialog
    (gtk-widget-destroy dialog)))

(defun example-dialog ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Dialog"
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
                                   :label "Open a Dialog Window")))
        (gtk-box-pack-start vbox button)
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the dialog
             (create-dialog))))
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-separator
                                         :orientation :horizontal))
      ;; Create a quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button))
      (gtk-widget-show-all window))))


(defun example-dialog-new ()
  (let ((response))
    (within-main-loop
      (let ((dialog (gtk-dialog-new)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (gtk-widget-destroy dialog)))
        ;; Set a title and a size for the window.
        (setf (gtk-window-title dialog) "Example Dialog New")
        (setf (gtk-window-default-size dialog) '(300 200))
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))

(defun example-dialog-new-with-buttons ()
  (let ((response))
    (within-main-loop
      (let ((dialog (gtk-dialog-new-with-buttons "Example Dialog New with Buttons"
                                                 nil
                                                 '(:modal)
                                                 "gtk-ok" :accept
                                                 "gtk-cancel" :reject)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (gtk-widget-destroy dialog)))
        ;; Set a size for the window.
        (setf (gtk-window-default-size dialog) '(350 200))
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))

(defun example-dialog-ui ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-from-file builder (sys-path "dialog.ui"))
      (let ((dialog (gtk-builder-object builder "dialog")))
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
      (gtk-widget-show-all dialog)))))

