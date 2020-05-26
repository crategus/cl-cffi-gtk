(in-package #:gtk-demo)

(defun demo-dialog-alternative-order (message)
 (let ((response nil))
   (within-main-loop
    (let (;; Create the widgets
          (dialog (gtk-dialog-new-with-buttons "Demo Toplevel Dialog"
                                               nil ; No Parent window
                                               '(:modal)))
          (label (gtk-label-new message)))
      ;; Signal handler for the dialog to handle the signal "destroy".
      (g-signal-connect dialog "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Quit the main loop and destroy the thread
                          (leave-gtk-main)))
      ;; Ensure that the dialog window is destroyed when the user responds.
      (g-signal-connect dialog "response"
                        (lambda (dialog response-id)
                          (setf response response-id)
                          (gtk-widget-destroy dialog)))
      ;; Add the buttons
      (gtk-dialog-add-button dialog "gtk-cancel" :cancel)
      (gtk-dialog-add-button dialog "gtk-ok" :ok)
      (gtk-dialog-add-button dialog "gtk-apply" :apply)
      ;; Set the default button.
      (gtk-widget-grab-default (gtk-dialog-widget-for-response dialog :ok))
      ;; Allow alternative button order. (The default is nil.)
      (setf (gtk-settings-gtk-alternative-button-order
              (gtk-settings-get-default))
            t)
      ;; Set the alternative button order.
      (gtk-dialog-set-alternative-button-order dialog '(:apply :ok :cancel))
      ;; Add the label, and show everything we have added to the dialog.
      (gtk-container-add (gtk-dialog-content-area dialog) label)
      (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (when response
      (format t "The response ID is ~A" response))))

#|
 cancel_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                        GTK_STOCK_CANCEL,
                                        GTK_RESPONSE_CANCEL);

 ok_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                    GTK_STOCK_OK,
                                    GTK_RESPONSE_OK);

 gtk_widget_grab_default (ok_button);

 help_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                      GTK_STOCK_HELP,
                                      GTK_RESPONSE_HELP);

 gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                          GTK_RESPONSE_OK,
                                          GTK_RESPONSE_CANCEL,
                                          GTK_RESPONSE_HELP,
                                          -1);
|#
