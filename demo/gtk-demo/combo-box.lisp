;;;; Combo Box

(defun example-combo-box ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :border-width 12
                                  :title "Example Combo Box"))
           (model (make-instance 'gtk-list-store
                                 :column-types '("gchararray" "gint")))
           (combo-box (make-instance 'gtk-combo-box :model model))
           (title-label (make-instance 'gtk-label :label "Title:"))
           (value-label (make-instance 'gtk-label :label "Value:"))
           (title-entry (make-instance 'gtk-entry))
           (value-entry (make-instance 'gtk-entry))
           (button (make-instance 'gtk-button :label "Add"))
           (table (make-instance 'gtk-table
                                 :n-rows 3
                                 :n-columns 3)))
      ;; Fill in data into the columns
      (gtk-list-store-set model (gtk-list-store-append model) "Monday" 1)
      (gtk-list-store-set model (gtk-list-store-append model) "Tuesday" 2)
      (gtk-list-store-set model (gtk-list-store-append model) "Wednesday" 3)
      (gtk-list-store-set model (gtk-list-store-append model) "Thursday" 4)
      (gtk-list-store-set model (gtk-list-store-append model) "Friday" 5)
      (gtk-list-store-set model (gtk-list-store-append model) "Saturday" 6)
      (gtk-list-store-set model (gtk-list-store-append model) "Sunday" 7)
      ;; Set the first entry to active
      (gtk-combo-box-set-active combo-box 0)
      ;; Define the signal handlers
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (leave-gtk-main)))
      (g-signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (gtk-list-store-set model
                               (gtk-list-store-append model)
                               (gtk-entry-text title-entry)
                               (or (parse-integer (gtk-entry-text value-entry)
                                                  :junk-allowed t)
                                   0))))
      (g-signal-connect combo-box "changed"
         (lambda (widget)
           (declare (ignore widget))
           (let ((dialog (make-instance 'gtk-message-dialog
                                        :message-type :info
                                        :buttons :ok
                                        :text "Info Message Dialog"
                                        :secondary-text
                                        (format nil "You clicked on row ~A~%"
                                                (gtk-combo-box-get-active combo-box)))))
             ;; Run the message dialog
             (gtk-dialog-run dialog)
             ;; Destroy the message dialog
             (gtk-widget-destroy dialog))))
      ;; Create renderers for the cells
      (let ((renderer (make-instance 'gtk-cell-renderer-text
                                     :text "A text")))
        (gtk-cell-layout-pack-start combo-box renderer :expand t)
        (gtk-cell-layout-add-attribute combo-box renderer "text" 0))
      (let ((renderer (make-instance 'gtk-cell-renderer-text
                                     :text "A number")))
        (gtk-cell-layout-pack-start combo-box renderer :expand nil)
        (gtk-cell-layout-add-attribute combo-box renderer "text" 1))
      ;; Align the labels
      (gtk-misc-set-alignment title-label 0.0 0.0)
      (gtk-misc-set-alignment value-label 0.0 0.0)
      ;; Put the widgets into the table
      (gtk-table-attach table title-label 0 1 0 1)
      (gtk-table-attach table value-label 1 2 0 1)
      (gtk-table-attach table title-entry 0 1 1 2)
      (gtk-table-attach table value-entry 1 2 1 2)
      (gtk-table-attach table button      2 3 1 2)
      (gtk-table-attach table combo-box   0 3 2 3)
      ;; Put the table into the window
      (gtk-container-add window table)
      ;; Show the window
      (gtk-widget-show-all window))))

