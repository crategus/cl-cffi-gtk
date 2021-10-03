;;;; Drag and Drop with Action - 2021-9-25

(in-package #:gtk-example)

(defun example-drag-and-drop-action ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :border-width 12
                                 :title "Drag and Drop with Action"))
          (grid (make-instance 'gtk-grid
                               :orientation :horizontal
                               :border-width 8))
          ;; FIXME: A global to pass the selection data.
          (selection nil))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Create an event box with an image as the drag source
      (let ((source (gtk-event-box-new))
            (image (gtk-image-new-from-icon-name "dialog-question" :dialog)))

        ;; Make source a drag source
        (gtk-drag-source-set source '(:button1-mask) nil '(:ask))
        (gtk-drag-source-add-image-targets source)

        (g-signal-connect source "drag-begin"
            (lambda (widget context)
              (declare (ignore widget))
              (format t "DRAG-BEGIN ~a~%" context)
              (let ((pixbuf (get-pixbuf-from-image image)))
                ;; Set pixbuf of image as the icon for the drag
                (gtk-drag-set-icon-pixbuf context pixbuf 0 0))
              ;; Return value not documented
              nil))

        (g-signal-connect source "drag-data-get"
            (lambda (widget context data info time)
              (declare (ignore widget info time))
              (format t "~&DRAG-DATA-GET ~a~%" context)
              (let ((pixbuf (get-pixbuf-from-image image)))
                (when (setf (gtk-selection-data-pixbuf data) pixbuf)
                  ;; FIXME: Copy the selection data into global the SELECTION.
                  ;; This is a workaround to pass the pixbuf to the signal
                  ;; handler "drag-date-received".
                  (setf selection (gtk-selection-data-copy data))))
              nil))

        ;; Pack the widgets in the grid
        (gtk-container-add source image)
        (gtk-container-add grid source))

      ;; Create a button as the drag destination
      (let ((dest (make-instance 'gtk-button
                                 :always-show-image t
                                 :border-width 18
                                 :height-request 96
                                 :width-request 196)))

        ;; Accept drops on dest
        (gtk-widget-add-events dest '(:all-events-mask))
        (gtk-drag-dest-set dest '(:motion :highlight) nil '(:ask :copy))
        (gtk-drag-dest-add-image-targets dest)

        (g-signal-connect dest "drag-drop"
           (lambda (widget context x y time)
             (declare (ignore x y))
             (format t "~&DRAG-DROP ~a~%" context)
             (gtk-drag-data widget context "image/png" time)
             ;; Return true for successful drop
             t))

        (g-signal-connect dest "drag-data-received"
          (lambda (widget context x y data info time)
            (declare (ignore x y info))
            (format t "~&DRAG-DATA-RECEIVED ~a~%" context)
            (setf data (gtk-selection-data-copy selection))
            (let* ((pixbuf (gtk-selection-data-pixbuf data))
                   (image (gtk-image-new-from-pixbuf pixbuf))
                   (action (gdk-drag-context-selected-action context)))
              (format t "     ~a~%" data)
              (format t "   data : ~a~%" (gtk-selection-data-data selection))
              (format t " pixbuf : ~a~%" pixbuf)
              (format t " action : ~a~%" action)
              (cond ((and pixbuf (member :ask action))
                     (let* ((dialog (make-instance 'gtk-message-dialog
                                                   :message-type :info
                                                   :buttons :ok-cancel
                                                   :text "Confirm Drag and Drop"))
                            (response (gtk-dialog-run dialog)))
                      (gtk-widget-destroy dialog)
                      (format t "  response : ~a~%" response)

                      (if (eq :ok response)
                          (progn
                            (format t "  Drag is sucessfull.~%")
                            (setf (gtk-button-image widget) image)
                            (gtk-drag-finish context t nil time))
                          (progn
                            (format t "  Drag is canceled.~%")
                            (gtk-drag-finish context nil nil time)))
                    ))
                    (t
                     (gtk-drag-finish context nil nil time)))

            )
            ;; Return value not documented
            nil))

        ;; Pack widgets in the grid
        (gtk-container-add grid dest))

      ;; Pack and show the widgets
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
