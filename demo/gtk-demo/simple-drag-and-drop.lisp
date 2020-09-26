;;;; Simple Drag and Drop

(in-package #:gtk-demo)

(defun get-image-pixbuf (image)
  (ecase (gtk-image-storage-type image)
    (:pixbuf (gtk-image-pixbuf image))
    (:stock (multiple-value-bind (stock-id size)
                (gtk-image-get-stock image)
              (gtk-widget-render-icon-pixbuf image stock-id size)))))

(defun demo-simple-drag-and-drop ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Simple Drag and Drop"))
          (hgrid (make-instance 'gtk-grid
                                :orientation :horizontal
                                :border-width 8)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Create an event box as the drag source
      (let ((image (gtk-image-new-from-stock "gtk-dialog-warning" :dialog))
            (drag-source (gtk-event-box-new)))
        (gtk-container-add drag-source image)
        (gtk-container-add hgrid drag-source)

        ;; Make drag-source a drag source
        (gtk-drag-source-set drag-source '(:button1-mask) nil '(:copy))
        (gtk-drag-source-add-image-targets drag-source)

        (g-signal-connect drag-source "drag-begin"
           (lambda (widget context)
             (declare (ignore widget))
             (format t "DRAG-BEGIN for drag-source ~A~%" context)
             (let ((pixbuf (get-image-pixbuf image)))
               ;; Sets pixbuf of image as the icon for a given drag
               (gtk-drag-set-icon-pixbuf context pixbuf 0 0))))

        (g-signal-connect drag-source "drag-data-get"
           (lambda (widget context selection-data info time)
             (declare (ignore widget info time))
             (format t "~&DRAG-DATA-GET context = ~A~%" context)
             (let ((pixbuf (get-image-pixbuf image)))
               (if (setf (gtk-selection-data-pixbuf selection-data) pixbuf)
                   (format t "     ~a~%" selection-data)))
             t))

        ;; Create a button as the drag destination
        (let ((drag-dest (make-instance 'gtk-button
                                        :always-show-image t
                                        :label "Drop the image on the Button")))
          (gtk-container-add hgrid drag-dest)

          (gtk-widget-add-events drag-dest '(:all-events-mask))

          ;; accept drops on drag-dest
          (gtk-drag-dest-set drag-dest '(:motion :highlight) nil '(:copy))
          (gtk-drag-dest-add-image-targets drag-dest)

          (g-signal-connect drag-dest "drag-drop"
             (lambda (widget drag-context x y time)
               (declare (ignore x y))
               (format t "~&DRAG-DROP context = ~A~%" drag-context)
               (gtk-drag-data widget drag-context "image/png" time)))

          (g-signal-connect drag-dest "drag-data-received"
            (lambda (widget context x y selection-data info time)
              (declare (ignore x y info time))
              (format t "~&DRAG-DATA-RECEIVED context = ~A~%" context)
              (format t "     ~a~%" selection-data)
              (let* ((pixbuf (gtk-selection-data-pixbuf selection-data))
                     (image (gtk-image-new-from-pixbuf pixbuf)))
                (format t "pixbuf = ~A~%" pixbuf)
                (setf (gtk-button-image widget) image)))))
        (gtk-container-add window hgrid))
      (gtk-widget-show-all window))))

