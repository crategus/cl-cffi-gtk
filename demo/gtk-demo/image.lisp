;;;; GtkImage
;;;;
;;;; GtkImage is used to display an image; the image can be in a number of
;;;; formats. Typically, you load an image into a GdkPixbuf, then display the
;;;; pixbuf.
;;;;
;;;; This demo code shows some of the more obscure cases, in the simple case a
;;;; call to the function gtk-image-new-from-file is all you need.

(in-package #:gtk-demo)

(let ((load-timeout nil)
      (pixbuf-loader nil)
      (image-stream nil))

(defun progressive-timeout (image)
  (if image-stream
      (let* ((buffer (make-array 512 :element-type '(unsigned-byte 8)))
             (len (read-sequence buffer image-stream)))
        (if (= 0 len)
            ;; We have reached the end of the file.
            (progn
              (close image-stream)
              (setf image-stream nil)
              (gdk-pixbuf-loader-close pixbuf-loader)
              (setf pixbuf-loader nil)
              (return-from progressive-timeout +g-source-remove+))
            ;; Load the buffer into GdkPixbufLoader
            (gdk-pixbuf-loader-write pixbuf-loader buffer 512)))
      (progn
        ;; Create the image stream and the GdkPixbufLoader
        (setf image-stream
              (open (rel-path "alphatest.png") :element-type '(unsigned-byte 8)))
        (when pixbuf-loader
          (gdk-pixbuf-loader-close pixbuf-loader)
          (setf pixbuf-loader nil))
        (setf pixbuf-loader (gdk-pixbuf-loader-new))
        (g-signal-connect pixbuf-loader "area-prepared"
           (lambda (loader)
             (let ((pixbuf (gdk-pixbuf-loader-get-pixbuf loader)))
               (gdk-pixbuf-fill pixbuf #xaaaaaaff)
               (gtk-image-set-from-pixbuf image pixbuf))))
        (g-signal-connect pixbuf-loader "area-updated"
           (lambda (loader x y width height)
             (declare (ignore loader x y width height))
             ;; We know the pixbuf inside the GtkImage has changed, but the
             ;; image itself does not know this; so queue a redraw. If we wanted
             ;; to be really efficient, we could use a drawing area or something
             ;; instead of a GtkImage, so we could control the exact position of
             ;; the pixbuf on the display, then we could queue a draw for only
             ;; the updated area of the image.
             (gtk-widget-queue-draw image)))))
  ;; Continue the GSource
  +g-source-continue+)

(defun demo-image ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Images"
                                  :border-width 12
                                  :default-width 300))
           (vgrid (make-instance 'gtk-grid
                                 :orientation :vertical
                                 :border-width 8)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Destroy the load-timeout source
                          (when load-timeout
                            (g-source-remove load-timeout)
                            (setf load-timeout nil))
                          ;; Close the GdkPixbufLoader object
                          (when pixbuf-loader
                            (gdk-pixbuf-loader-close pixbuf-loader)
                            (setf pixbuf-loader nil))
                          ;; Close open input stream
                          (when image-stream
                            (close image-stream)
                            (setf image-stream nil))
                          (leave-gtk-main)))

      ;; Image loaded from a file
      (let* ((label (make-instance 'gtk-label
                                   :margin-bottom 3
                                   :use-markup t
                                   :label
                                   "<b>Image loaded from a file</b>"))
             (frame (make-instance 'gtk-frame
                                   :shadow-type :in))
             (pixbuf (gdk-pixbuf-new-from-file (rel-path "gtk-logo-old.png")))
             (image (gtk-image-new-from-pixbuf pixbuf)))
        (gtk-container-add vgrid label)
        (gtk-container-add frame image)
        (gtk-container-add vgrid frame))

      ;; Animation loaded from a file
      (let* ((label (make-instance 'gtk-label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Animation loaded from a file</b>"))
             (frame (make-instance 'gtk-frame
                                   :shadow-type :in))
             (image (gtk-image-new-from-file (rel-path "floppybuddy.gif"))))
        (gtk-container-add vgrid label)
        (gtk-container-add frame image)
        (gtk-container-add vgrid frame))

      ;; Symbolic icon
      (let* ((label (make-instance 'gtk-label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Symbolic themed icon</b>"))
             (frame (make-instance 'gtk-frame
                                   :shadow-type :in))
             (gicon (g-themed-icon-new-with-default-fallbacks
                        "battery-caution-charging-symbolic"))
             (image (gtk-image-new-from-gicon gicon :dialog)))
        (gtk-container-add vgrid label)
        (gtk-container-add frame image)
        (gtk-container-add vgrid frame))

      ;; Progressive
      (let* ((label (make-instance 'gtk-label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Progressive image loading</b>"))
             (frame (make-instance 'gtk-frame
                                   :shadow-type :in))
             (event-box (make-instance 'gtk-event-box))
             ;; Create an empty image for now; the progressive loader
             ;; will create the pixbuf and fill it in.
             (image (gtk-image-new-from-pixbuf nil)))

        ;; start_progressive_loading
        ;; This is obviously totally contrived (we slow down loading
        ;; on purpose to show how incremental loading works).
        ;; The real purpose of incremental loading is the case where
        ;; you are reading data from a slow source such as the network.
        ;; The timeout simply simulates a slow data source by inserting
        ;; pauses in the reading process.
        (setf load-timeout
              (gdk-threads-add-timeout 100
                                       (lambda ()
                                         (progressive-timeout image))))

        ;; Restart loading the image from the file
        (g-signal-connect event-box "button-press-event"
           (lambda (event-box event)
             (format t "Event Box ~A clicked at (~A, ~A)~%"
                       event-box
                       (gdk-event-button-x event)
                       (gdk-event-button-y event))
             (setf load-timeout
                   (gdk-threads-add-timeout 100
                                            (lambda ()
                                              (progressive-timeout image))))))
        (gtk-container-add vgrid label)
        (gtk-container-add event-box image)
        (gtk-container-add frame event-box)
        (gtk-container-add vgrid frame))

      ;; Sensitivity control
      (let ((button (make-instance 'gtk-toggle-button
                                   :margin-top 12
                                   :label "Insensitive")))
        (g-signal-connect button "toggled"
           (lambda (widget)
             (declare (ignore widget))
             (let ((childs (gtk-container-children vgrid)))
               (dolist (child childs)
                 (unless (g-type-is-a (g-object-type child) "GtkToggleButton")
                   (if (gtk-toggle-button-active button)
                       (progn
                         (setf (gtk-widget-sensitive child) nil)
                         (setf (gtk-button-label button) "Sensitive"))
                       (progn
                         (setf (gtk-widget-sensitive child) t)
                         (setf (gtk-button-label button) "Insensitve"))))))))
        (gtk-container-add vgrid button))

      (gtk-container-add window vgrid)
      (gtk-widget-show-all window)))))

