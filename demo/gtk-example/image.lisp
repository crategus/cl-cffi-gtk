;;;; Example Image Widgets - 2021-12-21
;;;;
;;;; GtkImage is used to display an image. The image can be in a number of
;;;; formats. Typically, you load an image into a GdkPixbuf, then display the
;;;; pixbuf.
;;;;
;;;; This demo code shows some of the more obscure cases, in the simple case a
;;;; call to the function gtk-image-new-from-file is all you need.

(in-package :gtk-example)

(let ((pixbuf-loader nil)
      (image-stream nil))

  (defun progressive-timeout (image)
    (if image-stream
        (let* ((buffer (make-array 128 :element-type '(unsigned-byte 8)))
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
              (gdk-pixbuf-loader-write pixbuf-loader buffer 128)))
        (progn
          ;; Create the image stream and the GdkPixbufLoader
          (setf image-stream
                (open (sys-path "alphatest.png")
                      :element-type '(unsigned-byte 8)))
          (when pixbuf-loader
            (gdk-pixbuf-loader-close pixbuf-loader)
            (setf pixbuf-loader nil))
          (setf pixbuf-loader (gdk-pixbuf-loader-new))
          (g-signal-connect pixbuf-loader "area-prepared"
             (lambda (loader)
               (let ((pixbuf (gdk-pixbuf-loader-pixbuf loader)))
                 (gdk-pixbuf-fill pixbuf #xaaaaaaff)
                 (gtk-image-set-from-pixbuf image pixbuf))))
          (g-signal-connect pixbuf-loader "area-updated"
             (lambda (loader x y width height)
               (declare (ignore loader x y width height))
               ;; We know the pixbuf inside the GtkImage has changed, but the
               ;; image itself does not know this. So give it a hint by setting
               ;; the pixbuf again. Queuing a redraw used to be sufficient, but
               ;; nowadays GtkImage uses GtkIconHelper which caches the pixbuf
               ;; state and will just redraw from the cache.
               (let ((pixbuf (gtk-image-pixbuf image)))
                 (gtk-image-set-from-pixbuf image pixbuf))))))
    ;; Continue the GSource
    +g-source-continue+)

  (defun example-image (&optional application)
    (within-main-loop
      (let* ((timeout nil)
             (window (make-instance 'gtk-window
                                    :type :toplevel
                                    :application application
                                    :title "Example Image Widgets"
                                    :default-width 320
                                    :border-width 12))
             (vgrid (make-instance 'gtk-grid
                                   :orientation :vertical
                                   :row-spacing 6)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            ;; Destroy the timeout source
                            (when timeout
                              (g-source-remove timeout)
                              (setf timeout nil))
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
                                     :margin-top 9
                                     :margin-bottom 6
                                     :use-markup t
                                     :label
                                     "<b>Image loaded from a file</b>"))
               (pixbuf (gdk-pixbuf-new-from-file (sys-path "gtk3-demo.png")))
               (image (gtk-image-new-from-pixbuf pixbuf)))
          (gtk-container-add vgrid label)
          (gtk-container-add vgrid image))
        ;; Animation loaded from a file
        (let* ((label (make-instance 'gtk-label
                                     :margin-top 9
                                     :margin-bottom 6
                                     :use-markup t
                                     :label
                                     "<b>Animation loaded from a file</b>"))
               (image (gtk-image-new-from-file (sys-path "spinner.gif"))))
          (gtk-container-add vgrid label)
          (gtk-container-add vgrid image))
        ;; Symbolic icon
        (let* ((label (make-instance 'gtk-label
                                     :margin-top 9
                                     :margin-bottom 6
                                     :use-markup t
                                     :label
                                     "<b>Symbolic themed icon</b>"))
               (gicon (g-themed-icon-new-with-default-fallbacks
                          "battery-caution-charging-symbolic"))
               (image (gtk-image-new-from-gicon gicon :dialog)))
          (gtk-container-add vgrid label)
          (gtk-container-add vgrid image))
        ;; Progressive loading
        (let* ((label (make-instance 'gtk-label
                                     :margin-top 18
                                     :justify :center
                                     :use-markup t
                                     :label
                                     (format nil
                                             "<b>Progressive image loading</b>~
                                             ~%Click Image to repeat loading")))
               (frame (make-instance 'gtk-frame
                                     :shadow-type :none
                                     :width-request 340
                                     :height-request 220))
               (event-box (make-instance 'gtk-event-box))
               ;; Create an empty image for now. The progressive loader will
               ;; create the pixbuf and fill it in.
               (image (gtk-image-new-from-pixbuf nil)))
          ;; This is obviously totally contrived (we slow down loading on
          ;; purpose to show how incremental loading works). The real purpose
          ;; of incremental loading is the case where you are reading data from
          ;; a slow source such as the network. The timeout simply simulates a
          ;; slow data source by inserting pauses in the reading process.
          (setf timeout
                (g-timeout-add 50 (lambda () (progressive-timeout image))))
          ;; Restart loading the image from the file
          (g-signal-connect event-box "button-press-event"
             (lambda (widget event)
               (declare (ignore widget))
               (format t "Event Box clicked at (~,2f, ~,2f)~%"
                         (gdk-event-button-x event)
                         (gdk-event-button-y event))
               (gtk-image-clear image)
               (setf timeout
                     (g-timeout-add 100
                                    (lambda () (progressive-timeout image))))))
          (gtk-container-add vgrid label)
          (gtk-container-add event-box image)
          (gtk-container-add frame event-box)
          (gtk-container-add vgrid frame))
        (gtk-container-add window vgrid)
        (gtk-widget-show-all window)))))
