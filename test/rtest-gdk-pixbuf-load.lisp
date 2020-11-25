(def-suite gdk-pixbuf-load :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-load)

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_new_from_file
;;;     gdk_pixbuf_new_from_file_at_size
;;;     gdk_pixbuf_new_from_file_at_scale

;;;     gdk_pixbuf_get_file_info

(test gdk-pixbuf-file-info

  (multiple-value-bind (format width height)
      (gdk-pixbuf-file-info "floppybuddy.gif")

    (is (= 80 width))
    (is (= 70 height))

    (is (pointerp format))

))

;;;     gdk_pixbuf_get_file_info_async
;;;     gdk_pixbuf_get_file_info_finish
;;;     gdk_pixbuf_new_from_resource
;;;     gdk_pixbuf_new_from_resource_at_scale
;;;     gdk_pixbuf_new_from_stream
;;;     gdk_pixbuf_new_from_stream_async
;;;     gdk_pixbuf_new_from_stream_finish
;;;     gdk_pixbuf_new_from_stream_at_scale
;;;     gdk_pixbuf_new_from_stream_at_scale_async

;;; 2020-11-22
