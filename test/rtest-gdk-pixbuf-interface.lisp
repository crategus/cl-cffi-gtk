(def-suite gdk-pixbuf-interface :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-interface)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPixbufFormatFlags
;;;     GdkPixbufModulePattern
;;;     GdkPixbufModule
;;;     GdkPixbufAnimationClass
;;;     GdkPixbufAnimationIterClass
;;;     GdkPixbufFormat

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_get_formats

(test gdk-pixbuf-formats
  (is (every #'pointerp (gdk-pixbuf-formats)))
  #-windows
  (is (equal '("wmf" "ani" "bmp" "gif" "icns" "ico" "jpeg" "png" "pnm" "qtif"
               "svg" "tga" "tiff" "xbm" "xpm")
             (mapcar #'gdk-pixbuf-format-name (gdk-pixbuf-formats))))
  #+windows
  (is (equal '("ani" "bmp" "emf" "gif" "icns" "ico" "jpeg" "png" "pnm" "qtif"
               "svg" "tga" "tiff" "wmf" "wmf" "xbm" "xpm")
             (sort (mapcar #'gdk-pixbuf-format-name (gdk-pixbuf-formats))
                   #'string<))))

;;;     gdk_pixbuf_format_copy
;;;     gdk_pixbuf_format_free

;;;     gdk_pixbuf_format_get_name
;;;     gdk_pixbuf_format_get_description
;;;     gdk_pixbuf_format_get_mime_types
;;;     gdk_pixbuf_format_get_extensions
;;;     gdk_pixbuf_format_is_save_option_supported
;;;     gdk_pixbuf_format_is_writable
;;;     gdk_pixbuf_format_is_scalable
;;;     gdk_pixbuf_format_is_disabled
;;;     gdk_pixbuf_format_set_disabled
;;;     gdk_pixbuf_format_get_license

(test gdk-pixbuf-format-infos.1
  (let ((format (gdk-pixbuf-file-info "ducky.png")))
    (is (string= "png" (gdk-pixbuf-format-name format)))
    (is (string= "PNG" (gdk-pixbuf-format-description format)))
    (is (equal '("image/png") (gdk-pixbuf-format-mime-types format)))
    (is (equal '("png") (gdk-pixbuf-format-extensions format)))
    (is-true (gdk-pixbuf-format-is-save-option-supported format "compression"))
    (is-true (gdk-pixbuf-format-is-writable format))
    (is-false (gdk-pixbuf-format-is-scalable format))
    (is-false (gdk-pixbuf-format-is-disabled format))
    (is (string= "LGPL" (gdk-pixbuf-format-license format)))))

(test gdk-pixbuf-format-infos.2
  (let ((format (gdk-pixbuf-file-info "floppybuddy.gif")))
    (is (string= "gif" (gdk-pixbuf-format-name format)))
    (is (string= "GIF" (gdk-pixbuf-format-description format)))
    (is (equal '("image/gif") (gdk-pixbuf-format-mime-types format)))
    (is (equal '("gif") (gdk-pixbuf-format-extensions format)))
    (is-false (gdk-pixbuf-format-is-save-option-supported format "compression"))
    (is-false (gdk-pixbuf-format-is-writable format))
    (is-false (gdk-pixbuf-format-is-scalable format))
    (is-false (gdk-pixbuf-format-is-disabled format))
    (is (string= "LGPL" (gdk-pixbuf-format-license format)))))

;;;     GdkPixbufModuleFillVtableFunc
;;;     GdkPixbufModuleFillInfoFunc
;;;     GdkPixbufModuleSizeFunc
;;;     GdkPixbufModulePreparedFunc
;;;     GdkPixbufModuleUpdatedFunc

;;; 2021-10-14
