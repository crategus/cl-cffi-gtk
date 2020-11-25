(def-suite gdk-pixbuf-structures :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-structures)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPixbufError
;;;     GDK_PIXBUF_ERROR
;;;     GdkColorspace
;;;     GdkPixbufAlphaMode

;;;     GdkPixbuf

(test gdk-pixbuf-class
  ;; Type check
  (is (g-type-is-object "GdkPixbuf"))
  ;; Check the registered name
  (is (eq 'gdk-pixbuf
          (registered-object-type-by-name "GdkPixbuf")))
  ;; Check the type initializer
  (is (eq (gtype "GdkPixbuf")
          (gtype (foreign-funcall "gdk_pixbuf_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkPixbuf")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GdkPixbuf"))))
  ;; Check the interfaces
  (is (equal '("GIcon" "GLoadableIcon")
             (mapcar #'g-type-name (g-type-interfaces "GdkPixbuf"))))
  ;; Check the class properties
  (is (equal '("bits-per-sample" "colorspace" "has-alpha" "height" "n-channels"
               "pixel-bytes" "pixels" "rowstride" "width")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GdkPixbuf"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkPixbuf" GDK-PIXBUF
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GIcon" "GLoadableIcon") :TYPE-INITIALIZER
                        "gdk_pixbuf_get_type")
                       ((BITS-PER-SAMPLE GDK-PIXBUF-BITS-PER-SAMPLE
                         "bits-per-sample" "gint" T NIL)
                        (COLORSPACE GDK-PIXBUF-COLORSPACE "colorspace"
                         "GdkColorspace" T NIL)
                        (HAS-ALPHA GDK-PIXBUF-HAS-ALPHA "has-alpha" "gboolean"
                         T NIL)
                        (HEIGHT GDK-PIXBUF-HEIGHT "height" "gint" T NIL)
                        (N-CHANNELS GDK-PIXBUF-N-CHANNELS "n-channels" "gint" T
                         NIL)
                        (PIXEL-BYTES GDK-PIXBUF-PIXEL-BYTES "pixel-bytes"
                         "GBytes" T NIL)
                        (PIXELS GDK-PIXBUF-PIXELS "pixels" "gpointer" T NIL)
                        (ROWSTRIDE GDK-PIXBUF-ROWSTRIDE "rowstride" "gint" T
                         NIL)
                        (WIDTH GDK-PIXBUF-WIDTH "width" "gint" T NIL)))
             (get-g-type-definition "GdkPixbuf"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-pixbuf-properties
  (let ((pixbuf (gdk-pixbuf-new-from-file "ducky.png")))
    (is (= 8 (gdk-pixbuf-bits-per-sample pixbuf)))
    (is (eq :rgb (gdk-pixbuf-colorspace pixbuf)))
    (is-true (gdk-pixbuf-has-alpha pixbuf))
    (is (= 537 (gdk-pixbuf-height pixbuf)))
    (is (= 4 (gdk-pixbuf-n-channels pixbuf)))
    ;; TODO: GBytes is not implemented
    (signals (error) (gdk-pixbuf-pixel-bytes pixbuf))
    (is (pointerp (gdk-pixbuf-pixels pixbuf)))
    (is (eq 1956 (gdk-pixbuf-rowstride pixbuf)))
    (is (= 489 (gdk-pixbuf-width pixbuf)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_get_pixels_with_length
;;;     gdk_pixbuf_get_byte_length
;;;     gdk_pixbuf_get_option
;;;     gdk_pixbuf_set_option
;;;     gdk_pixbuf_remove_option
;;;     gdk_pixbuf_get_options
;;;     gdk_pixbuf_copy_options
;;;     gdk_pixbuf_read_pixels


;;; 2020-11-21
