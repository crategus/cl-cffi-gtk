(def-suite gtk-print-settings :in gtk-suite)
(in-suite gtk-print-settings)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintSettings
;;;     GTK_PRINT_SETTINGS_PRINTER

;;;     GtkPageOrientation
;;;     GTK_PRINT_SETTINGS_ORIENTATION
;;;     GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;     GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;     GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;     GTK_PRINT_SETTINGS_USE_COLOR
;;;     GTK_PRINT_SETTINGS_COLLATE
;;;     GTK_PRINT_SETTINGS_REVERSE

;;;     GtkPrintDuplex
;;;     GTK_PRINT_SETTINGS_DUPLEX

;;;     GtkPrintQuality
;;;     GTK_PRINT_SETTINGS_QUALITY
;;;     GTK_PRINT_SETTINGS_N_COPIES
;;;     GTK_PRINT_SETTINGS_NUMBER_UP

;;;     GtkNumberUpLayout
;;;     GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;     GTK_PRINT_SETTINGS_RESOLUTION
;;;     GTK_PRINT_SETTINGS_RESOLUTION_X
;;;     GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;     GTK_PRINT_SETTINGS_PRINTER_LPI
;;;     GTK_PRINT_SETTINGS_SCALE

;;;     GtkPrintPages
;;;     GTK_PRINT_SETTINGS_PRINT_PAGES

;;;     GtkPageRange
;;;     GTK_PRINT_SETTINGS_PAGE_RANGES

;;;     GtkPageSet
;;;     GTK_PRINT_SETTINGS_PAGE_SET
;;;     GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;     GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;     GTK_PRINT_SETTINGS_DITHER
;;;     GTK_PRINT_SETTINGS_FINISHINGS
;;;     GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;     GTK_PRINT_SETTINGS_OUTPUT_DIR
;;;     GTK_PRINT_SETTINGS_OUTPUT_BASENAME
;;;     GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;     GTK_PRINT_SETTINGS_OUTPUT_URI
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION

;;; --- Functions --------------------------------------------------------------

;;;   gtk_print_settings_new

(test gtk-print-settings-new
  (is (eql 'gtk-print-settings (type-of (gtk-print-settings-new)))))

;;;   gtk_print_settings_copy

(test gtk-print-settings-copy
  (let ((settings (gtk-print-settings-new)))
    (is (eql 'gtk-print-settings
             (type-of (gtk-print-settings-copy settings))))))

;;;   gtk_print_settings_has_key

(test gtk-print-settings-has-key
  (let ((settings (gtk-print-settings-new)))
    (is-false (gtk-print-settings-has-key settings "orientation"))
    (gtk-print-settings-set-orientation settings :landscape)
    (is-true (gtk-print-settings-has-key settings "orientation"))))

;;;   gtk_print_settings_get

(test gtk-print-settings-get
  (let ((settings (gtk-print-settings-new)))
    (is-false (gtk-print-settings-has-key settings "orientation"))
    (gtk-print-settings-set-orientation settings :landscape)
    (is-true (gtk-print-settings-has-key settings "orientation"))
    (is (equal "landscape" (gtk-print-settings-get settings "orientation")))))

;;;   gtk_print_settings_set

(test gtk-print-settings-set
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set settings "orientation" "landscape")
    (is (equal "landscape" (gtk-print-settings-get settings "orientation")))
    (is (eql :landscape (gtk-print-settings-get-orientation settings)))))

;;;   gtk_print_settings_unset

(test gtk-print-settings-unset
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set settings "orientation" "landscape")
    (is (equal "landscape" (gtk-print-settings-get settings "orientation")))
    (is (eql :landscape (gtk-print-settings-get-orientation settings)))
    (gtk-print-settings-unset settings "orientation")
    (is-false (gtk-print-settings-has-key settings "orientation"))))

;;;   gtk_print_settings_foreach

;;;   gtk_print_settings_get_bool
;;;   gtk_print_settings_set_bool

(test gtk-print-settings-bool
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-bool settings "use-color" t)
    (is-true (gtk-print-settings-get-bool settings "use-color"))
    (gtk-print-settings-set-bool settings "use-color" nil)
    (is-false (gtk-print-settings-get-bool settings "use-color"))))

;;;   gtk_print_settings_get_double
;;;   gtk_print_settings_get_double_with_default
;;;   gtk_print_settings_set_double

(test gtk-print-settings-double
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-double settings "paper-width" 100.0d0)
    (is (= 100 (gtk-print-settings-get-double settings "paper-width")))
    (is (= 100 (gtk-print-settings-get-double-with-default settings "paper-width" 90.0d0)))
    (is (= 90 (gtk-print-settings-get-double-with-default settings "xxx" 90.0d0)))))

;;;     gtk_print_settings_get_length
;;;     gtk_print_settings_set_length

(test gtk-print-settings-length
  (let ((settings (gtk-print-settings-new)))
    (is (= 0.0d0 (gtk-print-settings-get-length settings "paper-width" :mm)))
    (gtk-print-settings-set-length settings "paper-width" 100.0d0 :mm)
    (is (= 100.0d0 (gtk-print-settings-get-length settings "paper-width" :mm)))
    (gtk-print-settings-set-length settings "paper-width" 320.0d0 :points)
    (is (= 320.0d0 (gtk-print-settings-get-length settings "paper-width" :points)))
    (gtk-print-settings-set-length settings "paper-width" 100.0d0 :inch)
    (is (= 100.0d0 (gtk-print-settings-get-length settings "paper-width" :inch)))))

;;;     gtk_print_settings_get_int
;;;     gtk_print_settings_get_int_with_default
;;;     gtk_print_settings_set_int

(test gtk-print-settings-int
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-int settings "paper-width" 100)
    (is (= 100 (gtk-print-settings-get-int settings "paper-width")))
    (is (= 100 (gtk-print-settings-get-int-with-default settings "paper-width" 90)))
    (is (= 90 (gtk-print-settings-get-int-with-default settings "xxx" 90)))))

;;;     gtk_print_settings_get_printer
;;;     gtk_print_settings_set_printer

(test gtk-print-settings-printer
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-printer settings "printer")
    (is (string= "printer" (gtk-print-settings-get-printer settings)))))

;;;     gtk_print_settings_get_orientation
;;;     gtk_print_settings_set_orientation

(test gtk-print-settings-orientation
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-orientation settings :landscape)
    (is (eql :landscape (gtk-print-settings-get-orientation settings))
    (is (string= "landscape" (gtk-print-settings-get settings "orientation"))))))

;;;     gtk_print_settings_get_paper_size
;;;     gtk_print_settings_set_paper_size

(test gtk-print-settings-paper-size
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-paper-size settings (gtk-paper-size-new "iso_a4"))
    (is (eq 'gtk-paper-size (type-of (gtk-print-settings-get-paper-size settings))))))

;;;     gtk_print_settings_get_paper_width
;;;     gtk_print_settings_set_paper_width

(test gtk-print-settings-paper-width
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-paper-width settings 100.0d0 :mm)
    (is (= 100.0d0 (gtk-print-settings-get-paper-width settings :mm)))
    (is (string= "100" (gtk-print-settings-get settings "paper-width")))))

;;;     gtk_print_settings_get_paper_height
;;;     gtk_print_settings_set_paper_height

(test gtk-print-settings-paper-height
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-paper-height settings 100.0d0 :mm)
    (is (= 100.0d0 (gtk-print-settings-get-paper-height settings :mm)))
    (is (string= "100" (gtk-print-settings-get settings "paper-height")))))

;;;   gtk_print_settings_get_use_color
;;;   gtk_print_settings_set_use_color

(test gtk-print-settings-use-color.1
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-use-color settings t)
    (is-true (gtk-print-settings-get-use-color settings))
    (is (equal "true" (gtk-print-settings-get settings "use-color")))))

(test gtk-print-settings-use-color.2
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-use-color settings nil)
    (is-false (gtk-print-settings-get-use-color settings))
    (is (equal "false" (gtk-print-settings-get settings "use-color")))))

;;;     gtk_print_settings_get_collate
;;;     gtk_print_settings_set_collate

;;;     gtk_print_settings_get_reverse
;;;     gtk_print_settings_set_reverse

;;;     gtk_print_settings_get_duplex
;;;     gtk_print_settings_set_duplex

;;;     gtk_print_settings_get_quality
;;;     gtk_print_settings_set_quality

;;;     gtk_print_settings_get_n_copies
;;;     gtk_print_settings_set_n_copies

;;;     gtk_print_settings_get_number_up
;;;     gtk_print_settings_set_number_up

;;;     gtk_print_settings_get_number_up_layout
;;;     gtk_print_settings_set_number_up_layout

;;;     gtk_print_settings_get_resolution
;;;     gtk_print_settings_set_resolution
;;;     gtk_print_settings_set_resolution_xy

;;;     gtk_print_settings_get_resolution_x
;;;     gtk_print_settings_get_resolution_y
;;;

;;;     gtk_print_settings_get_printer_lpi
;;;     gtk_print_settings_set_printer_lpi
;;;

;;;     gtk_print_settings_get_scale
;;;     gtk_print_settings_set_scale
;;;

;;;     gtk_print_settings_get_print_pages
;;;     gtk_print_settings_set_print_pages
;;;

;;;     gtk_print_settings_get_page_ranges
;;;     gtk_print_settings_set_page_ranges
;;;

;;;     gtk_print_settings_get_page_set
;;;     gtk_print_settings_set_page_set
;;;

;;;     gtk_print_settings_get_default_source
;;;     gtk_print_settings_set_default_source
;;;

;;;     gtk_print_settings_get_media_type
;;;     gtk_print_settings_set_media_type
;;;

;;;     gtk_print_settings_get_dither
;;;     gtk_print_settings_set_dither
;;;

;;;     gtk_print_settings_get_finishings
;;;     gtk_print_settings_set_finishings
;;;

;;;     gtk_print_settings_get_output_bin
;;;     gtk_print_settings_set_output_bin
;;;

;;;     gtk_print_settings_new_from_file
;;;     gtk_print_settings_new_from_key_file
;;;     gtk_print_settings_new_from_gvariant
;;;     gtk_print_settings_load_file
;;;     gtk_print_settings_load_key_file
;;;     gtk_print_settings_to_file
;;;     gtk_print_settings_to_key_file
;;;     gtk_print_settings_to_gvariant

