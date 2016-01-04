
(def-suite gtk-print-settings :in gtk-suite)
(in-suite gtk-print-settings)

;;;   GtkPrintSettings

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

(test gtk-print-settings-has-key
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
    (is (= 100
           (gtk-print-settings-get-double-with-default settings
                                                       "paper-width" 90.0d0)))
    (is (= 90
           (gtk-print-settings-get-double-with-default settings
                                                       "xxx" 90.0d0)))))

;;;     gtk_print_settings_get_length
;;;     gtk_print_settings_set_length

;;;     gtk_print_settings_get_int
;;;     gtk_print_settings_get_int_with_default
;;;     gtk_print_settings_set_int

;;;     GTK_PRINT_SETTINGS_PRINTER
;;;     gtk_print_settings_get_printer
;;;     gtk_print_settings_set_printer

;;;   GtkPageOrientation

;;;   GTK_PRINT_SETTINGS_ORIENTATION
;;;   gtk_print_settings_get_orientation
;;;   gtk_print_settings_set_orientation

(test gtk-print-settings-orientation
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-orientation settings :landscape)
    (is (eql :landscape (gtk-print-settings-get-orientation settings))
    (is (equal "landscape" (gtk-print-settings-get settings "orientation"))))))

;;;     GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;     gtk_print_settings_get_paper_size
;;;     gtk_print_settings_set_paper_size

;;;     GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;     gtk_print_settings_get_paper_width
;;;     gtk_print_settings_set_paper_width

(test gtk-print-settings-paper-width
  (let ((settings (gtk-print-settings-new)))
    (gtk-print-settings-set-paper-width settings 100.0d0 :mm)
    (is (= 100.0d0 (gtk-print-settings-get-paper-width settings :mm)))
    (is (equal "100" (gtk-print-settings-get settings "paper-width")))))

;;;     GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;     gtk_print_settings_get_paper_height
;;;     gtk_print_settings_set_paper_height

;;;   GTK_PRINT_SETTINGS_USE_COLOR
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

;;;     GTK_PRINT_SETTINGS_COLLATE
;;;     gtk_print_settings_get_collate
;;;     gtk_print_settings_set_collate
;;;
;;;     GTK_PRINT_SETTINGS_REVERSE
;;;     gtk_print_settings_get_reverse
;;;     gtk_print_settings_set_reverse
;;;
;;;     GtkPrintDuplex
;;;
;;;     GTK_PRINT_SETTINGS_DUPLEX
;;;     gtk_print_settings_get_duplex
;;;     gtk_print_settings_set_duplex
;;;
;;;     GtkPrintQuality
;;;
;;;     GTK_PRINT_SETTINGS_QUALITY
;;;     gtk_print_settings_get_quality
;;;     gtk_print_settings_set_quality
;;;
;;;     GTK_PRINT_SETTINGS_N_COPIES
;;;     gtk_print_settings_get_n_copies
;;;     gtk_print_settings_set_n_copies
;;;
;;;     GTK_PRINT_SETTINGS_NUMBER_UP
;;;     gtk_print_settings_get_number_up
;;;     gtk_print_settings_set_number_up
;;;
;;;     GtkNumberUpLayout
;;;
;;;     GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;     gtk_print_settings_get_number_up_layout
;;;     gtk_print_settings_set_number_up_layout
;;;
;;;     GTK_PRINT_SETTINGS_RESOLUTION
;;;     gtk_print_settings_get_resolution
;;;     gtk_print_settings_set_resolution
;;;     gtk_print_settings_set_resolution_xy
;;;
;;;     GTK_PRINT_SETTINGS_RESOLUTION_X
;;;     gtk_print_settings_get_resolution_x
;;;
;;;     GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;     gtk_print_settings_get_resolution_y
;;;
;;;     GTK_PRINT_SETTINGS_PRINTER_LPI
;;;     gtk_print_settings_get_printer_lpi
;;;     gtk_print_settings_set_printer_lpi
;;;
;;;     GTK_PRINT_SETTINGS_SCALE
;;;     gtk_print_settings_get_scale
;;;     gtk_print_settings_set_scale
;;;
;;;     GtkPrintPages
;;;
;;;     GTK_PRINT_SETTINGS_PRINT_PAGES
;;;     gtk_print_settings_get_print_pages
;;;     gtk_print_settings_set_print_pages
;;;
;;;     GtkPageRange
;;;
;;;     GTK_PRINT_SETTINGS_PAGE_RANGES
;;;     gtk_print_settings_get_page_ranges
;;;     gtk_print_settings_set_page_ranges
;;;
;;;     GtkPageSet
;;;
;;;     GTK_PRINT_SETTINGS_PAGE_SET
;;;     gtk_print_settings_get_page_set
;;;     gtk_print_settings_set_page_set
;;;
;;;     GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;     gtk_print_settings_get_default_source
;;;     gtk_print_settings_set_default_source
;;;
;;;     GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;     gtk_print_settings_get_media_type
;;;     gtk_print_settings_set_media_type
;;;
;;;     GTK_PRINT_SETTINGS_DITHER
;;;     gtk_print_settings_get_dither
;;;     gtk_print_settings_set_dither
;;;
;;;     GTK_PRINT_SETTINGS_FINISHINGS
;;;     gtk_print_settings_get_finishings
;;;     gtk_print_settings_set_finishings
;;;
;;;     GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;     gtk_print_settings_get_output_bin
;;;     gtk_print_settings_set_output_bin
;;;
;;;     GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;     GTK_PRINT_SETTINGS_OUTPUT_URI
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
;;;
;;;     gtk_print_settings_new_from_file
;;;     gtk_print_settings_new_from_key_file
;;;     gtk_print_settings_load_file
;;;     gtk_print_settings_load_key_file
;;;     gtk_print_settings_to_file
;;;     gtk_print_settings_to_key_file

