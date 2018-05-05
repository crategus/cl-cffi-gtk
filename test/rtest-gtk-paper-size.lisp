(in-package :gtk-testsuite)

(def-suite gtk-paper-size :in gtk-suite)
(in-suite gtk-paper-size)

;;;     GtkPaperSize
;;;     GtkUnit

;;;     GTK_PAPER_NAME_A3
;;;     GTK_PAPER_NAME_A4
;;;     GTK_PAPER_NAME_A5
;;;     GTK_PAPER_NAME_B5
;;;     GTK_PAPER_NAME_LETTER
;;;     GTK_PAPER_NAME_EXECUTIVE
;;;     GTK_PAPER_NAME_LEGAL

;;;   gtk_paper_size_new

(test gtk-paper-size-new.1
  (is (eql 'gtk-paper-size (type-of (gtk-paper-size-new (null-pointer))))))

(test gtk-paper-size-new.2
  (is (eql 'gtk-paper-size (type-of (gtk-paper-size-new "iso_a4")))))

;;;   gtk_paper_size_new_from_ppd

(test gtk-paper-size-new-from-ppd
  (is (eql 'gtk-paper-size
           (type-of (gtk-paper-size-new-from-ppd "A4" "A4" 1.0d0 1.0d0)))))

;;;   gtk_paper_size_new_custom

(test gtk-paper-size-new-custom
  (let ((paper-size (gtk-paper-size-new-custom "myPaper"
                                               "myPaper" 0.0d0 0.0d0 :mm)))
    (is-true (gtk-paper-size-is-custom paper-size))))

;;;     gtk_paper_size_copy
;;;     gtk_paper_size_free
;;;     gtk_paper_size_is_equal

;;;   gtk_paper_size_get_paper_sizes

;;;   gtk_paper_size_get_name

(test gtk-paper-size-get-name
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (equal "iso_a4" (gtk-paper-size-get-name paper-size)))))

;;;   gtk_paper_size_get_display_name

(test gtk-paper-size-get-display-name
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (equal "A4" (gtk-paper-size-get-display-name paper-size)))))

;;;   gtk_paper_size_get_ppd_name

(test gtk-paper-size-get-ppd-name
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (equal "A4" (gtk-paper-size-get-ppd-name paper-size)))))

;;;   gtk_paper_size_get_width
;;;   gtk_paper_size_get_height

(test gtk-paper-size-get-width
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (= 210.0 (gtk-paper-size-get-width paper-size :mm)))
    (is (= 297.0 (gtk-paper-size-get-height paper-size :mm)))))

;;;   gtk_paper_size_is_custom
;;;   gtk_paper_size_set_size

(test gtk-paper-size-set-size
  (let ((paper-size (gtk-paper-size-new-custom "myPaper"
                                               "myPaper" 0.0d0 0.0d0 :mm)))
    (is-true (gtk-paper-size-is-custom paper-size))
    (gtk-paper-size-set-size paper-size 200.0d0 100.0d0 :mm)
    (is (= 200.0 (gtk-paper-size-get-width paper-size :mm)))
    (is (= 100.0 (gtk-paper-size-get-height paper-size :mm)))))

;;;   gtk_paper_size_get_default_top_margin

(test gtk-paper-size-get-default-right-margin
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (= 6.35d0 (gtk-paper-size-get-default-top-margin paper-size :mm)))))

;;;   gtk_paper_size_get_default_bottom_margin

(test gtk-paper-size-get-default-right-margin
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (= 6.35d0 (gtk-paper-size-get-default-bottom-margin paper-size :mm)))))

;;;   gtk_paper_size_get_default_left_margin

(test gtk-paper-size-get-default-right-margin
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (= 6.35d0 (gtk-paper-size-get-default-left-margin paper-size :mm)))))

;;;   gtk_paper_size_get_default_right_margin

(test gtk-paper-size-get-default-right-margin
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (= 6.35d0 (gtk-paper-size-get-default-right-margin paper-size :mm)))))

;;;   gtk_paper_size_get_default

(test gtk-paper-size-get-default
  (is (equal "iso_a4" (gtk-paper-size-get-default))))

;;;   gtk_paper_size_new_from_key_file
;;;   gtk_paper_size_to_key_file

(test gtk-paper-size-to-key-file
  (let ((key-file (g-key-file-new))
        (paper-size (gtk-paper-size-new "iso_a4")))
    ;; Write paper-size to key-file
    (gtk-paper-size-to-key-file paper-size key-file "Paper")
    ;; Read the paper-size back
    (let ((paper-size (gtk-paper-size-new-from-key-file key-file "Paper")))
      (is (equal "iso_a4" (gtk-paper-size-get-name paper-size))))))

