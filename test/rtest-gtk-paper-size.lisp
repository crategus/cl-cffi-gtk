
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

(test gtk-paper-size-get-paper-sizes
  (is (equal '("#10-Umschlag" "#11-Umschlag" "#12-Umschlag" "#14-Umschlag" "#9-Umschlag"
 "10×11" "10×13" "10×14" "10×15" "11×12" "11×15" "12×19" "5×7" "6x9-Umschlag"
 "7x9-Umschlag" "8x10-Umschlag" "9x11-Umschlag" "9x12-Umschlag" "A0" "A0×2"
 "A0×3" "A1" "A10" "A1×3" "A1×4" "A2" "a2-Umschlag" "A2×3" "A2×4" "A2×5" "A3"
 "A3 Extra" "A3×3" "A3×4" "A3×5" "A3×6" "A3×7" "A4" "A4 Extra" "A4 Tab" "A4×3"
 "A4×4" "A4×5" "A4×6" "A4×7" "A4×8" "A4×9" "A5" "A5 Extra" "A6" "A7" "A8" "A9"
 "Arch A" "Arch B" "Arch C" "Arch D" "Arch E" "asme_f" "b-plus" "B0" "B1" "B10"
 "B2" "B3" "B4" "B5" "B5 Extra" "B6" "B6/C4" "B7" "B8" "B9" "Breites Foto" "c"
 "C0" "C1" "C10" "C2" "C3" "C4" "C5" "c5-Umschlag" "C6" "C6/C5" "C7" "C7/C6"
 "C8" "C9" "Choukei 2-Umschlag" "Choukei 3-Umschlag" "Choukei 4-Umschlag"
 "Choukei 40-Umschlag" "d" "Dai-pa-kai" "DL-Umschlag" "e" "edp"
 "Endlospapier Amerikanisch" "Endlospapier Deutsch-Legal"
 "Endlospapier Europäisch" "Europäisches edp" "Executive" "f" "Folio"
 "Folio sp" "Foto L" "Government-Legal" "Government-Letter" "Großes Foto"
 "hagaki (Postkarte)" "Index 3x5" "Index 4x6 (Postkarte)" "Index 4x6 ext"
 "Index 5x8" "Invite-Umschlag" "Italien-Umschlag" "JB0" "JB1" "JB10" "JB2"
 "JB3" "JB4" "JB5" "JB6" "JB7" "JB8" "JB9" "jis exec" "juuro-ku-kai"
 "kahu-Umschlag" "kaku2-Umschlag" "kaku3-Umschlag" "kaku4-Umschlag"
 "kaku5-Umschlag" "kaku7-Umschlag" "kaku8-Umschlag" "Kleines Foto"
 "Mittelgroßes Fotos" "Monarch-Umschlag" "Oficio" "oufuku (Antwortpostkarte)"
 "pa-kai" "Personal-Umschlag" "Postfix-Umschlag" "prc 16k" "prc 32k"
 "prc1-Umschlag" "prc10-Umschlag" "prc2-Umschlag" "prc3-Umschlag"
 "prc4-Umschlag" "prc5-Umschlag" "prc6-Umschlag" "prc7-Umschlag"
 "prc8-Umschlag" "prc9-Umschlag" "Quarto" "RA0" "RA1" "RA2" "RA3" "RA4"
 "Rechnung" "ROC 16k" "ROC 8k" "SRA0" "SRA1" "SRA2" "SRA3" "SRA4" "Super B"
 "Super B" "Tabloid" "US-Legal" "US-Legal Extra" "US-Letter" "US-Letter Extra"
 "US-Letter Plus" "Weites Format" "you4-Umschlag" "you6-Umschlag")
             (stable-sort (mapcar #'gtk-paper-size-get-display-name
                                  (gtk-paper-size-get-paper-sizes nil))
                          #'string-lessp))))

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

