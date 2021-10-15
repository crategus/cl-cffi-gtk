(def-suite gtk-paper-size :in gtk-suite)
(in-suite gtk-paper-size)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkUnit

(test gtk-unit
  ;; Check the type
  (is (g-type-is-enum "GtkUnit"))
  ;; Check the type initializer
  (is (eq (gtype "GtkUnit")
          (gtype (foreign-funcall "gtk_unit_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-unit (registered-enum-type "GtkUnit")))
  ;; Check the names
  (is (equal '("GTK_UNIT_NONE" "GTK_UNIT_POINTS" "GTK_UNIT_INCH" "GTK_UNIT_MM")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkUnit"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkUnit"))))
  ;; Check the nick names
  (is (equal '("none" "points" "inch" "mm")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkUnit"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkUnit" GTK-UNIT
                (:EXPORT T :TYPE-INITIALIZER "gtk_unit_get_type")
                (:NONE 0)
                (:POINTS 1)
                (:INCH 2)
                (:MM 3))
             (get-g-type-definition "GtkUnit"))))

;;;     GtkPaperSize

(test gtk-paper-size-boxed
  ;; Type check
  (is-true (g-type-is-a (gtype "GtkPaperSize") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GtkPaperSize")
          (gtype (foreign-funcall "gtk_paper_size_get_type" g-size)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk-paper-size-new

(test gtk-paper-size-new
  (is (eq 'gtk-paper-size (type-of (gtk-paper-size-new))))
  (is (eq 'gtk-paper-size (type-of (gtk-paper-size-new nil))))
  (is (eq 'gtk-paper-size (type-of (gtk-paper-size-new "iso_a4")))))

;;;     gtk-paper-size-new-from-ppd

(test gtk-paper-size-new-from-ppd.1
  (let ((paper-size (gtk-paper-size-new-from-ppd "A4")))
    (is (eq 'gtk-paper-size (type-of paper-size)))
    (is (string= "iso_a4" (gtk-paper-size-name paper-size)))
    (is (string= "A4" (gtk-paper-size-display-name paper-size)))
    (is (= 210.0 (gtk-paper-size-width paper-size :mm)))
    (is (= 297.0 (gtk-paper-size-height paper-size :mm)))))

(test gtk-paper-size-new-from-ppd.2
  (let ((paper-size (gtk-paper-size-new-from-ppd "unknown" "myppd" 10 20)))
    (is (eq 'gtk-paper-size (type-of paper-size)))
    (is (string= "ppd_unknown" (gtk-paper-size-name paper-size)))
    (is (string= "myppd" (gtk-paper-size-display-name paper-size)))
    (is (= 10.0 (gtk-paper-size-width paper-size :points)))
    (is (= 20.0 (gtk-paper-size-height paper-size :points)))))

(test gtk-paper-size-new-from-ppd.3
  (let ((paper-size (gtk-paper-size-new-from-ppd "unknown" "myppd")))
    (is (eq 'gtk-paper-size (type-of paper-size)))
    (is (string= "ppd_unknown" (gtk-paper-size-name paper-size)))
    (is (string= "myppd" (gtk-paper-size-display-name paper-size)))
    (is (= 0 (gtk-paper-size-width paper-size :points)))
    (is (= 0 (gtk-paper-size-height paper-size :points)))))

;;;     gtk-paper-size-new-from-ipp

(test gtk-paper-size-new-from-ipp.1
  (let ((paper-size (gtk-paper-size-new-from-ipp "A4")))
    (is (eq 'gtk-paper-size (type-of paper-size)))
    (is-false (gtk-paper-size-is-ipp paper-size))
    (is-true (gtk-paper-size-is-custom paper-size))
    (is (string= "A4" (gtk-paper-size-name paper-size)))
    (is (string= "A4" (gtk-paper-size-display-name paper-size)))
    (is (= 0 (gtk-paper-size-width paper-size :points)))
    (is (= 0 (gtk-paper-size-height paper-size :points)))))

(test gtk-paper-size-new-from-ipp.2
  (let ((paper-size (gtk-paper-size-new-from-ipp "A4" 10 20)))
    (is (eq 'gtk-paper-size (type-of paper-size)))
    (is-false (gtk-paper-size-is-ipp paper-size))
    (is-true (gtk-paper-size-is-custom paper-size))
    (is (string= "A4" (gtk-paper-size-name paper-size)))
    (is (string= "A4" (gtk-paper-size-display-name paper-size)))
    (is (= 10 (gtk-paper-size-width paper-size :points)))
    (is (= 20 (gtk-paper-size-height paper-size :points)))))

;;;     gtk-paper-size-new-custom

(test gtk-paper-size-new-custom
  (let ((paper-size (gtk-paper-size-new-custom "myPaper"
                                               "myPaper" 10 20 :mm)))
    (is (eq 'gtk-paper-size (type-of paper-size)))
    (is-false (gtk-paper-size-is-ipp paper-size))
    (is-true (gtk-paper-size-is-custom paper-size))
    (is (string= "myPaper" (gtk-paper-size-name paper-size)))
    (is (string= "myPaper" (gtk-paper-size-display-name paper-size)))
    (is (= 10 (gtk-paper-size-width paper-size :mm)))
    (is (= 20 (gtk-paper-size-height paper-size :mm)))))

;;;     gtk-paper-size-copy
;;;     gtk-paper-size-free
;;;     gtk-paper-size-is-equal

(test gtk-papper-size-is-equal
  (let ((paper-size-1 (gtk-paper-size-new "iso_a4"))
        (paper-size-2 (gtk-paper-size-new "iso_a4"))
        (paper-size-3 (gtk-paper-size-new "iso_a3")))
    (is-false (equal paper-size-1 paper-size-2))
    (is-true (gtk-paper-size-is-equal paper-size-1 paper-size-2))
    (is-false (gtk-paper-size-is-equal paper-size-1 paper-size-3))
    (is-true (gtk-paper-size-is-equal paper-size-1
                                      (gtk-paper-size-copy paper-size-1)))
    (is-false (gtk-paper-size-is-equal paper-size-1
                                       (gtk-paper-size-copy paper-size-3)))))

;;;     gtk-paper-size-paper-sizes

(test gtk-paper-size-paper-sizes.1
  (is (member "iso_a4"
              (mapcar #'gtk-paper-size-name (gtk-paper-size-paper-sizes nil))
              :test #'string=)))

(test gtk-paper-size-paper-sizes.2
  (is (member "A4"
              (mapcar #'gtk-paper-size-display-name (gtk-paper-size-paper-sizes nil))
              :test #'string=)))

;;;     gtk-paper-size-name
;;;     gtk-paper-size-display-name
;;;     gtk-paper-size-ppd-name

(test gtk-paper-size-name.1
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (string= "iso_a4" (gtk-paper-size-name paper-size)))
    (is (string= "A4" (gtk-paper-size-display-name paper-size)))
    (is (string= "A4" (gtk-paper-size-ppd-name paper-size)))))

(test gtk-paper-size-name.2
  (let ((paper-size (gtk-paper-size-new "iso_2a0")))
    (is (string= "iso_2a0" (gtk-paper-size-name paper-size)))
    #-windows ; TODO: There seems a problem with the charset of Windows
    (is (string= "A0x2" (gtk-paper-size-display-name paper-size)))
    (is (string= "" (gtk-paper-size-ppd-name paper-size)))))

;;;     gtk-paper-size-width
;;;     gtk-paper-size-height

(test gtk-paper-size-width
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    (is (= 210.0 (gtk-paper-size-width paper-size :mm)))
    (is (= 297.0 (gtk-paper-size-height paper-size :mm)))))

;;;     gtk-paper-size-is-ipp
;;;     gtk-paper-size-is-custom

(test gtk-paper-size-is-ipp
  (let ((paper-size-1 (gtk-paper-size-new "iso_a4"))
        (paper-size-2 (gtk-paper-size-new-custom "myPaper" "myPaper" 0 0 :mm)))
    (is-false (gtk-paper-size-is-ipp paper-size-1))
    (is-false (gtk-paper-size-is-custom paper-size-1))
    (is-false (gtk-paper-size-is-ipp paper-size-2))
    (is-true (gtk-paper-size-is-custom paper-size-2))))

;;;     gtk-paper-size-set-size

(test gtk-paper-size-set-size
  (let ((paper-size (gtk-paper-size-new-custom "myPaper"
                                               "myPaper" 0.0d0 0.0d0 :mm)))
    (is-true (gtk-paper-size-is-custom paper-size))
    (is-false (gtk-paper-size-set-size paper-size 200 100 :points))
    (is (= 200 (round (gtk-paper-size-width paper-size :points))))
    (is (= 100 (round (gtk-paper-size-height paper-size :points))))
    (is-false (gtk-paper-size-set-size paper-size 200.0 100.0 :inch))
    (is (= 200.0 (gtk-paper-size-width paper-size :inch)))
    (is (= 100.0 (gtk-paper-size-height paper-size :inch)))
    (is-false (gtk-paper-size-set-size paper-size 200.0 100.0 :mm))
    (is (= 200.0 (gtk-paper-size-width paper-size :mm)))
    (is (= 100.0 (gtk-paper-size-height paper-size :mm)))))

;;;     gtk-paper-size-default-top-margin
;;;     gtk-paper-size-default-bottom-margin
;;;     gtk-paper-size-default-left-margin
;;;     gtk-paper-size-default-right-margin

(test gtk-paper-size-default-margins
  (let ((paper-size (gtk-paper-size-new "iso_a4")))
    ;; GtkUnit is :points
    (is (= 18 (gtk-paper-size-default-top-margin paper-size :points)))
    (is (= 40 (round (gtk-paper-size-default-bottom-margin paper-size :points))))
    (is (= 18 (gtk-paper-size-default-left-margin paper-size :points)))
    (is (= 18 (gtk-paper-size-default-right-margin paper-size :points)))
    ;; GtkUnit is :mm
    (is (= 6.35d0 (gtk-paper-size-default-top-margin paper-size :mm)))
    (is (= 14.224d0 (gtk-paper-size-default-bottom-margin paper-size :mm)))
    (is (= 6.35d0 (gtk-paper-size-default-left-margin paper-size :mm)))
    (is (= 6.35d0 (gtk-paper-size-default-right-margin paper-size :mm)))))

;;;     gtk-paper-size-default

(test gtk-paper-size-default
  (is (string= "iso_a4" (gtk-paper-size-default))))

;;;     gtk-paper-size-new-from-key-file
;;;     gtk-paper-size-to-key-file

(test gtk-paper-size-key-file
  (let ((key-file (g-key-file-new))
        (paper-size (gtk-paper-size-new "iso_a4")))
    ;; Write paper-size to key-file
    (gtk-paper-size-to-key-file paper-size key-file "Paper")

    (is (string= (g-key-file-to-data key-file)
"[Paper]
PPDName=A4
DisplayName=A4
Width=210
Height=297
"))
    ;; Read the paper size back
    (let ((paper-size (gtk-paper-size-new-from-key-file key-file "Paper")))
      (is (string= "iso_a4" (gtk-paper-size-name paper-size))))))

;;;     gtk-paper-size-new-from-gvariant
;;;     gtk-paper-size-to-gvariant

(test gtk-paper-size-gvariant
  (let* ((paper-size (gtk-paper-size-new "iso_a4"))
         (value (gtk-paper-size-to-gvariant paper-size)))
    ;; Check the value
    (is (string= (g-variant-print value nil)
                 "{'PPDName': <'A4'>, 'DisplayName': <'A4'>, 'Width': <210.0>, 'Height': <297.0>}"))
    ;; Read the value back to a paper size
    (is (eq 'gtk-paper-size (type-of (gtk-paper-size-new-from-gvariant value))))
    (is (string= "iso_a4" (gtk-paper-size-name (gtk-paper-size-new-from-gvariant value))))))

;;; 2021-10-15
