(def-suite gtk-page-setup :in gtk-suite)
(in-suite gtk-page-setup)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetup

(test gtk-page-setup-class
  ;; Type check
  (is-true  (g-type-is-object "GtkPageSetup"))
  ;; Check the registered name
  (is (eq 'gtk-page-setup
          (registered-object-type-by-name "GtkPageSetup")))
  ;; Check the type initializer
  (is (string= "GtkPageSetup"
               (g-type-name (gtype (foreign-funcall "gtk_page_setup_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkPageSetup")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkPageSetup"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkPageSetup"))))
  ;; Check the class properties
  (is (equal '()
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkPageSetup"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPageSetup" GTK-PAGE-SETUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_page_setup_get_type")
                       NIL)
             (get-g-type-definition "GtkPageSetup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk-page-setup-new

(test gtk-page-setup-new
  (is (eq 'gtk-page-setup (type-of (gtk-page-setup-new)))))

;;;     gtk-page-setup-copy

(test gtk-page-setup-copy
  (let ((page-setup (gtk-page-setup-new)))
    (is (eq 'gtk-page-setup (type-of page-setup)))
    (is (eq 'gtk-page-setup (type-of (gtk-page-setup-copy page-setup))))))

;;;     gtk-page-setup-orientation

(test gtk-page-setup-orientation
  (let ((page-setup (gtk-page-setup-new)))
    ;; Get the orientation
    (is (eq :portrait (gtk-page-setup-orientation page-setup)))
    ;; Set the orientation
    (is (eq :landscape (setf (gtk-page-setup-orientation page-setup) :landscape)))
    (is (eq :landscape (gtk-page-setup-orientation page-setup)))))

;;;     gtk-page-setup-paper-size

(test gtk-page-setup-paper-size
  (let ((page-setup (gtk-page-setup-new)))
    ;; Get the paper size
    (is (eq 'gtk-paper-size (type-of (gtk-page-setup-paper-size page-setup))))
    (is (string= "iso_a4" (gtk-paper-size-name (gtk-page-setup-paper-size page-setup))))
    ;; Set the paper size
    (is (eq 'gtk-paper-size
            (type-of (setf (gtk-page-setup-paper-size page-setup)
                           (gtk-paper-size-new "iso_a3")))))
    (is (string= "iso_a3" (gtk-paper-size-name (gtk-page-setup-paper-size page-setup))))))

;;;     gtk-page-setup-get-top-margin
;;;     gtk-page-setup-set-top-margin

(test gtk-page-setup-top-margin
  (let ((page-setup (gtk-page-setup-new)))
    ;; Get top margin
    (is (= 6.35d0 (gtk-page-setup-get-top-margin page-setup :mm)))
    ;; Set top margin
    (is-false (gtk-page-setup-set-top-margin page-setup 10.0 :mm))
    (is (= 10.0 (gtk-page-setup-get-top-margin page-setup :mm)))))

;;;     gtk-page-setup-get-bottom-margin
;;;     gtk-page-setup-set-bottom-margin

(test gtk-page-setup-bottom-margin
  (let ((page-setup (gtk-page-setup-new)))
    ;; Get top margin
    (is (= 14.224d0 (gtk-page-setup-get-bottom-margin page-setup :mm)))
    ;; Set top margin
    (is-false (gtk-page-setup-set-bottom-margin page-setup 10.0 :mm))
    (is (= 10.0 (gtk-page-setup-get-bottom-margin page-setup :mm)))))

;;;     gtk-page-setup-get-left-margin
;;;     gtk-page-setup-set-left-margin

(test gtk-page-setup-left-margin
  (let ((page-setup (gtk-page-setup-new)))
    ;; Get left margin
    (is (= 6.35d0 (gtk-page-setup-get-left-margin page-setup :mm)))
    ;; Set left margin
    (is-false (gtk-page-setup-set-left-margin page-setup 10.0 :mm))
    (is (= 10.0 (gtk-page-setup-get-left-margin page-setup :mm)))))

;;;     gtk-page-setup-get-right-margin
;;;     gtk-page-setup-set-right-margin

(test gtk-page-setup-right-margin
  (let ((page-setup (gtk-page-setup-new)))
    ;; Get right margin
    (is (= 6.35d0 (gtk-page-setup-get-right-margin page-setup :mm)))
    ;; Set right margin
    (is-false (gtk-page-setup-set-right-margin page-setup 10.0 :mm))
    (is (= 10.0 (gtk-page-setup-get-right-margin page-setup :mm)))))

;;;     gtk-page-setup-set-paper-size-and-default-margins

(test gtk-page-setup-set-paper-size-and-default-margins
  (let ((page-setup (gtk-page-setup-new)))
    ;; Paper size for A4
    (is (= 210.0d0 (gtk-page-setup-get-paper-width page-setup :mm)))
    (is (= 297.0d0 (gtk-page-setup-get-paper-height page-setup :mm)))
    ;; Set paper size to A3
    (is-false (gtk-page-setup-set-paper-size-and-default-margins page-setup (gtk-paper-size-new "iso_a3")))
    ;; Paper size for A3
    (is (= 297.0d0 (gtk-page-setup-get-paper-width page-setup :mm)))
    (is (= 420.0d0 (gtk-page-setup-get-paper-height page-setup :mm)))))

;;;     gtk-page-setup-get-paper-width
;;;     gtk-page-setup-get-paper-height
;;;     gtk-page-setup-get-page-width
;;;     gtk-page-setup-get-page-height

(test gtk-page-setup-paper-and-page
  (let ((page-setup (gtk-page-setup-new)))
    (is (= 210.0d0 (gtk-page-setup-get-paper-width page-setup :mm)))
    (is (= 297.0d0 (gtk-page-setup-get-paper-height page-setup :mm)))
    (is (= 197.3d0 (gtk-page-setup-get-page-width page-setup :mm)))
    (is (= 276.426d0 (gtk-page-setup-get-page-height page-setup :mm)))))

;;;     gtk-page-setup-new-from-file
;;;     gtk-page-setup-to-file
;;;     gtk-page-setup-load-file

(test gtk-page-setup-file
  (let ((page-setup (gtk-page-setup-new)))
    (is-true (gtk-page-setup-to-file page-setup "rtest-gtk-page-setup.ini"))
    (is-true (gtk-page-setup-load-file page-setup "rtest-gtk-page-setup.ini"))
    (is (eq 'gtk-page-setup (type-of (gtk-page-setup-new-from-file "rtest-gtk-page-setup.ini"))))))

;;;     gtk-page-setup-new-from-key-file
;;;     gtk-page-setup-load-key-file
;;;     gtk-page-setup-to-key-file

(test gtk-page-setup-key-file
  (let ((page-setup (gtk-page-setup-new))
        (key-file (g-key-file-new)))
    (is-false (gtk-page-setup-to-key-file page-setup key-file "Page Setup"))
    (is-true (gtk-page-setup-load-key-file page-setup key-file "Page Setup"))
    (is (eq 'gtk-page-setup (type-of (gtk-page-setup-new-from-key-file key-file "Page Setup"))))))

;;;     gtk-page-setup-new-from-gvariant
;;;     gtk-page-setup-to-gvariant

(test gtk-page-setup-gvariant
  (let ((page-setup (gtk-page-setup-new)))
    (is (string= (g-variant-print (gtk-page-setup-to-gvariant page-setup) nil)
"{'PPDName': <'A4'>, 'DisplayName': <'A4'>, 'Width': <210.0>, 'Height': <297.0>, 'MarginTop': <6.3499999999999996>, 'MarginBottom': <14.224>, 'MarginLeft': <6.3499999999999996>, 'MarginRight': <6.3499999999999996>, 'Orientation': <'portrait'>}"))
    (let ((variant (g-variant-parse (g-variant-type-new "a{sv}")
"{'PPDName': <'A4'>, 'DisplayName': <'A4'>, 'Width': <210.0>, 'Height': <297.0>, 'MarginTop': <6.3499999999999996>, 'MarginBottom': <14.224>, 'MarginLeft': <6.3499999999999996>, 'MarginRight': <6.3499999999999996>, 'Orientation': <'portrait'>}")))
      (is (eq 'gtk-page-setup (type-of (gtk-page-setup-new-from-gvariant variant)))))))

