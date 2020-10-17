(def-suite gtk-adjustment :in gtk-suite)
(in-suite gtk-adjustment)

;;; --- gtk-adjustment ---------------------------------------------------------

(test gtk-adjustment-class
  ;; Type check
  (is-true  (g-type-is-object "GtkAdjustment"))
  ;; Check the registered name
  (is (eq 'gtk-adjustment
          (registered-object-type-by-name "GtkAdjustment")))
  ;; Check the parent
  (is (equal (gtype "GInitiallyUnowned") (g-type-parent "GtkAdjustment")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkAdjustment"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkAdjustment"))))
  ;; Check the class properties
  (is (equal '("lower" "page-increment" "page-size" "step-increment" "upper" "value")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkAdjustment"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAdjustment" GTK-ADJUSTMENT
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_adjustment_get_type")
                       ((LOWER GTK-ADJUSTMENT-LOWER "lower" "gdouble" T T)
                        (PAGE-INCREMENT GTK-ADJUSTMENT-PAGE-INCREMENT
                         "page-increment" "gdouble" T T)
                        (PAGE-SIZE GTK-ADJUSTMENT-PAGE-SIZE "page-size"
                         "gdouble" T T)
                        (STEP-INCREMENT GTK-ADJUSTMENT-STEP-INCREMENT
                         "step-increment" "gdouble" T T)
                        (UPPER GTK-ADJUSTMENT-UPPER "upper" "gdouble" T T)
                        (VALUE GTK-ADJUSTMENT-VALUE "value" "gdouble" T T)))
             (get-g-type-definition "GtkAdjustment"))))

;;; --- gtk-adjustment-properties -----------------------------------------------

(test gtk-adjustment-properties
  (let ((adjustment (make-instance 'gtk-adjustment)))
    (is (=  0.0d0 (gtk-adjustment-lower adjustment)))
    (is (=  0.0d0 (gtk-adjustment-page-increment adjustment)))
    (is (=  0.0d0 (gtk-adjustment-page-size adjustment)))
    (is (=  0.0d0 (gtk-adjustment-step-increment adjustment)))
    (is (=  0.0d0 (gtk-adjustment-upper adjustment)))
    (is (=  0.0d0 (gtk-adjustment-value adjustment)))

    (setf (gtk-adjustment-upper adjustment) 90.0d0)
    (setf (gtk-adjustment-value adjustment) 10.0d0)
    ;; value is clamped
    (is (= 10.0d0 (gtk-adjustment-value adjustment)))
    (setf (gtk-adjustment-value adjustment) 100.0d0)
    (is (= 90.0d0 (gtk-adjustment-value adjustment)))))

;;; --- gtk-adjustment-new -----------------------------------------------------

(test gtk-adjustment-new
  (let ((adjustment (gtk-adjustment-new 10.0d0       ; value
                                         0.0d0       ; lower
                                       100.0d0       ; upper
                                         5.0d0       ; step-increment
                                        10.0d0       ; page-increment
                                        10.0d0)))    ; page-size
    (is (=  10.0d0 (gtk-adjustment-value adjustment)))
    (is (=   0.0d0 (gtk-adjustment-lower adjustment)))
    (is (= 100.0d0 (gtk-adjustment-upper adjustment)))
    (is (=   5.0d0 (gtk-adjustment-step-increment adjustment)))
    (is (=  10.0d0 (gtk-adjustment-page-increment adjustment)))
    (is (=  10.0d0 (gtk-adjustment-page-size adjustment)))))

;;;     gtk_adjustment_clamp_page
;;;     gtk_adjustment_changed                           * deprecated
;;;     gtk_adjustment_value_changed                     * deprecated
;;;     gtk_adjustment_configure

;;;     gtk_adjustment_get_minimum_increment

