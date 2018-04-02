(in-package :gtk-testsuite)

(def-suite gdk-seat :in gdk-suite)
(in-suite gdk-seat)

(test gdk-seat
  ;; Type checks
  (is-true  (g-type-is-object "GdkSeat"))
  (is-true (g-type-is-abstract "GdkSeat"))
  (is-true  (g-type-is-derived "GdkSeat"))
  (is-false (g-type-is-fundamental "GdkSeat"))
  (is-true  (g-type-is-value-type "GdkSeat"))
  (is-true  (g-type-has-value-table "GdkSeat"))
  (is-true  (g-type-is-classed "GdkSeat"))
  (is-true  (g-type-is-instantiatable "GdkSeat"))
  (is-true  (g-type-is-derivable "GdkSeat"))
  (is-true  (g-type-is-deep-derivable "GdkSeat"))
  (is-false (g-type-is-interface "GdkSeat"))

  ;; Check the registered name
  (is (eq 'gdk-seat
          (registered-object-type-by-name "GdkSeat")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GdkSeat"))))
    (is (equal (gtype "GdkSeat") (g-type-from-class class)))
    (is (equal (gtype "GdkSeat") (g-object-class-type class)))
    (is (equal "GdkSeat" (g-object-class-name class)))
    (is (equal (gtype "GdkSeat") (g-type-from-class  (g-type-class-peek "GdkSeat"))))
    (is (equal (gtype "GdkSeat") (g-type-from-class  (g-type-class-peek-static "GdkSeat"))))
    (g-type-class-unref class))

  ;; Test methods (except gdk-seat-{grab,ungrab} which need a window)
  (let ((seat (gdk-display-get-default-seat (gdk-display-get-default))))
    (is-true (typep seat 'gdk-seat))
    (is-true (typep (gdk-seat-display seat) 'gdk-display))
    (is-true (subsetp (gdk-seat-get-capabilities seat) '(:none :pointer :touch :tablet-stylus :keyboard)))
    (is-true (typep (gdk-seat-get-pointer seat) 'gdk-device))
    (is-true (typep (gdk-seat-get-keyboard seat) 'gdk-device))
    (is-true (every #'(lambda (x) (typep x 'gdk-device)) (gdk-seat-get-slaves seat '(:all))))))
