
(def-suite gobject-enumeration :in gobject-suite)
(in-suite gobject-enumeration)

;;;   g-enum-value

(test g-enum-value
  (is (= 12 (foreign-type-size '(:struct g-enum-value))))
  (is (equal '(:name :nick :value)
             (stable-sort (foreign-slot-names '(:struct g-enum-value))
                          #'string-lessp))))

;;;   g-enum-class

(test g-enum-class
  (is (= 20 (foreign-type-size '(:struct g-enum-class))))
  (is (equal '(:maximum :minimum :n-values :type-class :values)
             (stable-sort (foreign-slot-names '(:struct g-enum-class))
             #'string-lessp))))

;;;   g-flags-value

(test g-flags-value
  (is (= 12 (foreign-type-size '(:struct g-flags-value))))
  (is (equal '(:name :nick :value)
             (stable-sort (foreign-slot-names '(:struct g-flags-value))
                          #'string-lessp))))

;;;   g-flags-class

(test g-flags-class
  (is (= 16 (foreign-type-size '(:struct g-flags-class))))
  (is (equal '(:mask :n-values :type-class :values)
             (stable-sort (foreign-slot-names '(:struct g-flags-class))
                          #'string-lessp))))

;;;   g-enum-class-type

(test g-enum-class-type
  (is (equal (gtype "GdkDragProtocol")
             (g-enum-class-type (g-type-class-ref "GdkDragProtocol")))))

;;;   g-enum-class-type-name

(test g-enum-class-type-name
  (is (equal "GdkDragProtocol"
             (g-enum-class-type-name (g-type-class-ref "GdkDragProtocol")))))

;;;   g-type-is-enum

(test g-type-is-enum
  (is-true (g-type-is-enum "GdkDragProtocol"))
  (is-false (g-type-is-enum "GdkWindow")))


