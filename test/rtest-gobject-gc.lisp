(def-suite gobject-gc :in gobject-suite)
(in-suite gobject-gc)

#+nil
(test registered-object-types
  (is (= 283
         (length (alexandria:hash-table-alist gobject::*registered-object-types*)))))

#+nil
(test registered-object-types
  (is (= 226
         (length (alexandria:hash-table-alist gobject::*registered-object-types*)))))

#+nil
(test gobject-gc-hooks
  (is-false gobject::*gobject-gc-hooks*))

#+nil
(test foreign-gobjects-weak
  (is (equal '()
             (alexandria:hash-table-alist gobject::*foreign-gobjects-weak*))))

#+nil
(test foreign-gobjects-strong
  (is (equal '()
             (alexandria:hash-table-alist gobject::*foreign-gobjects-strong*))))

#+nil
(test create-instance
  (let ((label (make-instance 'gtk-label)))
    (is-false gobject::*gobject-gc-hooks*)
    (is (equal (list label)
               (alexandria:hash-table-values gobject::*foreign-gobjects-weak*)))
   (is (equal '()
              (alexandria:hash-table-alist gobject::*foreign-gobjects-strong*))))
  (sb-ext:gc :full t))

;;; 2021-10-18
