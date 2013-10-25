
(def-suite gobject-gc :in gobject-suite)
(in-suite gobject-gc)

(test registered-object-types
  (is (= 215
         (length (alexandria:hash-table-alist gobject::*registered-object-types*)))))

(test gobject-gc-hooks
  (is-false gobject::*gobject-gc-hooks*))

(test foreign-gobjects-weak
  (is (equal '()
             (alexandria:hash-table-alist gobject::*foreign-gobjects-weak*))))

(test foreign-gobjects-strong
  (is (equal '()
             (alexandria:hash-table-alist gobject::*foreign-gobjects-strong*))))

#+sbcl
(test create-instance
  (let ((label (make-instance 'gtk-label)))
    (is-false gobject::*gobject-gc-hooks*)
    (is (equal (list label)
               (alexandria:hash-table-values gobject::*foreign-gobjects-weak*)))
   (is (equal '()
              (alexandria:hash-table-alist gobject::*foreign-gobjects-strong*))))
  (sb-ext:gc :full t))



