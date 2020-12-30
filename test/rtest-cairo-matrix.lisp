(def-suite cairo-matrix :in cairo-suite)
(in-suite cairo-matrix)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_matrix_t

(test cairo-matrix-t
  (is (= 48 (foreign-type-size '(:struct cairo-matrix-t))))
  (is (equal '(CAIRO::XX CAIRO::YX CAIRO::XY CAIRO::YY CAIRO::X0 CAIRO::Y0)
             (foreign-slot-names '(:struct cairo-matrix-t)))))

;;; --- Functions --------------------------------------------------------------

;;;     cairo_matrix_init

(test cairo-matrix-init
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init matrix 0 0 0 0 0 0))
    (is (every #'approx-equal
               '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_init_identity

(test cairo-matrix-init-identiy
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init-identity matrix))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_init_translate

(test cairo-matrix-init-translate
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init-translate matrix 1 2))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 1.0d0 2.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_init_scale

(test cairo-matrix-init-scale
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init-scale matrix 2 3))
    (is (every #'approx-equal
               '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_init_rotate

(test cairo-matrix-init-rotate.1
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init-rotate matrix (/ pi 2)))
    (is (every #'approx-equal
               '(0.0d0 1.0d0 -1.0d0 0.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

(test cairo-matrix-init-rotate.2
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init-rotate matrix pi))
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_translate

(test cairo-matrix-translate
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init matrix 1 0 0 1 0 0))
    (is-false (cairo-matrix-translate matrix 2 3))
    (is (every #'approx-equal
               '(1.0d0 0.0d0 0.0d0 1.0d0 2.0d0 3.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_scale

(test cairo-matrix-scale
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init matrix 1 0 0 1 0 0))
    (is-false (cairo-matrix-scale matrix 2 3))
    (is (every #'approx-equal
               '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_rotate

(test cairo-matrix-rotate
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init matrix 1 0 0 1 0 0))
    (is-false (cairo-matrix-rotate matrix pi))
    (is (every #'approx-equal
               '(-1.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_invert

(test cairo-matrix-invert
  (with-foreign-object (matrix '(:struct cairo-matrix-t))
    (is-false (cairo-matrix-init matrix 2 0 0 2 0 0))
    (is (eq :success (cairo-matrix-invert matrix)))
    (is (every #'approx-equal
               '(0.5d0 0.0d0 0.0d0 0.5d0 0.0d0 0.0d0)
               (cairo-matrix-to-list matrix)))))

;;;     cairo_matrix_multiply

(test cairo-matrix-multiply
  (with-foreign-objects ((result '(:struct cairo-matrix-t))
                         (matrix-a '(:struct cairo-matrix-t))
                         (matrix-b '(:struct cairo-matrix-t)))
    (is-false (cairo-matrix-init-identity result))
    (is-false (cairo-matrix-init matrix-a 1 1 1 1 0 0))
    (is-false (cairo-matrix-init matrix-b 2 0 0 2 0 0))
    (is-false (cairo-matrix-multiply result matrix-a matrix-b))
    (is (every #'approx-equal
               '(2.0d0 2.0d0 2.0d0 2.0d0 0.0d0 0.0d0)
               (cairo-matrix-to-list result)))))

;;;     cairo_matrix_transform_distance

(test cairo-matrix-transform-distance
  (with-foreign-objects ((matrix '(:struct cairo-matrix-t))
                         (dx :double)
                         (dy :double))
    (is (= 1.0d0 (setf (mem-ref dx :double) 1.0d0)))
    (is (= 2.0d0 (setf (mem-ref dy :double) 2.0d0)))
    (is-false (cairo-matrix-init matrix 2 0 0 2 0 0))
    (is-false (cairo-matrix-transform-distance matrix dx dy))
    (is (= 2.0d0 (mem-ref dx :double)))
    (is (= 4.0d0 (mem-ref dy :double)))))

;;;     cairo_matrix_transform_point

(test cairo-matrix-transform-point
  (with-foreign-objects ((matrix '(:struct cairo-matrix-t))
                         (x :double)
                         (y :double))
    (is (= 1.0d0 (setf (mem-ref x :double) 1.0d0)))
    (is (= 2.0d0 (setf (mem-ref y :double) 2.0d0)))
    (is-false (cairo-matrix-init matrix 2 0 0 2 0 0))
    (is-false (cairo-matrix-transform-point matrix x y))
    (is (= 2.0d0 (mem-ref x :double)))
    (is (= 4.0d0 (mem-ref y :double)))))

;;; 2020-12-6
