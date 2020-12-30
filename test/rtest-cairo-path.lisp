(def-suite cairo-path :in cairo-suite)
(in-suite cairo-path)

;;; Types and Values

;;;     cairo_path_t
;;;     cairo_path_data_t
;;;     cairo_path_data_type_t

;;; Functions

;;;     cairo_copy_path

#+nil
(test cairo-copy-path
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))

    (is-false (cairo-new-path context))
    (is-false (cairo-move-to context 10 10))
    (is-false (cairo-line-to context 20 20))
    (is-false (cairo-close-path context))

    (let* ((path-list nil)
           (path (cairo-copy-path context))
           (data (foreign-slot-value path '(:struct cairo-path-t) 'cairo::data))
           (num-data (foreign-slot-value path
                                         '(:struct cairo-path-t)
                                         'cairo::num-data)))

      (is (eq :success
              (foreign-slot-value path '(:struct cairo-path-t) 'cairo::status)))
      (is (pointerp
            (foreign-slot-value path '(:struct cairo-path-t) 'cairo::data)))
      (is (= 7
             (foreign-slot-value path '(:struct cairo-path-t) 'cairo::num-data)))

      (loop for count from 0 below num-data
            for header = (foreign-slot-pointer path
                                               '(:struct cairo-path-data-t)
                                               'cairo::header)
            for data-type = (foreign-slot-value header
                                               '(:struct cairo::header-t)
                                               'cairo::data-type)
            for length = (foreign-slot-value header
                                             '(:struct cairo::header-t)
                                              'cairo::length)
            do (format t "~%     count : ~a~%" count)
               (format t "~&    header : ~a~%" header)
               (format t "~& data-type : ~a~%" data-type)
               (format t "~&    length : ~a~%" length)
               (format t "~&    offset : ~a~%" (* length (foreign-type-size :pointer)))


               (incf-pointer data (* length (foreign-type-size :pointer)))


      )
)))

#+nil
(test cairo-copy-path.1
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))

    (is-false (cairo-new-path context))
    (is-false (cairo-move-to context 10 10))
    (is-false (cairo-line-to context 20 20))
    (is-false (cairo-close-path context))

    (let* ((path-list nil)
           (path (cairo-copy-path context))
           (data (foreign-slot-value path '(:struct cairo-path-t) 'cairo::data))
           (num-data (foreign-slot-value path
                                         '(:struct cairo-path-t)
                                         'cairo::num-data)))

      (is (eq :success
              (foreign-slot-value path '(:struct cairo-path-t) 'cairo::status)))
      (is (pointerp
            (foreign-slot-value path '(:struct cairo-path-t) 'cairo::data)))
      (is (= 7
             (foreign-slot-value path '(:struct cairo-path-t) 'cairo::num-data)))

      (dotimes (count num-data)
        (let* ((path-data (mem-aptr data '(:struct cairo-path-data-t) count))
               (header (foreign-slot-pointer path-data
                                             '(:struct cairo-path-data-t)
                                             'cairo::header))
               (points (foreign-slot-pointer path-data
                                             '(:struct cairo-path-data-t)
                                             'cairo::point)))

          (is (pointerp (mem-aptr data '(:struct cairo-path-data-t) count)))

          (is (pointerp header))
          (is (pointerp points))

          (let ((data-type (foreign-slot-value header
                                               '(:struct cairo::header-t)
                                               'cairo::data-type))
                (length (foreign-slot-value header
                                            '(:struct cairo::header-t)
                                            'cairo::length)))

          (format t "~& ~a ~a~%" data-type length)

          (push (foreign-slot-value header
                                        '(:struct cairo::header-t)
                                        'cairo::data-type)
                          path-list)

          (push (foreign-slot-value header
                                        '(:struct cairo::header-t)
                                        'cairo::length)
                          path-list)

          (setf length (if (< length 10) length 10))

          (dotimes (i length)
            (let* ((point-ptr (mem-aptr points '(:struct cairo::point-t) i)))

              (is (pointerp point-ptr))

              (push (foreign-slot-value point-ptr
                                            '(:struct cairo::point-t)
                                            'cairo::x)
                    path-list)
              (push (foreign-slot-value point-ptr
                                            '(:struct cairo::point-t)
                                            'cairo::y)
                    path-list)


          ))


      )))

      (is (equal '() (reverse path-list)))

)))

;;;     cairo_copy_path_flat
;;;     cairo_path_destroy
;;;     cairo_append_path

;;;     cairo_has_current_point
;;;     cairo_get_current_point

(test cairo-current-point
  (let* ((surface (cairo-image-surface-create :rgb24 100 150))
         (context (cairo-create surface)))
    (is-false (cairo-new-path context))
    (is-false (cairo-move-to context 10 10))
    (is-true (cairo-has-current-point context))
    (is (equal '(10.0d0 10.0d0)
                (multiple-value-list (cairo-get-current-point context))))
    (is-false (cairo-line-to context 20 20))
    (is-true (cairo-has-current-point context))
    (is (equal '(20.0d0 20.0d0)
                (multiple-value-list (cairo-get-current-point context))))))

;;;     cairo_new_path
;;;     cairo_new_sub_path
;;;     cairo_close_path
;;;     cairo_arc
;;;     cairo_arc_negative
;;;     cairo_curve_to
;;;     cairo_line_to
;;;     cairo_move_to
;;;     cairo_rectangle
;;;     cairo_glyph_path
;;;     cairo_text_path
;;;     cairo_rel_curve_to
;;;     cairo_rel_line_to
;;;     cairo_rel_move_to
;;;     cairo_path_extents

;;; 2020-12-23
