(def-suite cairo-image-surface :in cairo-suite)
(in-suite cairo-image-surface)

(test cairo-format-stride-for-width
  (is (not (eql -1 (cairo-format-stride-for-width :rgb24 100)))))

(test cairo-image-surface-create-for-data
  (let* ((format :rgb24)
         (surface1 (cairo-image-surface-create format 100 150))
         (stride (cairo-format-stride-for-width format 100))
         (data (cairo-image-surface-get-data surface1))
         (surface2 (cairo-image-surface-create-for-data data format 100 150 stride))
         (context (cairo-create surface2)))
    (is-true (pointerp context))
    (is (eq :success (cairo-status context)))
    (is (eql 100 (cairo-image-surface-get-width surface2)))
    (is (eql 150 (cairo-image-surface-get-height surface2)))
    (is (eql stride (cairo-image-surface-get-stride surface2)))
    (cairo-destroy context)))
