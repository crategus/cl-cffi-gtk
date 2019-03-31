(def-suite gtk-box :in gtk-suite)
(in-suite gtk-box)

;;; --- gtk_box_new ------------------------------------------------------------

(test gtk-box-new
  (is (equal 'gtk-box (type-of (gtk-box-new :vertical))))
  (is (equal 'gtk-box (type-of (make-instance 'gtk-box :orientation :vertical))))
  (let ((box (make-instance 'gtk-box)))
    (is-false (gtk-box-baseline-position box))
    (is-false (gtk-box-homogeneous box))
    (is-false (gtk-box-spacing box))
  )
)

;;; --- gtk-box-pack-start -----------------------------------------------------

(test gtk-box-pack-start
  (let* ((box (make-instance 'gtk-box :orientation :vertical))
         (button1 (make-instance 'gtk-button))
         (button2 (make-instance 'gtk-button))
         (button3 (make-instance 'gtk-button)))
    (gtk-box-pack-start box button1)
    (is-false (gtk-box-child-position box button1))
    (gtk-box-pack-start box button2)
    (is-false (gtk-box-child-position box button1))
    (is-false (gtk-box-child-position box button2))
    (gtk-box-pack-start box button3)
    (is-false (gtk-box-child-position box button1))
    (is-false (gtk-box-child-position box button2))
    (is-false (gtk-box-child-position box button3))    
  )
)






;;;     gtk_box_pack_end
;;;     gtk_box_get_homogeneous                            Accessor
;;;     gtk_box_set_homogeneous                            Accessor
;;;     gtk_box_get_spacing                                Accessor
;;;     gtk_box_set_spacing                                Accessor
;;;     gtk_box_reorder_child
;;;     gtk_box_query_child_packing
;;;     gtk_box_set_child_packing
;;;     gtk_box_get_baseline_position                      Accessor
;;;     gtk_box_set_baseline_position                      Accessor
;;;     gtk_box_get_center_widget
;;;     gtk_box_set_center_widget
;;;


