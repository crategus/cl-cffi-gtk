(def-suite gdk-rectangle :in gdk-suite)
(in-suite gdk-rectangle)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkRectangle

(test gdk-rectangle-struct
  ;; Type check
  (is-true (g-type-is-a (gtype "GdkRectangle") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GdkRectangle")
          (gtype (foreign-funcall "gdk_rectangle_get_type" g-size)))))

(test gdk-rectangle-properties
  (let ((rect (gdk-rectangle-new)))
    (is (= 10 (setf (gdk-rectangle-x rect) 10)))
    (is (= 10 (gdk-rectangle-x rect)))
    (is (= 20 (setf (gdk-rectangle-y rect) 20)))
    (is (= 20 (gdk-rectangle-y rect)))
    (is (= 30 (setf (gdk-rectangle-width rect) 30)))
    (is (= 30 (gdk-rectangle-width rect)))
    (is (= 40 (setf (gdk-rectangle-width rect) 40)))
    (is (= 40 (gdk-rectangle-width rect)))))

(test gdk-rectangle-new
  (let ((rect (gdk-rectangle-new :x 10 :y 20 :width 30 :height 40)))
    (is (= 10 (gdk-rectangle-x rect)))
    (is (= 20 (gdk-rectangle-y rect)))
    (is (= 30 (gdk-rectangle-width rect)))
    (is (= 40 (gdk-rectangle-height rect)))))

(test gdk-rectangle-copy
  (let* ((rect1 (gdk-rectangle-new :x 10 :y 20 :width 30 :height 40))
         (rect2 (gdk-rectangle-copy rect1)))
    ;; Set new values for rect1
    (is (= 0 (setf (gdk-rectangle-x rect1) 0)))
    (is (= 0 (setf (gdk-rectangle-y rect1) 0)))
    (is (= 0 (setf (gdk-rectangle-width rect1) 0)))
    (is (= 0 (setf (gdk-rectangle-height rect1) 0)))
    ;; rect2 is a copy of rect1
    (is (= 10 (gdk-rectangle-x rect2)))
    (is (= 20 (gdk-rectangle-y rect2)))
    (is (= 30 (gdk-rectangle-width rect2)))
    (is (= 40 (gdk-rectangle-height rect2)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_rectangle_intersect

(test gdk-rectangle-intersect
  (let* ((rect1 (gdk-rectangle-new :x 10 :y 20 :width 100 :height 200))
         (rect2 (gdk-rectangle-new :x 50 :y 60 :width 100 :height 200))
         (rect3 (gdk-rectangle-intersect rect1 rect2)))
    (is (=  50 (gdk-rectangle-x rect3)))
    (is (=  60 (gdk-rectangle-y rect3)))
    (is (=  60 (gdk-rectangle-width rect3)))
    (is (= 160 (gdk-rectangle-height rect3)))))

;;;     gdk_rectangle_union

(test gdk-rectangle-union
  (let* ((rect1 (gdk-rectangle-new :x 10 :y 20 :width 100 :height 200))
         (rect2 (gdk-rectangle-new :x 50 :y 60 :width 100 :height 200))
         (rect3 (gdk-rectangle-union rect1 rect2)))
    (is (=  10 (gdk-rectangle-x rect3)))
    (is (=  20 (gdk-rectangle-y rect3)))
    (is (= 140 (gdk-rectangle-width rect3)))
    (is (= 240 (gdk-rectangle-height rect3)))))

;;;     gdk_rectangle_equal

(test gdk-rectangle-equal
  (let* ((rect1 (gdk-rectangle-new :x 10 :y 20 :width 100 :height 200))
         (rect2 (gdk-rectangle-new :x 50 :y 60 :width 100 :height 200))
         (rect3 (gdk-rectangle-copy rect1)))
    (is-false (gdk-rectangle-equal rect1 rect2))
    (is-true  (gdk-rectangle-equal rect1 rect3))))

;;; 2020-11-9
