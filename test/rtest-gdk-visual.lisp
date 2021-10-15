(def-suite gdk-visual :in gdk-suite)
(in-suite gdk-visual)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkVisualType

(test gdk-visual-type
  ;; Check the type
  (is (g-type-is-enum "GdkVisualType"))
  ;; Check the type initializer
  (is (eq (gtype "GdkVisualType")
          (gtype (foreign-funcall "gdk_visual_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-visual-type
          (registered-enum-type "GdkVisualType")))
  ;; Check the names
  (is (equal '("GDK_VISUAL_STATIC_GRAY" "GDK_VISUAL_GRAYSCALE"
               "GDK_VISUAL_STATIC_COLOR" "GDK_VISUAL_PSEUDO_COLOR"
               "GDK_VISUAL_TRUE_COLOR" "GDK_VISUAL_DIRECT_COLOR")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkVisualType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkVisualType"))))
  ;; Check the nick names
  (is (equal '("static-gray" "grayscale" "static-color" "pseudo-color"
               "true-color" "direct-color")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkVisualType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkVisualType"
                             GDK-VISUAL-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_visual_type_get_type")
                             (:STATIC-GRAY 0)
                             (:GRAYSCALE 1)
                             (:STATIC-COLOR 2)
                             (:PSEUDO-COLOR 3)
                             (:TRUE-COLOR 4)
                             (:DIRECT-COLOR 5))
             (get-g-type-definition "GdkVisualType"))))

;;;     GdkByteOrder

(test gdk-byte-order
  ;; Check the type
  (is (g-type-is-enum "GdkByteOrder"))
  ;; Check the type initializer
  (is (eq (gtype"GdkByteOrder")
          (gtype (foreign-funcall "gdk_byte_order_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gdk-byte-order
          (registered-enum-type "GdkByteOrder")))
  ;; Check the names
  (is (equal '("GDK_LSB_FIRST" "GDK_MSB_FIRST")
             (mapcar #'enum-item-name
                     (get-enum-items "GdkByteOrder"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "GdkByteOrder"))))
  ;; Check the nick names
  (is (equal '("lsb-first" "msb-first")
             (mapcar #'enum-item-nick
                     (get-enum-items "GdkByteOrder"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkByteOrder"
                             GDK-BYTE-ORDER
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_byte_order_get_type")
                             (:LSB-FIRST 0)
                             (:MSB-FIRST 1))
             (get-g-type-definition "GdkByteOrder"))))

;;;     GdkVisual

(test gdk-visual-class
  ;; Type check
  (is (g-type-is-object "GdkVisual"))
  ;; Check the registered name
  (is (eq 'gdk-visual
          (registered-object-type-by-name "GdkVisual")))
  ;; Check the type initializer
  (is (eq (gtype "GdkVisual")
          (gtype (foreign-funcall "gdk_visual_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GdkVisual")))
  ;; Check the children
  #-windows
  (is (equal '("GdkX11Visual")
             (mapcar #'g-type-name (g-type-children "GdkVisual"))))
  #+windows
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GdkVisual"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GdkVisual"))))
  ;; Check the class properties
  (is (equal '()
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GdkVisual"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkVisual" GDK-VISUAL
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_visual_get_type")
                       NIL)
             (get-g-type-definition "GdkVisual"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-query-depths                                   deprecated

(test gdk-query-depths
  (is (listp (gdk-query-depths)))
  (is (every #'integerp (gdk-query-depths))))

;;;     gdk-query-visual-types                             deprecated

(test gdk-query-visual-types
  (is (listp (gdk-query-visual-types)))
  (is (member :true-color (gdk-query-visual-types))))

;;;     gdk-list-visuals                                   deprecated

(test gdk-list-visuals
  (is (> (length (gdk-list-visuals)) 0))
  (is (every (lambda (x) (eq x 'gdk-visual))
             (mapcar #'type-of (gdk-list-visuals)))))

;;;     gdk-visual-bits-per-rgb                            deprecated

(test gdk-visual-bits-per-rgb
  (let ((visual (gdk-visual-system)))
    (is (integerp (gdk-visual-bits-per-rgb visual)))))

;;;     gdk-visual-blue-pixel-details

(test gdk-visual-blue-pixel-details
  (let ((visual (gdk-visual-system)))
    (multiple-value-bind (mask shift prec)
        (gdk-visual-blue-pixel-details visual)
      (is (= #xff mask))
      (is (= 0 shift))
      (is (= 8 prec)))))

;;;     gdk-visual-byte-order                              deprecated

(test gdk-visual-byte-order
  (let ((visual (gdk-visual-system)))
    (is (eq :lsb-first (gdk-visual-byte-order visual)))))

;;;     gdk-visual-colormap-size                           deprecated

(test gdk-visual-colormap-size
  (let ((visual (gdk-visual-system)))
    (is (= 256 (gdk-visual-colormap-size visual)))))

;;;     gdk-visual-depth

(test gdk-visual-depth
  (let ((visual (gdk-visual-system)))
    (is (= 24 (gdk-visual-depth visual)))))

;;;     gdk-visual-green-pixel-details

(test gdk-visual-green-pixel-details
  (let ((visual (gdk-visual-system)))
    (multiple-value-bind (mask shift prec)
        (gdk-visual-green-pixel-details visual)
      (is (= #xff00 mask))
      (is (= 8 shift))
      (is (= 8 prec)))))

;;;     gdk-visual-red-pixel-details

(test gdk-visual-red-pixel-details
  (let ((visual (gdk-visual-system)))
    (multiple-value-bind (mask shift prec)
        (gdk-visual-red-pixel-details visual)
      (is (= #xff0000 mask))
      (is (= 16 shift))
      (is (= 8 prec)))))

;;;     gdk-visual-visual-type

(test gdk-visual-visual-type
  (let ((visual (gdk-visual-system)))
    (is (member (gdk-visual-visual-type visual)
                '(:static-gray :grayscale :static-color :pseudo-color
                  :true-color :direct-color)))))

;;;     gdk-visual-best-depth                              deprecated

(test gdk-visual-best-depth
  (is (integerp (gdk-visual-best-depth)))
  (is (member (gdk-visual-best-depth)
              (mapcar #'gdk-visual-depth (gdk-list-visuals)))))

;;;     gdk-visual-best-type                               deprecated

(test gdk-visual-best-type
  (is (member (gdk-visual-best-type)
              '(:static-gray :grayscale :static-color :pseudo-color
                :true-color :direct-color)))
  (is (member (gdk-visual-best-type)
              (mapcar #'gdk-visual-visual-type (gdk-list-visuals)))))

;;;     gdk-visual-system                                  deprecated

(test gdk-visual-system
  (is (eq 'gdk-visual (type-of (gdk-visual-system)))))

;;;     gdk-visual-best                                    deprecated

(test gdk-visual-best
  (is (eq 'gdk-visual (type-of (gdk-visual-best))))
  (is (= 32 (gdk-visual-depth (gdk-visual-best))))
  (is (eq :true-color (gdk-visual-visual-type (gdk-visual-best)))))

;;;     gdk-visual-best-with-depth                         deprecated

(test gdk-visual-best-with-depth
  (is-false (gdk-visual-best-with-depth 64))
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-depth 32))))
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-depth 24)))))

;;;     gdk-visual-best-with-type                          deprecated

#-windows
(test gdk-visual-best-with-type
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-type :true-color))))
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-type :direct-color)))))

;;;     gdk-visual-best-with-both                          deprecated

#-windows
(test gdk-visual-best-with-both
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-both 32 :true-color))))
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-both 24 :true-color))))
  (is-false (gdk-visual-best-with-both 32 :direct-color))
  (is (eq 'gdk-visual (type-of (gdk-visual-best-with-both 24 :direct-color)))))

;;;     gdk visual-screen

(test gdk-visual-screen
  (is (eq 'gdk-screen (type-of (gdk-visual-screen (gdk-visual-system))))))

;;; 2021-10-14
