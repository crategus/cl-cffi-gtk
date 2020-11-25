(def-suite gobject-type-info :in gobject-suite)
(in-suite gobject-type-info)

;;; --- Types and Values -------------------------------------------------------

;;;     G_TYPE_INVALID
;;;     G_TYPE_NONE
;;;     G_TYPE_INTERFACE
;;;     G_TYPE_CHAR
;;;     G_TYPE_UCHAR
;;;     G_TYPE_BOOLEAN
;;;     G_TYPE_INT
;;;     G_TYPE_UINT
;;;     G_TYPE_LONG
;;;     G_TYPE_ULONG
;;;     G_TYPE_INT64
;;;     G_TYPE_UINT64
;;;     G_TYPE_ENUM
;;;     G_TYPE_FLAGS
;;;     G_TYPE_FLOAT
;;;     G_TYPE_DOUBLE
;;;     G_TYPE_STRING
;;;     G_TYPE_POINTER
;;;     G_TYPE_BOXED
;;;     G_TYPE_PARAM
;;;     G_TYPE_OBJECT
;;;     G_TYPE_GTYPE
;;;     G_TYPE_VARIANT
;;;     G_TYPE_CHECKSUM

(test g-type-constants.1
  (is (= (ash  0 2) +g-type-invalid+))
  (is (= (ash  1 2) +g-type-none+))
  (is (= (ash  2 2) +g-type-interface+))
  (is (= (ash  3 2) +g-type-char+))
  (is (= (ash  4 2) +g-type-uchar+))
  (is (= (ash  5 2) +g-type-boolean+))
  (is (= (ash  6 2) +g-type-int+))
  (is (= (ash  7 2) +g-type-uint+))
  (is (= (ash  8 2) +g-type-long+))
  (is (= (ash  9 2) +g-type-ulong+))
  (is (= (ash 10 2) +g-type-int64+))
  (is (= (ash 11 2) +g-type-uint64+))
  (is (= (ash 12 2) +g-type-enum+))
  (is (= (ash 13 2) +g-type-flags+))
  (is (= (ash 14 2) +g-type-float+))
  (is (= (ash 15 2) +g-type-double+))
  (is (= (ash 16 2) +g-type-string+))
  (is (= (ash 17 2) +g-type-pointer+))
  (is (= (ash 18 2) +g-type-boxed+))
  (is (= (ash 19 2) +g-type-param+))
  (is (= (ash 20 2) +g-type-object+))
  (is (= (ash 21 2) +g-type-variant+)))

(test g-type-constants.2
  (is (eq (gtype "void") (gtype +g-type-none+)))
  (is (eq (gtype "GInterface") (gtype +g-type-interface+)))
  (is (eq (gtype "gchar") (gtype +g-type-char+)))
  (is (eq (gtype "guchar") (gtype +g-type-uchar+)))
  (is (eq (gtype "gboolean") (gtype +g-type-boolean+)))
  (is (eq (gtype "gint") (gtype +g-type-int+)))
  (is (eq (gtype "guint") (gtype +g-type-uint+)))
  (is (eq (gtype "glong") (gtype +g-type-long+)))
  (is (eq (gtype "gulong") (gtype +g-type-ulong+)))
  (is (eq (gtype "gint64") (gtype +g-type-int64+)))
  (is (eq (gtype "guint64") (gtype +g-type-uint64+)))
  (is (eq (gtype "GEnum") (gtype +g-type-enum+)))
  (is (eq (gtype "GFlags") (gtype +g-type-flags+)))
  (is (eq (gtype "gfloat") (gtype +g-type-float+)))
  (is (eq (gtype "gdouble") (gtype +g-type-double+)))
  (is (eq (gtype "gchararray") (gtype +g-type-string+)))
  (is (eq (gtype "gpointer") (gtype +g-type-pointer+)))
  (is (eq (gtype "GBoxed") (gtype +g-type-boxed+)))
  (is (eq (gtype "GParam") (gtype +g-type-param+)))
  (is (eq (gtype "GObject") (gtype +g-type-object+)))
  (is (eq (gtype "GType") (gtype +g-type-gtype+)))
  (is (eq (gtype "GVariant") (gtype +g-type-variant+)))
  (is (eq (gtype "GChecksum") (gtype +g-type-checksum+))))

;;;     GType

;;;     GTypeInterface

(test g-type-interface-structure
  (let ((interface (g-type-default-interface-ref "GtkOrientable")))
    (is (= 16 (foreign-type-size '(:struct g-type-interface))))
    (is (equal '(:instance-type :type)
               (stable-sort (foreign-slot-names '(:struct g-type-interface))
                            #'string-lessp)))
    (is (eq (gtype "GtkOrientable")
            (foreign-slot-value interface
                                '(:struct g-type-interface) :type)))
    (is-false (foreign-slot-value interface
                                  '(:struct g-type-interface) :instance-type))))

;;;     GTypeInstance

(test g-type-instance-structure
  (let* ((button (make-instance 'gtk-button))
         (class (foreign-slot-value (pointer button)
                                    '(:struct g-type-instance) :class)))
    (is (= 8 (foreign-type-size '(:struct g-type-instance))))
    (is (equal '(:class) (foreign-slot-names '(:struct g-type-instance))))
    (is (eq (gtype "GtkButton") (g-type-from-class class)))))

;;;     GTypeClass

(test g-type-class-structure
  (let ((class (g-type-class-ref "GtkButton")))
    (is (= 8 (foreign-type-size '(:struct g-type-class))))
    (is (equal '(:type) (foreign-slot-names '(:struct g-type-class))))
    (is (eq (gtype "GtkButton")
            (foreign-slot-value class '(:struct g-type-class) :type)))))

;;;     GTypeInfo
;;;     GTypeFundamentalInfo
;;;     GInterfaceInfo
;;;     GTypeValueTable
;;;     GTypeDebugFlags                                    not implemented
;;;     GTypeQuery
;;;     GTypeFlags
;;;     GTypeFundamentalFlags

;;; --- Functions --------------------------------------------------------------

;;;   g_type_fundamental

(test g-type-fundamental
  (is (eq (gtype "GObject") (g-type-fundamental "GtkWidget")))
  (is (eq (gtype "GObject") (g-type-fundamental "GtkContainer")))
  (is (eq (gtype "GObject") (g-type-fundamental "GtkBox")))
  (is (eq (gtype "GObject") (g-type-fundamental "GtkWindow")))
  (is (eq (gtype "GInterface") (g-type-fundamental "GtkOrientable")))
  (is (eq (gtype "GFlags") (g-type-fundamental "GtkAccelFlags")))
  (is (eq (gtype "GEnum") (g-type-fundamental "GtkArrowPlacement")))
  (is (eq (gtype "GBoxed") (g-type-fundamental "GdkRGBA")))
  (is (eq (gtype "GBoxed") (g-type-fundamental "GtkTreePath"))))

;;;   g_type_make_fundamental                              not exported

;;;   g_type_is_abstract

(test g-type-is-abstract
  (is-false (g-type-is-abstract +g-type-invalid+))
  (is-false (g-type-is-abstract +g-type-none+))
  (is-false (g-type-is-abstract +g-type-interface+))
  (is-false (g-type-is-abstract +g-type-char+))
  (is-false (g-type-is-abstract +g-type-uchar+))
  (is-false (g-type-is-abstract +g-type-boolean+))
  (is-false (g-type-is-abstract +g-type-int+))
  (is-false (g-type-is-abstract +g-type-uint+))
  (is-false (g-type-is-abstract +g-type-long+))
  (is-false (g-type-is-abstract +g-type-ulong+))
  (is-false (g-type-is-abstract +g-type-int64+))
  (is-false (g-type-is-abstract +g-type-uint64+))
  (is-true  (g-type-is-abstract +g-type-enum+))
  (is-true  (g-type-is-abstract +g-type-flags+))
  (is-false (g-type-is-abstract +g-type-float+))
  (is-false (g-type-is-abstract +g-type-double+))
  (is-false (g-type-is-abstract +g-type-string+))
  (is-false (g-type-is-abstract +g-type-pointer+))
  (is-true  (g-type-is-abstract +g-type-boxed+))
  (is-true  (g-type-is-abstract +g-type-param+))
  (is-false (g-type-is-abstract +g-type-object+))
  (is-false (g-type-is-abstract +g-type-gtype+))
  (is-false (g-type-is-abstract +g-type-variant+))
  (is-false (g-type-is-abstract +g-type-checksum+))
  (is-true  (g-type-is-abstract "GtkWidget"))
  (is-true  (g-type-is-abstract "GtkContainer"))
  (is-false (g-type-is-abstract "GtkBox"))
  (is-false (g-type-is-abstract "GtkWindow"))
  (is-false (g-type-is-abstract "GtkOrientable"))
  (is-false (g-type-is-abstract "GtkAccelFlags"))
  (is-false (g-type-is-abstract "GtkArrowPlacement"))
  (is-false (g-type-is-abstract "GdkRGBA"))
  (is-false (g-type-is-abstract "GtkTreePath")))

;;;   g_type_is_dervied

(test g-type-is-derived
  (is-false (g-type-is-derived +g-type-invalid+))
  (is-false (g-type-is-derived +g-type-none+))
  (is-false (g-type-is-derived +g-type-interface+))
  (is-false (g-type-is-derived +g-type-char+))
  (is-false (g-type-is-derived +g-type-uchar+))
  (is-false (g-type-is-derived +g-type-boolean+))
  (is-false (g-type-is-derived +g-type-int+))
  (is-false (g-type-is-derived +g-type-uint+))
  (is-false (g-type-is-derived +g-type-long+))
  (is-false (g-type-is-derived +g-type-ulong+))
  (is-false (g-type-is-derived +g-type-int64+))
  (is-false (g-type-is-derived +g-type-uint64+))
  (is-false (g-type-is-derived +g-type-enum+))
  (is-false (g-type-is-derived +g-type-flags+))
  (is-false (g-type-is-derived +g-type-float+))
  (is-false (g-type-is-derived +g-type-double+))
  (is-false (g-type-is-derived +g-type-string+))
  (is-false (g-type-is-derived +g-type-pointer+))
  (is-false (g-type-is-derived +g-type-boxed+))
  (is-false (g-type-is-derived +g-type-param+))
  (is-false (g-type-is-derived +g-type-object+))
  (is-true  (g-type-is-derived +g-type-gtype+))
  (is-false (g-type-is-derived +g-type-variant+))
  (is-true  (g-type-is-derived +g-type-checksum+))
  (is-true  (g-type-is-derived "GtkWidget"))
  (is-true  (g-type-is-derived "GtkContainer"))
  (is-true  (g-type-is-derived "GtkBox"))
  (is-true  (g-type-is-derived "GtkWindow"))
  (is-true  (g-type-is-derived "GtkOrientable"))
  (is-true  (g-type-is-derived "GtkAccelFlags"))
  (is-true  (g-type-is-derived "GtkArrowPlacement"))
  (is-true  (g-type-is-derived "GdkRGBA"))
  (is-true  (g-type-is-derived "GtkTreePath")))

;;;   g_type_is_fundamental

(test g-type-is-fundamental
  (is-true  (g-type-is-fundamental +g-type-invalid+))
  (is-true  (g-type-is-fundamental +g-type-none+))
  (is-true  (g-type-is-fundamental +g-type-interface+))
  (is-true  (g-type-is-fundamental +g-type-char+))
  (is-true  (g-type-is-fundamental +g-type-uchar+))
  (is-true  (g-type-is-fundamental +g-type-boolean+))
  (is-true  (g-type-is-fundamental +g-type-int+))
  (is-true  (g-type-is-fundamental +g-type-uint+))
  (is-true  (g-type-is-fundamental +g-type-long+))
  (is-true  (g-type-is-fundamental +g-type-ulong+))
  (is-true  (g-type-is-fundamental +g-type-int64+))
  (is-true  (g-type-is-fundamental +g-type-uint64+))
  (is-true  (g-type-is-fundamental +g-type-enum+))
  (is-true  (g-type-is-fundamental +g-type-flags+))
  (is-true  (g-type-is-fundamental +g-type-float+))
  (is-true  (g-type-is-fundamental +g-type-double+))
  (is-true  (g-type-is-fundamental +g-type-string+))
  (is-true  (g-type-is-fundamental +g-type-pointer+))
  (is-true  (g-type-is-fundamental +g-type-boxed+))
  (is-true  (g-type-is-fundamental +g-type-param+))
  (is-true  (g-type-is-fundamental +g-type-object+))
  (is-false (g-type-is-fundamental +g-type-gtype+))
  (is-true  (g-type-is-fundamental +g-type-variant+))
  (is-false (g-type-is-fundamental +g-type-checksum+))
  (is-false (g-type-is-fundamental "GtkWidget"))
  (is-false (g-type-is-fundamental "GtkContainer"))
  (is-false (g-type-is-fundamental "GtkBox"))
  (is-false (g-type-is-fundamental "GtkWindow"))
  (is-false (g-type-is-fundamental "GtkOrientable"))
  (is-false (g-type-is-fundamental "GtkAccelFlags"))
  (is-false (g-type-is-fundamental "GtkArrowPlacement"))
  (is-false (g-type-is-fundamental "GdkRGBA"))
  (is-false (g-type-is-fundamental "GtkTreePath")))

;;;   g_type_is_value_type

(test g-type-is-value-type
  (is-false (g-type-is-value-type +g-type-invalid+))
  (is-false (g-type-is-value-type +g-type-none+))
  (is-false (g-type-is-value-type +g-type-interface+))
  (is-true  (g-type-is-value-type +g-type-char+))
  (is-true  (g-type-is-value-type +g-type-uchar+))
  (is-true  (g-type-is-value-type +g-type-boolean+))
  (is-true  (g-type-is-value-type +g-type-int+))
  (is-true  (g-type-is-value-type +g-type-uint+))
  (is-true  (g-type-is-value-type +g-type-long+))
  (is-true  (g-type-is-value-type +g-type-ulong+))
  (is-true  (g-type-is-value-type +g-type-int64+))
  (is-true  (g-type-is-value-type +g-type-uint64+))
  (is-false (g-type-is-value-type +g-type-enum+))
  (is-false (g-type-is-value-type +g-type-flags+))
  (is-true  (g-type-is-value-type +g-type-float+))
  (is-true  (g-type-is-value-type +g-type-double+))
  (is-true  (g-type-is-value-type +g-type-string+))
  (is-true  (g-type-is-value-type +g-type-pointer+))
  (is-false (g-type-is-value-type +g-type-boxed+))
  (is-true  (g-type-is-value-type +g-type-param+))
  (is-true  (g-type-is-value-type +g-type-object+))
  (is-true  (g-type-is-value-type +g-type-gtype+))
  (is-true  (g-type-is-value-type +g-type-variant+))
  (is-true  (g-type-is-value-type +g-type-checksum+))
  (is-true  (g-type-is-value-type "GtkWidget"))
  (is-true  (g-type-is-value-type "GtkContainer"))
  (is-true  (g-type-is-value-type "GtkBox"))
  (is-true  (g-type-is-value-type "GtkWindow"))
  (is-true  (g-type-is-value-type "GtkOrientable"))
  (is-true  (g-type-is-value-type "GtkAccelFlags"))
  (is-true  (g-type-is-value-type "GtkArrowPlacement"))
  (is-true  (g-type-is-value-type "GdkRGBA"))
  (is-true  (g-type-is-value-type "GtkTreePath")))

;;;   g_type_has_value_table                               not exported

;;;   g_type_is_classed

(test g-type-is-classed
  (is-false (g-type-is-classed +g-type-invalid+))
  (is-false (g-type-is-classed +g-type-none+))
  (is-false (g-type-is-classed +g-type-interface+))
  (is-false (g-type-is-classed +g-type-char+))
  (is-false (g-type-is-classed +g-type-uchar+))
  (is-false (g-type-is-classed +g-type-boolean+))
  (is-false (g-type-is-classed +g-type-int+))
  (is-false (g-type-is-classed +g-type-uint+))
  (is-false (g-type-is-classed +g-type-long+))
  (is-false (g-type-is-classed +g-type-ulong+))
  (is-false (g-type-is-classed +g-type-int64+))
  (is-false (g-type-is-classed +g-type-uint64+))
  (is-true  (g-type-is-classed +g-type-enum+))
  (is-true  (g-type-is-classed +g-type-flags+))
  (is-false (g-type-is-classed +g-type-float+))
  (is-false (g-type-is-classed +g-type-double+))
  (is-false (g-type-is-classed +g-type-string+))
  (is-false (g-type-is-classed +g-type-pointer+))
  (is-false (g-type-is-classed +g-type-boxed+))
  (is-true  (g-type-is-classed +g-type-param+))
  (is-true  (g-type-is-classed +g-type-object+))
  (is-false (g-type-is-classed +g-type-variant+))

  (is-true  (g-type-is-classed "GtkWidget"))
  (is-true  (g-type-is-classed "GtkContainer"))
  (is-true  (g-type-is-classed "GtkBox"))
  (is-true  (g-type-is-classed "GtkWindow"))
  (is-false (g-type-is-classed "GtkOrientable"))
  (is-true  (g-type-is-classed "GtkAccelFlags"))
  (is-true  (g-type-is-classed "GtkArrowPlacement"))
  (is-false (g-type-is-classed "GdkRGBA"))
  (is-false (g-type-is-classed "GtkTreePath")))

;;;   g_type_is_instantiatable                             not exported
;;;   g_type_is_derivable                                  not exported
;;;   g_type_is_deep_derivable                             not exported

;;; g_type_is_interface

(test g-type-is-interface
  (is-false (g-type-is-interface +g-type-invalid+))
  (is-false (g-type-is-interface +g-type-none+))
  (is-true  (g-type-is-interface +g-type-interface+))
  (is-false (g-type-is-interface +g-type-char+))
  (is-false (g-type-is-interface +g-type-uchar+))
  (is-false (g-type-is-interface +g-type-boolean+))
  (is-false (g-type-is-interface +g-type-int+))
  (is-false (g-type-is-interface +g-type-uint+))
  (is-false (g-type-is-interface +g-type-long+))
  (is-false (g-type-is-interface +g-type-ulong+))
  (is-false (g-type-is-interface +g-type-int64+))
  (is-false (g-type-is-interface +g-type-uint64+))
  (is-false (g-type-is-interface +g-type-enum+))
  (is-false (g-type-is-interface +g-type-flags+))
  (is-false (g-type-is-interface +g-type-float+))
  (is-false (g-type-is-interface +g-type-double+))
  (is-false (g-type-is-interface +g-type-string+))
  (is-false (g-type-is-interface +g-type-pointer+))
  (is-false (g-type-is-interface +g-type-boxed+))
  (is-false (g-type-is-interface +g-type-param+))
  (is-false (g-type-is-interface +g-type-object+))
  (is-false (g-type-is-interface +g-type-variant+))

  (is-false (g-type-is-interface "GtkWidget"))
  (is-false (g-type-is-interface "GtkContainer"))
  (is-false (g-type-is-interface "GtkBox"))
  (is-false (g-type-is-interface "GtkWindow"))
  (is-true  (g-type-is-interface "GtkOrientable"))
  (is-false (g-type-is-interface "GtkAccelFlags"))
  (is-false (g-type-is-interface "GtkArrowPlacement"))
  (is-false (g-type-is-interface "GdkRGBA"))
  (is-false (g-type-is-interface "GtkTreePath")))

;;; G_TYPE_FROM_INSTANCE

(test g-type-from-instance
  (is (eq (gtype "GtkButton")
          (g-type-from-instance (make-instance 'gtk-button)))))

;;;   G_TYPE_FROM_CLASS

(test g-type-from-class
  (is (eq (gtype "GtkWidget")
          (g-type-from-class (g-type-class-ref "GtkWidget"))))
  (is (eq (gtype "GtkContainer")
          (g-type-from-class (g-type-class-ref "GtkContainer"))))
  (is (eq (gtype "GtkButton")
          (g-type-from-class (g-type-class-ref "GtkButton")))))

;;;   G_TYPE_FROM_INTERFACE

(test g-type-from-interface
  (is (eq (gtype "GtkOrientable")
          (g-type-from-interface (g-type-default-interface-ref "GtkOrientable")))))

;;;     g-type-instance-class

(test g-type-instance-class
  (is (eq (gtype "GtkButton")
          (g-type-from-class (g-type-instance-class (make-instance 'gtk-button))))))

;;;     G_TYPE_INSTANCE_GET_INTERFACE                      not implemented
;;;     G_TYPE_INSTANCE_GET_PRIVATE                        not implemented
;;;     G_TYPE_CLASS_GET_PRIVATE                           not implemented
;;;     G_TYPE_CHECK_INSTANCE                              not implemented
;;;     G_TYPE_CHECK_INSTANCE_CAST                         not implemented

;;;     G_TYPE_CHECK_INSTANCE_TYPE

(test g-type-check-instance-type
  (let ((button (make-instance 'gtk-button)))
    (is-true (g-type-check-instance-type button "GObject"))
    (is-true (g-type-check-instance-type button "GtkButton"))))

;;;     G_TYPE_CHECK_CLASS_CAST                            not implemented

;;;   G_TYPE_CHECK_CLASS_TYPE

(test g-type-check-class-type
  (is-true  (g-type-check-class-type (g-type-class-ref "GtkButton") "GObject"))
  (is-false (g-type-check-class-type (g-type-class-ref "GtkButton") "GtkWindow")))

;;;     G_TYPE_CHECK_VALUE                                 not implemented
;;;     G_TYPE_CHECK_VALUE_TYPE                            not implemented
;;;
;;;     g_type_init
;;;     g_type_init_with_debug_flags                       not implemented

;;;   g_type_name

(test g-type-name
  (is (equal "gdouble" (g-type-name +g-type-double+)))
  (is (equal "GBoxed" (g-type-name +g-type-boxed+)))
  (is (equal "GtkWidget" (g-type-name (gtype "GtkWidget")))))

;;;     g_type_qname

;;;   g_type_from_name

(test g-type-from-name
  (is (eq (gtype "gdouble") (g-type-from-name "gdouble")))
  (is (eq (gtype "GBoxed") (g-type-from-name "GBoxed")))
  (is (eq (gtype "GtkWidget") (g-type-from-name "GtkWidget"))))

;;;     g_type_parent
;;;     g_type_depth
;;;     g_type_next_base
;;;     g_type_is_a

;;;     g-type-class-ref
;;;     g-type-class-unref

(test g-type-class-ref
  (let ((class nil))
    ;; gtype is a string
    (is (pointerp (setf class (g-type-class-ref "GtkBox"))))
    (is (eq (gtype "GtkBox") (g-type-from-class class)))
    (is-false (g-type-class-unref class))
    ;; gtype is a ID
    (is (pointerp (setf class (g-type-class-ref (gtype-id (gtype "GtkBox"))))))
    (is (eq (gtype "GtkBox") (g-type-from-class class)))
    (is-false (g-type-class-unref class))
    ;; gtype is a g-type
    (is (pointerp (setf class (g-type-class-ref (gtype "GtkBox")))))
    (is (eq (gtype "GtkBox") (g-type-from-class class)))
    (is-false (g-type-class-unref class))))

;;;     g_type_class_peek
;;;     g_type_class_peek_static
;;;     g_type_class_peek_parent
;;;     g_type_class_add_private
;;;     g_type_add_class_private                           not implemented
;;;     g_type_interface_peek
;;;     g_type_interface_peek_parent                       not implemented
;;;     g_type_default_interface_ref
;;;     g_type_default_interface_peek
;;;     g_type_default_interface_unref
;;;     g_type_children
;;;     g_type_interfaces
;;;     g_type_interface_prerequisites

;;;     g-type-qdata

(test g-type-qdata
  ;; Attach and read data for a "gboolean" type
  (is (string= "a string" (setf (g-type-qdata "gboolean" "mydata") "a string")))
  (is (string= "a string" (g-type-qdata "gboolean" "mydata")))
  (is (equal '(A B C) (setf (g-type-qdata "gboolean" "mydata") '(a b c))))
  (is (equal '(A B C) (g-type-qdata "gboolean" "mydata")))
  (is-false (setf (g-type-qdata "gboolean" "mydata") nil))
  (is-false (g-type-qdata "gboolean" "mydata"))
  ;; Attach and read data for a "GtkButton" type
  (is (string= "a string" (setf (g-type-qdata "GtkButton" "mydata") "a string")))
  (is (string= "a string" (g-type-qdata "GtkButton" "mydata")))
  (is (equal '(A B C) (setf (g-type-qdata "GtkButton" "mydata") '(a b c))))
  (is (equal '(A B C) (g-type-qdata "GtkButton" "mydata")))
  (is-false (setf (g-type-qdata "GtkButton" "mydata") nil))
  (is-false (g-type-qdata "GtkButton" "mydata")))

;;;     g_type_query                                       not exported
;;;     g_type_register_static                             not exported
;;;     g_type_register_static_simple                      not exported
;;;     g_type_register_dynamic                            not implemented
;;;     g_type_register_fundamental                        not implemented
;;;     g_type_add_interface_static                        not exported
;;;     g_type_add_interface_dynamic                       not implemented
;;;     g_type_interface_add_prerequisite                  not exported
;;;     g_type_get_plugin                                  not implemented
;;;     g_type_interface_get_plugin                        not implemented
;;;     g_type_fundamental_next                            not exported
;;;     g_type_create_instance                             not implemented
;;;     g_type_free_instance                               not implemented
;;;     g_type_add_class_cache_func                        not implemented
;;;     g_type_remove_class_cache_func                     not implemented
;;;     g_type_class_unref_uncached                        not implemented
;;;     g_type_add_interface_check                         not implemented
;;;     g_type_remove_interface_check                      not implemented
;;;     g_type_value_table_peek                            not exported
;;;     g_type_ensure                                      not exported
;;;     g_type_get_type_registration_serial                not implemented
;;;     G_DEFINE_TYPE                                      not implemented
;;;     G_DEFINE_TYPE_WITH_CODE                            not implemented
;;;     G_DEFINE_ABSTRACT_TYPE                             not implemented
;;;     G_DEFINE_ABSTRACT_TYPE_WITH_CODE                   not implemented
;;;     G_DEFINE_INTERFACE                                 not implemented
;;;     G_DEFINE_INTERFACE_WITH_CODE                       not implemented
;;;     G_IMPLEMENT_INTERFACE                              not implemented
;;;     G_DEFINE_TYPE_EXTENDED                             not implemented
;;;     G_DEFINE_BOXED_TYPE                                not implemented
;;;     G_DEFINE_BOXED_TYPE_WITH_CODE                      not implemented
;;;     G_DEFINE_POINTER_TYPE                              not implemented
;;;     G_DEFINE_POINTER_TYPE_WITH_CODE                    not implemented

;;; 2020-11-15
