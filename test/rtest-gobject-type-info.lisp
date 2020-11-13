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
; Does not work
;  (is (eq (gtype "GType") (gtype +g-type-gtype+)))
  (is (eq (gtype "GVariant") (gtype +g-type-variant+)))
; Does not work
;  (is (eq (gtype "GChecksum") (gtype +g-type-checksum+)))
)

;;;     GType

;;;     GTypeInterface

(test g-type-interface-structure
  (let ((interface (g-type-default-interface-ref "GtkOrientable")))
    (is (= 16 (foreign-type-size '(:struct g-type-interface))))
    (is (equal '(:instance-type :type)
               (stable-sort (foreign-slot-names '(:struct g-type-interface))
                            #'string-lessp)))
    (is-true (foreign-slot-value interface
                                 '(:struct g-type-interface) :type))
    (is-false (foreign-slot-value interface
                                  '(:struct g-type-interface) :instance-type))))

;;;     GTypeInstance

(test g-type-instance-structure
  (let ((button (make-instance 'gtk-button)))
    (is (= 8 (foreign-type-size '(:struct g-type-instance))))
    (is (equal '(:class) (foreign-slot-names '(:struct g-type-instance))))
    (is-true (foreign-slot-value (pointer button)
                                 '(:struct g-type-instance) :class))))

;;;     GTypeClass

(test g-type-class-structure
  (let ((class (g-type-class-ref "GtkButton")))
    (is (= 8 (foreign-type-size '(:struct g-type-class))))
    (is (equal '(:type) (foreign-slot-names '(:struct g-type-class))))
    (is-true (foreign-slot-value class '(:struct g-type-class) :type))))

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
  (is-false (g-type-is-abstract +g-type-variant+))

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
  (is-false (g-type-is-derived +g-type-variant+))

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
  (is-true  (g-type-is-fundamental +g-type-variant+))

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
  (is-true  (g-type-is-value-type +g-type-variant+))

  (is-true  (g-type-is-value-type "GtkWidget"))
  (is-true  (g-type-is-value-type "GtkContainer"))
  (is-true  (g-type-is-value-type "GtkBox"))
  (is-true  (g-type-is-value-type "GtkWindow"))
  (is-true  (g-type-is-value-type "GtkOrientable"))
  (is-true  (g-type-is-value-type "GtkAccelFlags"))
  (is-true  (g-type-is-value-type "GtkArrowPlacement"))
  (is-true  (g-type-is-value-type "GdkRGBA"))
  (is-true  (g-type-is-value-type "GtkTreePath")))

;;;   g_type_has_value_table

(test g-type-has-value-table
;  (is-false (g-type-has-value-table +g-type-invalid+))
;  (is-false (g-type-has-value-table +g-type-none+))
  (is-false (g-type-has-value-table +g-type-interface+))
  (is-true  (g-type-has-value-table +g-type-char+))
  (is-true  (g-type-has-value-table +g-type-uchar+))
  (is-true  (g-type-has-value-table +g-type-boolean+))
  (is-true  (g-type-has-value-table +g-type-int+))
  (is-true  (g-type-has-value-table +g-type-uint+))
  (is-true  (g-type-has-value-table +g-type-long+))
  (is-true  (g-type-has-value-table +g-type-ulong+))
  (is-true  (g-type-has-value-table +g-type-int64+))
  (is-true  (g-type-has-value-table +g-type-uint64+))
  (is-true  (g-type-has-value-table +g-type-enum+))
  (is-true  (g-type-has-value-table +g-type-flags+))
  (is-true  (g-type-has-value-table +g-type-float+))
  (is-true  (g-type-has-value-table +g-type-double+))
  (is-true  (g-type-has-value-table +g-type-string+))
  (is-true  (g-type-has-value-table +g-type-pointer+))
  (is-false (g-type-has-value-table +g-type-boxed+))
  (is-true  (g-type-has-value-table +g-type-param+))
  (is-true  (g-type-has-value-table +g-type-object+))
  (is-true  (g-type-has-value-table +g-type-variant+))

  (is-true  (g-type-has-value-table "GtkWidget"))
  (is-true  (g-type-has-value-table "GtkContainer"))
  (is-true  (g-type-has-value-table "GtkBox"))
  (is-true  (g-type-has-value-table "GtkWindow"))
  (is-true  (g-type-has-value-table "GtkOrientable"))
  (is-true  (g-type-has-value-table "GtkAccelFlags"))
  (is-true  (g-type-has-value-table "GtkArrowPlacement"))
  (is-true  (g-type-has-value-table "GdkRGBA"))
  (is-true  (g-type-has-value-table "GtkTreePath")))

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

;;;   g_type_is_instantiatable

(test g-type-is-instantiatable
  (is-false (g-type-is-instantiatable +g-type-invalid+))
  (is-false (g-type-is-instantiatable +g-type-none+))
  (is-false (g-type-is-instantiatable +g-type-interface+))
  (is-false (g-type-is-instantiatable +g-type-char+))
  (is-false (g-type-is-instantiatable +g-type-uchar+))
  (is-false (g-type-is-instantiatable +g-type-boolean+))
  (is-false (g-type-is-instantiatable +g-type-int+))
  (is-false (g-type-is-instantiatable +g-type-uint+))
  (is-false (g-type-is-instantiatable +g-type-long+))
  (is-false (g-type-is-instantiatable +g-type-ulong+))
  (is-false (g-type-is-instantiatable +g-type-int64+))
  (is-false (g-type-is-instantiatable +g-type-uint64+))
  (is-false (g-type-is-instantiatable +g-type-enum+))
  (is-false (g-type-is-instantiatable +g-type-flags+))
  (is-false (g-type-is-instantiatable +g-type-float+))
  (is-false (g-type-is-instantiatable +g-type-double+))
  (is-false (g-type-is-instantiatable +g-type-string+))
  (is-false (g-type-is-instantiatable +g-type-pointer+))
  (is-false (g-type-is-instantiatable +g-type-boxed+))
  (is-true  (g-type-is-instantiatable +g-type-param+))
  (is-true  (g-type-is-instantiatable +g-type-object+))
  (is-false (g-type-is-instantiatable +g-type-variant+))

  (is-true  (g-type-is-instantiatable "GtkWidget"))    ; Why is this TRUE?
  (is-true  (g-type-is-instantiatable "GtkContainer")) ; Why is this TRUE?
  (is-true  (g-type-is-instantiatable "GtkBox"))
  (is-true  (g-type-is-instantiatable "GtkWindow"))
  (is-false (g-type-is-instantiatable "GtkOrientable"))
  (is-false (g-type-is-instantiatable "GtkAccelFlags"))
  (is-false (g-type-is-instantiatable "GtkArrowPlacement"))
  (is-false (g-type-is-instantiatable "GdkRGBA"))
  (is-false (g-type-is-instantiatable "GtkTreePath")))

;;;   g_type_is_derivable

(test g-type-is-derivable
  (is-false (g-type-is-derivable +g-type-invalid+))
  (is-false (g-type-is-derivable +g-type-none+))
  (is-true  (g-type-is-derivable +g-type-interface+))
  (is-true  (g-type-is-derivable +g-type-char+))
  (is-true  (g-type-is-derivable +g-type-uchar+))
  (is-true  (g-type-is-derivable +g-type-boolean+))
  (is-true  (g-type-is-derivable +g-type-int+))
  (is-true  (g-type-is-derivable +g-type-uint+))
  (is-true  (g-type-is-derivable +g-type-long+))
  (is-true  (g-type-is-derivable +g-type-ulong+))
  (is-true  (g-type-is-derivable +g-type-int64+))
  (is-true  (g-type-is-derivable +g-type-uint64+))
  (is-true  (g-type-is-derivable +g-type-enum+))
  (is-true  (g-type-is-derivable +g-type-flags+))
  (is-true  (g-type-is-derivable +g-type-float+))
  (is-true  (g-type-is-derivable +g-type-double+))
  (is-true  (g-type-is-derivable +g-type-string+))
  (is-true  (g-type-is-derivable +g-type-pointer+))
  (is-true  (g-type-is-derivable +g-type-boxed+))
  (is-true  (g-type-is-derivable +g-type-param+))
  (is-true  (g-type-is-derivable +g-type-object+))
  (is-true  (g-type-is-derivable +g-type-variant+))

  (is-true  (g-type-is-derivable "GtkWidget"))
  (is-true  (g-type-is-derivable "GtkContainer"))
  (is-true  (g-type-is-derivable "GtkBox"))
  (is-true  (g-type-is-derivable "GtkWindow"))
  (is-true  (g-type-is-derivable "GtkOrientable"))
  (is-true  (g-type-is-derivable "GtkAccelFlags"))
  (is-true  (g-type-is-derivable "GtkArrowPlacement"))
  (is-true  (g-type-is-derivable "GdkRGBA"))
  (is-true  (g-type-is-derivable "GtkTreePath")))

;;;   g_type_is_deep_derivable

(test g-type-is-deep-derivable
  (is-false (g-type-is-deep-derivable +g-type-invalid+))
  (is-false (g-type-is-deep-derivable +g-type-none+))
  (is-false (g-type-is-deep-derivable +g-type-interface+))
  (is-false (g-type-is-deep-derivable +g-type-char+))
  (is-false (g-type-is-deep-derivable +g-type-uchar+))
  (is-false (g-type-is-deep-derivable +g-type-boolean+))
  (is-false (g-type-is-deep-derivable +g-type-int+))
  (is-false (g-type-is-deep-derivable +g-type-uint+))
  (is-false (g-type-is-deep-derivable +g-type-long+))
  (is-false (g-type-is-deep-derivable +g-type-ulong+))
  (is-false (g-type-is-deep-derivable +g-type-int64+))
  (is-false (g-type-is-deep-derivable +g-type-uint64+))
  (is-false (g-type-is-deep-derivable +g-type-enum+))
  (is-false (g-type-is-deep-derivable +g-type-flags+))
  (is-false (g-type-is-deep-derivable +g-type-float+))
  (is-false (g-type-is-deep-derivable +g-type-double+))
  (is-false (g-type-is-deep-derivable +g-type-string+))
  (is-false (g-type-is-deep-derivable +g-type-pointer+))
  (is-false (g-type-is-deep-derivable +g-type-boxed+))
  (is-true  (g-type-is-deep-derivable +g-type-param+))
  (is-true  (g-type-is-deep-derivable +g-type-object+))
  (is-false (g-type-is-deep-derivable +g-type-variant+))

  (is-true  (g-type-is-deep-derivable "GtkWidget"))
  (is-true  (g-type-is-deep-derivable "GtkContainer"))
  (is-true  (g-type-is-deep-derivable "GtkBox"))
  (is-true  (g-type-is-deep-derivable "GtkWindow"))
  (is-false (g-type-is-deep-derivable "GtkOrientable"))
  (is-false (g-type-is-deep-derivable "GtkAccelFlags"))
  (is-false (g-type-is-deep-derivable "GtkArrowPlacement"))
  (is-false (g-type-is-deep-derivable "GdkRGBA"))
  (is-false (g-type-is-deep-derivable "GtkTreePath")))

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

;;;     g_type_query
;;;     g_type_register_static
;;;     g_type_register_static_simple
;;;     g_type_register_dynamic                            not implemented
;;;     g_type_register_fundamental                        not implemented
;;;     g_type_add_interface_static
;;;     g_type_add_interface_dynamic                       not implemented
;;;     g_type_interface_add_prerequisite
;;;     g_type_get_plugin                                  not implemented
;;;     g_type_interface_get_plugin                        not implemented

;;;     g_type_fundamental_next

(test g-type-fundamental-next
  (is (= 196 (g-type-fundamental-next))))

;;;     g_type_create_instance                             not implemented
;;;     g_type_free_instance                               not implemented
;;;     g_type_add_class_cache_func                        not implemented
;;;     g_type_remove_class_cache_func                     not implemented
;;;     g_type_class_unref_uncached                        not implemented
;;;     g_type_add_interface_check                         not implemented
;;;     g_type_remove_interface_check                      not implemented

;;;     g_type_value_table_peek

(test g-type-value-table-peek
;  (is-false (g-type-value-table-peek +g-type-invalid+))
;  (is-false (g-type-value-table-peek +g-type-none+))
  (is-false (not (null-pointer-p (g-type-value-table-peek +g-type-interface+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-char+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-uchar+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-boolean+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-int+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-uint+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-long+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-ulong+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-int64+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-uint64+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-enum+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-flags+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-float+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-double+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-string+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-pointer+))))
  (is-false (not (null-pointer-p (g-type-value-table-peek +g-type-boxed+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-param+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-object+))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek +g-type-variant+))))

  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkWidget"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkContainer"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkBox"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkWindow"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkOrientable"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkAccelFlags"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkArrowPlacement"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GdkRGBA"))))
  (is-true  (not (null-pointer-p (g-type-value-table-peek "GtkTreePath")))))

;;;     g_type_ensure
;;;     g_type_get_type_registration_serial                not implemented
;;;
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

#+nil
(test g-type-info-char
  (let ((id +g-type-char+)
        (name "gchar")
        (gtype (gtype +g-type-char+)))
  ;; gtype-id
  (is (= +g-type-char+ (gtype-id gtype)))
  ;; gtype-name
  (is (equal name (gtype-name gtype)))
  ;; gtype-from-id
  (is (equal gtype (gtype-from-id id)))
  ;; gtype-from-name
  (is (equal gtype (gtype-from-name name)))
  ;; gobject::g-type=
  (is-false (gobject::g-type= gtype +g-type-invalid+))
  ;; gobject::g-type/=
  (is-true (gobject::g-type/= gtype +g-type-invalid+))
  ;; g-type-is-abstract
  (is-false (g-type-is-abstract gtype))
  (is-false (g-type-is-abstract id))
  (is-false (g-type-is-abstract name))
  ;; g-type-is-derived
  (is-false (g-type-is-derived gtype))
  (is-false (g-type-is-derived id))
  (is-false (g-type-is-derived name))
  ;; g-type-is-fundamental
  (is-true (g-type-is-fundamental gtype))
  (is-true (g-type-is-fundamental id))
  (is-true (g-type-is-fundamental name))
  ;; g-type-is-value-type
  (is-true (g-type-is-value-type gtype))
  (is-true (g-type-is-value-type id))
  (is-true (g-type-is-value-type name))
  ;; g-type-has-value-table
  (is-true (g-type-has-value-table gtype))
  (is-true (g-type-has-value-table id))
  (is-true (g-type-has-value-table name))
  ;; g-type-is-classed
  (is-false (g-type-is-classed gtype))
  (is-false (g-type-is-classed id))
  (is-false (g-type-is-classed name))
  ;; g-type-is-instantiatable
  (is-false (g-type-is-instantiatable gtype))
  (is-false (g-type-is-instantiatable id))
  (is-false (g-type-is-instantiatable name))
  ;; g-type-is-derivable
  (is-true (g-type-is-derivable gtype))
  (is-true (g-type-is-derivable id))
  (is-true (g-type-is-derivable name))
  ;; g-type-is-deep-derivable
  (is-false (g-type-is-deep-derivable gtype))
  (is-false (g-type-is-deep-derivable id))
  (is-false (g-type-is-deep-derivable name))
  ;; g-type-is-interface
  (is-false (g-type-is-interface gtype))
  (is-false (g-type-is-interface id))
  (is-false (g-type-is-interface name))
  ;; g-type-name
  (is (equal name (g-type-name gtype)))
  (is (equal name (g-type-name id)))
  (is (equal name (g-type-name name)))
  ;; g-type-from-name
  (is (equal gtype (g-type-from-name name)))
  ;; g-type-parent
  (is-false (g-type-parent gtype))
  (is-false (g-type-parent id))
  (is-false (g-type-parent name))
  ;; g-type-depth
  (is (= 1 (g-type-depth gtype)))
  (is (= 1 (g-type-depth id)))
  (is (= 1 (g-type-depth name)))
  ;; g-type-next-base
  (is-false (g-type-next-base gtype gtype))
  (is-false (g-type-next-base gtype id))
  (is-false (g-type-next-base gtype name))
  ;; g-type-is-a
  (is-false (g-type-is-a gtype +g-type-invalid+))
  (is-true (g-type-is-a gtype gtype))
  (is-true (g-type-is-a gtype id))
  (is-true (g-type-is-a gtype name))
  ;; g-type-children
  (is-false (g-type-children gtype))
  (is-false (g-type-children id))
  (is-false (g-type-children name))
  ;; g-type-interfaces
  (is-false (g-type-interfaces gtype))
  (is-false (g-type-interfaces id))
  (is-false (g-type-interfaces name))
  ;; g-type-interace-prerequisites
  ;; (setf g-type-qdata)
  (setf (g-type-qdata gtype "myData") (null-pointer))
  ;; g-type-qdata
  (is-true  (null-pointer-p (g-type-qdata gtype "myData")))
  ;; g-type-query
  ;; FIXME: This does not work for the type "gchar" or other basic types like
  ;; "gboolean". But it is not a problem for "GObject" and derived classes.
  #+nil
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query name query)
    (is-false (foreign-slot-value query '(:struct g-type-query) :type))
    (is-false (foreign-slot-value query '(:struct g-type-query) :type-name))
    (is-eql 0 (foreign-slot-value query '(:struct g-type-query) :class-size))
    (is-eql 0 (foreign-slot-value query '(:struct g-type-query) :instance-size)))
  ;; g-type-fundamental
  (is (equal gtype (g-type-fundamental gtype)))
  (is (equal gtype (g-type-fundamental id)))
  (is (equal gtype (g-type-fundamental name)))
  ;; g-type-value-table-peek
  ;;FIXME: This causes an error for the first run, but not for the second.
  #+nil
  (with-foreign-object (table 'g-type-value-table)
    (setq table (g-type-value-table-peek gtype))
    (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-init)))
    (is-true (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-free)))
    (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-copy)))
    (is-true (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-peek-pointer)))
    (is (equal "i" (foreign-slot-value table 'g-type-value-table :collect-format)))
    (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :collect-value)))
    (is (equal "p" (foreign-slot-value table 'g-type-value-table :lcopy-format)))
    (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :lcopy-value))))
))

#+nil
(test g-type-info-gtk-label
  (let ((id (gtype-id (gtype "GtkLabel")))
        (name "GtkLabel")
        (gtype (gtype "GtkLabel")))
    ;; gtype-id
    (is (= 134930720 (gtype-id gtype)))
    ;; gtype-name
    (is (equal name (gtype-name gtype)))
    ;; gtype-from-id
    (is (equal gtype (gtype-from-id id)))
    ;; gtype-from-name
    (is (equal gtype (gtype-from-name name)))
    ;; gobject::g-type=
    (is-false (gobject::g-type= gtype +g-type-invalid+))
    (is-true  (gobject::g-type= gtype name))
    (is-true  (gobject::g-type= gtype id))
    ;; gobject::g-type/=
    (is-true  (gobject::g-type/= gtype +g-type-invalid+))
    (is-false (gobject::g-type/= gtype name))
    (is-false (gobject::g-type/= gtype id))
    ;; g-type-is-abstract
    (is-false (g-type-is-abstract gtype))
    (is-false (g-type-is-abstract id))
    (is-false (g-type-is-abstract name))
    ;; g-type-is-derived
    (is-true (g-type-is-derived gtype))
    (is-true (g-type-is-derived id))
    (is-true (g-type-is-derived name))
    ;; g-type-is-fundamental
    (is-false (g-type-is-fundamental gtype))
    (is-false (g-type-is-fundamental id))
    (is-false (g-type-is-fundamental name))
    ;; g-type-is-value-type
    (is-true (g-type-is-value-type gtype))
    (is-true (g-type-is-value-type id))
    (is-true (g-type-is-value-type name))
    ;; g-type-has-value-table
    (is-true (g-type-has-value-table gtype))
    (is-true (g-type-has-value-table id))
    (is-true (g-type-has-value-table name))
    ;; g-type-is-classed
    (is-true (g-type-is-classed gtype))
    (is-true (g-type-is-classed id))
    (is-true (g-type-is-classed name))
    ;; g-type-is-instantiatable
    (is-true (g-type-is-instantiatable gtype))
    (is-true (g-type-is-instantiatable id))
    (is-true (g-type-is-instantiatable name))
    ;; g-type-is-derivable
    (is-true (g-type-is-derivable gtype))
    (is-true (g-type-is-derivable id))
    (is-true (g-type-is-derivable name))
    ;; g-type-is-deep-derivable
    (is-true (g-type-is-deep-derivable gtype))
    (is-true (g-type-is-deep-derivable id))
    (is-true (g-type-is-deep-derivable name))
    ;; g-type-is-interface
    (is-false (g-type-is-interface gtype))
    (is-false (g-type-is-interface id))
    (is-false (g-type-is-interface name))
    ;; g-type-from-instance
    (let ((label (make-instance 'gtk-label)))
      (is (equal gtype (g-type-from-instance label))))
    ;; g-type-from-class
    (is (equal gtype (g-type-from-class (g-type-class-ref name))))
    ;; g-type-from-interface
    ;; g-type-name
    (is (equal name (g-type-name gtype)))
    (is (equal name (g-type-name id)))
    (is (equal name (g-type-name name)))
    ;; g-type-from-name
    (is (equal gtype (g-type-from-name name)))
    ;; g-type-parent
    (is-true (g-type-is-a "GtkMisc" (g-type-parent gtype)))
    (is-true (g-type-is-a "GtkMisc" (g-type-parent id)))
    (is-true (g-type-is-a "GtkMisc" (g-type-parent name)))
    ;; g-type-depth
    (is (= 5 (g-type-depth gtype)))
    (is (= 5 (g-type-depth id)))
    (is (= 5 (g-type-depth name)))
    ;; g-type-next-base
    (is (eq (gtype "GInitiallyUnowned") (g-type-next-base gtype "GObject")))
    (is (eq (gtype "GInitiallyUnowned") (g-type-next-base id "GObject")))
    (is (eq (gtype "GInitiallyUnowned") (g-type-next-base name "GObject")))
    ;; g-type-is-a
    (is-false (g-type-is-a gtype +g-type-invalid+))
    (is-true (g-type-is-a gtype gtype))
    (is-true (g-type-is-a gtype id))
    (is-true (g-type-is-a gtype name))
    ;; g-type-class-ref
    (is (equal gtype (foreign-slot-value (g-type-class-ref gtype) 'g-type-class :type)))
    ;; g-type-class-peek
    (is (equal gtype (foreign-slot-value (g-type-class-peek gtype) 'g-type-class :type)))
    ;; g-type-class-peek-static
    (is (equal gtype (foreign-slot-value (g-type-class-peek-static gtype) 'g-type-class :type)))
    ;; g-type-class-unref
    ;; g-type-class-peek-parent
    (is (eq (gtype "GtkMisc")
            (foreign-slot-value (g-type-class-peek-parent (g-type-class-peek gtype))
                                '(:struct g-type-class) :type)))
    ;; g-type-interface-peek
    ;; g-type-interface-peek-parent
    ;; g-type-default-interface-ref
    ;; g-type-default-interface-peek
    ;; g-type-default-interface-unref
    ;; g-type-children
    (is (equal '("GtkAccelLabel") (mapcar #'g-type-name (g-type-children gtype))))
    (is (equal '("GtkAccelLabel") (mapcar #'g-type-name (g-type-children id))))
    (is (equal '("GtkAccelLabel") (mapcar #'g-type-name (g-type-children name))))
    ;; g-type-interfaces
    (is (equal '("AtkImplementorIface" "GtkBuildable")
                  (mapcar #'g-type-name (g-type-interfaces gtype))))
    (is (equal '("AtkImplementorIface" "GtkBuildable")
                  (mapcar #'g-type-name (g-type-interfaces id))))
    (is (equal '("AtkImplementorIface" "GtkBuildable")
                  (mapcar #'g-type-name (g-type-interfaces name))))
    ;; g-type-interace-prerequisites
    ;; (setf g-type-qdata)
    (setf (g-type-qdata gtype "myData") (null-pointer))
    ;; g-type-qdata
    (is-true  (null-pointer-p (g-type-qdata gtype "myData")))
    ;; g-type-query
    (with-foreign-object (query 'g-type-query)
      (g-type-query name query)
      (is (equal gtype (foreign-slot-value query 'g-type-query :type)))
      (is (equal name (foreign-slot-value query 'g-type-query :type-name)))
      (is (= 476 (foreign-slot-value query 'g-type-query :class-size)))
      (is (=  24 (foreign-slot-value query 'g-type-query :instance-size))))
    ;; g-type-fundamental
    (is (eq (gtype "GObject") (g-type-fundamental gtype)))
    (is (eq (gtype "GObject") (g-type-fundamental id)))
    (is (eq (gtype "GObject") (g-type-fundamental name)))
    ;; g-type-value-table-peek
    #+nil
    (with-foreign-object (table 'g-type-value-table)
      (setq table (g-type-value-table-peek gtype))
      (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-init)))
      (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-free)))
      (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-copy)))
      (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :value-peek-pointer)))
      (is (equal "p" (foreign-slot-value table 'g-type-value-table :collect-format)))
      (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :collect-value)))
      (is (equal "p" (foreign-slot-value table 'g-type-value-table :lcopy-format)))
      (is-false (null-pointer-p (foreign-slot-value table 'g-type-value-table :lcopy-value))))))

;;; 2020-11-1
