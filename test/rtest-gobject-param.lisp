
(def-suite gobject-param :in gobject-suite)
(in-suite gobject-param)

;;;     G_IS_PARAM_SPEC_BOOLEAN
;;;     G_PARAM_SPEC_BOOLEAN
;;;     G_VALUE_HOLDS_BOOLEAN

;;;   G_TYPE_PARAM_BOOLEAN

(test g-type-param-boolean
  (is (equal (gtype "GParamBoolean") g-type-param-boolean)))

;;;   GParamSpecBoolean

(test g-param-spec-boolean-struct
  (is (= 16 (foreign-type-size '(:struct g-param-spec-boolean))))
  (is (equal '(:parent-instance :default-value)
             (foreign-slot-names '(:struct g-param-spec-boolean)))))

;;;   g_param_spec_boolean

#-ccl
(test g-param-spec-boolean
  (with-foreign-object (param '(:struct g-param-spec-boolean))
    ;; Create a GParamSpecBoolean
    (setf param (g-param-spec-boolean "myBoolean"
                                      "myBool"
                                      "Documentation for myBoolean"
                                      t
                                      '(:readable :writable)))
    ;; Do we have a foreign pointer to a GParamSpecBoolean
    (is-true (pointerp param))

    ;; Check the field :parent-instance of GParamSpecBoolean
    (with-foreign-object (parent-instance '(:struct g-param-spec))
      ;; Get the pointer to the parent-instance of type GParamSpec
      (setf parent-instance
            (foreign-slot-pointer param
                                  '(:struct g-param-spec-boolean)
                                  :parent-instance))
      ;; Do we have a foreign pointer to a type-instance
      (is-true (pointerp (foreign-slot-pointer parent-instance
                                               '(:struct g-param-spec)
                                               :type-instance)))
      (with-foreign-object (type-instance '(:struct g-type-instance))
        (setf type-instance
              (foreign-slot-value parent-instance
                                  '(:struct g-param-spec)
                                  :type-instance))
        (is-true (pointerp type-instance))
        (with-foreign-object (class '(:struct g-type-class))
          (setf class (foreign-slot-pointer type-instance
                                            '(:struct g-type-instance)
                                            :class))
          (is-true (pointerp class))
          (is (equal (gtype "GParamBoolean")
                     (foreign-slot-value class
                                         '(:struct g-type-class) :type)))))
      ;; Read the fields of the structure GParamSpec
      (is (equal "myBoolean"
                 (foreign-slot-value parent-instance
                                     '(:struct g-param-spec) :name)))
      (is (equal '(:readable :writable)
                 (foreign-slot-value parent-instance
                                     '(:struct g-param-spec) :flags)))
      (is (equal (gtype "gboolean")
                 (foreign-slot-value parent-instance
                                     '(:struct g-param-spec) :value-type)))
      (is-false (foreign-slot-value parent-instance
                                    '(:struct g-param-spec) :owner-type)))
    ;; Check the field :default-value of GParamSpecBoolean
    (is (eq t (foreign-slot-value param
                                  '(:struct g-param-spec-boolean)
                                  :default-value)))))

;;;   g_value_set_boolean
;;;   g_value_get_boolean

(test g-value-set/get-boolean
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "gboolean")
    (g-value-set-boolean value t)
    (is-true (g-value-get-boolean value))
    (g-value-set-boolean value nil)
    (is-false (g-value-get-boolean value))))

;;;     G_IS_PARAM_SPEC_CHAR
;;;     G_PARAM_SPEC_CHAR
;;;     G_VALUE_HOLDS_CHAR

;;;     G_TYPE_PARAM_CHAR

(test g-type-param-char
  (is (equal (gtype "GParamChar") g-type-param-char)))

;;;   GParamSpecChar

(test g-param-spec-char-struct
  (is (= 16 (foreign-type-size '(:struct g-param-spec-char))))
  (is (equal '(:PARENT-INSTANCE :MINIMUM :MAXIMUM :DEFAULT-VALUE)
             (foreign-slot-names '(:struct g-param-spec-char)))))

;;;   g_param_spec_char

#-ccl
(test g-param-spec-char
  (with-foreign-object (param '(:struct g-param-spec-char))
    ;; Create a GParamSpecChar
    (setf param (g-param-spec-char "myChar"
                                   "myChar"
                                   "Documentation for myChar"
                                   65
                                   127
                                   65
                                   '(:readable :writable)))
    ;; Do we have a foreign pointer to a GParamSpecChar
    (is-true (pointerp param))
    ;; Check the field :parent-instance of GParamSpecChar
    (with-foreign-object (parent-instance '(:struct g-param-spec))
      ;; Get the pointer to the parent-instance of type GParamSpec
      (setf parent-instance
            (foreign-slot-pointer param '(:struct g-param-spec-char) :parent-instance))
      ;; Do we have a foreign pointer to a type-instance
      (is-true (pointerp (foreign-slot-pointer parent-instance '(:struct g-param-spec) :type-instance)))
      (with-foreign-object (type-instance '(:struct g-type-instance))
        (setf type-instance
              (foreign-slot-value parent-instance '(:struct g-param-spec) :type-instance))
        (is-true (pointerp type-instance))
        (with-foreign-object (class '(:struct g-type-class))
          (setf class (foreign-slot-pointer type-instance '(:struct g-type-instance) :class))
          (is-true (pointerp class))
          (is (equal (gtype "GParamChar")
                     (foreign-slot-value class '(:struct g-type-class) :type)))
        )
      )
      ;; Read the fields of the structure GParamSpec
      (is (equal "myChar"
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :name)))
      (is (equal '(:readable :writable)
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :flags)))
      (is (equal (gtype "gchar")
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :value-type)))
      (is-false (foreign-slot-value parent-instance '(:struct g-param-spec) :owner-type))  
    )

    ;; TODO: The values of :minimum, :maximum, and :default-value change per run.

    ;; Check the field :minimum of GParamSpecChar
;    (is (= 4 (foreign-slot-value param '(:struct g-param-spec-char) :minimum)))
    ;; Check the field :maximum of GParamSpecChar
;    (is (= 126 (foreign-slot-value param '(:struct g-param-spec-char) :maximum)))
    ;; Check the field :default-value of GParamSpecChar
;    (is (= 18 (foreign-slot-value param '(:struct g-param-spec-char) :default-value)))
))

;;;     g_value_set_char
;;;     g_value_get_char

(test g-value-set/get-char
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "gchar")
    (g-value-set-char value 65)
    (is (= 65 (g-value-get-char value)))
    (g-value-set-char value 66)
    (is (= 66 (g-value-get-char value)))))

;;;     g_value_get_schar
;;;     g_value_set_schar

(test g-value-set/get-schar
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "gchar")
    (g-value-set-schar value 65)
    (is (= 65 (g-value-get-schar value)))
    (g-value-set-schar value 66)
    (is (= 66 (g-value-get-schar value)))))

;;;     G_IS_PARAM_SPEC_UCHAR
;;;     G_PARAM_SPEC_UCHAR
;;;     G_VALUE_HOLDS_UCHAR

;;;     G_TYPE_PARAM_UCHAR

(test g-type-param-uchar
  (is (equal (gtype "GParamUChar") g-type-param-uchar)))

;;;     GParamSpecUChar

(test g-param-spec-uchar-struct
  (is (= 16 (foreign-type-size '(:struct g-param-spec-uchar))))
  (is (equal '(:PARENT-INSTANCE :MINIMUM :MAXIMUM :DEFAULT-VALUE)
             (foreign-slot-names '(:struct g-param-spec-uchar)))))

;;;     g_param_spec_uchar

#-ccl
(test g-param-spec-uchar
  (with-foreign-object (param '(:struct g-param-spec-uchar))
    ;; Create a GParamSpecUChar
    (setf param (g-param-spec-uchar "myUChar"
                                    "myUChar"
                                    "Documentation for myUChar"
                                    65
                                    127
                                    65
                                    '(:readable :writable)))
    ;; Do we have a foreign pointer to a GParamSpecChar
    (is-true (pointerp param))
    ;; Check the field :parent-instance of GParamSpecChar
    (with-foreign-object (parent-instance '(:struct g-param-spec))
      ;; Get the pointer to the parent-instance of type GParamSpec
      (setf parent-instance
            (foreign-slot-pointer param '(:struct g-param-spec-uchar) :parent-instance))
      ;; Do we have a foreign pointer to a type-instance
      (is-true (pointerp (foreign-slot-pointer parent-instance '(:struct g-param-spec) :type-instance)))
      (with-foreign-object (type-instance '(:struct g-type-instance))
        (setf type-instance
              (foreign-slot-value parent-instance '(:struct g-param-spec) :type-instance))
        (is-true (pointerp type-instance))
        (with-foreign-object (class '(:struct g-type-class))
          (setf class (foreign-slot-pointer type-instance '(:struct g-type-instance) :class))
          (is-true (pointerp class))
          (is (equal (gtype "GParamUChar")
                     (foreign-slot-value class '(:struct g-type-class) :type)))
        )
      )
      ;; Read the fields of the structure GParamSpec
      (is (equal "myUChar"
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :name)))
      (is (equal '(:readable :writable)
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :flags)))
      (is (equal (gtype "guchar")
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :value-type)))
      (is-false (foreign-slot-value parent-instance '(:struct g-param-spec) :owner-type))  
    )

    ;; TODO: The values of :minimum, :maximum, and :default-value change per run.

    ;; Check the field :minimum of GParamSpecChar
;    (is (= 11 (foreign-slot-value param '(:struct g-param-spec-uchar) :minimum)))
    ;; Check the field :maximum of GParamSpecChar
;    (is (= 126 (foreign-slot-value param '(:struct g-param-spec-uchar) :maximum)))
    ;; Check the field :default-value of GParamSpecChar
;    (is (= 18 (foreign-slot-value param '(:struct g-param-spec-uchar) :default-value)))
))

;;;     g_value_set_uchar
;;;     g_value_get_uchar

(test g-value-set/get-uchar
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "guchar")
    (g-value-set-uchar value 65)
    (is (= 65 (g-value-get-uchar value)))
    (g-value-set-uchar value 66)
    (is (= 66 (g-value-get-uchar value)))))

;;;     G_IS_PARAM_SPEC_INT
;;;     G_PARAM_SPEC_INT
;;;     G_VALUE_HOLDS_INT

;;;     G_TYPE_PARAM_INT

(test g-type-param-int
  (is (equal (gtype "GParamInt") g-type-param-int)))

;;;     GParamSpecInt

(test g-param-spec-int-struct
  (is (= 24 (foreign-type-size '(:struct g-param-spec-int))))
  (is (equal '(:PARENT-INSTANCE :MINIMUM :MAXIMUM :DEFAULT-VALUE)
             (foreign-slot-names '(:struct g-param-spec-int)))))

;;;   g_param_spec_int

#-ccl
(test g-param-spec-int
  (with-foreign-object (param '(:struct g-param-spec-int))
    ;; Create a GParamSpecInt
    (setf param (g-param-spec-int "myInteger"
                                    "myInt"
                                    "Documentation for myInteger"
                                    10
                                    100
                                    50
                                    '(:readable :writable)))
    ;; Do we have a foreign pointer to a GParamSpecChar
    (is-true (pointerp param))
    ;; Check the field :parent-instance of GParamSpecChar
    (with-foreign-object (parent-instance '(:struct g-param-spec))
      ;; Get the pointer to the parent-instance of type GParamSpec
      (setf parent-instance
            (foreign-slot-pointer param '(:struct g-param-spec-int) :parent-instance))
      ;; Do we have a foreign pointer to a type-instance
      (is-true (pointerp (foreign-slot-pointer parent-instance '(:struct g-param-spec) :type-instance)))
      (with-foreign-object (type-instance '(:struct g-type-instance))
        (setf type-instance
              (foreign-slot-value parent-instance '(:struct g-param-spec) :type-instance))
        (is-true (pointerp type-instance))
        (with-foreign-object (class '(:struct g-type-class))
          (setf class (foreign-slot-pointer type-instance '(:struct g-type-instance) :class))
          (is-true (pointerp class))
          (is (equal (gtype "GParamInt")
                     (foreign-slot-value class '(:struct g-type-class) :type)))
        )
      )
      ;; Read the fields of the structure GParamSpec
      (is (equal "myInteger"
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :name)))
      (is (equal '(:readable :writable)
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :flags)))
      (is (equal (gtype "gint")
                 (foreign-slot-value parent-instance '(:struct g-param-spec) :value-type)))
      (is-false (foreign-slot-value parent-instance '(:struct g-param-spec) :owner-type))  
    )

#|
    ;; TODO: The values of :minimum, :maximum, and :default-value change per run.

    ;; Check the field :minimum of GParamSpecChar
    (is (= 11 (foreign-slot-value param '(:struct g-param-spec-int) :minimum)))
    ;; Check the field :maximum of GParamSpecChar
    (is (= 126 (foreign-slot-value param '(:struct g-param-spec-int) :maximum)))
    ;; Check the field :default-value of GParamSpecChar
    (is (= 18 (foreign-slot-value param '(:struct g-param-spec-int) :default-value)))
|#
))

;;;     g_value_set_int
;;;     g_value_get_int
;;;
;;;     G_IS_PARAM_SPEC_UINT
;;;     G_PARAM_SPEC_UINT
;;;     G_VALUE_HOLDS_UINT
;;;     G_TYPE_PARAM_UINT
;;;
;;;     GParamSpecUInt
;;;
;;;     g_param_spec_uint
;;;     g_value_set_uint
;;;     g_value_get_uint
;;;
;;;     G_IS_PARAM_SPEC_LONG
;;;     G_PARAM_SPEC_LONG
;;;     G_VALUE_HOLDS_LONG
;;;     G_TYPE_PARAM_LONG
;;;
;;;     GParamSpecLong
;;;
;;;     g_param_spec_long
;;;     g_value_set_long
;;;     g_value_get_long
;;;
;;;     G_IS_PARAM_SPEC_ULONG
;;;     G_PARAM_SPEC_ULONG
;;;     G_VALUE_HOLDS_ULONG
;;;     G_TYPE_PARAM_ULONG
;;;
;;;     GParamSpecULong
;;;
;;;     g_param_spec_ulong
;;;     g_value_set_ulong
;;;     g_value_get_ulong
;;;
;;;     G_IS_PARAM_SPEC_INT64
;;;     G_PARAM_SPEC_INT64
;;;     G_VALUE_HOLDS_INT64
;;;     G_TYPE_PARAM_INT64
;;;
;;;     GParamSpecInt64
;;;
;;;     g_param_spec_int64
;;;     g_value_set_int64
;;;     g_value_get_int64
;;;
;;;     G_IS_PARAM_SPEC_UINT64
;;;     G_PARAM_SPEC_UINT64
;;;     G_VALUE_HOLDS_UINT64
;;;     G_TYPE_PARAM_UINT64
;;;
;;;     GParamSpecUInt64
;;;
;;;     g_param_spec_uint64
;;;     g_value_set_uint64
;;;     g_value_get_uint64
;;;
;;;     G_IS_PARAM_SPEC_FLOAT
;;;     G_PARAM_SPEC_FLOAT
;;;     G_VALUE_HOLDS_FLOAT
;;;     G_TYPE_PARAM_FLOAT
;;;
;;;     GParamSpecFloat
;;;
;;;     g_param_spec_float
;;;     g_value_set_float
;;;     g_value_get_float
;;;
;;;     G_IS_PARAM_SPEC_DOUBLE
;;;     G_PARAM_SPEC_DOUBLE
;;;     G_VALUE_HOLDS_DOUBLE
;;;     G_TYPE_PARAM_DOUBLE
;;;
;;;     GParamSpecDouble
;;;
;;;     g_param_spec_double
;;;     g_value_set_double
;;;     g_value_get_double
;;;
;;;     G_IS_PARAM_SPEC_ENUM
;;;     G_PARAM_SPEC_ENUM
;;;     G_VALUE_HOLDS_ENUM
;;;     G_TYPE_PARAM_ENUM
;;;
;;;     GParamSpecEnum
;;;
;;;     g_param_spec_enum
;;;     g_value_set_enum
;;;     g_value_get_enum
;;;
;;;     G_IS_PARAM_SPEC_FLAGS
;;;     G_PARAM_SPEC_FLAGS
;;;     G_VALUE_HOLDS_FLAGS
;;;     G_TYPE_PARAM_FLAGS
;;;
;;;     GParamSpecFlags
;;;
;;;     g_param_spec_flags
;;;     g_value_set_flags
;;;     g_value_get_flags
;;;
;;;     G_IS_PARAM_SPEC_STRING
;;;     G_PARAM_SPEC_STRING
;;;     G_VALUE_HOLDS_STRING
;;;     G_TYPE_PARAM_STRING
;;;
;;;     GParamSpecString
;;;     gchararray
;;;
;;;     g_param_spec_string
;;;     g_value_set_string
;;;     g_value_set_static_string
;;;     g_value_take_string
;;;     g_value_set_string_take_ownership
;;;     g_value_get_string
;;;     g_value_dup_string
;;;
;;;     G_IS_PARAM_SPEC_PARAM
;;;     G_PARAM_SPEC_PARAM
;;;     G_VALUE_HOLDS_PARAM
;;;     G_TYPE_PARAM_PARAM
;;;
;;;     GParamSpecParam
;;;
;;;     g_param_spec_param
;;;     g_value_set_param
;;;     g_value_take_param
;;;     g_value_set_param_take_ownership
;;;     g_value_get_param
;;;     g_value_dup_param
;;;
;;;     G_IS_PARAM_SPEC_BOXED
;;;     G_PARAM_SPEC_BOXED
;;;     G_VALUE_HOLDS_BOXED
;;;     G_TYPE_PARAM_BOXED
;;;
;;;     GParamSpecBoxed
;;;
;;;     g_param_spec_boxed
;;;     g_value_set_boxed
;;;     g_value_set_static_boxed
;;;     g_value_take_boxed
;;;     g_value_set_boxed_take_ownership
;;;     g_value_get_boxed
;;;     g_value_dup_boxed
;;;
;;;     G_IS_PARAM_SPEC_POINTER
;;;     G_PARAM_SPEC_POINTER
;;;     G_VALUE_HOLDS_POINTER
;;;     G_TYPE_PARAM_POINTER
;;;
;;;     GParamSpecPointer
;;;
;;;     g_param_spec_pointer
;;;     g_value_set_pointer
;;;     g_value_get_pointer
;;;
;;;     G_IS_PARAM_SPEC_OBJECT
;;;     G_PARAM_SPEC_OBJECT
;;;     G_VALUE_HOLDS_OBJECT
;;;     G_TYPE_PARAM_OBJECT
;;;
;;;     GParamSpecObject
;;;
;;;     g_param_spec_object
;;;     g_value_set_object
;;;     g_value_take_object
;;;     g_value_set_object_take_ownership
;;;     g_value_get_object
;;;     g_value_dup_object
;;;
;;;     G_IS_PARAM_SPEC_UNICHAR
;;;     G_PARAM_SPEC_UNICHAR
;;;     G_TYPE_PARAM_UNICHAR
;;;
;;;     GParamSpecUnichar
;;;
;;;     g_param_spec_unichar
;;;
;;;     G_IS_PARAM_SPEC_VALUE_ARRAY
;;;     G_PARAM_SPEC_VALUE_ARRAY
;;;     G_TYPE_PARAM_VALUE_ARRAY
;;;
;;;     GParamSpecValueArray
;;;
;;;     g_param_spec_value_array
;;;
;;;     G_IS_PARAM_SPEC_OVERRIDE
;;;     G_PARAM_SPEC_OVERRIDE
;;;     G_TYPE_PARAM_OVERRIDE
;;;
;;;     GParamSpecOverride
;;;
;;;     g_param_spec_override
;;;
;;;     G_IS_PARAM_SPEC_GTYPE
;;;     G_PARAM_SPEC_GTYPE
;;;     G_VALUE_HOLDS_GTYPE
;;;     G_TYPE_PARAM_GTYPE
;;;
;;;     GParamSpecGType
;;;
;;;     g_param_spec_gtype
;;;     g_value_get_gtype
;;;     g_value_set_gtype
;;;
;;;     G_IS_PARAM_SPEC_VARIANT
;;;     G_PARAM_SPEC_VARIANT
;;;     G_VALUE_HOLDS_VARIANT
;;;     G_TYPE_PARAM_VARIANT
;;;
;;;     GParamSpecVariant
;;;
;;;     g_param_spec_variant
;;;     g_value_get_variant
;;;     g_value_dup_variant
;;;     g_value_set_variant
;;;     g_value_take_variant
