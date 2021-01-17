(def-suite pango-attributes :in pango-suite)
(in-suite pango-attributes)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoAttrType

(test pango-attr-type
  ;; Check the type
  (is (g-type-is-enum "PangoAttrType"))
  ;; Check the type initializer
  (is (eq (gtype "PangoAttrType")
          (gtype (foreign-funcall "pango_attr_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'pango-attr-type
          (registered-enum-type "PangoAttrType")))
  ;; Check the names
  (is (equal '("PANGO_ATTR_INVALID" "PANGO_ATTR_LANGUAGE" "PANGO_ATTR_FAMILY"
               "PANGO_ATTR_STYLE" "PANGO_ATTR_WEIGHT" "PANGO_ATTR_VARIANT"
               "PANGO_ATTR_STRETCH" "PANGO_ATTR_SIZE" "PANGO_ATTR_FONT_DESC"
               "PANGO_ATTR_FOREGROUND" "PANGO_ATTR_BACKGROUND"
               "PANGO_ATTR_UNDERLINE" "PANGO_ATTR_STRIKETHROUGH"
               "PANGO_ATTR_RISE" "PANGO_ATTR_SHAPE" "PANGO_ATTR_SCALE"
               "PANGO_ATTR_FALLBACK" "PANGO_ATTR_LETTER_SPACING"
               "PANGO_ATTR_UNDERLINE_COLOR" "PANGO_ATTR_STRIKETHROUGH_COLOR"
               "PANGO_ATTR_ABSOLUTE_SIZE" "PANGO_ATTR_GRAVITY"
               "PANGO_ATTR_GRAVITY_HINT" "PANGO_ATTR_FONT_FEATURES"
               "PANGO_ATTR_FOREGROUND_ALPHA" "PANGO_ATTR_BACKGROUND_ALPHA"
               "PANGO_ATTR_ALLOW_BREAKS" "PANGO_ATTR_SHOW"
               "PANGO_ATTR_INSERT_HYPHENS" "PANGO_ATTR_OVERLINE"
               "PANGO_ATTR_OVERLINE_COLOR")
             (mapcar #'enum-item-name
                     (get-enum-items "PangoAttrType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29 30)
             (mapcar #'enum-item-value
                     (get-enum-items "PangoAttrType"))))
  ;; Check the nick names
  (is (equal '("invalid" "language" "family" "style" "weight" "variant"
               "stretch" "size" "font-desc" "foreground" "background"
               "underline" "strikethrough" "rise" "shape" "scale" "fallback"
               "letter-spacing" "underline-color" "strikethrough-color"
               "absolute-size" "gravity" "gravity-hint" "font-features"
               "foreground-alpha" "background-alpha" "allow-breaks" "show"
               "insert-hyphens" "overline" "overline-color")
             (mapcar #'enum-item-nick
                     (get-enum-items "PangoAttrType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoAttrType"
                             PANGO-ATTR-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_attr_type_get_type")
                             (:INVALID 0)
                             (:LANGUAGE 1)
                             (:FAMILY 2)
                             (:STYLE 3)
                             (:WEIGHT 4)
                             (:VARIANT 5)
                             (:STRETCH 6)
                             (:SIZE 7)
                             (:FONT-DESC 8)
                             (:FOREGROUND 9)
                             (:BACKGROUND 10)
                             (:UNDERLINE 11)
                             (:STRIKETHROUGH 12)
                             (:RISE 13)
                             (:SHAPE 14)
                             (:SCALE 15)
                             (:FALLBACK 16)
                             (:LETTER-SPACING 17)
                             (:UNDERLINE-COLOR 18)
                             (:STRIKETHROUGH-COLOR 19)
                             (:ABSOLUTE-SIZE 20)
                             (:GRAVITY 21)
                             (:GRAVITY-HINT 22)
                             (:FONT-FEATURES 23)
                             (:FOREGROUND-ALPHA 24)
                             (:BACKGROUND-ALPHA 25)
                             (:ALLOW-BREAKS 26)
                             (:SHOW 27)
                             (:INSERT-HYPHENS 28)
                             (:OVERLINE 29)
                             (:OVERLINE-COLOR 30))
             (get-g-type-definition "PangoAttrType"))))

;;;     PangoAttrClass

;;;     PangoAttribute

(test pango-attribute
  ;; Type check
  (is (g-type-is-a (gtype "PangoAttribute") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "PangoAttribute")
          (gtype (foreign-funcall "pango_attribute_get_type" g-size)))))

;;;     PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;;     PANGO_ATTR_INDEX_TO_TEXT_END

;;;     PangoAttrString
;;;     PangoAttrLanguage
;;;     PangoAttrColor
;;;     PangoAttrInt
;;;     PangoAttrFloat
;;;     PangoAttrFontDesc
;;;     PangoAttrShape
;;;     PangoAttrSize
;;;     PangoAttrFontFeatures

;;;     PangoUnderline

(test pango-underline
  ;; Check the type
  (is (g-type-is-enum "PangoUnderline"))
  ;; Check the type initializer
  (is (eq (gtype "PangoUnderline")
          (gtype (foreign-funcall "pango_underline_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'pango-underline
          (registered-enum-type "PangoUnderline")))
  ;; Check the names
  (is (equal '("PANGO_UNDERLINE_NONE" "PANGO_UNDERLINE_SINGLE"
               "PANGO_UNDERLINE_DOUBLE" "PANGO_UNDERLINE_LOW"
               "PANGO_UNDERLINE_ERROR" "PANGO_UNDERLINE_SINGLE_LINE"
               "PANGO_UNDERLINE_DOUBLE_LINE" "PANGO_UNDERLINE_ERROR_LINE")
             (mapcar #'enum-item-name
                     (get-enum-items "PangoUnderline"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (mapcar #'enum-item-value
                     (get-enum-items "PangoUnderline"))))
  ;; Check the nick names
  (is (equal '("none" "single" "double" "low" "error" "single-line"
               "double-line" "error-line")
             (mapcar #'enum-item-nick
                     (get-enum-items "PangoUnderline"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoUnderline"
                             PANGO-UNDERLINE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_underline_get_type")
                             (:NONE 0)
                             (:SINGLE 1)
                             (:DOUBLE 2)
                             (:LOW 3)
                             (:ERROR 4)
                             (:SINGLE-LINE 5)
                             (:DOUBLE-LINE 6)
                             (:ERROR-LINE 7))
             (get-g-type-definition "PangoUnderline"))))

;;;     PangoOverline

(test pango-overline
  ;; Check the type
  (is (g-type-is-enum "PangoOverline"))
  ;; Check the type initializer
  (is (eq (gtype "PangoOverline")
          (gtype (foreign-funcall "pango_overline_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'pango-overline
          (registered-enum-type "PangoOverline")))
  ;; Check the names
  (is (equal '("PANGO_OVERLINE_NONE" "PANGO_OVERLINE_SINGLE")
             (mapcar #'enum-item-name
                     (get-enum-items "PangoOverline"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "PangoOverline"))))
  ;; Check the nick names
  (is (equal '("none" "single")
             (mapcar #'enum-item-nick
                     (get-enum-items "PangoOverline"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoOverline"
                             PANGO-OVERLINE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_overline_get_type")
                             (:NONE 0)
                             (:SINGLE 1))
             (get-g-type-definition "PangoOverline"))))

;;;     PANGO_SCALE_XX_SMALL
;;;     PANGO_SCALE_X_SMALL
;;;     PANGO_SCALE_SMALL
;;;     PANGO_SCALE_MEDIUM
;;;     PANGO_SCALE_LARGE
;;;     PANGO_SCALE_X_LARGE
;;;     PANGO_SCALE_XX_LARGE

;;;     PangoShowFlags

(test pango-show-flags
  ;; Check the type
  (is (g-type-is-flags "PangoShowFlags"))
  ;; Check the registered name
  (is (eq 'pango-show-flags
          (registered-flags-type "PangoShowFlags")))
  ;; Check the type initializer
  (is (eq (gtype "PangoShowFlags")
          (gtype (foreign-funcall "pango_show_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("PANGO_SHOW_NONE" "PANGO_SHOW_SPACES" "PANGO_SHOW_LINE_BREAKS"
               "PANGO_SHOW_IGNORABLES")
             (mapcar #'flags-item-name
                     (get-flags-items "PangoShowFlags"))))
  ;; Check the values
  (is (equal '(0 1 2 4)
             (mapcar #'flags-item-value
                     (get-flags-items "PangoShowFlags"))))
  ;; Check the nick names
  (is (equal '("none" "spaces" "line-breaks" "ignorables")
             (mapcar #'flags-item-nick
                     (get-flags-items "PangoShowFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "PangoShowFlags"
                              PANGO-SHOW-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "pango_show_flags_get_type")
                              (:NONE 0)
                              (:SPACES 1)
                              (:LINE-BREAKS 2)
                              (:IGNORABLES 4))
             (get-g-type-definition "PangoShowFlags"))))

;;;     PangoColor

(test pango-color
  ;; Type check
  (is (g-type-is-a (gtype "PangoColor") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "PangoColor")
          (gtype (foreign-funcall "pango_color_get_type" g-size)))))

;;;     PangoAttrList

(test pango-attr-list
  ;; Type check
  (is (g-type-is-a (gtype "PangoAttrList") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "PangoAttrList")
          (gtype (foreign-funcall "pango_attr_list_get_type" g-size)))))

;;;     PangoAttrIterator

(test pango-attr-iterator
  ;; Type check
  (is (g-type-is-a (gtype "PangoAttrIterator") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "PangoAttrIterator")
          (gtype (foreign-funcall "pango_attr_iterator_get_type" g-size)))))

;;; --- Functions --------------------------------------------------------------
;;;
;;;     pango_attr_type_register
;;;     pango_attr_type_get_name
;;;     pango_attribute_init
;;;     pango_attribute_copy
;;;     pango_attribute_equal
;;;     pango_attribute_destroy
;;;
;;;     pango_attr_language_new
;;;     pango_attr_family_new
;;;     pango_attr_style_new
;;;     pango_attr_variant_new
;;;     pango_attr_stretch_new
;;;     pango_attr_weight_new
;;;     pango_attr_size_new
;;;     pango_attr_size_new_absolute
;;;     pango_attr_font_desc_new
;;;     pango_attr_foreground_new
;;;     pango_attr_background_new
;;;     pango_attr_strikethrough_new
;;;     pango_attr_strikethrough_color_new
;;;     pango_attr_underline_new
;;;     pango_attr_underline_color_new
;;;     pango_attr_overline_new
;;;     pango_attr_overline_color_new
;;;
;;;     pango_attr_shape_new
;;;     pango_attr_shape_new_with_data
;;;
;;;     PangoAttrDataCopyFunc
;;;
;;;     pango_attr_scale_new
;;;     pango_attr_rise_new
;;;     pango_attr_letter_spacing_new
;;;     pango_attr_fallback_new
;;;     pango_attr_gravity_new
;;;     pango_attr_gravity_hint_new
;;;     pango_attr_font_features_new
;;;     pango_attr_foreground_alpha_new
;;;     pango_attr_background_alpha_new
;;;     pango_attr_allow_breaks_new
;;;     pango_attr_insert_hyphens_new
;;;     pango_attr_show_new
;;;
;;;     pango_color_parse
;;;     pango_color_parse_with_alpha
;;;     pango_color_copy
;;;     pango_color_free
;;;     pango_color_to_string
;;;
;;;     pango_attr_list_new
;;;     pango_attr_list_ref
;;;     pango_attr_list_unref
;;;     pango_attr_list_copy
;;;     pango_attr_list_insert
;;;     pango_attr_list_insert_before
;;;     pango_attr_list_change
;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_update
;;;
;;;     PangoAttrFilterFunc
;;;
;;;     pango_attr_list_get_attributes

(test pango-attr-list-attributes
  (let* ((label (make-instance 'gtk-label :text
                                          "<span foreground='blue' ~
                                                 size='x-large'> ~
                                           Blue text</span> is <i>cool</i>!"
                                          :use-markup t))
         (attributes (pango-layout-attributes (gtk-label-layout label))))

    (is (typep attributes 'pango-attr-list))
    (is (every (lambda (x) (typep x 'pango-attribute))
               (pango-attr-list-attributes attributes)))
))

;;;     pango_attr_list_equal
;;;     pango_attr_list_get_iterator
;;;     pango_attr_iterator_copy
;;;     pango_attr_iterator_next
;;;     pango_attr_iterator_range
;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_get_font
;;;     pango_attr_iterator_get_attrs
;;;     pango_attr_iterator_destroy

;;; 2021-1-17
