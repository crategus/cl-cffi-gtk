(def-suite pango-fonts :in pango-suite)
(in-suite pango-fonts)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoFontDescription
;;;     PangoStyle
;;;     PangoWeight
;;;     PangoVariant
;;;     PangoStretch
;;;     PangoFontMask
;;;     PangoFontMetrics
;;;     PangoFont
;;;     PangoFontFamily
;;;     PangoFontFace
;;;     PangoFontMap
;;;     PangoFontMapClass
;;;     PangoFontset
;;;     PangoFontsetClass

;;; --- Functions --------------------------------------------------------------

;;;     pango_font_description_new

(test pango-font-description-new
  (is (typep (pango-font-description-new) 'pango-font-description)))

;;;     pango_font_description_copy

(test pango-font-description-copy
  (let ((desc (pango-font-description-new)))
    (is (typep (pango-font-description-copy desc) 'pango-font-description))))

;;;     pango_font_description_copy_static

(test pango-font-description-copy-static
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (typep (pango-font-description-copy-static desc)
               'pango-font-description))))

;;;     pango_font_description_hash

(test pango-font-description-hash
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (integerp (pango-font-description-hash desc)))))

;;;     pango_font_description_equal

(test pango-font-description-equal
  (let ((desc1 (pango-font-description-from-string "Sans Bold 16"))
        (desc2 (pango-font-description-from-string "Sans Bold 18")))
    (is-false (pango-font-description-equal desc1 desc2))
    (is-true (pango-font-description-equal desc1 desc1))
    (is-true (pango-font-description-equal desc2 desc2))))

;;;     pango_font_description_free
;;;     pango_font_descriptions_free

;;;     pango_font_description_set_family
;;;     pango_font_description_set_family_static
;;;     pango_font_description_get_family

(test pango-font-description-family
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (string= "Sans" (pango-font-description-family desc)))
    (is (string= "Verdana"
                 (setf (pango-font-description-family desc) "Verdana")))
    (is (string= "Verdana Bold 16"
                 (pango-font-description-to-string desc)))))

;;;     pango_font_description_set_style
;;;     pango_font_description_get_style

(test pango-font-description-style
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (eq :normal (pango-font-description-style desc)))
    (is (eq :italic (setf (pango-font-description-style desc) :italic)))
    (is (eq :italic (pango-font-description-style desc)))))

;;;     pango_font_description_set_variant
;;;     pango_font_description_get_variant

(test pango-font-description-variant
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (eq :normal (pango-font-description-variant desc)))
    (is (eq :small-caps
            (setf (pango-font-description-variant desc) :small-caps)))
    (is (eq :small-caps (pango-font-description-variant desc)))))

;;;     pango_font_description_set_weight
;;;     pango_font_description_get_weight

(test pango-font-description-weight
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (eq :bold (pango-font-description-weight desc)))
    (is (eq :thin (setf (pango-font-description-weight desc) :thin)))
    (is (eq :thin (pango-font-description-weight desc)))))

;;;     pango_font_description_set_stretch
;;;     pango_font_description_get_stretch

(test pango-font-description-stretch
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (eq :normal (pango-font-description-stretch desc)))
    (is (eq :expanded (setf (pango-font-description-stretch desc) :expanded)))
    (is (eq :expanded (pango-font-description-stretch desc)))))

;;;     pango_font_description_set_size
;;;     pango_font_description_get_size

(test pango-font-description-size
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (= (* 16 +pango-scale+) (pango-font-description-size desc)))
    (is (= (* 18 +pango-scale+)
           (setf (pango-font-description-size desc) (* 18 +pango-scale+))))
    (is (= (* 18 +pango-scale+)
           (pango-font-description-size desc)))))

;;;     pango_font_description_set_absolute_size
;;;     pango_font_description_get_size_is_absolute

(test pango-font-description-absolute-size
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is-false (pango-font-description-size-is-absolute desc))
    (is (= (* 16 +pango-scale+) (pango-font-description-size desc)))
    (is-false (pango-font-description-set-absolute-size desc 18))
    (is-true (pango-font-description-size-is-absolute desc))
    (is (= 18 (pango-font-description-size desc)))))

;;;     pango_font_description_set_gravity
;;;     pango_font_description_get_gravity

(test pango-font-description-gravity
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (eq :south (pango-font-description-gravity desc)))
    (is (eq :north (setf (pango-font-description-gravity desc) :north)))
    (is (eq :north (pango-font-description-gravity desc)))))

;;;     pango_font_description_set_variations
;;;     pango_font_description_set_variations_static
;;;     pango_font_description_get_variations

;; no example for a font with variations

(test pango-font-description-variations
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is-false (pango-font-description-variations desc))))

;;;     pango_font_description_get_set_fields
;;;     pango_font_description_unset_fields

(test pango-font-description-set-fields
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (equal '(:FAMILY :STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
               (pango-font-description-set-fields desc)))
    (is-false (pango-font-description-unset-fields desc :family))
    (is (equal '(:STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
               (pango-font-description-set-fields desc)))
    (is-false (pango-font-description-unset-fields desc '(:style :variant)))
    (is (equal '(:WEIGHT :STRETCH :SIZE)
               (pango-font-description-set-fields desc)))))

;;;     pango_font_description_merge
;;;     pango_font_description_merge_static

(test pango-font-description-merge
  (let ((desc1 (pango-font-description-from-string "16"))
        (desc2 (pango-font-description-from-string "Sans Bold")))
    (is (string= "Normal 16" (pango-font-description-to-string desc1)))
    (is (string= "Sans Bold" (pango-font-description-to-string desc2)))
    (is-false (pango-font-description-merge desc1 desc2 t))
    (is (string= "Sans Bold 16" (pango-font-description-to-string desc1)))))

;;;     pango_font_description_better_match

;;;     pango_font_description_from_string
;;;     pango_font_description_to_string
;;;     pango_font_description_to_filename

(test pango-font-description-from-string
  (let ((desc (pango-font-description-from-string "Sans Bold 16")))
    (is (typep desc 'pango-font-description))
    (is (string= "Sans Bold 16" (pango-font-description-to-string desc)))
    (is (string= "sans_bold_16" (pango-font-description-to-filename desc)))))

;;;     pango_font_metrics_ref
;;;     pango_font_metrics_unref

;;;     pango_font_metrics_get_ascent
;;;     pango_font_metrics_get_descent
;;;     pango_font_metrics_get_height
;;;     pango_font_metrics_get_approximate_char_width
;;;     pango_font_metrics_get_approximate_digit_width
;;;     pango_font_metrics_get_underline_thickness
;;;     pango_font_metrics_get_underline_position
;;;     pango_font_metrics_get_strikethrough_thickness
;;;     pango_font_metrics_get_strikethrough_position

(test pango-font-metrics.1
  (let* ((fontmap (pango-cairo-font-map-default))
         (context (pango-font-map-create-context fontmap))
         (desc (pango-font-description-from-string "Sans 12"))
         (lang (pango-language-from-string "de-de"))
         (metrics (pango-context-metrics context desc lang)))
    (is (typep metrics 'pango-font-metrics))
    (is (= 15208 (pango-font-metrics-ascent metrics)))
    (is (=  3864 (pango-font-metrics-descent metrics)))
    (is (= 19072 (pango-font-metrics-height metrics)))
    (is (=  8553 (pango-font-metrics-approximate-char-width metrics)))
    (is (= 10240 (pango-font-metrics-approximate-digit-width metrics)))
    (is (=   720 (pango-font-metrics-underline-thickness metrics)))
    (is (=  -320 (pango-font-metrics-underline-position metrics)))
    (is (=   816 (pango-font-metrics-strikethrough-thickness metrics)))
    (is (=  4240 (pango-font-metrics-strikethrough-position metrics)))))

(test pango-font-metrics.2
  (let* ((fontmap (pango-cairo-font-map-default))
         (context (pango-font-map-create-context fontmap))
         (desc (pango-font-description-from-string "Sans 18"))
         (lang (pango-language-from-string "de-de"))
         (metrics (pango-context-metrics context desc lang)))
    (is (typep metrics 'pango-font-metrics))
    (is (= 22812 (pango-font-metrics-ascent metrics)))
    (is (=  5796 (pango-font-metrics-descent metrics)))
    (is (= 28608 (pango-font-metrics-height metrics)))
    (is (= 12829 (pango-font-metrics-approximate-char-width metrics)))
    (is (= 15360 (pango-font-metrics-approximate-digit-width metrics)))
    (is (=  1080 (pango-font-metrics-underline-thickness metrics)))
    (is (=  -480 (pango-font-metrics-underline-position metrics)))
    (is (=  1224 (pango-font-metrics-strikethrough-thickness metrics)))
    (is (=  6360 (pango-font-metrics-strikethrough-position metrics)))))

;;;     pango_font_find_shaper
;;;     pango_font_describe
;;;     pango_font_describe_with_absolute_size
;;;     pango_font_get_face
;;;     pango_font_get_coverage
;;;     pango_font_has_char
;;;     pango_font_get_glyph_extents
;;;     pango_font_get_metrics
;;;     pango_font_get_font_map
;;;     pango_font_get_features
;;;     pango_font_get_hb_font
;;;
;;;     pango_font_family_get_name
;;;     pango_font_family_is_monospace
;;;     pango_font_family_is_variable
;;;     pango_font_family_list_faces
;;;     pango_font_family_get_face
;;;
;;;     pango_font_face_get_face_name
;;;     pango_font_face_list_sizes
;;;     pango_font_face_describe
;;;     pango_font_face_is_synthesized
;;;     pango_font_face_get_family
;;;
;;;     pango_font_map_create_context
;;;     pango_font_map_load_font
;;;     pango_font_map_load_fontset
;;;     pango_font_map_list_families
;;;     pango_font_map_get_family
;;;     pango_font_map_get_serial
;;;     pango_font_map_changed
;;;     pango_font_map_get_shape_engine_type
;;;
;;;     pango_fontset_get_font
;;;     pango_fontset_get_metrics
;;;     PangoFontsetForeachFunc
;;;     pango_fontset_foreach

;;; 2020-1-7
