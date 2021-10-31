(def-suite pango-cairo-rendering :in pango-suite)
(in-suite pango-cairo-rendering)

;;; -- Types and Values --------------------------------------------------------

;;;     PangoCairoFont

(test pango-cairo-font-interface
  ;; Type check
  (is (g-type-is-interface "PangoCairoFont"))
  ;; Check the registered name
  (is (eq 'pango-cairo-font
          (registered-object-type-by-name "PangoCairoFont")))
  ;; Check the type initializer
  (is (eq (gtype "PangoCairoFont")
          (gtype (foreign-funcall "pango_cairo_font_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "PangoCairoFont"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "PangoCairoFont"
                                  PANGO-CAIRO-FONT
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "pango_cairo_font_get_type"))
             (get-g-type-definition "PangoCairoFont"))))

;;;     PangoCairoFontMap

(test pango-cairo-font-map-interface
  ;; Type check
  (is (g-type-is-interface "PangoCairoFontMap"))
  ;; Check the registered name
  (is (eq 'pango-cairo-font-map
          (registered-object-type-by-name "PangoCairoFontMap")))
  ;; Check the type initializer
  (is (eq (gtype "PangoCairoFontMap")
          (gtype (foreign-funcall "pango_cairo_font_map_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "PangoCairoFontMap"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "PangoCairoFontMap"
                                  PANGO-CAIRO-FONT-MAP
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "pango_cairo_font_map_get_type"))
             (get-g-type-definition "PangoCairoFontMap"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_cairo_font_map_get_default
;;;     pango_cairo_font_map_set_default

(test pango-cairo-font-map-default
  (is (typep (pango-cairo-font-map-default) 'pango-font-map))
  (is (typep (setf (pango-cairo-font-map-default) (pango-cairo-font-map-new))
             'pango-font-map))
  (is (typep (pango-cairo-font-map-default) 'pango-font-map)))

;;;     pango_cairo_font_map_new

(test pango-cairo-font-map-new
  (is (typep (pango-cairo-font-map-new) 'pango-font-map)))

;;;     pango_cairo_font_map_new_for_font_type
;;;     pango_cairo_font_map_get_font_type
;;;     pango_cairo_font_map_set_resolution
;;;     pango_cairo_font_map_get_resolution
;;;     pango_cairo_font_map_create_context
;;;     pango_cairo_font_get_scaled_font
;;;     pango_cairo_context_set_resolution
;;;     pango_cairo_context_get_resolution

;;;     pango_cairo_context_set_font_options
;;;     pango_cairo_context_get_font_options

(test pango-cairo-context-font-options
  (let ((context (gdk-pango-context-get))
        (options (cairo-font-options-create)))
    (is (cairo-font-options-equal options
                                  (setf (pango-cairo-context-font-options context)
                                        options)))
    (is (cairo-font-options-equal options
                                  (pango-cairo-context-font-options context)))

    (is-false (setf (pango-cairo-context-font-options context) nil))
    (is-false (pango-cairo-context-font-options context))))

;;;     PangoCairoShapeRendererFunc
;;;
;;;     pango_cairo_context_set_shape_renderer
;;;     pango_cairo_context_get_shape_renderer
;;;     pango_cairo_create_context
;;;     pango_cairo_update_context
;;;     pango_cairo_create_layout
;;;     pango_cairo_update_layout
;;;     pango_cairo_show_glyph_string
;;;     pango_cairo_show_glyph_item
;;;     pango_cairo_show_layout_line
;;;     pango_cairo_show_layout
;;;     pango_cairo_show_error_underline
;;;     pango_cairo_glyph_string_path
;;;     pango_cairo_layout_line_path
;;;     pango_cairo_layout_path
;;;     pango_cairo_error_underline_path

;;; 2021-10-28
