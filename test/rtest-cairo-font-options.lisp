(def-suite cairo-font-options-suite :in cairo-suite)
(in-suite cairo-font-options-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_font_options_t
;;;     cairo_subpixel_order_t
;;;     cairo_hint_style_t
;;;     cairo_hint_metrics_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_font_options_create

(test cairo-font-options-create
  (let ((options (cairo-font-options-create)))
    (is (pointerp options))
    (is (eq :default (cairo-font-options-antialias options)))
    (is (eq :default (cairo-font-options-subpixel-order options)))
    (is (eq :default (cairo-font-options-hint-style options)))
    (is (eq :default (cairo-font-options-hint-metrics options)))
    (is-false (cairo-font-options-variations options))))

;;;     cairo_font_options_copy
;;;     cairo_font_options_destroy
;;;     cairo_font_options_status
;;;     cairo_font_options_merge
;;;     cairo_font_options_hash
;;;     cairo_font_options_equal

;;;     cairo_font_options_set_antialias
;;;     cairo_font_options_get_antialias
;;;     cairo_font_options_set_subpixel_order
;;;     cairo_font_options_get_subpixel_order
;;;     cairo_font_options_set_hint_style
;;;     cairo_font_options_get_hint_style
;;;     cairo_font_options_set_hint_metrics
;;;     cairo_font_options_get_hint_metrics
;;;     cairo_font_options_get_variations
;;;     cairo_font_options_set_variations

#-windows ; On Windows we have a null-pointer for the font options
(test cairo-font-options-features
  (let ((options (gdk-screen-font-options (gdk-screen-default))))
    (is (eq :subpixel (cairo-font-options-antialias options)))
    (is (eq :rgb (cairo-font-options-subpixel-order options)))
    (is (eq :slight (cairo-font-options-hint-style options)))
    (is (eq :on (cairo-font-options-hint-metrics options)))
    (is-false (cairo-font-options-variations options))))

#-windows ; On Windows we have a null-pointer for the font options
(test cairo-font-options-variations
  (let ((options (gdk-screen-font-options (gdk-screen-default))))
    (is (string= "wght 200, wdth 140.5"
                 (setf (cairo-font-options-variations options)
                       "wght 200, wdth 140.5")))
    (is (string= "wght 200, wdth 140.5"
                 (cairo-font-options-variations options)))
    ;; Check a NIL value
    (is-false (setf (cairo-font-options-variations options) nil))
    (is-false (cairo-font-options-variations options))))

;;; 2021-10-29
