(def-suite pango-rendering :in pango-suite)
(in-suite pango-rendering)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoItem
;;;     PangoAnalysis
;;;
;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;     PANGO_ANALYSIS_FLAG_NEED_HYPHEN
;;;
;;;     PangoLogAttr
;;;     PangoShapeFlags

;;; --- Functions --------------------------------------------------------------

;;;     pango_itemize

(test pango-itemize
  (let* ((label (make-instance 'gtk-label :label "Text"))
;         (string "text text text")
         (context (gtk-widget-pango-context label)))

    (is (typep context 'pango-context))

    (is (typep (pango-context-font-map context) 'pango-font-map))
    (is (typep (pango-context-font-description context) 'pango-font-description))
    (is (typep (pango-context-language context) 'pango-language))
    (is (eq :ltr (pango-context-base-dir context)))
    (is (eq :south (pango-context-base-gravity context)))
    (is (eq :south (pango-context-gravity context)))

    (is (eq :natural (pango-context-gravity-hint context)))



;    (is-false (pango-itemize context string 0 (length string) nil nil))
;    (is (every (lambda (x) (typep x 'pango-item))
;               (pango-itemize context string 0 (length string) nil nil)))
))

;;;     pango_itemize_with_base_dir

#+nil
(test pango-itemize-with-base-dir
  (let* ((label (make-instance 'gtk-label))
         (string "text text text")
         (context (gtk-widget-pango-context label))
         (attrs (pango::%pango-attr-list-new)))
    (is (typep context 'pango-context))
    (is (every (lambda (x) (typep x 'pango-item))
               (pango-itemize-with-base-dir context
                                            :ltr
                                            string
                                            0 (length string) attrs nil)))
))

;;;     pango_item_free

;;;     pango_item_copy

(test pango-item-copy
  (is (typep (pango-item-copy (pango-item-new)) 'pango-item)))

;;;     pango_item_new

(test pango-item-new
  (is (typep (pango-item-new) 'pango-item)))

;;;     pango_item_split

#+nil
(test pango-item-split
  (let* ((label (make-instance 'gtk-label))
         (string "this is the text to split")
         (context (gtk-widget-pango-context label))
         (items (pango-itemize context string 0 (length string) nil nil)))

    (is-false items)
;    (is-false (pango-item-split item 12 0))
;    (is-false item)

))


;;;     pango_item_apply_attrs

;;;     pango_reorder_items
;;;     pango_break
;;;     pango_get_log_attrs
;;;     pango_find_paragraph_boundary
;;;     pango_default_break
;;;     pango_tailor_break
;;;     pango_shape
;;;     pango_shape_full
;;;     pango_shape_with_flags

;;; 2021-1-11
