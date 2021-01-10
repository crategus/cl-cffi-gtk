(def-suite pango-context :in pango-suite)
(in-suite pango-context)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoContext

(test pango-context-class
  ;; Type check
  (is (g-type-is-object "PangoContext"))
  ;; Check the registered name
  (is (eq 'pango-context
          (registered-object-type-by-name "PangoContext")))
  ;; Check the type initializer
  (is (eq (gtype "PangoContext")
          (gtype (foreign-funcall "pango_context_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "PangoContext")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "PangoContext"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "PangoContext"))))
  ;; Check the class properties
  (is (equal '()
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "PangoContext"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "PangoContext" PANGO-CONTEXT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "pango_context_get_type")
                       NIL)
             (get-g-type-definition "PangoContext"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_context_new

(test pango-context-new
  (is (typep (pango-context-new) 'pango-context)))

;;;     pango_context_changed

(test pango-context-changed
  (is-false (pango-context-changed (pango-context-new))))

;;;     pango_context_get_serial

(test pango-context-serial
  (let ((context (pango-context-new)))
    (is (= 1 (pango-context-serial context)))))

;;;     pango_context_set_font_map
;;;     pango_context_get_font_map

(test pango-context-font-map
  (let ((context (pango-context-new)))
    (is-false (pango-context-font-map context))
    (is (typep (setf (pango-context-font-map context)
                     (pango-cairo-font-map-default)) 'pango-font-map))
    (is (typep (pango-context-font-map context) 'pango-font-map))))

;;;     pango_context_get_font_description
;;;     pango_context_set_font_description

(test pango-context-font-description
  (let ((context (pango-context-new)))
    (is (typep (pango-context-font-description context)
               'pango-font-description))
    (is (typep (setf (pango-context-font-description context)
                     (pango-font-description-new))
               'pango-font-description))
    (is (typep (pango-context-font-description context)
               'pango-font-description))))

;;;     pango_context_get_language
;;;     pango_context_set_language

(test pango-context-language
  (let ((context (pango-context-new)))
    (is (typep (pango-context-language context) 'pango-language))
    (is (typep (setf (pango-context-language context)
                     (pango-language-default))
               'pango-language))
   (is (typep (pango-context-language context) 'pango-language))))

;;;     pango_context_get_base_dir
;;;     pango_context_set_base_dir

(test pango-context-base-dir
  (let ((context (pango-context-new)))
    (is (eq :weak-ltr (pango-context-base-dir context)))
    (is (eq :ltr (setf (pango-context-base-dir context) :ltr)))
    (is (eq :ltr (pango-context-base-dir context)))))

;;;     pango_context_get_base_gravity
;;;     pango_context_set_base_gravity
;;;     pango_context_get_gravity

(test pango-context-base-gravity
  (let ((context (pango-context-new)))
    (is (eq :south (pango-context-base-gravity context)))
    (is (eq :auto (setf (pango-context-base-gravity context) :auto)))
    (is (eq :auto (pango-context-base-gravity context)))
    (is (eq :south (pango-context-gravity context)))))

;;;     pango_context_get_gravity_hint
;;;     pango_context_set_gravity_hint
;;;     pango_context_get_matrix
;;;     pango_context_set_matrix
;;;     pango_context_get_round_glyph_positions
;;;     pango_context_set_round_glyph_positions
;;;     pango_context_load_font
;;;     pango_context_load_fontset
;;;     pango_context_get_metrics
;;;     pango_context_list_families

;;; 2021-1-2
