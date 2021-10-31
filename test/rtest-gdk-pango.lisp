(def-suite gdk-pango :in gdk-suite)
(in-suite gdk-pango)

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pango_layout_get_clip_region
;;;     gdk_pango_layout_line_get_clip_region

;;;     gdk_pango_context_get

(test gdk-pango-context-get
  (is (typep (gdk-pango-context-get) 'pango-context)))

;;;     gdk_pango_context_get_for_screen

(test gdk-pango-context-for-screen
  (is (typep (gdk-pango-context-for-screen (gdk-screen-default))
             'pango-context)))

;;;     gdk_pango_context_get_for_display

(test gdk-pango-context-for-display
  (is (typep (gdk-pango-context-for-display (gdk-display-default))
             'pango-context)))

;;; 2021-10-28
