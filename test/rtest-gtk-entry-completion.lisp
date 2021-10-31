(def-suite gtk-entry-completion :in gtk-suite)
(in-suite gtk-entry-completion)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryCompletion

(test g-entry-completion-class
  ;; Type check
  (is (g-type-is-object "GtkEntryCompletion"))
  ;; Check the registered name
  (is (eq 'gtk-entry-completion
          (registered-object-type-by-name "GtkEntryCompletion")))
  ;; Check the type initializer
  (is (eq (gtype "GtkEntryCompletion")
          (gtype (foreign-funcall "gtk_entry_completion_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkEntryCompletion")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkEntryCompletion"))))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkEntryCompletion"))))
  ;; Check the class properties
  (is (equal '("cell-area" "inline-completion" "inline-selection"
               "minimum-key-length" "model" "popup-completion" "popup-set-width"
               "popup-single-match" "text-column")
             (list-class-property-names "GtkEntryCompletion")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEntryCompletion" GTK-ENTRY-COMPLETION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout") :TYPE-INITIALIZER
                        "gtk_entry_completion_get_type")
                       ((CELL-AREA GTK-ENTRY-COMPLETION-CELL-AREA "cell-area"
                         "GtkCellArea" T NIL)
                        (INLINE-COMPLETION
                         GTK-ENTRY-COMPLETION-INLINE-COMPLETION
                         "inline-completion" "gboolean" T T)
                        (INLINE-SELECTION GTK-ENTRY-COMPLETION-INLINE-SELECTION
                         "inline-selection" "gboolean" T T)
                        (MINIMUM-KEY-LENGTH
                         GTK-ENTRY-COMPLETION-MINIMUM-KEY-LENGTH
                         "minimum-key-length" "gint" T T)
                        (MODEL GTK-ENTRY-COMPLETION-MODEL "model"
                         "GtkTreeModel" T T)
                        (POPUP-COMPLETION GTK-ENTRY-COMPLETION-POPUP-COMPLETION
                         "popup-completion" "gboolean" T T)
                        (POPUP-SET-WIDTH GTK-ENTRY-COMPLETION-POPUP-SET-WIDTH
                         "popup-set-width" "gboolean" T T)
                        (POPUP-SINGLE-MATCH
                         GTK-ENTRY-COMPLETION-POPUP-SINGLE-MATCH
                         "popup-single-match" "gboolean" T T)
                        (TEXT-COLUMN GTK-ENTRY-COMPLETION-TEXT-COLUMN
                         "text-column" "gint" T T)))
             (get-g-type-definition "GtkEntryCompletion"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkCellArea*   cell-area             Read / Write / Construct Only
;;;        gboolean    inline-completion     Read / Write
;;;        gboolean    inline-selection      Read / Write
;;;            gint    minimum-key-length    Read / Write
;;;    GtkTreeModel*   model                 Read / Write
;;;        gboolean    popup-completion      Read / Write
;;;        gboolean    popup-set-width       Read / Write
;;;        gboolean    popup-single-match    Read / Write
;;;            gint    text-column           Read / Write

;;; --- Signals ----------------------------------------------------------------

;;;            void    action-activated      Run Last
;;;        gboolean    cursor-on-match       Run Last
;;;        gboolean    insert-prefix         Run Last
;;;        gboolean    match-selected        Run Last
;;;            void    no-matches            Run Last

;;; --- Functions --------------------------------------------------------------

;;;     GtkEntryCompletionMatchFunc

#+nil
(test gtk-entry-completion-match-func
  (is (equal '()
             (macroexpand '(define-cb-methods gtk-entry-completion-match-func :int
                               ((current-page :int)))))))

;;;     gtk_entry_completion_new
;;;     gtk_entry_completion_new_with_area
;;;     gtk_entry_completion_get_entry
;;;     gtk_entry_completion_set_model
;;;     gtk_entry_completion_get_model
;;;     gtk_entry_completion_set_match_func
;;;     gtk_entry_completion_set_minimum_key_length
;;;     gtk_entry_completion_get_minimum_key_length
;;;     gtk_entry_completion_compute_prefix
;;;     gtk_entry_completion_complete
;;;     gtk_entry_completion_get_completion_prefix
;;;     gtk_entry_completion_insert_prefix
;;;     gtk_entry_completion_insert_action_text
;;;     gtk_entry_completion_insert_action_markup
;;;     gtk_entry_completion_delete_action
;;;     gtk_entry_completion_set_text_column
;;;     gtk_entry_completion_get_text_column
;;;     gtk_entry_completion_set_inline_completion
;;;     gtk_entry_completion_get_inline_completion
;;;     gtk_entry_completion_set_inline_selection
;;;     gtk_entry_completion_get_inline_selection
;;;     gtk_entry_completion_set_popup_completion
;;;     gtk_entry_completion_get_popup_completion
;;;     gtk_entry_completion_set_popup_set_width
;;;     gtk_entry_completion_get_popup_set_width
;;;     gtk_entry_completion_set_popup_single_match
;;;     gtk_entry_completion_get_popup_single_match

;;; 2021-10-26
