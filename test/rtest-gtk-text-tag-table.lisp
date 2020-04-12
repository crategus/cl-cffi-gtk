(def-suite gtk-text-tag-table :in gtk-suite)
(in-suite gtk-text-tag-table)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextTagTable

(test gtk-text-tag-table-class
  ;; Type check
  (is-true  (g-type-is-object "GtkTextTagTable"))
  ;; Check the registered name
  (is (eq 'gtk-text-tag-table
          (registered-object-type-by-name "GtkTextTagTable")))
  ;; Check the type initializer
  (is (string= "GtkTextTagTable"
               (g-type-name (gtype (foreign-funcall "gtk_text_tag_table_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkTextTagTable")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkTextTagTable"))))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkTextTagTable"))))
  ;; Check the class properties
  (is (equal '()
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkTextTagTable"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTextTagTable" GTK-TEXT-TAG-TABLE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_text_tag_table_get_type")
                       NIL)
             (get-g-type-definition "GtkTextTagTable"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_tag_table_new

(test gtk-text-tag-table-new
  (is (eq 'gtk-text-tag-table (type-of (gtk-text-tag-table-new))))
  (is (= 0 (gtk-text-tag-table-size (gtk-text-tag-table-new)))))

;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_get_size

(test gtk-text-tag-table-add
  (let ((table (gtk-text-tag-table-new)))
    (is (= 0 (gtk-text-tag-table-size table)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "bold" :weight 700)))
    (is (= 1 (gtk-text-tag-table-size table)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "blue-foreground" :foreground "blue")))
    (is (= 2 (gtk-text-tag-table-size table)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "italic" :style :italic)))
    (is (= 3 (gtk-text-tag-table-size table)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "font" :font "fixed")))
    (is (= 4 (gtk-text-tag-table-size table)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "font-italic" :font "fixed" :style :italic)))
    (is (= 5 (gtk-text-tag-table-size table)))

    (let ((tag (gtk-text-tag-table-lookup table "bold")))
      (is (eq 'gtk-text-tag (type-of tag)))
      (is-false (gtk-text-tag-table-remove table tag))
      (is-false (gtk-text-tag-table-lookup table "bold")
      (is (= 4 (gtk-text-tag-table-size table)))))))

;;;     gtk_text_tag_table_foreach

(test gtk-text-tag-table-foreach
  (let ((table (gtk-text-tag-table-new)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "bold" :weight 700)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "blue-foreground" :foreground "blue")))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "italic" :style :italic)))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "font" :font "fixed")))
    (is-true (gtk-text-tag-table-add table
                                     (gtk-text-tag-new "font-italic" :font "fixed" :style :italic)))
    ;; TODO: Implement a better example?
    (is-false (gtk-text-tag-table-foreach table #'gtk-text-tag-priority))))

;;; --- Signals ----------------------------------------------------------------

;;;     void   tag-added      Run Last
;;;     void   tag-changed    Run Last
;;;     void   tag-removed    Run Last

(test gtk-text-tag-table-signals
  (let ((result nil)
        (table (gtk-text-tag-table-new)))
    (g-signal-connect table "tag-added"
                      (lambda (table tag)
                        (setf result (cons "added" result))
                        (is (eq 'gtk-text-tag-table (type-of table)))
                        (is (eq 'gtk-text-tag (type-of tag)))))
    (g-signal-connect table "tag-changed"
                      (lambda (table tag size-changed)
                        (setf result (cons "changed" result))
                        (is (eq 'gtk-text-tag-table (type-of table)))
                        (is (eq 'gtk-text-tag (type-of tag)))
                        (is (eq 'boolean (type-of size-changed)))))
    (g-signal-connect table "tag-removed"
                      (lambda (table tag)
                        (setf result (cons "removed" result))
                        (is (eq 'gtk-text-tag-table (type-of table)))
                        (is (eq 'gtk-text-tag (type-of tag)))))
    (is-true (gtk-text-tag-table-add table (gtk-text-tag-new "bold" :weight 700)))
    (is-true (gtk-text-tag-table-add table (gtk-text-tag-new "font" :font "fixed")))
    (is-false (gtk-text-tag-table-remove table (gtk-text-tag-table-lookup table "bold")))
    (setf (gtk-text-tag-font (gtk-text-tag-table-lookup table "font")) "italic")
    (is (equal '("changed" "removed" "added" "added") result))))
