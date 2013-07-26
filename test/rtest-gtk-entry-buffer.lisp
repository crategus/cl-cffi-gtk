
(def-suite gtk-entry-buffer :in gtk-suite)
(in-suite gtk-entry-buffer)

;;;   gtk-entry-buffer

(test gtk-entry-buffer-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkEntryBuffer"))
  (is-false (g-type-is-abstract "GtkEntryBuffer"))
  (is-true  (g-type-is-derived "GtkEntryBuffer"))
  (is-false (g-type-is-fundamental "GtkEntryBuffer"))
  (is-true  (g-type-is-value-type "GtkEntryBuffer"))
  (is-true  (g-type-has-value-table "GtkEntryBuffer"))
  (is-true  (g-type-is-classed "GtkEntryBuffer"))
  (is-true  (g-type-is-instantiatable "GtkEntryBuffer"))
  (is-true  (g-type-is-derivable "GtkEntryBuffer"))
  (is-true  (g-type-is-deep-derivable "GtkEntryBuffer"))
  (is-false (g-type-is-interface "GtkEntryBuffer"))

  ;; Check the registered name
  (is (eq 'gtk-entry-buffer
          (registered-object-type-by-name "GtkEntryBuffer")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkEntryBuffer"))))
    (is (equal (gtype "GtkEntryBuffer") (g-type-from-class class)))
    (is (equal (gtype "GtkEntryBuffer") (g-object-class-type class)))
    (is (equal "GtkEntryBuffer" (g-object-class-name class)))
    (is (equal (gtype "GtkEntryBuffer") (g-type-from-class  (g-type-class-peek "GtkEntryBuffer"))))
    (is (equal (gtype "GtkEntryBuffer") (g-type-from-class  (g-type-class-peek-static "GtkEntryBuffer"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-entry-buffer)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-entry-buffer (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkEntryBuffer" (gobject-class-g-type-name class)))
    (is (equal "GtkEntryBuffer" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_entry_buffer_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GtkEntryBuffer")))
  (is (= 2 (g-type-depth "GtkEntryBuffer")))
  (is (equal (gtype "GtkEntryBuffer")
             (g-type-next-base "GtkEntryBuffer" "GObject")))
  (is-true  (g-type-is-a "GtkEntryBuffer" "GObject"))
  (is-false (g-type-is-a "GtkEntryBuffer" "GtkWidget"))
  (is-false (g-type-is-a "GtkEntryBuffer" "gboolean"))
  (is-false (g-type-is-a "GtkEntryBuffer" "GtkWindow"))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkEntryBuffer"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkEntryBuffer"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkEntryBuffer" query)
    (is (equal (gtype "GtkEntryBuffer")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkEntryBuffer"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 124 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  16 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("text" "length" "max-length")
             (mapcar #'param-spec-name (g-object-class-list-properties "GtkEntryBuffer"))))

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEntryBuffer" GTK-ENTRY-BUFFER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_entry_buffer_get_type")
                       ((LENGTH GTK-ENTRY-BUFFER-LENGTH "length" "guint" T NIL)
                        (MAX-LENGTH GTK-ENTRY-BUFFER-MAX-LENGTH "max-length"
                         "gint" T T)
                        (TEXT GTK-ENTRY-BUFFER-TEXT "text" "gchararray" T T)))
             (get-g-type-definition "GtkEntryBuffer"))))

(test gtk-entry-buffer-properties
  (let ((object (make-instance 'gtk-entry-buffer :text "text")))
    (is (= 4 (gtk-entry-buffer-length object)))
    (is (= 0 (gtk-entry-buffer-max-length object)))
    (is (equal "text" (gtk-entry-buffer-text object)))))

;;;   gtk_entry_buffer_new

(test gtk-entry-buffer-new.1
  (let ((buffer (gtk-entry-buffer-new)))
    (is (= 0 (gtk-entry-buffer-length buffer)))
    (is (= 0 (gtk-entry-buffer-max-length buffer)))
    (is (equal "" (gtk-entry-buffer-text buffer)))))

(test gtk-entry-buffer-new.2
  (let ((buffer (gtk-entry-buffer-new nil)))
    (is (= 0 (gtk-entry-buffer-length buffer)))
    (is (= 0 (gtk-entry-buffer-max-length buffer)))
    (is (equal "" (gtk-entry-buffer-text buffer)))))

(test gtk-entry-buffer-new.3
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (= 4 (gtk-entry-buffer-length buffer)))
    (is (= 0 (gtk-entry-buffer-max-length buffer)))
    (is (equal "text" (gtk-entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_get_text

(test gtk-entry-buffer-get-text
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (equal "text" (gtk-entry-buffer-get-text buffer)))))

;;;   gtk_entry_buffer_set_text

(test gtk-entry-buffer-get-text
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (equal "text" (gtk-entry-buffer-get-text buffer)))
    (gtk-entry-buffer-set-text buffer "new text")
    (is (equal "new text" (gtk-entry-buffer-get-text buffer)))
    (is (= 8 (gtk-entry-buffer-length buffer)))))

;;;   gtk_entry_buffer_get_bytes

(test gtk-entry-buffer-get-bytes
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (equal "text" (gtk-entry-buffer-get-text buffer)))
    (is (= 4 (gtk-entry-buffer-get-bytes buffer)))
    (gtk-entry-buffer-set-text buffer "Äpfel")
    (is (equal "Äpfel" (gtk-entry-buffer-get-text buffer)))
    (is (= 6 (gtk-entry-buffer-get-bytes buffer)))))

;;;   gtk_entry_buffer_get_length

(test gtk-entry-buffer-get-length
  (let ((buffer (gtk-entry-buffer-new "Äpfel")))
    (is (= 5 (gtk-entry-buffer-get-length buffer)))))

;;;   gtk_entry_buffer_get_max_length

(test gtk-entry-buffer-get-max-length
  (let ((buffer (gtk-entry-buffer-new "This is a text.")))
    (is (= 0 (gtk-entry-buffer-get-max-length buffer)))))

;;;   gtk_entry_buffer_set_max_length

(test gtk-entry-buffer-set-max-length
  (let ((buffer (gtk-entry-buffer-new "This is a text.")))
    (gtk-entry-buffer-set-max-length buffer 9)
    (is (= 9 (gtk-entry-buffer-get-max-length buffer)))
    (is (equal "This is a" (gtk-entry-buffer-get-text buffer)))))

;;;  gtk_entry_buffer_insert_text

(test gtk-entry-buffer-insert-text
  (let ((buffer (gtk-entry-buffer-new)))
    (is (= 6 (gtk-entry-buffer-insert-text buffer 0 "first ")))
    (is (equal "first " (gtk-entry-buffer-get-text buffer)))
    (is (= 5 (gtk-entry-buffer-insert-text buffer 6 "third")))
    (is (equal "first third" (gtk-entry-buffer-get-text buffer)))
    (is (= 7 (gtk-entry-buffer-insert-text buffer 6 "second ")))
    (is (equal "first second third" (gtk-entry-buffer-get-text buffer)))
    (gtk-entry-buffer-set-max-length buffer 27)
    (is (= 9 (gtk-entry-buffer-insert-text buffer 6 "and than a ")))
    (is (equal "first and than second third" (gtk-entry-buffer-get-text buffer)))))

;;;   gtk_entry_buffer_delete_text

(test gtk-entry-buffer-delete-text
  (let ((buffer (gtk-entry-buffer-new "first second third")))
    (is (= 7 (gtk-entry-buffer-delete-text buffer 6 7)))
    (is (equal "first third" (gtk-entry-buffer-get-text buffer)))
    (is (= 6 (gtk-entry-buffer-delete-text buffer 5 -1)))
    (is (equal "first" (gtk-entry-buffer-get-text buffer)))))

;;;   gtk_entry_buffer_emit_deleted_text

(test gtk-entry-buffer-emit-deleted-text
  (let ((buffer (gtk-entry-buffer-new "first second third")))
    (g-signal-connect buffer "deleted-text"
       (lambda (object position n-chars)
         (is (eq buffer object))
         (is (= 6 position))
         (is (= 7 n-chars))
         t))
    (gtk-entry-buffer-emit-deleted-text buffer 6 7)))

;;;   gtk_entry_buffer_emit_inserted_text

(test gtk-entry-buffer-emit-inserted-text
  (let ((buffer (gtk-entry-buffer-new "first second third")))
    (g-signal-connect buffer "inserted-text"
       (lambda (object position text n-chars)
         (is (eq buffer object))
         (is (= 6 position))
         (is (equal "text" text))
         (is (= 7 n-chars))
         t))
    (gtk-entry-buffer-emit-inserted-text buffer 6 "text" 7)))

