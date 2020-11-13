(def-suite gtk-entry-buffer :in gtk-suite)
(in-suite gtk-entry-buffer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryBuffer

(test gtk-entry-buffer-class
  ;; Type check
  (is (g-type-is-object "GtkEntryBuffer"))
  ;; Check the registered name
  (is (eq 'gtk-entry-buffer
          (registered-object-type-by-name "GtkEntryBuffer")))
  ;; Check the type initializer
  (is (eq (gtype "GtkEntryBuffer")
          (gtype (foreign-funcall "gtk_entry_buffer_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GtkEntryBuffer")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkEntryBuffer"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkEntryBuffer"))))
  ;; Check the class properties
  (is (equal '("length" "max-length" "text")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkEntryBuffer"))
                          #'string-lessp)))
  ;; Check the class definition
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

(test gtk-entry-buffer-text
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (equal "text" (gtk-entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_set_text

(test gtk-entry-buffer-text
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (equal "text" (gtk-entry-buffer-text buffer)))
    (setf (gtk-entry-buffer-text buffer) "new text")
    (is (equal "new text" (gtk-entry-buffer-text buffer)))
    (is (= 8 (gtk-entry-buffer-length buffer)))))

;;;   gtk_entry_buffer_get_bytes

(test gtk-entry-buffer-bytes
  (let ((buffer (gtk-entry-buffer-new "text")))
    (is (equal "text" (gtk-entry-buffer-text buffer)))
    (is (= 4 (gtk-entry-buffer-bytes buffer)))
    (setf (gtk-entry-buffer-text buffer) "Äpfel")
    (is (equal "Äpfel" (gtk-entry-buffer-text buffer)))
    (is (= 6 (gtk-entry-buffer-bytes buffer)))))

;;;   gtk_entry_buffer_get_length

(test gtk-entry-buffer-length
  (let ((buffer (gtk-entry-buffer-new "Äpfel")))
    (is (= 5 (gtk-entry-buffer-length buffer)))))

;;;   gtk_entry_buffer_get_max_length

(test gtk-entry-buffer-max-length
  (let ((buffer (gtk-entry-buffer-new "This is a text.")))
    (is (= 0 (gtk-entry-buffer-max-length buffer)))))

;;;   gtk_entry_buffer_set_max_length

(test gtk-entry-buffer-max-length
  (let ((buffer (gtk-entry-buffer-new "This is a text.")))
    (setf (gtk-entry-buffer-max-length buffer) 9)
    (is (= 9 (gtk-entry-buffer-max-length buffer)))
    (is (equal "This is a" (gtk-entry-buffer-text buffer)))))

;;;  gtk_entry_buffer_insert_text

(test gtk-entry-buffer-insert-text
  (let ((buffer (gtk-entry-buffer-new)))
    (is (= 6 (gtk-entry-buffer-insert-text buffer 0 "first ")))
    (is (equal "first " (gtk-entry-buffer-text buffer)))
    (is (= 5 (gtk-entry-buffer-insert-text buffer 6 "third")))
    (is (equal "first third" (gtk-entry-buffer-text buffer)))
    (is (= 7 (gtk-entry-buffer-insert-text buffer 6 "second ")))
    (is (equal "first second third" (gtk-entry-buffer-text buffer)))
    (setf (gtk-entry-buffer-max-length buffer) 27)
    (is (= 9 (gtk-entry-buffer-insert-text buffer 6 "and than a ")))
    (is (equal "first and than second third" (gtk-entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_delete_text

(test gtk-entry-buffer-delete-text
  (let ((buffer (gtk-entry-buffer-new "first second third")))
    (is (= 7 (gtk-entry-buffer-delete-text buffer 6 7)))
    (is (equal "first third" (gtk-entry-buffer-text buffer)))
    (is (= 6 (gtk-entry-buffer-delete-text buffer 5 -1)))
    (is (equal "first" (gtk-entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_emit_deleted_text

#+nil
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

#+nil
(test gtk-entry-buffer-emit-inserted-text
  (let ((buffer (gtk-entry-buffer-new "first second third")))
    (g-signal-connect buffer "inserted-text"
       (lambda (object position text n-chars)
;         (format t "~&in Signal 'inserted-text' for ~a~%" object)
         (is (eq buffer object))
         (is (= 6 position))
         (is (equal "text" text))
         (is (= 7 n-chars))
         t))
;    (format t "~&buffer = ~A~%" buffer)
    (gtk-entry-buffer-emit-inserted-text buffer 6 "text" 7)
)
)

