(def-suite gtk-text-iter :in gtk-suite)
(in-suite gtk-text-iter)

(defvar *gtk-text-iter-sample-text*
  "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien
leben die Blindtexte. Abgeschieden wohnen Sie in Buchstabenhausen an der Küste
des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden
fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein
paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen.
Nicht einmal von der allmächtigen Interpunktion werden die Blindtexte beherrscht
– ein geradezu unorthographisches Leben.

Eines Tages aber beschloss eine kleine Zeile Blindtext, ihr Name war Lorem
Ipsum, hinaus zu gehen in die weite Grammatik. Der große Oxmox riet ihr davon
ab, da es dort wimmele von bösen Kommata, wilden Fragezeichen und hinterhältigen
Semikola, doch das Blindtextchen ließ sich nicht beirren. Es packte seine sieben
Versalien, schob sich sein Initial in den Gürtel und machte sich auf den Weg.

Als es die ersten Hügel des Kursivgebirges erklommen hatte, warf es einen
letzten Blick zurück auf die Skyline seiner Heimatstadt Buchstabenhausen, die
Headline von Alphabetdorf und die Subline seiner eigenen Straße, der
Zeilengasse. Wehmütig lief ihm eine rhetorische Frage über die Wange, dann
setzte es seinen Weg fort.

Unterwegs traf es eine Copy. Die Copy warnte das Blindtextchen, da, wo sie
herkäme, wäre sie zigmal umgeschrieben worden und alles, was von ihrem Ursprung
noch übrig wäre, sei das Wort »und« und das Blindtextchen solle umkehren und
wieder in sein eigenes, sicheres Land zurückkehren.

Doch alles Gutzureden konnte es nicht überzeugen und so dauerte es nicht lange,
bis ihm ein paar heimtückische Werbetexter auflauerten, es mit Longe und Parole
betrunken machten und es dann in ihre Agentur schleppten, wo sie es für ihre
Projekte wieder und wieder missbrauchten. Und wenn es nicht umgeschrieben wurde,
dann benutzen Sie es immer noch.")

;;;   GtkTextIter

(test gtk-text-iter-boxed
  (is-true (g-type-is-a (gtype "GtkTextIter") +g-type-boxed+))
  (is-true (eq 'gtk-text-iter (type-of (make-instance 'gtk-text-iter))))

  (is (eq 'gobject::g-boxed-opaque-wrapper-info
          (type-of (gobject::get-g-boxed-foreign-info 'gtk-text-iter))))
  (is (eq 'gobject::g-boxed-opaque-wrapper-info
          (type-of (gobject::get-g-boxed-foreign-info-for-gtype (gtype "GtkTextIter")))))
)


(test gtk-text-iter-boxed.2
  (let* ((buffer (make-instance 'gtk-text-buffer :text "text")))

    (dotimes (i 1000000)
      (gtk-text-buffer-get-start-iter buffer))
))


;;;     gtk_text_iter_get_buffer

(test gtk-text-iter-buffer
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-start-iter buffer)))
    (is (eq buffer (gtk-text-iter-buffer iter)))))

;;;     gtk_text_iter_copy
;;;     gtk_text_iter_assign
;;;     gtk_text_iter_free

;;;     gtk_text_iter_get_offset
;;;     gtk_text_iter_set_offset

(test gtk-text-iter-offset
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-offset iter)))
    (is (eq #\t (gtk-text-iter-char iter)))
    (is (= 17 (setf (gtk-text-iter-offset iter) 17)))
    (is (eq #\f (gtk-text-iter-char iter)))))

;;;     gtk_text_iter_get_line
;;;     gtk_text_iter_set_line

(test gtk-text-iter-line
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text *gtk-text-iter-sample-text*))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 100)))
    (is (= 1 (gtk-text-iter-line iter)))
    (is (eq #\A (gtk-text-iter-char iter)))
    (is (= 5 (setf (gtk-text-iter-line iter) 5)))
    (is (eq #\N (gtk-text-iter-char iter)))))

;;;     gtk_text_iter_get_line_offset
;;;     gtk_text_iter_set_line_offset

(test gtk-text-iter-line-offset
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-line-offset iter)))))

;;;  gtk_text_iter_get_line_index

(test gtk-text-iter-line-index
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-line-index iter)))))

;;;   gtk_text_iter_get_visible_line_index

(test gtk-text-iter-visible-line-index
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-visible-line-index iter)))))

;;;   gtk_text_iter_get_visible_line_offset

(test gtk-text-iter-visible-line-offset
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (= 12 (gtk-text-iter-visible-line-offset iter)))))

;;;   gtk_text_iter_get_char

(test gtk-text-iter-char
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (is (eql #\t (gtk-text-iter-char iter)))))

;;;   gtk_text_iter_get_slice

(test gtk-text-iter-slice
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-slice start end)))))

;;;   gtk_text_iter_get_text

(test gtk-text-iter-text
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-text start end)))))

;;;   gtk_text_iter_get_visible_slice

(test gtk-text-iter-visible-slice
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-visible-slice start end)))))

;;;   gtk_text_iter_get_visible_text

(test gtk-text-iter-visible-text
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (is (equal "text" (gtk-text-iter-visible-text start end)))))

;;;   gtk_text_iter_get_pixbuf

(test gtk-text-iter-pixbuf
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (gtk-text-buffer-insert-pixbuf buffer iter (make-instance 'gdk-pixbuf))
    (let ((iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
      (is (eq 'gdk-pixbuf (type-of (gtk-text-iter-pixbuf iter)))))))

;;;   gtk_text_iter_get_marks

(test gtk-text-iter-marks
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (gtk-text-buffer-add-mark buffer (gtk-text-mark-new nil t) iter)
    (is (eq 'gtk-text-mark
            (type-of (first (gtk-text-iter-marks iter)))))))

;;;   gtk_text_iter_get_toggled_tags

(test gtk-text-iter-toggled-tags
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (start (gtk-text-buffer-get-iter-at-offset buffer 12))
         (end (gtk-text-buffer-get-iter-at-offset buffer 16)))
    (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer)
                             (make-instance 'gtk-text-tag
                                            :name "bold"
                                            :weight 700))
    (gtk-text-buffer-apply-tag-by-name buffer "bold" start end)
    (is (eq 'gtk-text-tag
            (type-of (first (gtk-text-iter-toggled-tags start t)))))))

;;;   gtk_text_iter_get_child_anchor

#+nil
(test gtk-text-iter-child-anchor
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text
                                "Some sample text for the text buffer."))
         (iter (gtk-text-buffer-get-iter-at-offset buffer 12)))
    (gtk-text-buffer-create-child-anchor buffer iter)
    (is (eq 'gtk-text-tag
            (type-of (gtk-text-iter-child-anchor iter))))))


;;;     gtk_text_iter_begins_tag
;;;     gtk_text_iter_ends_tag
;;;     gtk_text_iter_toggles_tag
;;;     gtk_text_iter_has_tag
;;;     gtk_text_iter_get_tags
;;;     gtk_text_iter_editable
;;;     gtk_text_iter_can_insert
;;;     gtk_text_iter_starts_word
;;;     gtk_text_iter_ends_word
;;;     gtk_text_iter_inside_word
;;;     gtk_text_iter_starts_line
;;;     gtk_text_iter_ends_line
;;;     gtk_text_iter_starts_sentence
;;;     gtk_text_iter_ends_sentence
;;;     gtk_text_iter_inside_sentence
;;;     gtk_text_iter_is_cursor_position
;;;     gtk_text_iter_get_chars_in_line
;;;     gtk_text_iter_get_bytes_in_line

;;;     gtk_text_iter_get_attributes

(test gtk-text-iter-attributes
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text "Some sample text for the text buffer."))
         (view (gtk-text-view-new-with-buffer buffer))
         (attributes (gtk-text-view-default-attributes view))
         (iter (gtk-text-buffer-get-start-iter buffer)))

    (is (eq 'gtk-text-buffer (type-of buffer)))
    (is (eq 'gtk-text-view (type-of view)))
    (is (eq 'gtk-text-attributes (type-of attributes)))

    (is-false (gtk-text-iter-attributes iter attributes))
;    (is-false (gtk-text-iter-attributes iter (null-pointer)))

))

;;;     gtk_text_iter_get_language

(test gtk-text-iter-language
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text "Some sample text for the text buffer."))
         (view (gtk-text-view-new-with-buffer buffer))
         (attributes (gtk-text-view-default-attributes view))
         (iter (gtk-text-buffer-get-start-iter buffer)))

    (is (eq 'gtk-text-buffer (type-of buffer)))
    (is (eq 'gtk-text-view (type-of view)))
    (is (eq 'gtk-text-attributes (type-of attributes)))

    (is (eq 'pango-language (type-of (gtk-text-iter-language iter))))

))

;;;     gtk_text_iter_is_end
;;;     gtk_text_iter_is_start
;;;     gtk_text_iter_forward_char
;;;     gtk_text_iter_backward_char
;;;     gtk_text_iter_forward_chars
;;;     gtk_text_iter_backward_chars
;;;     gtk_text_iter_forward_line
;;;     gtk_text_iter_backward_line
;;;     gtk_text_iter_forward_lines
;;;     gtk_text_iter_backward_lines
;;;     gtk_text_iter_forward_word_ends
;;;     gtk_text_iter_backward_word_starts
;;;     gtk_text_iter_forward_word_end
;;;     gtk_text_iter_backward_word_start
;;;     gtk_text_iter_forward_cursor_position
;;;     gtk_text_iter_backward_cursor_position
;;;     gtk_text_iter_forward_cursor_positions
;;;     gtk_text_iter_backward_cursor_positions
;;;     gtk_text_iter_backward_sentence_start
;;;     gtk_text_iter_backward_sentence_starts
;;;     gtk_text_iter_forward_sentence_end
;;;     gtk_text_iter_forward_sentence_ends
;;;     gtk_text_iter_forward_visible_word_ends
;;;     gtk_text_iter_backward_visible_word_starts
;;;     gtk_text_iter_forward_visible_word_end
;;;     gtk_text_iter_backward_visible_word_start
;;;     gtk_text_iter_forward_visible_cursor_position
;;;     gtk_text_iter_backward_visible_cursor_position
;;;     gtk_text_iter_forward_visible_cursor_positions
;;;     gtk_text_iter_backward_visible_cursor_positions
;;;     gtk_text_iter_forward_visible_line
;;;     gtk_text_iter_backward_visible_line
;;;     gtk_text_iter_forward_visible_lines
;;;     gtk_text_iter_backward_visible_lines
;;;     gtk_text_iter_set_line_index
;;;     gtk_text_iter_set_visible_line_index
;;;     gtk_text_iter_set_visible_line_offset
;;;     gtk_text_iter_forward_to_end
;;;     gtk_text_iter_forward_to_line_end
;;;     gtk_text_iter_forward_to_tag_toggle
;;;     gtk_text_iter_backward_to_tag_toggle
;;;     gtk_text_iter_forward_find_char
;;;     gtk_text_iter_backward_find_char
;;;
;;;     GtkTextSearchFlags
;;;
;;;     gtk_text_iter_forward_search
;;;     gtk_text_iter_backward_search
;;;     gtk_text_iter_equal
;;;     gtk_text_iter_compare
;;;     gtk_text_iter_in_range
;;;     gtk_text_iter_order

