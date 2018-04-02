(in-package :gtk-testsuite)

(def-suite gtk-text-buffer :in gtk-suite)
(in-suite gtk-text-buffer)

(defvar *sample-text-1*
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


(defvar *sample-text-2*
  "Dies ist ein Typoblindtext.
An ihm kann man sehen, ob alle Buchstaben da sind und wie sie aussehen. Manchmal
benutzt man Worte wie Hamburgefonts, Rafgenduks oder Handgloves, um Schriften zu
testen. Manchmal Sätze, die alle Buchstaben des Alphabets enthalten – man nennt
diese Sätze »Pangramme«. Sehr bekannt ist dieser: The quick brown fox jumps over
the lazy dog. Oft werden in Typoblindtexte auch fremdsprachige Satzteile
eingebaut (AVAIL® and Wefox™ are testing aussi la Kerning), um die Wirkung in
anderen Sprachen zu testen. In Lateinisch sieht zum Beispiel fast jede Schrift
gut aus. Quod erat demonstrandum.

Seit 1975 fehlen in den meisten Testtexten die Zahlen, weswegen nach TypoGb.
204 § ab dem Jahr 2034 Zahlen in 86 der Texte zur Pflicht werden.
Nichteinhaltung wird mit bis zu 245 € oder 368 $ bestraft. Genauso wichtig in
sind mittlerweile auch Â ç c è ñ t ë , die in neueren Schriften aber fast immer
enthalten sind. Ein wichtiges, aber schwierig zu integrierendes Feld sind
OpenType-Funktionalitäten. Je nach Software und Voreinstellungen können
eingebaute Kapitälchen, Kerning oder Ligaturen (sehr pfi ffi g) nicht richtig
dargestellt werden.")

;;;   GtkTextBuffer

;;;   gtk_text_buffer_new

(test gtk-text-buffer-new.1
  (is (eq 'gtk-text-buffer (type-of (gtk-text-buffer-new))))
  (is (eq 'gtk-text-tag-table
          (type-of (gtk-text-buffer-get-tag-table (gtk-text-buffer-new))))))

(test gtk-text-buffer-new.2
  (is (eq 'gtk-text-buffer (type-of (gtk-text-buffer-new nil))))
  (is (eq 'gtk-text-tag-table
          (type-of (gtk-text-buffer-get-tag-table (gtk-text-buffer-new nil))))))

(test gtk-text-buffer-new.3
  (let ((tag-table (gtk-text-tag-table-new)))
    (is (eq 'gtk-text-buffer (type-of (gtk-text-buffer-new tag-table))))
    (is (eq tag-table
            (gtk-text-buffer-get-tag-table (gtk-text-buffer-new tag-table))))))

;;;   gtk_text_buffer_get_line_count

(test gtk-text-buffer-get-line-count.1
  (let ((buffer (gtk-text-buffer-new)))
    (gtk-text-buffer-set-text buffer *sample-text-1*)
    (is (= 30 (gtk-text-buffer-get-line-count buffer)))))

(test gtk-text-buffer-get-line-count.2
  (let ((buffer (gtk-text-buffer-new)))
    (gtk-text-buffer-set-text buffer *sample-text-2*)
    (is (= 18 (gtk-text-buffer-get-line-count buffer)))))

;;;   gtk_text_buffer_get_char_count

(test gtk-text-buffer-get-char-count.1
  (let ((buffer (gtk-text-buffer-new)))
    (gtk-text-buffer-set-text buffer *sample-text-1*)
    (is (= 1867 (gtk-text-buffer-get-char-count buffer)))))

(test gtk-text-buffer-get-char-count.2
  (let ((buffer (gtk-text-buffer-new)))
    (gtk-text-buffer-set-text buffer *sample-text-2*)
    (is (= 1160 (gtk-text-buffer-get-char-count buffer)))))

;;;   gtk_text_buffer_get_tag_table

(test gtk-text-buffer-get-tab-table
  (let ((tag-table (gtk-text-tag-table-new)))
    (is (eq tag-table
            (gtk-text-buffer-get-tag-table (gtk-text-buffer-new tag-table))))))

;;;     gtk_text_buffer_insert
;;;     gtk_text_buffer_insert_at_cursor
;;;     gtk_text_buffer_insert_interactive
;;;     gtk_text_buffer_insert_interactive_at_cursor
;;;     gtk_text_buffer_insert_range
;;;     gtk_text_buffer_insert_range_interactive
;;;     gtk_text_buffer_insert_with_tags
;;;     gtk_text_buffer_insert_with_tags_by_name
;;;     gtk_text_buffer_delete
;;;     gtk_text_buffer_delete_interactive
;;;     gtk_text_buffer_backspace

;;;   gtk_text_buffer_set_text

(test gtk-text-buffer-set-text.1
  (let ((buffer (gtk-text-buffer-new)))
    (gtk-text-buffer-set-text buffer *sample-text-1*)
    (is (equal *sample-text-1* (gtk-text-buffer-text buffer)))))

(test gtk-text-buffer-set-text.2
  (let ((buffer (gtk-text-buffer-new)))
    (gtk-text-buffer-set-text buffer *sample-text-2*)
    (is (equal *sample-text-2* (gtk-text-buffer-text buffer)))))

;;;     gtk_text_buffer_get_text
;;;     gtk_text_buffer_get_slice
;;;     gtk_text_buffer_insert_pixbuf
;;;     gtk_text_buffer_insert_child_anchor
;;;     gtk_text_buffer_create_child_anchor
;;;     gtk_text_buffer_create_mark
;;;     gtk_text_buffer_move_mark
;;;     gtk_text_buffer_move_mark_by_name
;;;     gtk_text_buffer_add_mark
;;;     gtk_text_buffer_delete_mark
;;;     gtk_text_buffer_delete_mark_by_name
;;;     gtk_text_buffer_get_mark
;;;     gtk_text_buffer_get_insert
;;;     gtk_text_buffer_get_selection_bound
;;;     gtk_text_buffer_get_has_selection
;;;     gtk_text_buffer_place_cursor
;;;     gtk_text_buffer_select_range
;;;     gtk_text_buffer_apply_tag
;;;     gtk_text_buffer_remove_tag
;;;     gtk_text_buffer_apply_tag_by_name
;;;     gtk_text_buffer_remove_tag_by_name
;;;     gtk_text_buffer_remove_all_tags
;;;     gtk_text_buffer_create_tag
;;;     gtk_text_buffer_get_iter_at_line_offset
;;;     gtk_text_buffer_get_iter_at_offset
;;;     gtk_text_buffer_get_iter_at_line
;;;     gtk_text_buffer_get_iter_at_line_index
;;;     gtk_text_buffer_get_iter_at_mark
;;;     gtk_text_buffer_get_iter_at_child_anchor
;;;     gtk_text_buffer_get_start_iter
;;;     gtk_text_buffer_get_end_iter
;;;     gtk_text_buffer_get_bounds
;;;     gtk_text_buffer_get_modified
;;;     gtk_text_buffer_set_modified
;;;     gtk_text_buffer_delete_selection
;;;     gtk_text_buffer_paste_clipboard
;;;     gtk_text_buffer_copy_clipboard
;;;     gtk_text_buffer_cut_clipboard
;;;     gtk_text_buffer_get_selection_bounds
;;;     gtk_text_buffer_begin_user_action
;;;     gtk_text_buffer_end_user_action
;;;     gtk_text_buffer_add_selection_clipboard
;;;     gtk_text_buffer_remove_selection_clipboard
;;;
;;;     GtkTextBufferTargetInfo
;;;
;;;     gtk_text_buffer_deserialize
;;;     gtk_text_buffer_deserialize_get_can_create_tags
;;;     gtk_text_buffer_deserialize_set_can_create_tags
;;;     gtk_text_buffer_get_copy_target_list
;;;     gtk_text_buffer_get_deserialize_formats
;;;     gtk_text_buffer_get_paste_target_list
;;;     gtk_text_buffer_get_serialize_formats
;;;     gtk_text_buffer_register_deserialize_format
;;;     gtk_text_buffer_register_deserialize_tagset
;;;     gtk_text_buffer_register_serialize_format
;;;     gtk_text_buffer_register_serialize_tagset
;;;     gtk_text_buffer_serialize
;;;     gtk_text_buffer_unregister_deserialize_format
;;;     gtk_text_buffer_unregister_serialize_format

