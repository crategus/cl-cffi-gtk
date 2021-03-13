(def-suite gtk-text-attributes :in gtk-suite)
(in-suite gtk-text-attributes)

;;;     GtkWrapMode                                     <--- gtk.text-view.lisp

(test gtk-wrap-mode
  ;; Check the type
  (is (g-type-is-enum "GtkWrapMode"))
  ;; Check the type initializer
  (is (eq (gtype "GtkWrapMode")
          (gtype (foreign-funcall "gtk_wrap_mode_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-wrap-mode (registered-enum-type "GtkWrapMode")))
  ;; Check the names
  (is (equal '("GTK_WRAP_NONE" "GTK_WRAP_CHAR" "GTK_WRAP_WORD"
               "GTK_WRAP_WORD_CHAR")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkWrapMode"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkWrapMode"))))
  ;; Check the nick names
  (is (equal '("none" "char" "word" "word-char")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkWrapMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkWrapMode"
                             GTK-WRAP-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_wrap_mode_get_type")
                             (:NONE 0)
                             (:CHAR 1)
                             (:WORD 2)
                             (:WORD-CHAR 3))
             (get-g-type-definition "GtkWrapMode"))))

;;;     GtkTextAppearance                               <--- gtk.text-tag.lisp

;;;     GtkTextAttributes                               <--- gtk.text-tag.lisp

(test gtk-text-attributes-struct
  ;; Type check
  (is (g-type-is-a (gtype "GtkTextAttributes") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GtkTextAttributes")
          (gtype (foreign-funcall "gtk_text_attributes_get_type" g-size)))))

(test gtk-text-attributes-slots
  (let* ((buffer (make-instance 'gtk-text-buffer
                                :text "Some sample text for the text buffer."))
         (view (gtk-text-view-new-with-buffer buffer))
         (attr (gtk-text-view-default-attributes view)))

    (is (typep buffer 'gtk-text-buffer))
    (is (typep view 'gtk-text-view))
    (is (typep attr 'gtk-text-attributes))

    (is (pointerp (gtk-text-attributes-appearance attr)))
    (is (eq :dummy2 (gtk-text-attributes-justification attr)))
    (is (eq :none (gtk-text-attributes-direction attr)))
    (is (typep (gtk-text-attributes-font attr) 'pango-font-description))
    (is (= 0.0d0 (gtk-text-attributes-font-scale attr)))
    (is (= 0 (gtk-text-attributes-left-margin attr)))
    (is (= 0 (gtk-text-attributes-right-margin attr)))
    (is (= 0 (gtk-text-attributes-indent attr)))
    (is (= 0 (gtk-text-attributes-pixels-above-lines attr)))
    (is (= 0 (gtk-text-attributes-pixels-below-lines attr)))
    (is (= 1 (gtk-text-attributes-pixels-inside-wrap attr)))
    (is (pointerp (gtk-text-attributes-tabs attr)))
    (is (eq :none (gtk-text-attributes-wrap-mode attr)))
    (is (typep (gtk-text-attributes-language attr) 'pango-language))
    (is-false (gtk-text-attributes-invisible attr))
    (is-false (gtk-text-attributes-bg-full-height attr))
    (is-false (gtk-text-attributes-editable attr))
    (is-false (gtk-text-attributes-no-fallback attr))
    (is (= 0 (gtk-text-attributes-letter-spacing attr)))))

;;;     gtk-text-attributes-new

(test gtk-text-attributes-new.1
  (is (typep (gtk-text-attributes-new) 'gtk-text-attributes)))

#+nil
(test gtk-text-attributes-new.2
  (let ((attr (gtk-text-attributes-new)))
;    (is (= 1 (gtk-text-attributes-refcount attr)))
    (is (pointerp (gtk-text-attributes-appearance attr)))
    (is (eq :dummy2 (gtk-text-attributes-justification attr)))
    (is (eq :none (gtk-text-attributes-direction attr)))
    (is (typep (gtk-text-attributes-font attr) 'pango-font-description))
    (is (= 0.0d0 (gtk-text-attributes-font-scale attr)))
    (is (= 0 (gtk-text-attributes-left-margin attr)))
    (is (= 0 (gtk-text-attributes-right-margin attr)))
    (is (= 0 (gtk-text-attributes-indent attr)))
    (is (= 0 (gtk-text-attributes-pixels-above-lines attr)))
    (is (= 0 (gtk-text-attributes-pixels-below-lines attr)))
    (is (= 1 (gtk-text-attributes-pixels-inside-wrap attr)))
    (is (pointerp (gtk-text-attributes-tabs attr)))
    (is (eq :none (gtk-text-attributes-wrap-mode attr)))
    (is (typep (gtk-text-attributes-language attr) 'pango-language))
    (is-false (gtk-text-attributes-invisible attr))
    (is-false (gtk-text-attributes-bg-full-height attr))
    (is-false (gtk-text-attributes-editable attr))
    (is-false (gtk-text-attributes-no-fallback attr))
    (is (= 0 (gtk-text-attributes-letter-spacing attr)))))

;;;     gtk-text-attributes-copy

(test gtk-text-attributes-copy
  (let ((attr (gtk-text-attributes-new)))
    (is (typep (gtk-text-attributes-copy attr) 'gtk-text-attributes))))

;;;     gtk-text-attributes-values

#+nil
(test gtk-text-attributes-copy-values
  (let ((dest (gtk-text-attributes-new))
        (src (gtk-text-attributes-new)))
    (is-false (gtk-text-attributes-copy-values src dest))))

;;; 2021-2-8
