(def-suite glib-misc :in glib-suite)
(in-suite glib-misc)

;;;   g_size

(test g-size
  (is (eq :unsigned-long (cffi::canonicalize-foreign-type 'g-size)))
  (is (= 8 (foreign-type-size 'g-size))))

;;;   g_ssize
  
(test g-ssize
  (is (eq :long (cffi::canonicalize-foreign-type 'g-ssize)))
  (is (= 8 (foreign-type-size 'g-ssize))))

;;;   g_offset
  
(test g-offset
  (is (eq :unsigned-long (cffi::canonicalize-foreign-type 'g-offset)))
  (is (= 8 (foreign-type-size 'g-offset))))

;;;   g_malloc
  
(test g-malloc
  (let ((mem nil))
    (is-true (pointerp (setq mem (g-malloc 10))))
    (g-free mem)
    (is-true (null-pointer-p (g-malloc 0)))))

;;;   GTimeVal

(test g-time-val
  (is (equal '(:struct g-time-val)
             (cffi::canonicalize-foreign-type '(:struct g-time-val))))
  (with-foreign-object (ptr '(:struct g-time-val))
    ;; Write values into the slots.
    ;; The CStruct is exported, but not the slots.
    (setf (foreign-slot-value ptr '(:struct g-time-val) 'glib::tv-sec) 412776
          (foreign-slot-value ptr '(:struct g-time-val) 'glib::tv-usec) 132423)
    ;; Read the slots.
    (with-foreign-slots ((glib::tv-sec glib::tv-usec) ptr (:struct g-time-val))
      (is (equal (list 412776 132423)
                 (list glib::tv-sec glib::tv-usec))))))

;;;   g_get_current_time

(test g-get-current-time
  (multiple-value-bind (seconds microseconds)
      (g-get-current-time)
    (is-true (integerp seconds))
    (is-true (integerp microseconds))))

;;;   g_get_monotonic_time

(test g-get-monotonic-time
  (is-true (integerp (g-get-monotonic-time))))

;;;   g_get_real_time

(test g-get-real-time
  (is-true (integerp (g-get-real-time))))

;;;   GString

(test g-string
  (is (eq :pointer (cffi::canonicalize-foreign-type 'g-string)))
  (let ((ptr (convert-to-foreign "Hello" 'g-string)))
    (is-true (pointerp ptr))
    (is (equal "Hello" (convert-from-foreign ptr 'g-string)))))

;;;   GStrv

(test g-strv
  (is (eq :pointer (cffi::canonicalize-foreign-type 'g-strv)))
  (let ((ptr (convert-to-foreign (list "Hello" "World") 'g-strv)))
    (is-true (pointerp ptr))
    (is (equal '("Hello" "World") (convert-from-foreign ptr 'g-strv)))))

;;;   GList

(test g-list
  (is (equal '(a b c) (translate-to-foreign '(a b c) 'g-list-type))))

;;;   GSList

(test g-slist
  (is (equal '(a b c) (translate-to-foreign '(a b c) 'g-slist-type))))


