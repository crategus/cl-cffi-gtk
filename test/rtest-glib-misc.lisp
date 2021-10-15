(def-suite glib-misc :in glib-suite)
(in-suite glib-misc)

;;;   g_size

(test g-size
  #-windows
  (is (eq :unsigned-long (cffi::canonicalize-foreign-type 'g-size)))
  #+windows 
  (is (eq :unsigned-long-long (cffi::canonicalize-foreign-type 'g-size)))
  (is (= 8 (foreign-type-size 'g-size))))

;;;   g_ssize
  
(test g-ssize
  (is (eq :long (cffi::canonicalize-foreign-type 'g-ssize)))
  #-windows
  (is (= 8 (foreign-type-size 'g-ssize)))
  #+windows
  (is (= 4 (foreign-type-size 'g-ssize)))) 

;;;   g_offset
  
(test g-offset
  #-windows
  (is (eq :unsigned-long (cffi::canonicalize-foreign-type 'g-offset)))
  #+windows
  (is (eq :unsigned-long-long (cffi::canonicalize-foreign-type 'g-offset)))
  (is (= 8 (foreign-type-size 'g-offset))))

;;;   g_malloc
  
(test g-malloc
  (let ((mem nil))
    (is-true (pointerp (setq mem (g-malloc 10))))
    (g-free mem)
    (is-true (null-pointer-p (g-malloc 0)))))

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

;;; 2021-10-14
