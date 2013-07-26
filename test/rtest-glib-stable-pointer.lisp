
(def-suite glib-stable-pointer :in glib-suite)
(in-suite glib-stable-pointer)

(test glib-stable-pointer
  (let* ((func (lambda() 88))
         (ptr (glib::allocate-stable-pointer func)))
    (is (= 88 (funcall (glib::get-stable-pointer-value ptr))))
    (is-false (glib::free-stable-pointer ptr))
    (is-false (glib::get-stable-pointer-value ptr))
  (let ((func (lambda () 99)))
    (with-stable-pointer (ptr func)
      (is (= 99 (funcall (glib::get-stable-pointer-value ptr))))))))
      
